package io.github.edadma.nex

import io.github.edadma.dal.{DAL, ComplexDAL, DALNumber}

case class BuiltinError(msg: String) extends Exception(msg)

object Builtins:
  // Helper to extract int from Value
  private def requireInt(v: Value, name: String): Int =
    v match
      case ScalarValue(n) if n.value.isInstanceOf[Integer] => n.value.intValue
      case _ => throw BuiltinError(s"$name requires an integer argument")

  // Helper to extract array from Value (scalars become 1-element arrays)
  private def requireArray(v: Value, name: String): ArrayValue =
    v match
      case arr: ArrayValue => arr
      case ScalarValue(n) => ArrayValue.vector(Vector(n))
      case _ => throw BuiltinError(s"$name requires an array argument")

  // Helper to convert Value to data vector
  private def toDataVector(v: Value): Vector[DALNumber] =
    v match
      case ScalarValue(n) => Vector(n)
      case ArrayValue(_, data) => data
      case _ => throw BuiltinError("Expected numeric value")

  // Helper for element-wise unary operations on numbers
  private def mapScalarOrArray(v: Value, f: DALNumber => DALNumber, name: String): Value =
    v match
      case ScalarValue(n) => ScalarValue(f(n))
      case ArrayValue(shape, data) => ArrayValue(shape, data.map(f))
      case _ => throw BuiltinError(s"$name requires a numeric argument")

  // Helper for element-wise unary operations using DAL functions
  private def mapDALFunction(v: Value, f: Any => Number, name: String): Value =
    mapScalarOrArray(v, n => DALNumber(f(n.value)), name)

  // ============ Unary builtins ============

  val iota: Builtin = Builtin("iota", {
    case List(v) =>
      val count = requireInt(v, "iota")
      if count < 0 then throw BuiltinError("iota requires non-negative argument")
      ArrayValue.vector((0 until count).map(i => DALNumber(i)).toVector)
    case args => throw BuiltinError(s"iota requires 1 argument, got ${args.length}")
  })

  val sum: Builtin = Builtin("sum", {
    case List(v) =>
      val data = toDataVector(v)
      if data.isEmpty then ScalarValue(DALNumber(0))
      else
        val result = data.reduce((a, b) =>
          ComplexDAL.compute(Symbol("+"), a, b, DALNumber.apply))
        ScalarValue(result)
    case args => throw BuiltinError(s"sum requires 1 argument, got ${args.length}")
  })

  val product: Builtin = Builtin("product", {
    case List(v) =>
      val data = toDataVector(v)
      if data.isEmpty then ScalarValue(DALNumber(1))
      else
        val result = data.reduce((a, b) =>
          ComplexDAL.compute(Symbol("*"), a, b, DALNumber.apply))
        ScalarValue(result)
    case args => throw BuiltinError(s"product requires 1 argument, got ${args.length}")
  })

  val count: Builtin = Builtin("count", {
    case List(v) =>
      val data = toDataVector(v)
      ScalarValue(DALNumber(data.length))
    case args => throw BuiltinError(s"count requires 1 argument, got ${args.length}")
  })

  val shape: Builtin = Builtin("shape", {
    case List(v) =>
      v match
        case ScalarValue(_) => ArrayValue.vector(Vector.empty)
        case ArrayValue(sh, _) => ArrayValue.vector(sh.map(i => DALNumber(i)))
        case StringValue(s) => ArrayValue.vector(Vector(DALNumber(s.length)))
        case _: FunctionValue | _: Builtin | _: ComposedFunction =>
          throw BuiltinError("shape: cannot get shape of function")
    case args => throw BuiltinError(s"shape requires 1 argument, got ${args.length}")
  })

  // ============ Curried builtins ============

  val take: Builtin = Builtin("take", {
    case List(nVal) =>
      val n = requireInt(nVal, "take")
      Builtin(s"take($n)", {
        case List(arrVal) =>
          val arr = requireArray(arrVal, "take")
          ArrayValue.vector(arr.data.take(n))
        case args => throw BuiltinError(s"take($n) requires 1 argument, got ${args.length}")
      })
    case args => throw BuiltinError(s"take requires 1 argument, got ${args.length}")
  })

  val drop: Builtin = Builtin("drop", {
    case List(nVal) =>
      val n = requireInt(nVal, "drop")
      Builtin(s"drop($n)", {
        case List(arrVal) =>
          val arr = requireArray(arrVal, "drop")
          ArrayValue.vector(arr.data.drop(n))
        case args => throw BuiltinError(s"drop($n) requires 1 argument, got ${args.length}")
      })
    case args => throw BuiltinError(s"drop requires 1 argument, got ${args.length}")
  })

  // ============ Multi-arg builtins ============

  val range: Builtin = Builtin("range", {
    case List(startVal, endVal) =>
      val start = requireInt(startVal, "range")
      val end = requireInt(endVal, "range")
      ArrayValue.vector((start until end).map(i => DALNumber(i)).toVector)
    case args => throw BuiltinError(s"range requires 2 arguments, got ${args.length}")
  })

  val reshape: Builtin = Builtin("reshape", {
    case List(shapeVal, dataVal) =>
      val shapeArr = requireArray(shapeVal, "reshape")
      val newShape = shapeArr.data.map(n => n.value.intValue).toVector
      val data = toDataVector(dataVal)
      if data.isEmpty then throw BuiltinError("reshape: cannot reshape empty data")
      val totalSize = newShape.product
      val cycledData = Vector.tabulate(totalSize)(i => data(i % data.length))
      ArrayValue(newShape, cycledData)
    case args => throw BuiltinError(s"reshape requires 2 arguments, got ${args.length}")
  })

  // ============ Math functions (unary, element-wise) ============

  val abs: Builtin = Builtin("abs", {
    case List(v) => mapDALFunction(v, ComplexDAL.absFunction, "abs")
    case args => throw BuiltinError(s"abs requires 1 argument, got ${args.length}")
  })

  val sqrt: Builtin = Builtin("sqrt", {
    case List(v) => mapDALFunction(v, ComplexDAL.sqrtFunction, "sqrt")
    case args => throw BuiltinError(s"sqrt requires 1 argument, got ${args.length}")
  })

  val floor: Builtin = Builtin("floor", {
    case List(v) => mapDALFunction(v, ComplexDAL.floorFunction, "floor")
    case args => throw BuiltinError(s"floor requires 1 argument, got ${args.length}")
  })

  val ceil: Builtin = Builtin("ceil", {
    case List(v) => mapDALFunction(v, ComplexDAL.ceilFunction, "ceil")
    case args => throw BuiltinError(s"ceil requires 1 argument, got ${args.length}")
  })

  val sin: Builtin = Builtin("sin", {
    case List(v) => mapDALFunction(v, ComplexDAL.sinFunction, "sin")
    case args => throw BuiltinError(s"sin requires 1 argument, got ${args.length}")
  })

  val cos: Builtin = Builtin("cos", {
    case List(v) => mapDALFunction(v, ComplexDAL.cosFunction, "cos")
    case args => throw BuiltinError(s"cos requires 1 argument, got ${args.length}")
  })

  val tan: Builtin = Builtin("tan", {
    case List(v) => mapDALFunction(v, ComplexDAL.tanFunction, "tan")
    case args => throw BuiltinError(s"tan requires 1 argument, got ${args.length}")
  })

  val exp: Builtin = Builtin("exp", {
    case List(v) => mapDALFunction(v, ComplexDAL.expFunction, "exp")
    case args => throw BuiltinError(s"exp requires 1 argument, got ${args.length}")
  })

  val ln: Builtin = Builtin("ln", {
    case List(v) => mapDALFunction(v, ComplexDAL.lnFunction, "ln")
    case args => throw BuiltinError(s"ln requires 1 argument, got ${args.length}")
  })

  val asin: Builtin = Builtin("asin", {
    case List(v) => mapDALFunction(v, ComplexDAL.asinFunction, "asin")
    case args => throw BuiltinError(s"asin requires 1 argument, got ${args.length}")
  })

  val acos: Builtin = Builtin("acos", {
    case List(v) => mapDALFunction(v, ComplexDAL.acosFunction, "acos")
    case args => throw BuiltinError(s"acos requires 1 argument, got ${args.length}")
  })

  val atan: Builtin = Builtin("atan", {
    case List(v) => mapDALFunction(v, ComplexDAL.atanFunction, "atan")
    case args => throw BuiltinError(s"atan requires 1 argument, got ${args.length}")
  })

  // ============ Math functions (binary) ============

  // Helper for binary operations using DAL compute
  private def binaryDALOp(op: Symbol, leftVal: Value, rightVal: Value, name: String): Value =
    (leftVal, rightVal) match
      case (ScalarValue(a), ScalarValue(b)) =>
        ScalarValue(ComplexDAL.compute(op, a, b, DALNumber.apply))
      case (ArrayValue(shape, data), ScalarValue(b)) =>
        ArrayValue(shape, data.map(a => ComplexDAL.compute(op, a, b, DALNumber.apply)))
      case (ScalarValue(a), ArrayValue(shape, data)) =>
        ArrayValue(shape, data.map(b => ComplexDAL.compute(op, a, b, DALNumber.apply)))
      case (ArrayValue(shape1, data1), ArrayValue(shape2, data2)) =>
        if shape1 != shape2 then throw BuiltinError(s"$name: shape mismatch")
        ArrayValue(shape1, data1.zip(data2).map((a, b) =>
          ComplexDAL.compute(op, a, b, DALNumber.apply)))
      case _ => throw BuiltinError(s"$name requires numeric arguments")

  val pow: Builtin = Builtin("pow", {
    case List(baseVal, expVal) => binaryDALOp(Symbol("^"), baseVal, expVal, "pow")
    case args => throw BuiltinError(s"pow requires 2 arguments, got ${args.length}")
  })

  val mod: Builtin = Builtin("mod", {
    case List(aVal, bVal) => binaryDALOp(Symbol("mod"), aVal, bVal, "mod")
    case args => throw BuiltinError(s"mod requires 2 arguments, got ${args.length}")
  })

  // ============ Reduction functions (variable arity) ============

  val min: Builtin = Builtin("min", {
    case List(arr: ArrayValue) =>
      if arr.data.isEmpty then throw BuiltinError("min: empty array")
      val result = arr.data.reduce((a, b) =>
        if ComplexDAL.compare(a, b) <= 0 then a else b)
      ScalarValue(result)
    case List(ScalarValue(n)) => ScalarValue(n)
    case args if args.length >= 2 && args.forall(_.isInstanceOf[ScalarValue]) =>
      val numbers = args.map(_.asInstanceOf[ScalarValue].n)
      val result = numbers.reduce((a, b) =>
        if ComplexDAL.compare(a, b) <= 0 then a else b)
      ScalarValue(result)
    case List(_) => throw BuiltinError("min requires numeric arguments")
    case args => throw BuiltinError(s"min requires at least 1 argument, got ${args.length}")
  })

  val max: Builtin = Builtin("max", {
    case List(arr: ArrayValue) =>
      if arr.data.isEmpty then throw BuiltinError("max: empty array")
      val result = arr.data.reduce((a, b) =>
        if ComplexDAL.compare(a, b) >= 0 then a else b)
      ScalarValue(result)
    case List(ScalarValue(n)) => ScalarValue(n)
    case args if args.length >= 2 && args.forall(_.isInstanceOf[ScalarValue]) =>
      val numbers = args.map(_.asInstanceOf[ScalarValue].n)
      val result = numbers.reduce((a, b) =>
        if ComplexDAL.compare(a, b) >= 0 then a else b)
      ScalarValue(result)
    case List(_) => throw BuiltinError("max requires numeric arguments")
    case args => throw BuiltinError(s"max requires at least 1 argument, got ${args.length}")
  })
