package io.github.edadma.nex

import io.github.edadma.dal.{ComplexDAL, DALNumber}

case class BuiltinError(msg: String) extends Exception(msg)

object Builtins:
  // Helper to extract int from Value
  private def requireInt(v: Value, name: String): Int =
    v match
      case ScalarValue(n) if n.value.isInstanceOf[Integer] => n.value.intValue
      case _ => throw BuiltinError(s"$name requires an integer argument")

  // Helper to extract array from Value
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

  // ============ Unary builtins (take one arg, return result) ============

  val iota: BuiltinFunction = BuiltinFunction("iota", { v =>
    val count = requireInt(v, "iota")
    if count < 0 then throw BuiltinError("iota requires non-negative argument")
    ArrayValue.vector((0 until count).map(i => DALNumber(i)).toVector)
  })

  val sum: BuiltinFunction = BuiltinFunction("sum", { v =>
    val data = toDataVector(v)
    if data.isEmpty then ScalarValue(DALNumber(0))
    else
      val result = data.reduce((a, b) =>
        ComplexDAL.compute(Symbol("+"), a, b, DALNumber.apply))
      ScalarValue(result)
  })

  val product: BuiltinFunction = BuiltinFunction("product", { v =>
    val data = toDataVector(v)
    if data.isEmpty then ScalarValue(DALNumber(1))
    else
      val result = data.reduce((a, b) =>
        ComplexDAL.compute(Symbol("*"), a, b, DALNumber.apply))
      ScalarValue(result)
  })

  val count: BuiltinFunction = BuiltinFunction("count", { v =>
    val data = toDataVector(v)
    ScalarValue(DALNumber(data.length))
  })

  val shape: BuiltinFunction = BuiltinFunction("shape", { v =>
    v match
      case ScalarValue(_) => ArrayValue.vector(Vector.empty)
      case ArrayValue(sh, _) => ArrayValue.vector(sh.map(i => DALNumber(i)))
      case StringValue(s) => ArrayValue.vector(Vector(DALNumber(s.length)))
      case _: FunctionValue | _: BuiltinFunction | _: MultiArgBuiltin | _: ComposedFunction =>
        throw BuiltinError("shape: cannot get shape of function")
  })

  // ============ Curried builtins (take first arg, return function for second) ============

  val take: BuiltinFunction = BuiltinFunction("take", { nVal =>
    val n = requireInt(nVal, "take")
    BuiltinFunction(s"take($n)", { arrVal =>
      val arr = requireArray(arrVal, "take")
      ArrayValue.vector(arr.data.take(n))
    })
  })

  val drop: BuiltinFunction = BuiltinFunction("drop", { nVal =>
    val n = requireInt(nVal, "drop")
    BuiltinFunction(s"drop($n)", { arrVal =>
      val arr = requireArray(arrVal, "drop")
      ArrayValue.vector(arr.data.drop(n))
    })
  })

  // ============ Multi-arg builtins ============

  val range: MultiArgBuiltin = MultiArgBuiltin("range", 2, {
    case List(startVal, endVal) =>
      val start = requireInt(startVal, "range")
      val end = requireInt(endVal, "range")
      ArrayValue.vector((start until end).map(i => DALNumber(i)).toVector)
    case args => throw BuiltinError(s"range requires 2 arguments, got ${args.length}")
  })

  val reshape: MultiArgBuiltin = MultiArgBuiltin("reshape", 2, {
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
