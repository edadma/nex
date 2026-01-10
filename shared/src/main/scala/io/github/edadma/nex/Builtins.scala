package io.github.edadma.nex

import io.github.edadma.dal.{ComplexDAL, DALNumber}

case class BuiltinError(msg: String) extends Exception(msg)

object Builtins:
  def call(name: String, args: List[Value]): Value =
    name match
      case "iota" => iota(args)
      case "reshape" => reshape(args)
      case "shape" => shape(args)
      case "sum" => sum(args)
      case "product" => product(args)
      case "count" => count(args)
      case "range" => range(args)
      case "take" => take(args)
      case "drop" => drop(args)
      case _ => throw BuiltinError(s"Unknown builtin: $name")

  private def requireInt(v: Value, name: String): Int =
    v match
      case ScalarValue(n) if n.value.isInstanceOf[Integer] => n.value.intValue
      case _ => throw BuiltinError(s"$name requires an integer argument")

  private def requireArray(v: Value, name: String): ArrayValue =
    v match
      case arr: ArrayValue => arr
      case _ => throw BuiltinError(s"$name requires an array argument")

  private def toDataVector(v: Value): Vector[DALNumber] =
    v match
      case ScalarValue(n) => Vector(n)
      case ArrayValue(_, data) => data
      case _ => throw BuiltinError("Expected numeric value")

  def iota(args: List[Value]): Value =
    args match
      case List(n) =>
        val count = requireInt(n, "iota")
        if count < 0 then throw BuiltinError("iota requires non-negative argument")
        ArrayValue.vector((0 until count).map(i => DALNumber(i)).toVector)
      case _ => throw BuiltinError("iota requires exactly 1 argument")

  def reshape(args: List[Value]): Value =
    args match
      case List(shapeVal, dataVal) =>
        val shapeArr = requireArray(shapeVal, "reshape")
        val newShape = shapeArr.data.map(n => n.value.intValue).toVector
        val data = toDataVector(dataVal)
        if data.isEmpty then throw BuiltinError("reshape: cannot reshape empty data")
        val totalSize = newShape.product
        val cycledData = Vector.tabulate(totalSize)(i => data(i % data.length))
        ArrayValue(newShape, cycledData)
      case _ => throw BuiltinError("reshape requires exactly 2 arguments")

  def shape(args: List[Value]): Value =
    args match
      case List(v) =>
        v match
          case ScalarValue(_) => ArrayValue.vector(Vector.empty)
          case ArrayValue(sh, _) => ArrayValue.vector(sh.map(i => DALNumber(i)))
          case StringValue(s) => ArrayValue.vector(Vector(DALNumber(s.length)))
          case _: FunctionValue | _: CurriedBuiltin | _: ComposedFunction =>
            throw BuiltinError("shape: cannot get shape of function")
      case _ => throw BuiltinError("shape requires exactly 1 argument")

  def sum(args: List[Value]): Value =
    args match
      case List(v) =>
        val data = toDataVector(v)
        if data.isEmpty then ScalarValue(DALNumber(0))
        else
          val result = data.reduce((a, b) =>
            ComplexDAL.compute(Symbol("+"), a, b, DALNumber.apply))
          ScalarValue(result)
      case _ => throw BuiltinError("sum requires exactly 1 argument")

  def product(args: List[Value]): Value =
    args match
      case List(v) =>
        val data = toDataVector(v)
        if data.isEmpty then ScalarValue(DALNumber(1))
        else
          val result = data.reduce((a, b) =>
            ComplexDAL.compute(Symbol("*"), a, b, DALNumber.apply))
          ScalarValue(result)
      case _ => throw BuiltinError("product requires exactly 1 argument")

  def count(args: List[Value]): Value =
    args match
      case List(v) =>
        val data = toDataVector(v)
        ScalarValue(DALNumber(data.length))
      case _ => throw BuiltinError("count requires exactly 1 argument")

  def range(args: List[Value]): Value =
    args match
      case List(startVal, endVal) =>
        val start = requireInt(startVal, "range")
        val end = requireInt(endVal, "range")
        ArrayValue.vector((start until end).map(i => DALNumber(i)).toVector)
      case _ => throw BuiltinError("range requires exactly 2 arguments")

  def take(args: List[Value]): Value =
    args match
      case List(nVal, arrVal) =>
        val n = requireInt(nVal, "take")
        val arr = requireArray(arrVal, "take")
        ArrayValue.vector(arr.data.take(n))
      case _ => throw BuiltinError("take requires exactly 2 arguments")

  def drop(args: List[Value]): Value =
    args match
      case List(nVal, arrVal) =>
        val n = requireInt(nVal, "drop")
        val arr = requireArray(arrVal, "drop")
        ArrayValue.vector(arr.data.drop(n))
      case _ => throw BuiltinError("drop requires exactly 2 arguments")
