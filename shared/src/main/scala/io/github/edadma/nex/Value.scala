package io.github.edadma.nex

import io.github.edadma.dal.DALNumber
import io.github.edadma.numbers.{Rational, SmallRational}

sealed trait Value:
  def isScalar: Boolean

case class ScalarValue(n: DALNumber) extends Value:
  def isScalar: Boolean = true

case class StringValue(s: String) extends Value:
  def isScalar: Boolean = true

case class FunctionValue(params: List[String], body: Expr, env: Environment) extends Value:
  def isScalar: Boolean = true

case class Builtin(name: String, f: List[Value] => Value) extends Value:
  def isScalar: Boolean = true

case class ComposedFunction(f: Value, g: Value) extends Value:
  def isScalar: Boolean = true

case class ArrayValue(shape: Vector[Int], data: Vector[DALNumber]) extends Value:
  def rank: Int = shape.length
  def size: Int = if shape.isEmpty then 0 else shape.product
  def isScalar: Boolean = false

object ArrayValue:
  def vector(data: Vector[DALNumber]): ArrayValue = ArrayValue(Vector(data.length), data)
  def empty: ArrayValue = ArrayValue(Vector(0), Vector.empty)

object Value:
  def display(v: Value): String = v match
    case ScalarValue(n) => formatNumber(n.value)
    case StringValue(s) => s"\"$s\""
    case FunctionValue(params, _, _) => s"<function(${params.mkString(", ")})>"
    case Builtin(name, _) => s"<$name>"
    case ComposedFunction(_, _) => "<composed>"
    case ArrayValue(shape, data) if shape.length == 1 =>
      s"[${data.map(d => formatNumber(d.value)).mkString(", ")}]"
    case ArrayValue(shape, data) if shape.length == 2 =>
      val rows = shape(0)
      val cols = shape(1)
      val rowStrs = (0 until rows).map { r =>
        val rowData = data.slice(r * cols, (r + 1) * cols)
        s"[${rowData.map(d => formatNumber(d.value)).mkString(", ")}]"
      }
      s"[${rowStrs.mkString(", ")}]"
    case ArrayValue(shape, data) =>
      s"ArrayValue(shape=${shape.mkString("x")}, data=[${data.take(10).map(d => formatNumber(d.value)).mkString(", ")}${if data.length > 10 then ", ..." else ""}])"

  def formatNumber(n: Number): String = n match
    case sr: SmallRational => if sr.isWhole then sr.numerator.toString else s"${sr.numerator}/${sr.denominator}"
    case r: Rational => if r.isWhole then r.numerator.toString else s"${r.numerator}/${r.denominator}"
    case d: java.lang.Double => if d == d.toLong then d.toLong.toString else d.toString
    case other => other.toString
