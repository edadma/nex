package io.github.edadma.nex

import io.github.edadma.dal.DALNumber
import io.github.edadma.numbers.{Rational, SmallRational, ComplexBigInt, ComplexRational, ComplexDouble, ComplexBigDecimal}

sealed trait Value:
  def isScalar: Boolean

case class ScalarValue(n: DALNumber) extends Value:
  def isScalar: Boolean = true

case class StringValue(s: String) extends Value:
  def isScalar: Boolean = true

case class FunctionValue(param: String, body: Expr, env: Environment) extends Value:
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
    case FunctionValue(param, _, _) => s"<function($param)>"
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
    case c: ComplexBigInt => formatComplex(c.re.toString, c.im.toString)
    case c: ComplexRational => formatComplex(formatNumber(c.re), formatNumber(c.im))
    case c: ComplexDouble => formatComplex(formatDouble(c.re), formatDouble(c.im))
    case c: ComplexBigDecimal => formatComplex(c.re.toString, c.im.toString)
    case d: java.lang.Double => formatDouble(d)
    case other => other.toString

  private def formatDouble(d: Double): String =
    if d.isWhole then d.toLong.toString else d.toString

  private def formatComplex(re: String, im: String): String =
    if im.startsWith("-") then s"$re${im}i"
    else s"$re+${im}i"
