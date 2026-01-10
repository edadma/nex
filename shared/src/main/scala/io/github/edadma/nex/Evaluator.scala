package io.github.edadma.nex

import io.github.edadma.dal.{ComplexDAL, DALNumber, ComplexBigIntType}
import io.github.edadma.numbers.{Rational, SmallRational, ComplexBigInt}
import scala.util.parsing.input.Position

case class RuntimeError(msg: String, pos: Position) extends Exception:
  override def getMessage: String =
    s"Error at line ${pos.line}, column ${pos.column}: $msg\n${pos.longString}"

class Evaluator:
  val globals = new Environment()

  // Initialize built-in constants
  globals.set("i", ScalarValue(DALNumber(ComplexBigIntType, ComplexBigInt(0, 1))))
  globals.set("pi", ScalarValue(DALNumber(math.Pi)))
  globals.set("e", ScalarValue(DALNumber(math.E)))

  def eval(expr: Expr): Value =
    evalIn(expr, globals)

  private def evalIn(expr: Expr, env: Environment): Value =
    try
      expr match
        case Num(s) => ScalarValue(parseNumber(s))
        case Str(s) => StringValue(s)
        case Var(name) =>
          env.get(name).getOrElse(throw RuntimeError(s"Undefined variable: $name", expr.pos))
        case Placeholder() =>
          throw RuntimeError("Placeholder '_' used outside of function context", expr.pos)
        case Lambda(param, body) =>
          FunctionValue(param, body, env)
        case ArrayLit(elems) =>
          val values = elems.map(e => evalIn(e, env))
          val data = values.flatMap {
            case ScalarValue(n) => Vector(n)
            case ArrayValue(_, d) => d
            case other => throw RuntimeError(s"Cannot include ${valueType(other)} in array literal", expr.pos)
          }.toVector
          if data.isEmpty then ArrayValue.empty
          else ArrayValue.vector(data)
        case BinOp(op, left, right) =>
          evalBinOp(op, evalIn(left, env), evalIn(right, env), expr.pos)
        case UnaryOp("-", e) =>
          evalIn(e, env) match
            case ScalarValue(n) =>
              ScalarValue(DALNumber(ComplexDAL.negate(n.typ, n.value)))
            case ArrayValue(shape, data) =>
              ArrayValue(shape, data.map(n => DALNumber(ComplexDAL.negate(n.typ, n.value))))
            case other => throw RuntimeError(s"Cannot negate ${valueType(other)}", expr.pos)
        case UnaryOp(op, _) =>
          throw RuntimeError(s"Unknown unary operator: $op", expr.pos)
        case Call(name, args) =>
          evalCall(name, args, env, expr.pos)
        case Assign(name, e) =>
          val value = evalIn(e, env)
          globals.set(name, value)
          value
    catch
      case e: BuiltinError => throw RuntimeError(e.msg, expr.pos)

  private def evalCall(name: String, args: List[Expr], env: Environment, pos: Position): Value =
    name match
      case "map" => evalMap(args, env, pos)
      case "filter" => evalFilter(args, env, pos)
      case "iota" => Builtins.iota(args.map(a => evalIn(a, env)))
      case "reshape" => Builtins.reshape(args.map(a => evalIn(a, env)))
      case "shape" => Builtins.shape(args.map(a => evalIn(a, env)))
      case "sum" => Builtins.sum(args.map(a => evalIn(a, env)))
      case "product" => Builtins.product(args.map(a => evalIn(a, env)))
      case "count" => Builtins.count(args.map(a => evalIn(a, env)))
      case "range" => Builtins.range(args.map(a => evalIn(a, env)))
      case "take" => Builtins.take(args.map(a => evalIn(a, env)))
      case "drop" => Builtins.drop(args.map(a => evalIn(a, env)))
      case _ =>
        env.get(name) match
          case Some(FunctionValue(param, body, closureEnv)) =>
            if args.length != 1 then
              throw RuntimeError(s"Function $name expects 1 argument, got ${args.length}", pos)
            val argValue = evalIn(args.head, env)
            val callEnv = closureEnv.child
            callEnv.set(param, argValue)
            evalIn(body, callEnv)
          case Some(_) =>
            throw RuntimeError(s"$name is not a function", pos)
          case None =>
            throw RuntimeError(s"Unknown function: $name", pos)

  private def evalMap(args: List[Expr], env: Environment, pos: Position): Value =
    args match
      case List(fnExpr, arrExpr) =>
        val fn = evalIn(fnExpr, env) match
          case f: FunctionValue => f
          case other => throw RuntimeError(s"map requires a function, got ${valueType(other)}", pos)
        val arr = evalIn(arrExpr, env) match
          case a: ArrayValue => a
          case ScalarValue(n) => ArrayValue.vector(Vector(n))
          case other => throw RuntimeError(s"map requires an array, got ${valueType(other)}", pos)
        val results = arr.data.map { n =>
          val callEnv = fn.env.child
          callEnv.set(fn.param, ScalarValue(n))
          evalIn(fn.body, callEnv) match
            case ScalarValue(r) => r
            case other => throw RuntimeError(s"map function must return scalar, got ${valueType(other)}", pos)
        }
        ArrayValue.vector(results)
      case _ => throw RuntimeError("map requires exactly 2 arguments", pos)

  private def evalFilter(args: List[Expr], env: Environment, pos: Position): Value =
    args match
      case List(fnExpr, arrExpr) =>
        val fn = evalIn(fnExpr, env) match
          case f: FunctionValue => f
          case other => throw RuntimeError(s"filter requires a function, got ${valueType(other)}", pos)
        val arr = evalIn(arrExpr, env) match
          case a: ArrayValue => a
          case ScalarValue(n) => ArrayValue.vector(Vector(n))
          case other => throw RuntimeError(s"filter requires an array, got ${valueType(other)}", pos)
        val results = arr.data.filter { n =>
          val callEnv = fn.env.child
          callEnv.set(fn.param, ScalarValue(n))
          evalIn(fn.body, callEnv) match
            case ScalarValue(r) => isTruthy(r)
            case other => throw RuntimeError(s"filter predicate must return scalar, got ${valueType(other)}", pos)
        }
        ArrayValue.vector(results)
      case _ => throw RuntimeError("filter requires exactly 2 arguments", pos)

  private def isTruthy(n: DALNumber): Boolean =
    n.value match
      case i: Integer => i != 0
      case l: java.lang.Long => l != 0L
      case d: java.lang.Double => d != 0.0
      case bi: BigInt => bi != 0
      case r: Rational => !r.isZero
      case sr: SmallRational => !sr.isWhole || sr.numerator != 0
      case _ => true

  private def evalBinOp(op: String, left: Value, right: Value, pos: Position): Value =
    (left, right) match
      case (ScalarValue(l), ScalarValue(r)) =>
        val result = ComplexDAL.compute(Symbol(op), l, r, DALNumber.apply)
        ScalarValue(result)
      case (ArrayValue(shape, data), ScalarValue(r)) =>
        ArrayValue(shape, data.map(l => ComplexDAL.compute(Symbol(op), l, r, DALNumber.apply)))
      case (ScalarValue(l), ArrayValue(shape, data)) =>
        ArrayValue(shape, data.map(r => ComplexDAL.compute(Symbol(op), l, r, DALNumber.apply)))
      case (ArrayValue(shape1, data1), ArrayValue(shape2, data2)) =>
        if shape1 != shape2 then
          throw RuntimeError(s"Shape mismatch: ${shape1.mkString("x")} vs ${shape2.mkString("x")}", pos)
        ArrayValue(shape1, data1.zip(data2).map((l, r) => ComplexDAL.compute(Symbol(op), l, r, DALNumber.apply)))
      case _ =>
        throw RuntimeError(s"Cannot apply $op to ${valueType(left)} and ${valueType(right)}", pos)

  private def parseNumber(s: String): DALNumber =
    if s.contains('/') then
      val parts = s.split('/')
      val num = BigInt(parts(0))
      val den = BigInt(parts(1))
      if num.isValidLong && den.isValidLong then
        DALNumber(SmallRational(num.toLong, den.toLong))
      else
        DALNumber(Rational(num, den))
    else if s.contains('.') then
      DALNumber(ComplexDAL.decimal(s).asInstanceOf[Number])
    else
      val n = BigInt(s)
      if n.isValidInt then DALNumber(n.toInt)
      else if n.isValidLong then DALNumber(n.toLong)
      else DALNumber(n)

  private def valueType(v: Value): String = v match
    case _: ScalarValue => "scalar"
    case _: StringValue => "string"
    case _: ArrayValue => "array"
    case _: FunctionValue => "function"
