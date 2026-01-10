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

  // Arity of built-in functions for currying
  private val builtinArity = Map(
    "map" -> 2, "filter" -> 2, "iota" -> 1, "reshape" -> 2,
    "shape" -> 1, "sum" -> 1, "product" -> 1, "count" -> 1,
    "range" -> 2, "take" -> 2, "drop" -> 2
  )

  def eval(expr: Expr): Value =
    evalIn(expr, globals)

  private def evalIn(expr: Expr, env: Environment): Value =
    try
      expr match
        case Num(s) => ScalarValue(parseNumber(s))
        case Str(s) => StringValue(s)
        case Var(name) =>
          env.get(name).orElse {
            // Check if it's a builtin name - return as a curried function
            builtinArity.get(name).map(arity => CurriedBuiltin(name, arity, List()))
          }.getOrElse(throw RuntimeError(s"Undefined variable: $name", expr.pos))
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
        case Pipe(valueExpr, fnExpr) =>
          val value = evalIn(valueExpr, env)
          val fn = evalIn(fnExpr, env)
          applyFunction(fn, value, expr.pos)
        case Compose(fExpr, gExpr) =>
          val f = evalIn(fExpr, env)
          val g = evalIn(gExpr, env)
          ComposedFunction(f, g)
    catch
      case e: BuiltinError => throw RuntimeError(e.msg, expr.pos)

  private def applyFunction(fn: Value, arg: Value, pos: Position): Value =
    fn match
      case FunctionValue(param, body, closureEnv) =>
        val callEnv = closureEnv.child
        callEnv.set(param, arg)
        evalIn(body, callEnv)
      case CurriedBuiltin(name, arity, appliedArgs) =>
        val newArgs = appliedArgs :+ arg
        if newArgs.length == arity then
          if name == "map" || name == "filter" then
            // map and filter need special handling - can't use Builtins.call
            evalHigherOrder(name, newArgs, pos)
          else
            Builtins.call(name, newArgs)
        else
          CurriedBuiltin(name, arity, newArgs)
      case ComposedFunction(f, g) =>
        val intermediate = applyFunction(g, arg, pos)
        applyFunction(f, intermediate, pos)
      case other =>
        throw RuntimeError(s"Cannot apply ${valueType(other)} as function", pos)

  private def evalHigherOrder(name: String, args: List[Value], pos: Position): Value =
    def applyFnValue(fn: Value, arg: Value): Value =
      fn match
        case f: FunctionValue =>
          val callEnv = f.env.child
          callEnv.set(f.param, arg)
          evalIn(f.body, callEnv)
        case c: ComposedFunction =>
          applyFunction(c, arg, pos)
        case c: CurriedBuiltin =>
          applyFunction(c, arg, pos)
        case _ =>
          throw RuntimeError(s"Expected a function, got ${valueType(fn)}", pos)

    name match
      case "map" =>
        val fn = args(0)
        val arr = args(1) match
          case a: ArrayValue => a
          case ScalarValue(n) => ArrayValue.vector(Vector(n))
          case other => throw RuntimeError(s"map requires an array, got ${valueType(other)}", pos)
        val results = arr.data.map { n =>
          applyFnValue(fn, ScalarValue(n)) match
            case ScalarValue(r) => r
            case other => throw RuntimeError(s"map function must return scalar, got ${valueType(other)}", pos)
        }
        ArrayValue.vector(results)
      case "filter" =>
        val fn = args(0)
        val arr = args(1) match
          case a: ArrayValue => a
          case ScalarValue(n) => ArrayValue.vector(Vector(n))
          case other => throw RuntimeError(s"filter requires an array, got ${valueType(other)}", pos)
        val results = arr.data.filter { n =>
          applyFnValue(fn, ScalarValue(n)) match
            case ScalarValue(r) => isTruthy(r)
            case other => throw RuntimeError(s"filter predicate must return scalar, got ${valueType(other)}", pos)
        }
        ArrayValue.vector(results)
      case _ => throw RuntimeError(s"Unknown higher-order function: $name", pos)

  private def evalCall(name: String, args: List[Expr], env: Environment, pos: Position): Value =
    builtinArity.get(name) match
      case Some(arity) =>
        val evaluatedArgs = args.map(a => evalIn(a, env))
        if evaluatedArgs.length < arity then
          // Partial application
          CurriedBuiltin(name, arity, evaluatedArgs)
        else if evaluatedArgs.length == arity then
          // Full application
          if name == "map" || name == "filter" then
            evalHigherOrder(name, evaluatedArgs, pos)
          else
            Builtins.call(name, evaluatedArgs)
        else
          throw RuntimeError(s"$name expects $arity arguments, got ${evaluatedArgs.length}", pos)
      case None =>
        env.get(name) match
          case Some(FunctionValue(param, body, closureEnv)) =>
            if args.length != 1 then
              throw RuntimeError(s"Function $name expects 1 argument, got ${args.length}", pos)
            val argValue = evalIn(args.head, env)
            val callEnv = closureEnv.child
            callEnv.set(param, argValue)
            evalIn(body, callEnv)
          case Some(curried: CurriedBuiltin) =>
            // Apply arguments to curried builtin
            args.foldLeft(curried: Value) { (fn, argExpr) =>
              applyFunction(fn, evalIn(argExpr, env), pos)
            }
          case Some(composed: ComposedFunction) =>
            if args.length != 1 then
              throw RuntimeError(s"Composed function expects 1 argument, got ${args.length}", pos)
            applyFunction(composed, evalIn(args.head, env), pos)
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
    val comparisonOps = Set(">", "<", ">=", "<=", "==", "!=")
    if comparisonOps.contains(op) then
      evalComparison(op, left, right, pos)
    else
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

  private def evalComparison(op: String, left: Value, right: Value, pos: Position): Value =
    def compare(l: DALNumber, r: DALNumber): DALNumber =
      val cmp = ComplexDAL.compare(l, r)
      val result = op match
        case ">" => cmp > 0
        case "<" => cmp < 0
        case ">=" => cmp >= 0
        case "<=" => cmp <= 0
        case "==" => cmp == 0
        case "!=" => cmp != 0
      DALNumber(if result then 1 else 0)

    (left, right) match
      case (ScalarValue(l), ScalarValue(r)) =>
        ScalarValue(compare(l, r))
      case (ArrayValue(shape, data), ScalarValue(r)) =>
        ArrayValue(shape, data.map(l => compare(l, r)))
      case (ScalarValue(l), ArrayValue(shape, data)) =>
        ArrayValue(shape, data.map(r => compare(l, r)))
      case (ArrayValue(shape1, data1), ArrayValue(shape2, data2)) =>
        if shape1 != shape2 then
          throw RuntimeError(s"Shape mismatch: ${shape1.mkString("x")} vs ${shape2.mkString("x")}", pos)
        ArrayValue(shape1, data1.zip(data2).map((l, r) => compare(l, r)))
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
    case _: CurriedBuiltin => "function"
    case _: ComposedFunction => "function"
