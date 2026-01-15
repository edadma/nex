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

  // Initialize unary builtins
  globals.set("iota", Builtins.iota)
  globals.set("sum", Builtins.sum)
  globals.set("product", Builtins.product)
  globals.set("count", Builtins.count)
  globals.set("shape", Builtins.shape)

  // Initialize curried builtins
  globals.set("take", Builtins.take)
  globals.set("drop", Builtins.drop)
  globals.set("range", Builtins.range)
  globals.set("reshape", Builtins.reshape)

  // Initialize math builtins
  globals.set("abs", Builtins.abs)
  globals.set("sqrt", Builtins.sqrt)
  globals.set("floor", Builtins.floor)
  globals.set("ceil", Builtins.ceil)
  globals.set("round", Builtins.round)
  globals.set("sin", Builtins.sin)
  globals.set("cos", Builtins.cos)
  globals.set("tan", Builtins.tan)
  globals.set("asin", Builtins.asin)
  globals.set("acos", Builtins.acos)
  globals.set("atan", Builtins.atan)
  globals.set("exp", Builtins.exp)
  globals.set("ln", Builtins.ln)
  globals.set("pow", Builtins.pow)
  globals.set("mod", Builtins.mod)
  globals.set("min", Builtins.min)
  globals.set("max", Builtins.max)

  // Initialize higher-order curried builtins (map and filter)
  // These need access to the evaluator for applying functions
  globals.set("map", Builtin("map", {
    case List(fn) =>
      Builtin("map(_)", {
        case List(arrVal) =>
          val arr = arrVal match
            case a: ArrayValue => a
            case ScalarValue(n) => ArrayValue.vector(Vector(n))
            case other => throw BuiltinError(s"map requires an array, got ${valueType(other)}")
          val results = arr.data.map { n =>
            applyFunctionValue(fn, ScalarValue(n)) match
              case ScalarValue(r) => r
              case other => throw BuiltinError(s"map function must return scalar, got ${valueType(other)}")
          }
          ArrayValue.vector(results)
        case args => throw BuiltinError(s"map(_) requires 1 argument, got ${args.length}")
      })
    case args => throw BuiltinError(s"map requires 1 argument, got ${args.length}")
  }))

  globals.set("filter", Builtin("filter", {
    case List(pred) =>
      Builtin("filter(_)", {
        case List(arrVal) =>
          val arr = arrVal match
            case a: ArrayValue => a
            case ScalarValue(n) => ArrayValue.vector(Vector(n))
            case other => throw BuiltinError(s"filter requires an array, got ${valueType(other)}")
          val results = arr.data.filter { n =>
            applyFunctionValue(pred, ScalarValue(n)) match
              case ScalarValue(r) => isTruthy(r)
              case other => throw BuiltinError(s"filter predicate must return scalar, got ${valueType(other)}")
          }
          ArrayValue.vector(results)
        case args => throw BuiltinError(s"filter(_) requires 1 argument, got ${args.length}")
      })
    case args => throw BuiltinError(s"filter requires 1 argument, got ${args.length}")
  }))

  def eval(expr: Expr): Value =
    evalIn(expr, globals)

  // Apply a function value to an argument (used by map/filter)
  private def applyFunctionValue(fn: Value, arg: Value): Value =
    fn match
      case FunctionValue(param, body, closureEnv) =>
        val callEnv = closureEnv.child
        callEnv.set(param, arg)
        evalIn(body, callEnv)
      case Builtin(_, f) =>
        f(List(arg))
      case ComposedFunction(f, g) =>
        val intermediate = applyFunctionValue(g, arg)
        applyFunctionValue(f, intermediate)
      case _ =>
        throw BuiltinError(s"Expected a function, got ${valueType(fn)}")

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
        case Apply(fnExpr, args) =>
          val fn = evalIn(fnExpr, env)
          fn match
            case Builtin(_, f) =>
              // Evaluate all args and pass to builtin
              val evaluatedArgs = args.map(a => evalIn(a, env))
              f(evaluatedArgs)
            case _ =>
              args.foldLeft(fn) { (currentFn, argExpr) =>
                val argVal = evalIn(argExpr, env)
                applyFunction(currentFn, argVal, expr.pos)
              }
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
      case Builtin(_, f) =>
        f(List(arg))
      case ComposedFunction(f, g) =>
        val intermediate = applyFunction(g, arg, pos)
        applyFunction(f, intermediate, pos)
      case other =>
        throw RuntimeError(s"Cannot apply ${valueType(other)} as function", pos)

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
    case _: Builtin => "function"
    case _: ComposedFunction => "function"
