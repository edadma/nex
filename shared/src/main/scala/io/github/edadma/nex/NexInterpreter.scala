package io.github.edadma.nex

import scala.collection.mutable

// ============================================================================
// Runtime values
// ============================================================================

/** Runtime values handled by the tree-walking interpreter. The shape is
  * deliberately close to the typed AST's [[Type]] lattice so that
  * `evalExpr` can dispatch on TExpr shape and produce a Value of the
  * matching variant.
  *
  * Arrays are mutable [[mutable.ArrayBuffer]] so that index-assignment
  * (`a[i] = x`) can update in place. Rank-2 arrays are INTERNALLY
  * row-major — element (r, c) lives at `buf(r * cols + c)` — even though
  * spec §3.3 specifies column-major. The two reconcile via the prelude:
  * `flatten` and `reshape` (§10.4) are the only externally observable
  * rank-2 ↔ rank-1 conversions, and both perform an explicit column-major
  * re-stride so the user sees column-major-equivalent output. Element
  * access (`m[i, j]`, `m[i]` row slice, transpose, matmul, element-wise
  * ops) is layout-invariant. The internal row-major choice is purely
  * implementation-private and will move to column-major when FFI to
  * LAPACK/BLAS lands — at which point the flatten/reshape paths become
  * identity instead of re-stride.
  */
sealed trait Value

case class VInt(value: Long)                                  extends Value
case class VReal(value: Double)                               extends Value
case class VComplex(re: Double, im: Double)                   extends Value
case class VBool(value: Boolean)                              extends Value
case class VString(value: String)                             extends Value
case object VUnit                                             extends Value
case class VArray1(buf: mutable.ArrayBuffer[Value])           extends Value
case class VArray2(buf: mutable.ArrayBuffer[Value], rows: Int, cols: Int) extends Value
case class VTuple(elems: List[Value])                         extends Value
case class VStruct(name: String, fields: mutable.LinkedHashMap[String, Value]) extends Value

/** Either a user-defined function (params + body + captured env) or a
  * built-in prelude function (Scala lambda).
  */
sealed trait VFunc                                            extends Value
case class VUserFunc(params: List[Symbol], body: TExpr, env: Env) extends VFunc
case class VBuiltin(name: String, fn: List[Value] => Value)   extends VFunc

// ============================================================================
// Environment (top-level so VUserFunc.env doesn't need a path-dependent type)
// ============================================================================

/** A binding cell — using a single-element wrapper means `var` updates
  * and closure capture both see the same slot.
  */
final class Cell(var v: Value)

/** A lexical scope. `outer` is the parent frame (None at the global
  * frame). Bindings are keyed by [[Symbol]] id assigned by the
  * elaborator.
  */
class Env(val outer: Option[Env]):
  val cells: mutable.Map[Int, Cell] = mutable.Map.empty

  def lookup(id: Int): Option[Cell] =
    cells.get(id).orElse(outer.flatMap(_.lookup(id)))

  def define(id: Int, value: Value): Cell =
    val c = new Cell(value)
    cells(id) = c
    c

  /** Bind an existing cell into this scope under the given id. Used by
    * the `mut`-parameter call mechanism so the callee's parameter and
    * the caller's storage share the same Cell — writes through the param
    * are observed by the caller after the call returns.
    */
  def bind(id: Int, cell: Cell): Cell =
    cells(id) = cell
    cell

  def child: Env = new Env(Some(this))

// ============================================================================
// Interpreter exceptions
// ============================================================================

/** Surface trap — runtime failure with a source position. The CLI catches
  * this and prints `pos: trap: msg` to stderr.
  */
class NexTrap(val msg: String, val pos: Option[scala.util.parsing.input.Position])
    extends RuntimeException(msg)

/** Non-local return from a function body. Caught at the function-call
  * boundary in `callFunction`.
  */
private class ReturnException(val value: Value) extends RuntimeException with scala.util.control.NoStackTrace

// ============================================================================
// Interpreter
// ============================================================================

/** Tree-walking interpreter for Nex's typed AST. Construct one per
  * program; call `runProgram` to execute. Streams I/O to `Console.out`
  * by default — pair with `Console.withOut(buf) { ... }` in tests.
  */
class NexInterpreter:

  private val globalEnv = new Env(None)

  /** Cache of struct field-lists keyed by the struct-type Symbol id. The
    * elaborator updates the symbol's `tpe` to a fully-resolved
    * [[TyStruct]] post Pass A, but [[TVarRef]] nodes still embed the
    * Pass-A snapshot — so the interpreter reads from this map instead.
    */
  private val structFields = scala.collection.mutable.Map.empty[Int, List[(String, Type)]]

  // --------------------------------------------------------------------------
  // Public entry
  // --------------------------------------------------------------------------

  /** Run a fully-elaborated program. Top-level declarations are processed
    * in source order: structs and functions are registered as values in
    * the global frame; top-bindings have their RHS evaluated immediately;
    * `def main()` is invoked at the end if present, otherwise the
    * top-level expressions (if any) suffice as the program's effect.
    */
  def runProgram(p: TProgram): Value =
    initializeProgram(p)
    findMain(p) match
      case Some(sym) =>
        val cell = globalEnv.lookup(sym.id).get
        callFunction(cell.v.asInstanceOf[VFunc], Nil, None)
      case None => VUnit

  /** Initialize the program — bind all top-level decls (Pass A) and run
    * every top-level initializer (Pass B for top-bindings). Doesn't call
    * `main`. Public so the test runner can set up the environment and
    * then call test functions individually.
    */
  def initializeProgram(p: TProgram): Unit =
    registerPrelude(p)

    // Two-stage initialization so top-level val/var/const initializers
    // can call any top-level function regardless of source order. All
    // function cells get their VUserFunc values FIRST; THEN binding
    // initializers run. Without this split, a `val x = f()` declared
    // ahead of `def f() = ...` would observe `f` as VUnit and trap.
    val funcInits    = mutable.ListBuffer.empty[() => Unit]
    val bindingInits = mutable.ListBuffer.empty[() => Unit]
    for d <- p.decls do d match
      case f: TFunDecl =>
        val cell = globalEnv.define(f.sym.id, VUnit)
        funcInits += (() => cell.v = VUserFunc(f.params, f.body, globalEnv))
      case s: TStructDecl =>
        structFields(s.sym.id) = s.fields
        globalEnv.define(s.sym.id, VStruct(s.sym.name, mutable.LinkedHashMap.empty))
      case b: TTopBinding =>
        val cell = globalEnv.define(b.sym.id, VUnit)
        bindingInits += (() => cell.v = evalExpr(b.value, globalEnv))
      case _: TModuleDecl | _: TImportDecl => ()

    funcInits.foreach(_())
    bindingInits.foreach(_())

  /** Call a previously-registered top-level function by Symbol id with no
    * arguments. Used by the test runner to invoke each `@test` function
    * after [[initializeProgram]]. Throws `NexTrap` if the function traps.
    */
  def callNullary(sym: Symbol): Value =
    globalEnv.lookup(sym.id) match
      case Some(cell) =>
        cell.v match
          case f: VFunc => callFunction(f, Nil, None)
          case other    => throw new NexTrap(s"`${sym.name}` is not a function: ${formatValue(other)}", None)
      case None => throw new NexTrap(s"`${sym.name}` is not bound in the global environment", None)

  private def findMain(p: TProgram): Option[Symbol] =
    p.decls.collectFirst { case f: TFunDecl if f.sym.name == "main" => f.sym }

  // --------------------------------------------------------------------------
  // Prelude — built-in symbols are bound to VBuiltin closures keyed by
  // their Symbol.id (assigned during elaboration).
  // --------------------------------------------------------------------------

  private def registerPrelude(p: TProgram): Unit =
    for s <- p.symbols.all if s.kind == SymKind.Prelude do
      val v: Value = s.name match
        case "pi"  => VReal(math.Pi)
        case "e"   => VReal(math.E)
        case "inf" => VReal(Double.PositiveInfinity)
        case "nan" => VReal(Double.NaN)
        case "i"   => VComplex(0.0, 1.0)
        case other => VBuiltin(other, preludeFn(other))
      globalEnv.define(s.id, v)

  private def preludeFn(name: String): List[Value] => Value = args => name match
    case "print"  => doPrint(args); VUnit
    case "format" => VString(formatArgs(args))
    case "sqrt"   => unary1(args, "sqrt")(sqrtV)
    case "cbrt"   => unary1(args, "cbrt")(v => VReal(math.cbrt(asReal(v))))
    case "abs"    => unary1(args, "abs")(absV)
    case "sign"   => unary1(args, "sign")(signV)
    // Spec §10.2 line 32: sin, cos, exp, log, sqrt apply to real and
    // complex. The complex branches use the standard analytic
    // extensions; reals fall through to the libm path unchanged.
    case "exp"    =>
      unary1(args, "exp") {
        case VComplex(re, im) =>
          val s = math.exp(re)
          VComplex(s * math.cos(im), s * math.sin(im))
        case v => VReal(math.exp(asReal(v)))
      }
    case "log"    =>
      unary1(args, "log") {
        case VComplex(re, im) =>
          VComplex(0.5 * math.log(re * re + im * im), math.atan2(im, re))
        case v => VReal(math.log(asReal(v)))
      }
    case "log2"   =>
      unary1(args, "log2") {
        case VComplex(re, im) =>
          val ln2 = math.log(2)
          VComplex(0.5 * math.log(re * re + im * im) / ln2, math.atan2(im, re) / ln2)
        case v => VReal(math.log(asReal(v)) / math.log(2))
      }
    case "log10"  =>
      unary1(args, "log10") {
        case VComplex(re, im) =>
          val ln10 = math.log(10)
          VComplex(0.5 * math.log(re * re + im * im) / ln10, math.atan2(im, re) / ln10)
        case v => VReal(math.log10(asReal(v)))
      }
    case "sin"    =>
      unary1(args, "sin") {
        case VComplex(re, im) =>
          VComplex(math.sin(re) * math.cosh(im), math.cos(re) * math.sinh(im))
        case v => VReal(math.sin(asReal(v)))
      }
    case "cos"    =>
      unary1(args, "cos") {
        case VComplex(re, im) =>
          VComplex(math.cos(re) * math.cosh(im), -math.sin(re) * math.sinh(im))
        case v => VReal(math.cos(asReal(v)))
      }
    case "tan"    =>
      unary1(args, "tan") {
        case VComplex(re, im) =>
          // tan(z) = sin(z)/cos(z), expanded for stability across small |im|.
          val sr = math.sin(re); val cr = math.cos(re)
          val sh = math.sinh(im); val ch = math.cosh(im)
          val denom = cr * cr * ch * ch + sr * sr * sh * sh
          VComplex((sr * cr) / denom, (sh * ch) / denom)
        case v => VReal(math.tan(asReal(v)))
      }
    case "asin"   => unary1(args, "asin")(v => VReal(math.asin(asReal(v))))
    case "acos"   => unary1(args, "acos")(v => VReal(math.acos(asReal(v))))
    case "atan"   => unary1(args, "atan")(v => VReal(math.atan(asReal(v))))
    case "atan2"  =>
      args match
        case List(y, x) => VReal(math.atan2(asReal(y), asReal(x)))
        case _          => trap(s"atan2 expects 2 args, got ${args.size}", None)
    case "sinh"   => unary1(args, "sinh")(v => VReal(math.sinh(asReal(v))))
    case "cosh"   => unary1(args, "cosh")(v => VReal(math.cosh(asReal(v))))
    case "tanh"   => unary1(args, "tanh")(v => VReal(math.tanh(asReal(v))))
    case "asinh"  => unary1(args, "asinh")(v => VReal(asinh(asReal(v))))
    case "acosh"  => unary1(args, "acosh")(v => VReal(acosh(asReal(v))))
    case "atanh"  => unary1(args, "atanh")(v => VReal(atanh(asReal(v))))
    case "floor"  => unary1(args, "floor")(v => VReal(math.floor(asReal(v))))
    case "ceil"   => unary1(args, "ceil")(v => VReal(math.ceil(asReal(v))))
    case "round"  => unary1(args, "round")(v => VReal(math.round(asReal(v)).toDouble))
    case "trunc"  => unary1(args, "trunc")(v => VReal(asReal(v).toLong.toDouble))
    case "min"    =>
      args match
        case List(a, b) => if cmpNum(a, b) <= 0 then a else b
        case _          => trap(s"min expects 2 args, got ${args.size}", None)
    case "max"    =>
      args match
        case List(a, b) => if cmpNum(a, b) >= 0 then a else b
        case _          => trap(s"max expects 2 args, got ${args.size}", None)
    case "conj"   =>
      unary1(args, "conj") {
        case VComplex(re, im) => VComplex(re, -im)
        case v                => v // real conjugate is itself
      }
    case "arg"    =>
      unary1(args, "arg") {
        case VComplex(re, im) => VReal(math.atan2(im, re))
        case v                => if asReal(v) >= 0 then VReal(0) else VReal(math.Pi)
      }
    case "length" =>
      unary1(args, "length") {
        case VArray1(b)       => VInt(b.size.toLong)
        case VArray2(_, r, _) => VInt(r.toLong)
        case VString(s)       => VInt(s.length.toLong)
        case v                => trap(s"length: not an array/string: $v", None)
      }
    case "rows" =>
      unary1(args, "rows") {
        case VArray2(_, r, _) => VInt(r.toLong)
        case VArray1(b)       => VInt(b.size.toLong)
        case v                => trap(s"rows: not an array: $v", None)
      }
    case "cols" =>
      unary1(args, "cols") {
        case VArray2(_, _, c) => VInt(c.toLong)
        case VArray1(_)       => VInt(1)
        case v                => trap(s"cols: not an array: $v", None)
      }
    case "shape" =>
      unary1(args, "shape") {
        case VArray1(b)       => VTuple(List(VInt(b.size.toLong)))
        case VArray2(_, r, c) => VTuple(List(VInt(r.toLong), VInt(c.toLong)))
        case v                => trap(s"shape: not an array: $v", None)
      }
    case "sum"     => unary1(args, "sum")(sumV)
    case "product" => unary1(args, "product")(productV)
    case "dot"     =>
      args match
        case List(VArray1(a), VArray1(b)) =>
          if a.size != b.size then trap(s"dot: length mismatch (${a.size} vs ${b.size})", None)
          var acc: Value = VInt(0)
          var i = 0
          while i < a.size do
            acc = addV(acc, mulV(a(i), b(i)))
            i += 1
          acc
        case _ => trap(s"dot expects two rank-1 arrays, got $args", None)
    case "map" =>
      args match
        case List(arr, fn: VFunc) => mapArray(arr, fn)
        case List(fn: VFunc, arr) => mapArray(arr, fn) // forgiving arg order
        case _                     => trap(s"map expects (array, fn)", None)
    case "reduce" =>
      args match
        case List(arr, init, fn: VFunc) => reduceArray(arr, init, fn)
        case _                           => trap(s"reduce expects (array, init, fn)", None)
    case "filter" =>
      args match
        case List(arr: VArray1, fn: VFunc) =>
          val out = mutable.ArrayBuffer.empty[Value]
          for v <- arr.buf do
            callFunction(fn, List(v), None) match
              case VBool(true) => out += v
              case _            => ()
          VArray1(out)
        case _ => trap(s"filter expects (array, fn)", None)
    case "range" =>
      args match
        case List(VInt(lo), VInt(hi)) =>
          val buf = mutable.ArrayBuffer.empty[Value]
          var i   = lo
          while i < hi do { buf += VInt(i); i += 1 }
          VArray1(buf)
        case _ => trap(s"range expects (integer, integer)", None)
    case "enumerate" =>
      args match
        case List(VArray1(b)) =>
          val out = mutable.ArrayBuffer.empty[Value]
          var i   = 0
          while i < b.size do { out += VTuple(List(VInt(i.toLong), b(i))); i += 1 }
          VArray1(out)
        case _ => trap(s"enumerate expects (rank-1 array)", None)
    case "zip" =>
      args match
        case List(VArray1(a), VArray1(b)) =>
          val n   = math.min(a.size, b.size)
          val out = mutable.ArrayBuffer.empty[Value]
          var i   = 0
          while i < n do { out += VTuple(List(a(i), b(i))); i += 1 }
          VArray1(out)
        case _ => trap(s"zip expects two rank-1 arrays", None)
    case "transpose" =>
      args match
        case List(VArray2(b, r, c)) =>
          val out = new mutable.ArrayBuffer[Value](r * c)
          for _ <- 0 until (r * c) do out += VUnit
          var i = 0
          while i < r do
            var j = 0
            while j < c do
              out(j * r + i) = b(i * c + j)
              j += 1
            i += 1
          VArray2(out, c, r)
        case _ => trap(s"transpose expects a rank-2 array", None)
    case "matmul" =>
      args match
        case List(a, b) => matMul(a, b)
        case _          => trap(s"matmul expects 2 args", None)
    case "diag" =>
      args match
        case List(VArray1(b)) =>
          val n   = b.size
          val out = new mutable.ArrayBuffer[Value](n * n)
          for _ <- 0 until n * n do out += VInt(0)
          var i = 0
          while i < n do
            out(i * n + i) = b(i)
            i += 1
          VArray2(out, n, n)
        case _ => trap(s"diag expects a rank-1 array", None)
    case "reshape" =>
      // Spec §10.4 signature: `reshape(a: [T], m: integer, n: integer)`.
      // Treats `a` as a column-major flat buffer of an `m x n` matrix,
      // i.e. input[c * m + r] is the element at (r, c) of the result.
      // Internal storage is row-major (output[r * n + c]).
      args match
        case List(arr, VInt(rL), VInt(cL)) =>
          val flat   = flatten1(arr)
          val rows   = rL.toInt
          val cols   = cL.toInt
          if flat.size != rows * cols then
            trap(s"reshape: size mismatch (${flat.size} into ${rows}×${cols})", None)
          val out = mutable.ArrayBuffer.fill(rows * cols)(VInt(0).asInstanceOf[Value])
          var r = 0
          while r < rows do
            var c = 0
            while c < cols do
              out(r * cols + c) = flat(c * rows + r)
              c += 1
            r += 1
          VArray2(out, rows, cols)
        case _ => trap(s"reshape expects (array, rows: integer, cols: integer)", None)

    case "flatten" =>
      // Spec §10.4: column-major flatten. For a row-major `VArray2(b, r, c)`
      // we materialize `[b[0,0], b[1,0], ..., b[r-1,0], b[0,1], ...]` — i.e.
      // walk columns first, rows inside.
      args match
        case List(VArray1(b))       => VArray1(b.clone())
        case List(VArray2(b, r, c)) =>
          val out = mutable.ArrayBuffer.fill(r * c)(VInt(0).asInstanceOf[Value])
          var idx = 0
          var col = 0
          while col < c do
            var row = 0
            while row < r do
              out(idx) = b(row * c + col)
              idx += 1
              row += 1
            col += 1
          VArray1(out)
        case _ => trap(s"flatten expects 1 arg", None)
    case "zeros" =>
      args match
        case List(VInt(n))                 => VArray1(mutable.ArrayBuffer.fill(n.toInt)(VInt(0).asInstanceOf[Value]))
        case List(VTuple(List(VInt(r), VInt(c)))) =>
          VArray2(mutable.ArrayBuffer.fill(r.toInt * c.toInt)(VInt(0).asInstanceOf[Value]), r.toInt, c.toInt)
        case _ => trap(s"zeros expects integer or (rows, cols)", None)
    case "ones" =>
      args match
        case List(VInt(n))                 => VArray1(mutable.ArrayBuffer.fill(n.toInt)(VInt(1).asInstanceOf[Value]))
        case List(VTuple(List(VInt(r), VInt(c)))) =>
          VArray2(mutable.ArrayBuffer.fill(r.toInt * c.toInt)(VInt(1).asInstanceOf[Value]), r.toInt, c.toInt)
        case _ => trap(s"ones expects integer or (rows, cols)", None)
    case "fill" =>
      args match
        case List(VInt(n), v)                                    => VArray1(mutable.ArrayBuffer.fill(n.toInt)(v))
        case List(VTuple(List(VInt(r), VInt(c))), v)             =>
          VArray2(mutable.ArrayBuffer.fill(r.toInt * c.toInt)(v), r.toInt, c.toInt)
        case _ => trap(s"fill expects (n, v) or ((rows, cols), v)", None)
    case "linspace" =>
      args match
        case List(a, b, VInt(n)) =>
          val lo = asReal(a); val hi = asReal(b); val k = n.toInt
          if k <= 1 then VArray1(mutable.ArrayBuffer(VReal(lo)))
          else
            val step = (hi - lo) / (k - 1)
            val buf  = mutable.ArrayBuffer.empty[Value]
            var i    = 0
            while i < k do { buf += VReal(lo + step * i); i += 1 }
            VArray1(buf)
        case _ => trap(s"linspace expects (a, b, n)", None)
    case "identity" =>
      args match
        case List(VInt(n)) =>
          val k   = n.toInt
          val buf = new mutable.ArrayBuffer[Value](k * k)
          for _ <- 0 until k * k do buf += VInt(0)
          var i = 0
          while i < k do { buf(i * k + i) = VInt(1); i += 1 }
          VArray2(buf, k, k)
        case _ => trap(s"identity expects an integer", None)
    case "to_real"    => unary1(args, "to_real")(v => VReal(asReal(v)))
    case "to_integer" => unary1(args, "to_integer")(v => VInt(asReal(v).toLong))
    case "to_complex" =>
      unary1(args, "to_complex") {
        case VComplex(re, im) => VComplex(re, im)
        case v                => VComplex(asReal(v), 0.0)
      }
    case "assert" =>
      args match
        case List(VBool(true))                  => VUnit
        case List(VBool(false))                 => trap("assertion failed", None)
        case List(VBool(false), VString(msg))   => trap(s"assertion failed: $msg", None)
        case _                                   => trap(s"assert expects (bool) or (bool, string)", None)
    case "assert_eq" =>
      args match
        case List(a, b) =>
          if valueEq(a, b) then VUnit
          else trap(s"assert_eq: ${formatValue(a)} != ${formatValue(b)}", None)
        case _ => trap(s"assert_eq expects 2 args", None)
    case "assert_approx" =>
      args match
        case List(a, b, eps) =>
          val diff = math.abs(asReal(a) - asReal(b))
          if diff <= asReal(eps) then VUnit
          else trap(s"assert_approx: |${asReal(a)} - ${asReal(b)}| = $diff > ${asReal(eps)}", None)
        case _ => trap(s"assert_approx expects (a, b, eps)", None)
    case "assert_traps" =>
      args match
        case List(fn: VFunc) =>
          val caught =
            try { callFunction(fn, Nil, None); None }
            catch case t: NexTrap => Some(t)
          caught match
            case Some(_) => VUnit
            case None    => trap("assert_traps: expected trap, got normal return", None)
        case List(fn: VFunc, VString(expected)) =>
          val caught =
            try { callFunction(fn, Nil, None); None }
            catch case t: NexTrap => Some(t)
          caught match
            case Some(t) =>
              if t.msg.contains(expected) then VUnit
              else trap(s"assert_traps: expected substring `$expected`, got `${t.msg}`", None)
            case None =>
              trap(s"assert_traps: expected trap containing `$expected`, got normal return", None)
        case _ => trap("assert_traps expects (() -> _) or (() -> _, string)", None)
    case other => trap(s"prelude function `$other` is not yet implemented", None)

  private def asinh(x: Double): Double = math.log(x + math.sqrt(x * x + 1))
  private def acosh(x: Double): Double = math.log(x + math.sqrt(x * x - 1))
  private def atanh(x: Double): Double = 0.5 * math.log((1 + x) / (1 - x))

  // --------------------------------------------------------------------------
  // Expression evaluation
  // --------------------------------------------------------------------------

  def evalExpr(e: TExpr, env: Env): Value = e match
    case TIntLit(v, _, _)     => VInt(v)
    case TRealLit(v, _, _)    => VReal(v)
    case TBoolLit(v, _, _)    => VBool(v)
    case TStringLit(v, _, _)  => VString(v)
    case TUnitLit(_)          => VUnit

    case TInterpStringLit(parts, _, _) =>
      val sb = new StringBuilder
      for p <- parts do p match
        case TInterpText(s)   => sb.append(s)
        case TInterpRef(sym)  =>
          env.lookup(sym.id) match
            case Some(c) => sb.append(formatValue(c.v))
            case None    => trap(s"interpolation: undefined `${sym.name}`", e.pos)
        case TInterpExpr(x)   => sb.append(formatValue(evalExpr(x, env)))
        case TInterpRaw(raw)  =>
          // Only reached when the elaborator failed to parse `${raw}` and
          // accumulated the parse error; the program should already have
          // aborted before run-time. Emit something useful if we do hit it.
          sb.append("${").append(raw).append("}")
      VString(sb.toString)

    case TVarRef(sym, _, _) =>
      env.lookup(sym.id) match
        case Some(c) => c.v
        case None    => trap(s"undefined symbol `${sym.name}` (id=${sym.id})", e.pos)

    case TBinOp(op, l, r, p, _) =>
      // short-circuit logical ops
      op match
        case "and" =>
          val lv = evalExpr(l, env)
          lv match
            case VBool(false) => VBool(false)
            case VBool(true)  => evalExpr(r, env)
            case _             => trap(s"and: non-bool left operand", p)
        case "or" =>
          val lv = evalExpr(l, env)
          lv match
            case VBool(true)  => VBool(true)
            case VBool(false) => evalExpr(r, env)
            case _             => trap(s"or: non-bool left operand", p)
        case _ =>
          val lv = evalExpr(l, env)
          val rv = evalExpr(r, env)
          applyBinOp(op, lv, rv, p)

    case TUnaryOp(op, x, p, _) =>
      val v = evalExpr(x, env)
      op match
        case "-"   => negV(v, p)
        case "not" => v match
          case VBool(b) => VBool(!b)
          case _         => trap("not: non-bool operand", p)
        case _ => trap(s"unknown unary op `$op`", p)

    case TJuxtapose(c, b, p, _) =>
      // Stage 3 normally lowers TJuxtapose; treat any survivor as scalar mul.
      applyBinOp("*", evalExpr(c, env), evalExpr(b, env), p)

    case TElementWise(op, l, r, p, _) =>
      elementWise(op, evalExpr(l, env), evalExpr(r, env), p)

    case TBroadcast(scalar, arr, op, scalarFirst, p, _) =>
      broadcast(op, evalExpr(scalar, env), evalExpr(arr, env), scalarFirst, p)

    case TMap(arr, fn, _, _) =>
      mapArray(evalExpr(arr, env), evalExpr(fn, env).asInstanceOf[VFunc])

    case TReduce(arr, init, fn, _, _) =>
      reduceArray(evalExpr(arr, env), evalExpr(init, env), evalExpr(fn, env).asInstanceOf[VFunc])

    case TMatMul(l, r, p, _) =>
      matMul(evalExpr(l, env), evalExpr(r, env), p)

    case TFusedLoop(loopVar, length, body, cols, p, _) =>
      // Materialize an array by evaluating `body` once per flat i in
      // 0..length-1 with `loopVar` bound to i. cols=None → rank-1
      // (VArray1); cols=Some(c) → rank-2 (VArray2(rows=length/c, c)).
      // Introduced by NexFusion; the un-fused TElementWise/TBroadcast/
      // map paths are still valid and produce the same result.
      val n = evalExpr(length, env) match
        case VInt(v) => v.toInt
        case other   => trap(s"TFusedLoop: length not an integer, got ${formatValue(other)}", p)
      val out = mutable.ArrayBuffer.empty[Value]
      var i = 0
      while i < n do
        val frame = env.child
        frame.define(loopVar.id, VInt(i.toLong))
        out += evalExpr(body, frame)
        i += 1
      cols match
        case None => VArray1(out)
        case Some(cExpr) =>
          val c = evalExpr(cExpr, env) match
            case VInt(v) => v.toInt
            case other   => trap(s"TFusedLoop: cols not an integer, got ${formatValue(other)}", p)
          if c <= 0 then trap(s"TFusedLoop: cols must be positive, got $c", p)
          VArray2(out, n / c, c)

    case TFlatIndex(arr, idx, p, _) =>
      // Flat single-element access, rank-agnostic. Used inside fused-loop
      // bodies so the same loop shape works for rank-1 and rank-2 sources.
      val av = evalExpr(arr, env)
      val iv = evalExpr(idx, env) match
        case VInt(v) => v.toInt
        case other   => trap(s"TFlatIndex: idx not an integer, got ${formatValue(other)}", p)
      av match
        case VArray1(b)       => b(iv)
        case VArray2(b, _, _) => b(iv)
        case other            => trap(s"TFlatIndex: not an array: ${formatValue(other)}", p)

    case TCall(callee, args, p, _) =>
      // Struct construction: callee is a TypeName symbol.
      callee match
        case TVarRef(s, _, _) if s.kind == SymKind.TypeName =>
          constructStruct(s, args.map(evalExpr(_, env)), p)
        case _ =>
          val cv = evalExpr(callee, env)
          cv match
            case uf: VUserFunc =>
              // Route user-function calls through the mode-aware path so
              // `mut` parameters get aliased to the caller's storage.
              // The call-site mode check has already validated that
              // mut-position args are l-value-rooted.
              callUserFunctionWithModes(uf, args, callee.tpe, env, p)
            case bf: VBuiltin =>
              callFunction(bf, args.map(evalExpr(_, env)), p)
            case other =>
              trap(s"call: not a function: ${formatValue(other)}", p)

    case TIndex(arr, idx, p, _) =>
      val av = evalExpr(arr, env)
      val iv = idx.map(evalExpr(_, env))
      indexGet(av, iv, p)

    case TSlice2(arr, rowAx, colAx, p, _) =>
      // Spec §4.14 rank-2 slice. Each axis is either:
      //   - TAxisAll: keep the full extent of this axis.
      //   - TAxisIndex(e): collapse this axis to a single position.
      //   - TAxisRange(lo, hi, inclusive): keep a sub-extent.
      // Result rank = number of non-collapsed axes; freshly-owned.
      val av = evalExpr(arr, env)
      val (rows, cols, buf) = av match
        case VArray2(b, r, c) => (r, c, b)
        case other            => trap(s"rank-2 slice requires a rank-2 array, got ${formatValue(other)}", p)

      def resolveAxis(spec: TAxisSpec, extent: Int, label: String): (Int, Int, Boolean) =
        // returns (lo, hi_exclusive, collapsed)
        spec match
          case TAxisAll => (0, extent, false)
          case TAxisIndex(e) =>
            evalExpr(e, env) match
              case VInt(i) =>
                if i < 0 || i >= extent then trap(s"$label index $i out of bounds for extent $extent", p)
                (i.toInt, i.toInt + 1, true)
              case other => trap(s"$label index must be integer, got ${formatValue(other)}", p)
          case TAxisRange(lo, hi, inclusive) =>
            (evalExpr(lo, env), evalExpr(hi, env)) match
              case (VInt(l), VInt(h)) =>
                val lI = l.toInt
                val hExcl = if inclusive then h.toInt + 1 else h.toInt
                if lI < 0 || hExcl > extent || lI > hExcl then
                  trap(s"$label slice [$lI..${if inclusive then "=" else ""}${h}] out of bounds for extent $extent", p)
                (lI, hExcl, false)
              case (l, h) => trap(s"$label slice bounds must be integers, got ${formatValue(l)} and ${formatValue(h)}", p)

      val (rLo, rHi, rCollapsed) = resolveAxis(rowAx, rows, "row")
      val (cLo, cHi, cCollapsed) = resolveAxis(colAx, cols, "col")
      val outRows = rHi - rLo
      val outCols = cHi - cLo

      // Materialize the sub-buffer in row-major order (regardless of
      // spec-§3.3 column-major external storage — that reconciliation
      // lives in flatten/reshape; here we just produce a fresh array).
      val out = mutable.ArrayBuffer.empty[Value]
      var r = rLo
      while r < rHi do
        var c = cLo
        while c < cHi do
          out += buf(r * cols + c)
          c += 1
        r += 1

      (rCollapsed, cCollapsed) match
        case (true, true)   => out.head                              // both collapsed → scalar (rare via this path)
        case (true, false)  => VArray1(out)                          // row collapsed → rank-1 of cols
        case (false, true)  => VArray1(out)                          // col collapsed → rank-1 of rows
        case (false, false) => VArray2(out, outRows, outCols)

    case _: TAxisAllMark =>
      trap("internal: TAxisAllMark survived to interpreter; should be Stage-2-only", e.pos)

    case TClone(arr, p, _) =>
      // Spec §8.3: deep-copy an array. Inserted by NexLifetime at move
      // sites where the source has a later use. The result is a freshly-
      // owned VArray1 / VArray2 with element-by-element copies (scalar
      // values clone trivially; nested arrays would recurse but aren't a
      // v0 surface).
      evalExpr(arr, env) match
        case VArray1(b)       => VArray1(b.clone())
        case VArray2(b, r, c) => VArray2(b.clone(), r, c)
        case other            => other

    case TSlice(arr, lo, hi, inclusive, p, _) =>
      // Spec §4.14: rank-1 slice. Half-open `lo..hi` or closed
      // `lo..=hi`. Out-of-bounds bounds trap. The result is a fresh
      // VArray1 — slicing never aliases the source buffer.
      val av = evalExpr(arr, env)
      val (loI, hiI) = (evalExpr(lo, env), evalExpr(hi, env)) match
        case (VInt(l), VInt(h)) => (l.toInt, h.toInt)
        case (l, h)             => trap(s"slice bounds must be integers, got ${formatValue(l)} and ${formatValue(h)}", p)
      av match
        case VArray1(b) =>
          val upper = if inclusive then hiI + 1 else hiI
          if loI < 0 || upper > b.size || loI > upper then
            trap(s"slice [$loI..${if inclusive then "=" else ""}$hiI] out of bounds for array of size ${b.size}", p)
          VArray1(b.slice(loI, upper).to(mutable.ArrayBuffer))
        case other =>
          trap(s"rank-1 slice requires a rank-1 array, got ${formatValue(other)}", p)

    case TField(receiver, name, p, _) =>
      val rv = evalExpr(receiver, env)
      fieldGet(rv, name, p)

    case TTupleProj(receiver, idx, p, _) =>
      evalExpr(receiver, env) match
        case VTuple(es) if idx < es.size => es(idx)
        case VTuple(es) =>
          trap(s"tuple projection index $idx out of bounds (size ${es.size})", p)
        case other => trap(s"tuple projection on non-tuple value: ${formatValue(other)}", p)

    case TMethodCall(_, name, _, p, _) =>
      // Stage 3 should have lowered this; if not, it's a bug.
      trap(s"interpreter: un-lowered method call `$name`", p)

    case TLambda(params, body, _, _) =>
      VUserFunc(params, body, env)

    case TTuple(elems, _, _) =>
      VTuple(elems.map(evalExpr(_, env)))

    case TArrayLit(elems, _, _) =>
      val vs = elems.map(evalExpr(_, env))
      // rank-2 if every element is itself a VArray1 of the same length
      vs match
        case (VArray1(_) :: _) if vs.forall(_.isInstanceOf[VArray1]) =>
          val rows = vs.map(_.asInstanceOf[VArray1].buf)
          val r    = rows.size
          val c    = rows.head.size
          if rows.exists(_.size != c) then trap("rank-2 array: row length mismatch", None)
          val flat = mutable.ArrayBuffer.empty[Value]
          for row <- rows; v <- row do flat += v
          VArray2(flat, r, c)
        case _ =>
          VArray1(mutable.ArrayBuffer.from(vs))

    case TIf(cond, th, el, p, _) =>
      evalExpr(cond, env) match
        case VBool(true)  => evalExpr(th, env)
        case VBool(false) => el match
          case Some(e2) => evalExpr(e2, env)
          case None     => VUnit
        case other => trap(s"if: non-bool condition: ${formatValue(other)}", p)

    case TFor(loopVars, iter, body, p, _) =>
      val it = evalExpr(iter, env)
      it match
        case VArray1(buf) =>
          val inner = env.child
          // Pre-define each loop var so we can rebind in-place each iteration.
          val cells = loopVars.map(s => inner.define(s.id, VUnit))
          for v <- buf do
            loopVars match
              case s :: Nil => cells.head.v = v
              case _        =>
                v match
                  case VTuple(es) if es.size == loopVars.size =>
                    cells.zip(es).foreach { case (c, x) => c.v = x }
                  case _ =>
                    trap(s"for: tuple destructuring mismatch", p)
            evalExpr(body, inner)
          VUnit
        case other => trap(s"for: iterable is not an array: ${formatValue(other)}", p)

    case TWhile(cond, body, p, _) =>
      var run = true
      while run do
        evalExpr(cond, env) match
          case VBool(true)  => evalExpr(body, env)
          case VBool(false) => run = false
          case other         => trap(s"while: non-bool condition: ${formatValue(other)}", p)
      VUnit

    case TReturn(v, _, _) =>
      val rv = v.map(evalExpr(_, env)).getOrElse(VUnit)
      throw new ReturnException(rv)

    case TAssign(target, value, p, _) =>
      val rhs = evalExpr(value, env)
      assignLValue(target, rhs, env, p)
      VUnit

    case TBlock(items, result, _, _) =>
      val inner = env.child
      for it <- items do it match
        case TBlockBinding(sym, _, v) =>
          inner.define(sym.id, evalExpr(v, inner))
        case TBlockExpr(x) =>
          evalExpr(x, inner)
      evalExpr(result, inner)

  // --------------------------------------------------------------------------
  // Function call / struct construction
  // --------------------------------------------------------------------------

  private[nex] def callFunction(f: VFunc, args: List[Value], p: Option[scala.util.parsing.input.Position]): Value =
    f match
      case VBuiltin(_, fn) => fn(args)
      case VUserFunc(params, body, env) =>
        if params.size != args.size then
          trap(s"arity mismatch: function expects ${params.size}, got ${args.size}", p)
        val frame = env.child
        params.zip(args).foreach { case (s, v) => frame.define(s.id, v) }
        try evalExpr(body, frame)
        catch case r: ReturnException => r.value

  /** Mode-aware user-function call. For each `mut` parameter whose
    * call-site argument is a [[TVarRef]] (or projection thereof) the
    * callee's parameter Cell is aliased to the caller's storage, so the
    * callee's `param = expr` writes through to the caller's variable.
    * Read-mode parameters and non-l-value mut args (which the elaborator's
    * call-site check forbids in well-formed programs) fall back to the
    * by-value path.
    *
    * Phase 1 supports only direct `TVarRef` mut args. Struct-field /
    * array-index / tuple-projection mut args (allowed by the elaborator
    * when rooted at a var/mut) fall back to by-value: the elaborator
    * check accepted them but full by-ref of those slot kinds wants a
    * proper LValueRef abstraction and is left for phase 2.
    */
  private def callUserFunctionWithModes(
      f: VUserFunc,
      args: List[TExpr],
      calleeTpe: Type,
      callerEnv: Env,
      p: Option[scala.util.parsing.input.Position],
  ): Value =
    if f.params.size != args.size then
      trap(s"arity mismatch: function expects ${f.params.size}, got ${args.size}", p)

    val modes: List[ParamMode] = calleeTpe match
      case TyFunc(ps, _) if ps.size == args.size => ps.map(_._2)
      case _                                      => List.fill(args.size)(ParamMode.Read)

    val frame = f.env.child
    f.params.zip(args).zip(modes).foreach { case ((param, argExpr), mode) =>
      mode match
        case ParamMode.Mut =>
          argExpr match
            case TVarRef(s, _, _) =>
              callerEnv.lookup(s.id) match
                case Some(cell) => frame.bind(param.id, cell)
                case None       => frame.define(param.id, evalExpr(argExpr, callerEnv))
            case _ =>
              // Slot-into-struct / array element / tuple-proj — phase 2.
              frame.define(param.id, evalExpr(argExpr, callerEnv))
        case ParamMode.Read =>
          frame.define(param.id, evalExpr(argExpr, callerEnv))
    }
    try evalExpr(f.body, frame)
    catch case r: ReturnException => r.value

  private def constructStruct(s: Symbol, args: List[Value], p: Option[scala.util.parsing.input.Position]): Value =
    val fs = structFields.getOrElse(s.id, s.tpe match
      case TyStruct(_, f) => f
      case _              => trap(s"not a struct type: ${s.tpe}", p)
    )
    if fs.size != args.size then
      trap(s"struct `${s.name}`: expected ${fs.size} args, got ${args.size}", p)
    val m = mutable.LinkedHashMap.empty[String, Value]
    fs.map(_._1).zip(args).foreach { case (k, v) => m(k) = v }
    VStruct(s.name, m)

  // --------------------------------------------------------------------------
  // L-value assignment
  // --------------------------------------------------------------------------

  private def assignLValue(target: TExpr, rhs: Value, env: Env, p: Option[scala.util.parsing.input.Position]): Unit =
    target match
      case TVarRef(sym, _, _) =>
        env.lookup(sym.id) match
          case Some(c) => c.v = rhs
          case None    => trap(s"assign: undefined `${sym.name}`", p)

      case TField(r, name, _, _) =>
        evalExpr(r, env) match
          case VStruct(_, fields) =>
            if !fields.contains(name) then trap(s"assign: no field `$name`", p)
            fields(name) = rhs
          case VComplex(re, im) =>
            // Cannot rebind components of a complex through TField (they're
            // read-only views of a value type). Trap.
            trap(s"cannot assign to `$name` of a complex value", p)
          case other => trap(s"assign field: not a struct: ${formatValue(other)}", p)

      case TIndex(arr, idx, _, _) =>
        val av = evalExpr(arr, env)
        val iv = idx.map(evalExpr(_, env))
        indexSet(av, iv, rhs, p)

      case other => trap(s"invalid assignment target", p)

  // --------------------------------------------------------------------------
  // Arithmetic, comparison, conversions
  // --------------------------------------------------------------------------

  private def applyBinOp(op: String, l: Value, r: Value, p: Option[scala.util.parsing.input.Position]): Value =
    op match
      case "+"   => addV(l, r)
      case "-"   => subV(l, r)
      case "*"   => mulV(l, r)
      case "/"   => divV(l, r, p)
      case "div" => idivV(l, r, p)
      case "%"   => modV(l, r, p)
      case "^"   => powV(l, r)
      case "=="  => VBool(valueEq(l, r))
      case "!="  => VBool(!valueEq(l, r))
      case "<"   => VBool(cmpNum(l, r) < 0)
      case "<="  => VBool(cmpNum(l, r) <= 0)
      case ">"   => VBool(cmpNum(l, r) > 0)
      case ">="  => VBool(cmpNum(l, r) >= 0)
      case ".."  => rangeV(l, r, inclusive = false, p)
      case "..=" => rangeV(l, r, inclusive = true, p)
      case _     => trap(s"unknown binary op `$op`", p)

  private def addV(a: Value, b: Value): Value = (a, b) match
    case (VInt(x), VInt(y))         => VInt(x + y)
    case (VInt(x), VReal(y))        => VReal(x + y)
    case (VReal(x), VInt(y))        => VReal(x + y)
    case (VReal(x), VReal(y))       => VReal(x + y)
    case (VComplex(r, i), VInt(y))  => VComplex(r + y, i)
    case (VInt(x), VComplex(r, i))  => VComplex(x + r, i)
    case (VComplex(r, i), VReal(y)) => VComplex(r + y, i)
    case (VReal(x), VComplex(r, i)) => VComplex(x + r, i)
    case (VComplex(r1, i1), VComplex(r2, i2)) => VComplex(r1 + r2, i1 + i2)
    case (VString(x), y)            => VString(x + formatValue(y))
    case (x, VString(y))            => VString(formatValue(x) + y)
    case _ => throw new NexTrap(s"cannot add ${formatValue(a)} and ${formatValue(b)}", None)

  private def subV(a: Value, b: Value): Value = (a, b) match
    case (VInt(x), VInt(y))         => VInt(x - y)
    case (VInt(x), VReal(y))        => VReal(x - y)
    case (VReal(x), VInt(y))        => VReal(x - y)
    case (VReal(x), VReal(y))       => VReal(x - y)
    case (VComplex(r, i), VInt(y))  => VComplex(r - y, i)
    case (VInt(x), VComplex(r, i))  => VComplex(x - r, -i)
    case (VComplex(r, i), VReal(y)) => VComplex(r - y, i)
    case (VReal(x), VComplex(r, i)) => VComplex(x - r, -i)
    case (VComplex(r1, i1), VComplex(r2, i2)) => VComplex(r1 - r2, i1 - i2)
    case _ => throw new NexTrap(s"cannot subtract ${formatValue(b)} from ${formatValue(a)}", None)

  private def mulV(a: Value, b: Value): Value = (a, b) match
    case (VInt(x), VInt(y))         => VInt(x * y)
    case (VInt(x), VReal(y))        => VReal(x * y)
    case (VReal(x), VInt(y))        => VReal(x * y)
    case (VReal(x), VReal(y))       => VReal(x * y)
    case (VComplex(r, i), VInt(y))  => VComplex(r * y, i * y)
    case (VInt(x), VComplex(r, i))  => VComplex(x * r, x * i)
    case (VComplex(r, i), VReal(y)) => VComplex(r * y, i * y)
    case (VReal(x), VComplex(r, i)) => VComplex(x * r, x * i)
    case (VComplex(r1, i1), VComplex(r2, i2)) =>
      VComplex(r1 * r2 - i1 * i2, r1 * i2 + i1 * r2)
    case _ => throw new NexTrap(s"cannot multiply ${formatValue(a)} and ${formatValue(b)}", None)

  private def divV(a: Value, b: Value, p: Option[scala.util.parsing.input.Position]): Value =
    // `/` is real-division on numerics; int/int → real.
    (a, b) match
      case (_, VInt(0))            => trap("division by zero", p)
      case (_, VReal(0.0))         => trap("division by zero", p)
      case (VInt(x), VInt(y))      => VReal(x.toDouble / y.toDouble)
      case (VInt(x), VReal(y))     => VReal(x / y)
      case (VReal(x), VInt(y))     => VReal(x / y)
      case (VReal(x), VReal(y))    => VReal(x / y)
      case (VComplex(_, _), _) | (_, VComplex(_, _)) => complexDiv(a, b)
      case _ => trap(s"cannot divide ${formatValue(a)} by ${formatValue(b)}", p)

  private def complexDiv(a: Value, b: Value): Value =
    val (ar, ai) = toComplexParts(a)
    val (br, bi) = toComplexParts(b)
    val denom    = br * br + bi * bi
    if denom == 0.0 then throw new NexTrap("complex division by zero", None)
    VComplex((ar * br + ai * bi) / denom, (ai * br - ar * bi) / denom)

  private def toComplexParts(v: Value): (Double, Double) = v match
    case VInt(x)         => (x.toDouble, 0.0)
    case VReal(x)        => (x, 0.0)
    case VComplex(r, i)  => (r, i)
    case _               => throw new NexTrap(s"not numeric: ${formatValue(v)}", None)

  private def idivV(a: Value, b: Value, p: Option[scala.util.parsing.input.Position]): Value = (a, b) match
    case (_, VInt(0))           => trap("integer division by zero", p)
    case (VInt(x), VInt(y))     => VInt(x / y)
    case _ => trap(s"`div` requires integer operands, got ${formatValue(a)}, ${formatValue(b)}", p)

  private def modV(a: Value, b: Value, p: Option[scala.util.parsing.input.Position]): Value = (a, b) match
    case (_, VInt(0))           => trap("`%` by zero", p)
    case (VInt(x), VInt(y))     => VInt(x % y)
    case _ => trap(s"`%` requires integer operands", p)

  private def powV(a: Value, b: Value): Value = (a, b) match
    case (VInt(x), VInt(y)) if y >= 0 => VInt(intPow(x, y))
    case _ =>
      val ar = asReal(a); val br = asReal(b)
      VReal(math.pow(ar, br))

  private def intPow(base: Long, exp: Long): Long =
    var result: Long = 1L
    var b = base; var e = exp
    while e > 0 do
      if (e & 1L) == 1L then result *= b
      e >>= 1
      if e > 0 then b *= b
    result

  private def negV(v: Value, p: Option[scala.util.parsing.input.Position]): Value = v match
    case VInt(x)        => VInt(-x)
    case VReal(x)       => VReal(-x)
    case VComplex(r, i) => VComplex(-r, -i)
    case VArray1(buf)   => VArray1(buf.map(x => negV(x, p)))
    case VArray2(b, r, c) => VArray2(b.map(x => negV(x, p)), r, c)
    case _              => trap(s"cannot negate ${formatValue(v)}", p)

  private def cmpNum(a: Value, b: Value): Int = (a, b) match
    case (VInt(x), VInt(y))   => java.lang.Long.compare(x, y)
    case (VInt(x), VReal(y))  => java.lang.Double.compare(x.toDouble, y)
    case (VReal(x), VInt(y))  => java.lang.Double.compare(x, y.toDouble)
    case (VReal(x), VReal(y)) => java.lang.Double.compare(x, y)
    case _ => throw new NexTrap(s"cannot order ${formatValue(a)} and ${formatValue(b)}", None)

  private def valueEq(a: Value, b: Value): Boolean = (a, b) match
    case (VInt(x), VInt(y))     => x == y
    case (VReal(x), VReal(y))   => x == y
    case (VInt(x), VReal(y))    => x.toDouble == y
    case (VReal(x), VInt(y))    => x == y.toDouble
    case (VBool(x), VBool(y))   => x == y
    case (VString(x), VString(y)) => x == y
    case (VComplex(r1, i1), VComplex(r2, i2)) => r1 == r2 && i1 == i2
    case (VComplex(r, i), VInt(y))  => i == 0 && r == y.toDouble
    case (VComplex(r, i), VReal(y)) => i == 0 && r == y
    case (VInt(x), VComplex(r, i))  => i == 0 && r == x.toDouble
    case (VReal(x), VComplex(r, i)) => i == 0 && r == x
    case (VUnit, VUnit)         => true
    case (VTuple(xs), VTuple(ys)) => xs.size == ys.size && xs.zip(ys).forall((a, b) => valueEq(a, b))
    case (VArray1(x), VArray1(y)) => x.size == y.size && x.zip(y).forall((a, b) => valueEq(a, b))
    case (VArray2(b1, r1, c1), VArray2(b2, r2, c2)) =>
      r1 == r2 && c1 == c2 && b1.zip(b2).forall((a, b) => valueEq(a, b))
    case (VStruct(n1, f1), VStruct(n2, f2)) =>
      n1 == n2 && f1.keys.toList == f2.keys.toList &&
        f1.values.zip(f2.values).forall((a, b) => valueEq(a, b))
    case _ => false

  private def rangeV(lo: Value, hi: Value, inclusive: Boolean, p: Option[scala.util.parsing.input.Position]): Value =
    (lo, hi) match
      case (VInt(a), VInt(b)) =>
        val buf = mutable.ArrayBuffer.empty[Value]
        var i   = a
        val end = if inclusive then b else b - 1
        while i <= end do { buf += VInt(i); i += 1 }
        VArray1(buf)
      case _ => trap(s"range bounds must be integers", p)

  private def asReal(v: Value): Double = v match
    case VInt(x)        => x.toDouble
    case VReal(x)       => x
    case VComplex(r, 0) => r
    case _              => throw new NexTrap(s"expected numeric, got ${formatValue(v)}", None)

  // --------------------------------------------------------------------------
  // Element-wise and broadcast
  // --------------------------------------------------------------------------

  private def elementWise(op: String, l: Value, r: Value, p: Option[scala.util.parsing.input.Position]): Value =
    (l, r) match
      case (VArray1(a), VArray1(b)) =>
        if a.size != b.size then trap(s"element-wise `$op`: size mismatch (${a.size} vs ${b.size})", p)
        VArray1(a.zip(b).map((x, y) => applyBinOp(op, x, y, p)).to(mutable.ArrayBuffer))
      case (VArray2(a, r1, c1), VArray2(b, r2, c2)) =>
        if r1 != r2 || c1 != c2 then trap(s"element-wise `$op`: shape mismatch", p)
        VArray2(a.zip(b).map((x, y) => applyBinOp(op, x, y, p)).to(mutable.ArrayBuffer), r1, c1)
      case _ => trap(s"element-wise `$op`: not arrays", p)

  private def broadcast(op: String, scalar: Value, arr: Value, scalarFirst: Boolean, p: Option[scala.util.parsing.input.Position]): Value =
    // Order matters for non-commutative ops: `xs - 1` is `x - 1` per
    // element (scalarFirst=false), while `1 - xs` is `1 - x`.
    def step(x: Value): Value =
      if scalarFirst then applyBinOp(op, scalar, x, p)
      else                applyBinOp(op, x, scalar, p)
    arr match
      case VArray1(b)       => VArray1(b.map(step))
      case VArray2(b, r, c) => VArray2(b.map(step), r, c)
      case _                 => trap(s"broadcast: not an array", p)

  // --------------------------------------------------------------------------
  // Indexing
  // --------------------------------------------------------------------------

  private def indexGet(arr: Value, idx: List[Value], p: Option[scala.util.parsing.input.Position]): Value =
    (arr, idx) match
      case (VArray1(b), List(VInt(i))) =>
        if i < 0 || i >= b.size then trap(s"index out of bounds: $i (len=${b.size})", p)
        b(i.toInt)
      case (VArray2(b, r, c), List(VInt(i), VInt(j))) =>
        if i < 0 || i >= r || j < 0 || j >= c then trap(s"index out of bounds: ($i, $j) (shape=$r×$c)", p)
        b(i.toInt * c + j.toInt)
      case (VArray2(b, r, c), List(VInt(i))) =>
        if i < 0 || i >= r then trap(s"index out of bounds: $i (rows=$r)", p)
        val row = mutable.ArrayBuffer.empty[Value]
        var k   = 0
        while k < c do { row += b(i.toInt * c + k); k += 1 }
        VArray1(row)
      case (VString(s), List(VInt(i))) =>
        if i < 0 || i >= s.length then trap(s"string index out of bounds: $i", p)
        VString(s.charAt(i.toInt).toString)
      case _ => trap(s"cannot index ${formatValue(arr)} with ${idx.map(formatValue).mkString(", ")}", p)

  private def indexSet(arr: Value, idx: List[Value], rhs: Value, p: Option[scala.util.parsing.input.Position]): Unit =
    (arr, idx) match
      case (VArray1(b), List(VInt(i))) =>
        if i < 0 || i >= b.size then trap(s"index out of bounds: $i (len=${b.size})", p)
        b(i.toInt) = rhs
      case (VArray2(b, r, c), List(VInt(i), VInt(j))) =>
        if i < 0 || i >= r || j < 0 || j >= c then trap(s"index out of bounds: ($i, $j) (shape=$r×$c)", p)
        b(i.toInt * c + j.toInt) = rhs
      case _ => trap(s"cannot index-assign ${formatValue(arr)}", p)

  // --------------------------------------------------------------------------
  // Field access
  // --------------------------------------------------------------------------

  private def fieldGet(v: Value, name: String, p: Option[scala.util.parsing.input.Position]): Value = v match
    case VStruct(_, fs) =>
      fs.get(name) match
        case Some(x) => x
        case None    => trap(s"no field `$name` on struct", p)
    case VComplex(re, im) =>
      name match
        case "re" => VReal(re)
        case "im" => VReal(im)
        case _     => trap(s"no field `$name` on complex", p)
    case _ => trap(s"cannot access field `$name` on ${formatValue(v)}", p)

  // --------------------------------------------------------------------------
  // Array helpers
  // --------------------------------------------------------------------------

  private def sumV(v: Value): Value = v match
    case VArray1(b)       => b.foldLeft(VInt(0): Value)((acc, x) => addV(acc, x))
    case VArray2(b, _, _) => b.foldLeft(VInt(0): Value)((acc, x) => addV(acc, x))
    case _                 => trap(s"sum: not an array", None)

  private def productV(v: Value): Value = v match
    case VArray1(b)       => b.foldLeft(VInt(1): Value)((acc, x) => mulV(acc, x))
    case VArray2(b, _, _) => b.foldLeft(VInt(1): Value)((acc, x) => mulV(acc, x))
    case _                 => trap(s"product: not an array", None)

  private def absV(v: Value): Value = v match
    case VInt(x)        => VInt(math.abs(x))
    case VReal(x)       => VReal(math.abs(x))
    case VComplex(r, i) => VReal(math.hypot(r, i))
    case _              => trap(s"abs: non-numeric", None)

  private def signV(v: Value): Value = v match
    case VInt(x)  => VInt(java.lang.Long.signum(x).toLong)
    case VReal(x) => VReal(math.signum(x))
    case _        => trap(s"sign: non-numeric", None)

  private def sqrtV(v: Value): Value = v match
    case VComplex(r, i) =>
      val mag = math.hypot(r, i)
      val real = math.sqrt((mag + r) / 2)
      val imag = math.signum(i) * math.sqrt((mag - r) / 2)
      VComplex(real, if i == 0 && r < 0 then math.sqrt(-r) else imag)
    case _ =>
      val x = asReal(v)
      if x < 0 then VComplex(0.0, math.sqrt(-x)) else VReal(math.sqrt(x))

  private def mapArray(arr: Value, fn: VFunc): Value = arr match
    case VArray1(b)       => VArray1(b.map(x => callFunction(fn, List(x), None)))
    case VArray2(b, r, c) => VArray2(b.map(x => callFunction(fn, List(x), None)), r, c)
    case _                 => trap(s"map: not an array", None)

  private def reduceArray(arr: Value, init: Value, fn: VFunc): Value =
    arr match
      case VArray1(b) =>
        var acc = init
        for x <- b do acc = callFunction(fn, List(acc, x), None)
        acc
      case VArray2(b, _, _) =>
        var acc = init
        for x <- b do acc = callFunction(fn, List(acc, x), None)
        acc
      case _ => trap(s"reduce: not an array", None)

  private def matMul(a: Value, b: Value, p: Option[scala.util.parsing.input.Position] = None): Value = (a, b) match
    case (VArray2(ab, ar, ac), VArray2(bb, br, bc)) =>
      if ac != br then trap(s"matmul: shape mismatch ($ar×$ac) @ ($br×$bc)", p)
      val out = new mutable.ArrayBuffer[Value](ar * bc)
      for _ <- 0 until ar * bc do out += VInt(0)
      var i = 0
      while i < ar do
        var j = 0
        while j < bc do
          var acc: Value = VInt(0)
          var k = 0
          while k < ac do
            acc = addV(acc, mulV(ab(i * ac + k), bb(k * bc + j)))
            k += 1
          out(i * bc + j) = acc
          j += 1
        i += 1
      VArray2(out, ar, bc)
    case (VArray2(ab, ar, ac), VArray1(bb)) =>
      if ac != bb.size then trap(s"matmul: shape mismatch ($ar×$ac) @ (${bb.size})", p)
      val out = new mutable.ArrayBuffer[Value](ar)
      var i   = 0
      while i < ar do
        var acc: Value = VInt(0)
        var k = 0
        while k < ac do
          acc = addV(acc, mulV(ab(i * ac + k), bb(k)))
          k += 1
        out += acc
        i += 1
      VArray1(out)
    case (VArray1(ab), VArray2(bb, br, bc)) =>
      if ab.size != br then trap(s"matmul: shape mismatch (${ab.size}) @ ($br×$bc)", p)
      val out = new mutable.ArrayBuffer[Value](bc)
      var j   = 0
      while j < bc do
        var acc: Value = VInt(0)
        var k = 0
        while k < br do
          acc = addV(acc, mulV(ab(k), bb(k * bc + j)))
          k += 1
        out += acc
        j += 1
      VArray1(out)
    case (VArray1(ab), VArray1(bb)) =>
      if ab.size != bb.size then trap(s"matmul: dot length mismatch", p)
      var acc: Value = VInt(0)
      var i = 0
      while i < ab.size do
        acc = addV(acc, mulV(ab(i), bb(i)))
        i += 1
      acc
    case _ => trap(s"matmul: non-array operands", p)

  private def flatten1(v: Value): mutable.ArrayBuffer[Value] = v match
    case VArray1(b)       => b.clone()
    case VArray2(b, _, _) => b.clone()
    case _                 => trap(s"flatten: not an array", None)

  // --------------------------------------------------------------------------
  // I/O and formatting
  // --------------------------------------------------------------------------

  private def doPrint(args: List[Value]): Unit =
    Console.out.println(args.map(formatValue).mkString(" "))

  private def formatArgs(args: List[Value]): String =
    args.map(formatValue).mkString(" ")

  /** Default printable form. Numeric → its literal; arrays → bracketed
    * comma-list; strings → unquoted; structs → `Name { field=value, ... }`.
    */
  def formatValue(v: Value): String = v match
    case VInt(x)        => x.toString
    case VReal(x)       =>
      if x.isNaN then "nan"
      else if x.isInfinite then if x > 0 then "inf" else "-inf"
      else if x == x.toLong.toDouble && math.abs(x) < 1e15 then s"${x.toLong}.0"
      else x.toString
    case VComplex(r, i) =>
      val rs = formatValue(VReal(r))
      val is = formatValue(VReal(math.abs(i)))
      val sign = if i >= 0 then "+" else "-"
      s"$rs$sign${is}i"
    case VBool(b)       => b.toString
    case VString(s)     => s
    case VUnit          => "()"
    case VArray1(b)     => b.map(formatValue).mkString("[", ", ", "]")
    case VArray2(b, r, c) =>
      val rows = for i <- 0 until r yield
        (for j <- 0 until c yield formatValue(b(i * c + j))).mkString("[", ", ", "]")
      rows.mkString("[", ", ", "]")
    case VTuple(es)     => es.map(formatValue).mkString("(", ", ", ")")
    case VStruct(n, fs) => fs.map((k, v) => s"$k=${formatValue(v)}").mkString(s"$n { ", ", ", " }")
    case VBuiltin(n, _) => s"<builtin $n>"
    case VUserFunc(ps, _, _) => s"<func/${ps.size}>"

  // --------------------------------------------------------------------------
  // Misc utilities
  // --------------------------------------------------------------------------

  private def unary1(args: List[Value], name: String)(f: Value => Value): Value =
    args match
      case List(x) => f(x)
      case _        => throw new NexTrap(s"$name expects 1 arg, got ${args.size}", None)

  private def trap(msg: String, p: Option[scala.util.parsing.input.Position]): Nothing =
    throw new NexTrap(msg, p)
