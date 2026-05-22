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

/** Packed byte buffer — runtime form of `[byte]`. Indexed read widens
  * `buf(i) & 0xFF` to a `VInt`; indexed write requires the right-hand
  * value to land in 0..255 (otherwise the interpreter traps). Slicing
  * copies. No element-wise arithmetic, no broadcasting, no views.
  */
case class VByteArray(buf: mutable.ArrayBuffer[Byte])         extends Value

/** A non-copying borrow into an owned rank-1 array. `buf` aliases the
  * source array's buffer directly; `off` and `len` define the window.
  * Writing through a view updates the source. Lifetime is managed by
  * the JVM GC (Scala holds the source buffer alive as long as any view
  * references it).
  */
case class VArray1View(buf: mutable.ArrayBuffer[Value], off: Int, len: Int, stride: Int = 1) extends Value

case class VArray2(buf: mutable.ArrayBuffer[Value], rows: Int, cols: Int) extends Value

/** A non-copying row-range borrow into an owned rank-2 array. `buf`
  * aliases the source array's row-major buffer directly; `rowOff` is
  * the starting row, `rows` is the row count, `cols` matches the
  * source. The flat region `buf[rowOff*cols .. (rowOff+rows)*cols)`
  * is the view's window — row-major layout makes any row range
  * contiguous, so no stride field is needed.
  */
/** A non-copying view into an owned rank-2 array. `rowOff`/`colOff`
  * are the offsets of the view's (0, 0) cell within the source's flat
  * row-major buffer; `rowStride` is the pitch between rows in that
  * buffer (the owner's `cols`). The (r, c) element of the view lives
  * at `buf((rowOff + r) * rowStride + colOff + c)`. For row-range
  * views `colOff=0` and `rowStride=cols`; for sub-rectangle views
  * `rowStride > cols` (encoding the gap between visible rows) and
  * `colOff > 0` is allowed.
  */
case class VArray2View(
    buf: mutable.ArrayBuffer[Value],
    rowOff: Int,
    rows: Int,
    cols: Int,
    rowStride: Int,
    colOff: Int,
) extends Value
case class VTuple(elems: List[Value])                         extends Value
case class VStruct(name: String, fields: mutable.LinkedHashMap[String, Value]) extends Value

/** A constructed enum value. `enumName` identifies the parent sum type,
  * `variantName` and `variantIdx` identify which case was constructed,
  * and `fields` carries the variant's positional field values (empty
  * for bare variants).
  */
case class VEnum(enumName: String, variantName: String, variantIdx: Int, fields: List[Value]) extends Value

/** Either a user-defined function (params + body + captured env) or a
  * built-in prelude function (Scala lambda).
  */
sealed trait VFunc                                            extends Value
case class VUserFunc(params: List[Symbol], body: TExpr, env: Env) extends VFunc
case class VBuiltin(name: String, fn: List[Value] => Value)   extends VFunc

/** A callable variant constructor — the value reached when a user
  * references a fielded variant name without applying it. Calling it
  * produces a `VEnum`. Bare variants don't need this wrapper; they're
  * already values.
  */
case class VEnumCtor(
    enumName:    String,
    variantName: String,
    variantIdx:  Int,
    fields:      List[(String, Type)],
) extends VFunc

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
  *
  * The body is split into self-typed sibling traits that compose into
  * this class:
  *
  *   - [[NexInterpPrelude]] — `registerPrelude` and the giant `preludeFn`
  *     match (every `SymKind.Prelude` symbol → its built-in closure).
  *   - [[NexInterpEval]] — `evalExpr`, the user-function call path with
  *     `mut`-parameter aliasing, `@intrinsic` dispatch, L-value
  *     assignment, scalar arithmetic / comparison / range primitives.
  *   - [[NexInterpArrays]] — element-wise / broadcast ops, view
  *     construction, indexed read/write, field access, the
  *     `sum`/`product`/`map`/`reduce`/`matmul` helpers, struct
  *     value-cloning, and value formatting / printing.
  *
  * What stays in this file: per-instance mutable state
  * (`globalEnv` / `structFields` / `enumVariantInfo`), the public
  * entry (`runProgram` / `initializeProgram` / `callNullary`), struct
  * construction, and the trap / arity helpers used everywhere.
  */
class NexInterpreter
    extends NexInterpPrelude
    with NexInterpEval
    with NexInterpArrays:

  protected val globalEnv = new Env(None)

  /** Cache of struct field-lists keyed by the struct-type Symbol id. The
    * elaborator updates the symbol's `tpe` to a fully-resolved
    * [[TyStruct]] post Pass A, but [[TVarRef]] nodes still embed the
    * Pass-A snapshot — so the interpreter reads from this map instead.
    */
  protected val structFields = scala.collection.mutable.Map.empty[Int, List[(String, Type)]]

  /** Map from each variant Symbol id to its (parent-enum-name, declared
    * tag index, declared fields). Looked up at variant construction
    * sites and at pattern-match dispatch. Populated when the
    * [[TEnumDecl]] is processed during program initialization.
    */
  protected val enumVariantInfo =
    scala.collection.mutable.Map.empty[Int, (String, Int, List[(String, Type)])]

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
    // Source-prelude decls (`@intrinsic` defs from prelude/*.nex) live
    // in `auxDecls`; the interpreter has to bind them in the global
    // environment so a user call to `floor(3.7)` resolves to a
    // VUserFunc with a TIntrinsic body, which evalUserBody then
    // dispatches via [[intrinsicDispatch]]. Walking `allDecls` covers
    // user decls + source-prelude decls in a single pass.
    for d <- p.allDecls do d match
      case f: TFunDecl =>
        val cell = globalEnv.define(f.sym.id, VUnit)
        funcInits += (() => cell.v = VUserFunc(f.params, f.body, globalEnv))
      case s: TStructDecl =>
        structFields(s.sym.id) = s.fields
        globalEnv.define(s.sym.id, VStruct(s.sym.name, mutable.LinkedHashMap.empty))
      case e: TEnumDecl =>
        // Register the enum type itself as a placeholder cell so any
        // accidental TVarRef to the type name resolves to *something*
        // rather than a missing binding. The interesting state lives in
        // the per-variant cells: each variant's Symbol id maps to a
        // value the user can reference by name (bare variants) or call
        // (fielded variants).
        globalEnv.define(e.sym.id, VUnit)
        e.variants.zipWithIndex.foreach { case ((vs, fs), idx) =>
          enumVariantInfo(vs.id) = (e.sym.name, idx, fs)
          val cell = globalEnv.define(vs.id, VUnit)
          if fs.isEmpty then
            // Bare variant — the name *is* a value of the enum type.
            cell.v = VEnum(e.sym.name, vs.name, idx, Nil)
          else
            // Fielded variant — the name is a callable constructor.
            cell.v = VEnumCtor(e.sym.name, vs.name, idx, fs)
        }
      case b: TTopBinding =>
        val cell = globalEnv.define(b.sym.id, VUnit)
        bindingInits += (() => cell.v = cloneStructValue(evalExpr(b.value, globalEnv)))
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
  // Struct construction
  // --------------------------------------------------------------------------

  protected def constructStruct(s: Symbol, args: List[Value], p: Option[scala.util.parsing.input.Position]): Value =
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
  // Misc utilities
  // --------------------------------------------------------------------------

  protected def unary1(args: List[Value], name: String)(f: Value => Value): Value =
    args match
      case List(x) => f(x)
      case _        => throw new NexTrap(s"$name expects 1 arg, got ${args.size}", None)

  protected def trap(msg: String, p: Option[scala.util.parsing.input.Position]): Nothing =
    throw new NexTrap(msg, p)
