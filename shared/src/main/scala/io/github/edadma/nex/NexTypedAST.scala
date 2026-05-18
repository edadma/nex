package io.github.edadma.nex

import scala.util.parsing.input.Position

// ============================================================================
// Types
// ============================================================================

/** Type lattice for Nex v0.
  *
  * Stage 1 of the elaborator leaves every expression with [[TyUnknown]];
  * Stage 2 (type inference) fills concrete types in. Element-wise and
  * reduction nodes are first-class so the eventual fusion pass can pattern-
  * match on them at the HIR level — see the elaborator handoff memo.
  */
sealed trait Type

/** Stage-1 placeholder: type is not yet inferred. */
case object TyUnknown extends Type

case object TyBool    extends Type
case object TyInteger extends Type
case object TyReal    extends Type
case object TyComplex extends Type
case object TyUnit    extends Type
case object TyString  extends Type

/** Rank-1 or rank-2 array. `rank` is `1` or `2` in v0; higher ranks are
  * deferred (§11).
  */
case class TyArray(elem: Type, rank: Int) extends Type

case class TyTuple(elems: List[Type]) extends Type

/** Nominal struct type. Field list is resolved from the declaration. */
case class TyStruct(name: String, fields: List[(String, Type)]) extends Type

/** Function type. Each parameter carries its mode (read or mut) per §6.4. */
case class TyFunc(params: List[(Type, ParamMode)], ret: Type) extends Type

// ============================================================================
// Symbols (resolved names)
// ============================================================================

/** A resolved binding. The `id` is a fresh integer assigned by the
  * elaborator; equality is structural so two references to the same binding
  * compare equal. `tpe` is `TyUnknown` after Stage 1.
  */
case class Symbol(id: Int, name: String, tpe: Type, kind: SymKind)

enum SymKind:
  /** Local binding inside a function body (val / var / const). */
  case Local
  /** Function parameter. */
  case Param
  /** Module-level binding (val / var / const). */
  case TopLevel
  /** A `def`. */
  case Function
  /** A `struct`. */
  case TypeName
  /** A module name introduced by `import`. */
  case Module
  /** An imported symbol; carries the imported name. */
  case Import
  /** A built-in name from the standard prelude (§10). */
  case Prelude
  /** A struct field name within destructuring or method dispatch. */
  case Field

// ============================================================================
// Typed expressions
// ============================================================================

sealed trait TExpr:
  def tpe: Type
  def pos: Option[Position]

// -- Literals --------------------------------------------------------------

case class TIntLit(value: Long,    pos: Option[Position] = None, tpe: Type = TyInteger) extends TExpr
case class TRealLit(value: Double, pos: Option[Position] = None, tpe: Type = TyReal)    extends TExpr
case class TBoolLit(value: Boolean,pos: Option[Position] = None, tpe: Type = TyBool)    extends TExpr
case class TStringLit(value: String,pos: Option[Position] = None, tpe: Type = TyString) extends TExpr

/** An interpolated string. `${...}` placeholders carry the raw source
  * fragment in Stage 1; Stage 3 re-parses + elaborates each fragment.
  */
case class TInterpStringLit(parts: List[TInterpPart], pos: Option[Position] = None, tpe: Type = TyString) extends TExpr

sealed trait TInterpPart
case class TInterpText(text: String)        extends TInterpPart
case class TInterpRef(sym: Symbol)          extends TInterpPart
case class TInterpRaw(rawExpr: String)      extends TInterpPart
case class TInterpExpr(expr: TExpr)         extends TInterpPart

case class TUnitLit(pos: Option[Position] = None) extends TExpr:
  val tpe: Type = TyUnit

// -- References ------------------------------------------------------------

case class TVarRef(sym: Symbol, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

// -- Operators -------------------------------------------------------------

case class TBinOp(op: String, lhs: TExpr, rhs: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr
case class TUnaryOp(op: String, operand: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

/** Juxtaposition multiplication placeholder; Stage 3 lowers to TBinOp
  * after types are known.
  */
case class TJuxtapose(coeff: TExpr, body: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

// Element-wise array ops as FIRST-CLASS nodes — the fusion pass will
// pattern-match on these and lower a fused chain into a single loop.
//
// Status as of v0: TElementWise, TBroadcast, and TMatMul are actively
// produced by Stage 2 (inferBinOp rewrites arithmetic over array
// operands). TMap and TReduce are forward-looking *stubs*: the
// elaborator never constructs them, and the prelude `map` / `reduce`
// functions currently dispatch through the ordinary TCall path. The
// stubs are kept (with pass-through cases in lowerExpr and
// walkForMutations) so the eventual fusion pass can introduce them
// without a follow-up typed-AST refactor.
case class TElementWise(op: String, lhs: TExpr, rhs: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr
/** Broadcast a scalar across an array element-wise. `scalarFirst` records
  * which side the scalar was on in the source: `true` for `scalar op arr`,
  * `false` for `arr op scalar`. Non-commutative ops (`-`, `/`, `%`, `div`,
  * `^`, `<`, `<=`, `>`, `>=`) depend on this; without it, `xs - 1` would
  * compute `1 - x` per element.
  */
case class TBroadcast(scalar: TExpr, arr: TExpr, op: String, scalarFirst: Boolean = true, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr
case class TMap(arr: TExpr, fn: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr
case class TReduce(arr: TExpr, init: TExpr, fn: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr
case class TMatMul(lhs: TExpr, rhs: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

/** A fused-loop array producer. The result is an array `[body(i) for i in
  * 0..length]` — `loopVar` is the integer flat index bound during body
  * evaluation; `length` is the TOTAL element count (rows*cols for rank-2);
  * `body` evaluates once per iteration in a scope where `loopVar` resolves
  * to the current flat index. Source arrays are accessed through
  * [[TFlatIndex]] so rank-1 and rank-2 share the same loop shape.
  *
  *  - `cols = None` (chunks 1–4) — the result is rank-1 (`VArray1`).
  *  - `cols = Some(c)` (chunk 5) — the result is rank-2 (`VArray2` with
  *    `rows = length / c`, `cols = c`). The row-major flat layout matches
  *    [[NexInterpreter.VArray2]]'s storage so a single linear loop fills
  *    the buffer in-order.
  *
  * Introduced by [[NexFusion]] (Stage 4, opt-in) as the rewrite target for
  * [[TElementWise]] / [[TBroadcast]] / `map(...)` call sites. A fusion
  * pass that combines chains rewrites nested TFusedLoop expressions into a
  * single loop with merged body.
  */
case class TFusedLoop(
    loopVar: Symbol,
    length:  TExpr,
    body:    TExpr,
    cols:    Option[TExpr] = None,
    pos:     Option[Position] = None,
    tpe:     Type = TyUnknown,
) extends TExpr

/** Flat single-element access into an array regardless of rank — rank-1
  * indexes directly into the buffer; rank-2 indexes into the row-major
  * flat buffer (so `a[i*cols + j]` semantics are the caller's job). Used
  * exclusively by [[NexFusion]] inside [[TFusedLoop]] bodies so the loop
  * can iterate one flat index across both ranks. The interpreter handles
  * both `VArray1` and `VArray2` uniformly: `buf(idx)`.
  */
case class TFlatIndex(arr: TExpr, idx: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

// -- Application / projection ----------------------------------------------

case class TCall(callee: TExpr, args: List[TExpr], pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr
case class TIndex(arr: TExpr, indices: List[TExpr], pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr
case class TField(receiver: TExpr, field: String, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

/** Static tuple projection — picks element `idx` from a tuple-typed
  * receiver. Emitted by the elaborator when lowering `val a, b = pair`
  * into per-name bindings (`a = $tmp.0`, `b = $tmp.1`); not yet
  * surface-syntax-accessible. The receiver is normally a `TVarRef` to a
  * synthetic temp introduced by the same lowering step.
  */
case class TTupleProj(receiver: TExpr, idx: Int, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

/** Rank-1 slice — `arr[lo..hi]` (half-open) or `arr[lo..=hi]` (closed)
  * per spec §4.14. Returns a freshly-owned rank-1 array. `inclusive`
  * encodes whether the upper bound is included. Emitted by the
  * elaborator's `inferIndex` when the lone index expression is a
  * `TBinOp("..", _, _)` / `TBinOp("..=", _, _)`.
  */
case class TSlice(
    arr:       TExpr,
    lo:        TExpr,
    hi:        TExpr,
    inclusive: Boolean,
    pos:       Option[Position] = None,
    tpe:       Type = TyUnknown,
) extends TExpr

/** Stage-1 placeholder for `r.name(args)` before we know whether it
  * resolves to field access or function-call sugar (§4.9). Stage 3 lowers.
  */
case class TMethodCall(receiver: TExpr, name: String, args: List[TExpr], pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

// -- Lambdas ---------------------------------------------------------------

case class TLambda(params: List[Symbol], body: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

// -- Aggregates -----------------------------------------------------------

case class TTuple(elems: List[TExpr], pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr
case class TArrayLit(elems: List[TExpr], pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

// -- Control flow ----------------------------------------------------------

case class TIf(cond: TExpr, thenB: TExpr, elseB: Option[TExpr], pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr
case class TFor(loopVars: List[Symbol], iter: TExpr, body: TExpr, pos: Option[Position] = None, tpe: Type = TyUnit) extends TExpr
case class TWhile(cond: TExpr, body: TExpr, pos: Option[Position] = None, tpe: Type = TyUnit) extends TExpr
case class TReturn(value: Option[TExpr], pos: Option[Position] = None, tpe: Type = TyUnit) extends TExpr
case class TAssign(target: TExpr, value: TExpr, pos: Option[Position] = None, tpe: Type = TyUnit) extends TExpr

// -- Block ----------------------------------------------------------------

case class TBlock(items: List[TBlockItem], result: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

sealed trait TBlockItem
case class TBlockBinding(sym: Symbol, kind: BindingKind, value: TExpr) extends TBlockItem
case class TBlockExpr(expr: TExpr)                                     extends TBlockItem

// ============================================================================
// Typed declarations
// ============================================================================

sealed trait TDecl:
  def pos: Option[Position]

case class TFunDecl(
    sym:        Symbol,
    params:     List[Symbol],
    returnType: Type,
    body:       TExpr,
    isPrivate:  Boolean,
    attributes: List[String],
    pos:        Option[Position] = None,
) extends TDecl

case class TStructDecl(
    sym:       Symbol,
    fields:    List[(String, Type)],
    isPrivate: Boolean,
    pos:       Option[Position] = None,
) extends TDecl

case class TTopBinding(
    sym:   Symbol,
    kind:  BindingKind,
    value: TExpr,
    pos:   Option[Position] = None,
) extends TDecl

case class TModuleDecl(
    path:       List[String],
    isTestOnly: Boolean,
    pos:        Option[Position] = None,
) extends TDecl

case class TImportDecl(
    path:      List[String],
    selectors: List[(Symbol, Option[String])],
    pos:       Option[Position] = None,
) extends TDecl

enum BindingKind:
  case Val
  case Var
  case Const

// ============================================================================
// Program
// ============================================================================

case class TProgram(
    modulePath: List[String],
    decls:      List[TDecl],
    symbols:    SymbolTable,
)

/** A flat registry of every symbol minted during elaboration, indexed by
  * `id`. Useful for debugging and for the interpreter's environment.
  */
class SymbolTable:
  private val byId = scala.collection.mutable.LinkedHashMap.empty[Int, Symbol]
  private var nextId: Int = 0

  def mint(name: String, tpe: Type, kind: SymKind): Symbol =
    val sym = Symbol(nextId, name, tpe, kind)
    byId(nextId) = sym
    nextId += 1
    sym

  /** Replace the symbol at `id` with `sym`. Used when Stage 2 updates a
    * symbol's type after inference.
    */
  def update(sym: Symbol): Unit = byId(sym.id) = sym

  def get(id: Int): Option[Symbol] = byId.get(id)
  def all: Iterable[Symbol]        = byId.values
  def size: Int                    = byId.size
