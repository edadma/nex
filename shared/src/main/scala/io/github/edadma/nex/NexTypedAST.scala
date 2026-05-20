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

/** A kind-parameterized type variable as it appears in a generic `def`
  * signature: `def my_norm[T: Float](xs: [T]) -> T`. The `name` is the
  * source-level identifier (`T`, `A`, etc.) and `constraint` is the
  * closed kind set the variable is allowed to range over.
  *
  * `TyKindVar` only ever appears inside a generic function's signature
  * or body — it is substituted out by the monomorphization pass before
  * codegen runs, so backends never need to handle it.
  */
case class TyKindVar(name: String, constraint: KindConstraint) extends Type

/** Closed set of kinds a `TyKindVar` may range over. Each constraint
  * pins down a hardcoded list of concrete types in [[KindConstraint.members]];
  * v1 keeps the constraint set small and non-extensible so the elaborator's
  * kind-unification check is finite and decidable.
  */
enum KindConstraint:
  case Any
  case Numeric
  case Real
  case Float
  case Complex
  case Inexact

  /** The concrete types this constraint admits, in the current v0 type
    * world. `real`/`real64` are the same physical type until split-precision
    * types (real32, real128) land; the constraint set anticipates that
    * future widening by being expressed as a membership predicate rather
    * than tied to a single canonical type.
    *
    * `Inexact` covers the IEEE-754 continuous-number types — real and
    * complex — without admitting integer. Used by the elementary functions
    * sqrt/log/exp/sin/cos/tan that the spec extends to both. Integer
    * arguments at the call site promote to real before the unifier sees
    * them; see `unifyKindVars`.
    */
  def admits(t: Type): Boolean = (this, t) match
    case (Any,     _)         => true
    case (Numeric, TyInteger) => true
    case (Numeric, TyReal)    => true
    case (Numeric, TyComplex) => true
    case (Real,    TyInteger) => true
    case (Real,    TyReal)    => true
    case (Float,   TyReal)    => true
    case (Complex, TyComplex) => true
    case (Inexact, TyReal)    => true
    case (Inexact, TyComplex) => true
    case _                    => false

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
  *  - `cols = None` — the result is rank-1 (`VArray1`).
  *  - `cols = Some(c)` — the result is rank-2 (`VArray2` with
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

/** Deep-copy of an array per spec §8.3 (auto-clone insertion). Inserted by
  * [[NexLifetime]] at move sites where the source is a `var` array binding
  * that has a later use in the same function, so the move receives a fresh
  * buffer and the original binding remains live. The interpreter deep-
  * copies `VArray1` / `VArray2` element-by-element (rank-2 preserves rows
  * and cols).
  */
case class TClone(arr: TExpr, pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

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

/** Stage-1-only sentinel marking a `:` axis-all inside an index list.
  * Survives through `infExpr`'s TIndex case to `inferIndex`, which is
  * responsible for rewriting the surrounding `TIndex` into a `TSlice2`.
  * It never appears in a Stage-3 program. The `tpe` is `TyUnknown` —
  * `:` is not a value.
  */
case class TAxisAllMark(pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

/** Per-axis spec for a rank-2 slice (spec §4.14). Each axis is either:
  *   - [[TAxisAll]]: the full extent of this axis (`:`) — preserves rank.
  *   - [[TAxisIndex]]: a single integer index — collapses this axis.
  *   - [[TAxisRange]]: a sub-extent (`lo..hi` or `lo..=hi`) — preserves rank.
  */
sealed trait TAxisSpec
case object TAxisAll                                                            extends TAxisSpec
case class  TAxisIndex(idx: TExpr)                                              extends TAxisSpec
case class  TAxisRange(lo: TExpr, hi: TExpr, inclusive: Boolean)                extends TAxisSpec

/** Rank-2 slice — `m[axis0, axis1]` per spec §4.14. The result rank
  * depends on how many axes are preserved (0 = scalar, 1 = rank-1,
  * 2 = rank-2). Result is always a freshly-owned array. Emitted by
  * the elaborator's `inferIndex` when a rank-2 receiver is indexed
  * with a range or axis-all element.
  */
case class TSlice2(
    arr:   TExpr,
    rowAx: TAxisSpec,
    colAx: TAxisSpec,
    pos:   Option[Position] = None,
    tpe:   Type = TyUnknown,
) extends TExpr

/** Stage-1 placeholder for `r.name(args)` before we know whether it
  * resolves to field access or function-call sugar (§4.9). Stage 3 lowers.
  */
case class TMethodCall(receiver: TExpr, name: String, args: List[TExpr], pos: Option[Position] = None, tpe: Type = TyUnknown) extends TExpr

/** Body marker for a function whose implementation is provided by the
  * compiler rather than expressed in Nex source. The `opId` is a dotted
  * name (e.g. `"libm.sqrt"`) keyed against per-backend dispatch tables:
  * the interpreter maps it to a Scala closure, the LLVM backend to an
  * emit-function, etc. Decls bearing an `@intrinsic("opId")` attribute
  * are minted with this body in place of an ordinary `TExpr`.
  *
  * `typeRefs` carries the source-level type-parameter names from any
  * trailing identifier arguments to `@intrinsic("libm.sqrt", T)` — they
  * tell the monomorphization pass which type parameters specialize the
  * opId. After monomorph the list is empty and `opId` is the fully
  * mangled per-type name (e.g. `"libm.sqrt$real"`). A non-generic
  * intrinsic decl (no trailing type-ref args) keeps `typeRefs = Nil`
  * and its `opId` passes through monomorph unchanged.
  */
case class TIntrinsic(opId: String, typeRefs: List[String] = Nil, pos: Option[Position] = None, tpe: Type = TyUnit) extends TExpr

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

/** Elaborated, lowered program.
  *
  *  - `decls` carries the user-written declarations only — what every
  *    walker / test inspecting the user's source should see.
  *  - `auxDecls` carries declarations injected by the toolchain itself,
  *    today exclusively the source-prelude's `@intrinsic` declarations
  *    (`prelude/scalar.nex`'s `cbrt`, `floor`, etc.). They live here so
  *    codegen can pick them up — populating intrinsic dispatch tables,
  *    skipping wrapper emission for collision-prone libm bridges, etc.
  *    — without bloating the user's `decls` list and breaking every
  *    structural assertion in the elaborator test suite.
  */
case class TProgram(
    modulePath: List[String],
    decls:      List[TDecl],
    symbols:    SymbolTable,
    auxDecls:   List[TDecl] = Nil,
):
  /** All declarations the codegen layer needs to see, in elaboration
    * order (aux first so prelude intrinsics are registered before any
    * user call site that references them).
    */
  def allDecls: List[TDecl] = auxDecls ++ decls

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
