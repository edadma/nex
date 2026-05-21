package io.github.edadma.nex

import scala.collection.mutable
import scala.util.parsing.input.{Position, Positional}

/** Shared state, scope/symbol-table infrastructure, prelude registration,
  * error reporting, and abstract cross-trait method declarations for the
  * Nex elaborator. Split out of [[NexElaborator]] so the stage traits
  * (inference, lowering) can mix this in without each owning a copy of
  * the bookkeeping state.
  *
  * Cross-trait dispatch: any method one trait needs to call on another
  * (e.g. `infExpr` in inference invoked from prelude routing, `inferCall`
  * in the prelude path called from lowering) is declared `protected def`
  * here as abstract. The owning trait supplies the body. The class
  * mixes everything in, so the linearization resolves both ends.
  */
protected trait NexElabState:

  // ==========================================================================
  // Errors
  // ==========================================================================

  case class ElabError(message: String, pos: Option[Position]) extends RuntimeException(message):
    override def toString: String =
      pos match
        case Some(p) => s"${p.line}:${p.column}: $message"
        case None    => message

  protected val errors = mutable.ListBuffer.empty[ElabError]

  protected def err(msg: String, p: Positional): Unit =
    errors += ElabError(msg, Some(p.pos))

  protected def err(msg: String, p: Option[Position]): Unit =
    errors += ElabError(msg, p)

  // ==========================================================================
  // Scopes
  // ==========================================================================

  /** A scope is a flat map from name → list of Symbols. Most names bind a
    * single Symbol; multiple Symbols are only valid when ALL of them are
    * `def`s (function overloading). Scopes stack via the `outer` pointer
    * and `lookup` walks the chain.
    *
    * Overloading rules at definition time:
    *   - `val x = 1; val x = 2`              — error (redeclaration)
    *   - `def f(x: real); val f = 1`         — error (mixed kinds)
    *   - `def f(x: real); def f(z: complex)` — accepted (overload set)
    *
    * Signature-disjointness of an overload set is checked at the end of
    * Pass B, when each def's TyFunc is known (Pass A only sees names).
    */
  protected class Scope(val outer: Option[Scope]):
    val bindings: mutable.LinkedHashMap[String, List[Symbol]] = mutable.LinkedHashMap.empty

    /** Return the primary Symbol bound to `name` — the first entry in the
      * overload list, or whatever the singleton binding holds. Most call
      * sites don't care about overloading; the call-resolution path uses
      * `lookupAll` to enumerate every candidate.
      */
    def lookup(name: String): Option[Symbol] =
      lookupAll(name).headOption

    /** Return every Symbol bound to `name` in this scope chain. Singleton
      * bindings come back as a one-element list; overload sets come back
      * with all peers, in declaration order. Walks outer scopes only when
      * the local scope has no binding for `name` — local overload sets
      * SHADOW outer bindings rather than merging across scopes.
      */
    def lookupAll(name: String): List[Symbol] =
      bindings.get(name) match
        case Some(syms) => syms
        case None       => outer.map(_.lookupAll(name)).getOrElse(Nil)

    /** Insert `sym` under `name`. Allows appending to an existing overload
      * set when both the existing entries and `sym` are `SymKind.Function`;
      * otherwise treats any name collision as a redeclaration. Returns
      * `true` on success, `false` if the new binding clashes.
      */
    def define(name: String, sym: Symbol): Boolean =
      bindings.get(name) match
        case None => bindings(name) = List(sym); true
        case Some(existing) =>
          if sym.kind == SymKind.Function && existing.forall(_.kind == SymKind.Function) then
            bindings(name) = existing :+ sym
            true
          else false

  protected var current: Scope = new Scope(None)

  protected def pushScope(): Unit = current = new Scope(Some(current))
  protected def popScope(): Unit =
    current = current.outer.getOrElse(sys.error("elaborator: popped past root scope"))

  protected inline def scoped[T](body: => T): T =
    pushScope()
    try body
    finally popScope()

  // ==========================================================================
  // Symbol table
  // ==========================================================================

  protected val symbols    = new SymbolTable
  protected val paramModes = mutable.Map.empty[Int, ParamMode]

  /** Symbol ids that are legitimate mutation targets — `var` bindings
    * (block-level + top-level) and `mut` parameters. Used by the call-site
    * mode check to verify a `mut`-position argument resolves to something
    * the caller is actually allowed to hand out for mutation.
    */
  protected val mutableSymIds = mutable.Set.empty[Int]

  /** Function symbol id → parallel list of optional default expressions,
    * one entry per parameter (None when the param has no default). Spec
    * §6.5: defaults are captured untouched at decl time and substituted
    * into the call site's argument list when a positional caller omits
    * them or a named caller doesn't supply them. Re-elaborated per call
    * so each call evaluates the default fresh. Populated in `elabFun`.
    */
  protected val paramDefaults = mutable.Map.empty[Int, List[Option[ExprAST]]]

  /** Function symbol id → parallel list of parameter names. Used by the
    * call-site resolver to map `name = expr` (NamedArg) to its position
    * in the param list. Populated in `elabFun`.
    */
  protected val paramNames = mutable.Map.empty[Int, List[String]]

  /** Symbol id → TLambda value, for every `val/var = lambda` binding whose
    * lambda has any `TyUnknown` param. Populated by inferTopBinding /
    * inferBlockItem. Consumed by [[inferArg]] when a TVarRef to such a
    * Symbol appears in an arg position with a concrete expected `TyFunc`:
    * the stored lambda gets re-inferred with the pushed-down types and
    * the entry is replaced with the refined version. Stage 3 lowering
    * then swaps the binding's `value` field in place.
    *
    * Monomorphic by construction — the param-set check `currentType(s) ==
    * TyUnknown` inside the TLambda branch of `inferArg` means only the
    * first refinement sticks. Calling a bound-then-called lambda with
    * conflicting expected types at different sites surfaces as a normal
    * type error from `checkAssignable`, which is the right v0 behaviour.
    */
  protected val deferredLambdas = mutable.Map.empty[Int, TLambda]

  /** Symbol ids of top-level `const` bindings — used by
    * [[validateConstExpr]] to allow references between consts.
    */
  protected val constSymIds = mutable.Set.empty[Int]

  /** Define a name in the current scope, minting a fresh symbol. Returns
    * either the new symbol or the existing one (and records an error).
    */
  protected def define(name: String, kind: SymKind, tpe: Type, where: Positional): Symbol =
    val sym = symbols.mint(name, tpe, kind)
    if !current.define(name, sym) then
      err(s"redeclaration of `$name` in the same scope", where)
    sym

  protected def defineNoError(name: String, kind: SymKind, tpe: Type): Symbol =
    val sym = symbols.mint(name, tpe, kind)
    current.define(name, sym)
    sym

  // ==========================================================================
  // Prelude registration (§10)
  // ==========================================================================

  /** Mints prelude symbols into the root scope. Types are placeholders
    * (TyUnknown for functions whose proper signatures will land in Stage 2).
    */
  protected def registerPrelude(): Unit =
    // §10.1 Constants — `inf`, `nan`, and the imaginary unit `i` remain
    // built-in because Nex has no literal syntax for IEEE-754 specials
    // and the `<number>i`-style complex-construction juxtaposition would
    // create a self-reference if `i` were defined in source. `pi` and
    // `e` live in `prelude/scalar.nex` as `const` decls.
    defineNoError("inf", SymKind.Prelude, TyReal)
    defineNoError("nan", SymKind.Prelude, TyReal)
    defineNoError("i",   SymKind.Prelude, TyComplex)

    val preludeFuncs = List(
      // §10.2 scalar math — only names that vary their return type by
      // argument type still need compiler name-based dispatch:
      //   abs(int) → int / abs(real) → real / abs(complex) → real
      //   sign / min / max similarly.
      // The transcendentals sqrt/exp/log/log2/log10/sin/cos/tan plus
      // the unambiguous real-only entries (cbrt, floor, ceil, round,
      // trunc, asin, acos, atan, atan2, sinh, cosh, tanh, asinh,
      // acosh, atanh, hypot) live in `prelude/scalar.nex` and come in
      // via the source-prelude auto-import. The complex variants are
      // overloaded Nex source defs that compose the real bridges.
      "abs", "sign",
      "min", "max",
      // §10.3 complex
      "conj", "arg",
      // §10.4 array ops
      "length", "sum", "product", "dot",
      "map", "flatMap", "reduce", "filter", "range", "enumerate", "zip",
      "rows", "cols", "shape",
      "transpose", "matmul", "diag",
      "reshape", "flatten",
      "sum_axis",
      // view-style slicing
      "view",
      // §10.5 construction
      "zeros", "ones", "fill", "linspace", "identity",
      // §10.6 I/O
      "print",
      // §10.7 type conversions
      "to_real", "to_integer", "to_complex",
      // §10.8 assertions
      "assert", "assert_eq", "assert_approx", "assert_traps",
    )
    for f <- preludeFuncs do defineNoError(f, SymKind.Prelude, TyUnknown)

  // ==========================================================================
  // Cross-trait abstract methods
  // ==========================================================================
  //
  // These let one trait call into another without taking an explicit
  // dependency on it. The owning trait supplies the body; the class
  // mixes everything in so the linearization closes the loop.

  // Implemented in NexElaborator (Stage 1):
  protected def elabExpr(e: ExprAST): TExpr

  // Implemented in NexElabInference (Stage 2):
  protected def inferProgram(p: TProgram): TProgram
  protected def infExpr(e: TExpr): TExpr
  protected def inferArg(arg: TExpr, expected: Type): TExpr
  protected def inferCall(callee: TExpr, args: List[TExpr], p: Option[Position]): TExpr
  protected def coerceTo(v: TExpr, expected: Type): TExpr
  protected def currentType(s: Symbol): Type
  protected def elemOf(t: Type): Option[(Type, Int)]
  protected def refreshSym(s: Symbol): Symbol

  // Implemented in NexElabLowering (Stage 4):
  protected def lowerProgram(p: TProgram): TProgram
