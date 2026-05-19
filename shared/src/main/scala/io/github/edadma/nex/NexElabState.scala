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

  /** A scope is a flat map from name → Symbol. Scopes are stacked via the
    * `outer` pointer; lookup walks the chain. Same-block redeclaration is
    * caught at insertion time by checking the local map.
    */
  protected class Scope(val outer: Option[Scope]):
    val bindings: mutable.LinkedHashMap[String, Symbol] = mutable.LinkedHashMap.empty

    def lookup(name: String): Option[Symbol] =
      bindings.get(name).orElse(outer.flatMap(_.lookup(name)))

    def define(name: String, sym: Symbol): Boolean =
      if bindings.contains(name) then false
      else { bindings(name) = sym; true }

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
    // §10.1 Constants
    defineNoError("pi",  SymKind.Prelude, TyReal)
    defineNoError("e",   SymKind.Prelude, TyReal)
    defineNoError("inf", SymKind.Prelude, TyReal)
    defineNoError("nan", SymKind.Prelude, TyReal)
    defineNoError("i",   SymKind.Prelude, TyComplex)

    val preludeFuncs = List(
      // §10.2 scalar math — overloaded names that still need compiler
      // name-based dispatch (sqrt/log/exp/sin/cos/tan accept complex
      // arguments; abs/sign/min/max change return type by arg type).
      // The unambiguous real-only entries (cbrt, floor, ceil, round,
      // trunc, asin, acos, atan, atan2, sinh, cosh, tanh, asinh,
      // acosh, atanh, log2, log10) live in `prelude/scalar.nex` and
      // come in via the source-prelude auto-import.
      "sqrt", "abs", "sign",
      "exp", "log",
      "sin", "cos", "tan",
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
      // §10.5 construction
      "zeros", "ones", "fill", "linspace", "identity",
      // §10.6 I/O
      "print", "format",
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
