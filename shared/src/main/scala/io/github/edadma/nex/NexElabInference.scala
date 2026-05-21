package io.github.edadma.nex

import scala.util.parsing.input.Position

/** Stage 2 type inference for the Nex elaborator. Walks the Stage-1
  * typed AST, fills in concrete types via bottom-up inference, applies
  * the integer→real→complex promotion lattice (§3.2), rewrites array
  * arithmetic into [[TElementWise]] / [[TBroadcast]] (§4.5), and rewrites
  * `@` into [[TMatMul]] (§4.6).
  *
  * The Stage 2 surface is split across four trait files in this package:
  *
  *   - this file ([[NexElabInference]])        — the program-level driver,
  *                                               bidirectional arg / binding
  *                                               inference, the [[infExpr]]
  *                                               dispatcher, and the
  *                                               binop / arith / compare /
  *                                               call paths.
  *   - [[NexElabInferGenerics]] (Stage 3-β.1)  — kind-variable unification,
  *                                               overload resolution, and
  *                                               the generic-call /
  *                                               generic-struct construction
  *                                               paths.
  *   - [[NexElabInferPrelude]]                 — `const` validation, purity
  *                                               analysis, the hand-rolled
  *                                               bidirectional inference for
  *                                               prelude HOFs and rank-1-only
  *                                               helpers, and the per-name
  *                                               return-type lookup table.
  *   - [[NexElabInferPatterns]]                — match-pattern typing,
  *                                               exhaustiveness, and the
  *                                               index / field inference
  *                                               paths.
  */
protected trait NexElabInference extends NexElabState:

  // ==========================================================================
  // Promotion & type helpers
  // ==========================================================================

  /** Numeric promotion lattice (§3.2): integer → real → complex. Returns
    * the higher of the two types, or `None` if either is non-numeric.
    *
    * Inside a generic body, an operand may carry a `TyKindVar` rather than
    * a concrete numeric. The lattice extends as follows:
    *  - `T op T` (same variable) — result is `T`.
    *  - `T op concrete` / `concrete op T` — result is `T`. After
    *    monomorphization substitutes `T := X`, the result becomes `X`
    *    (which is the same as `promote(X, concrete)` collapsing through
    *    `X`'s position). Strictly this is too narrow when
    *    `promote(X, concrete)` widens (e.g. `T := integer`, concrete = real,
    *    promote should give real), but `Stage 3-β.1` does not insert an
    *    implicit coerce against a kind variable, so the body computes
    *    `X op concrete` natively at runtime and the lattice mismatch
    *    surfaces as an explicit return-type-coercion check at the call
    *    site rather than a silent miscompile. The common pattern in v0
    *    generic code is `T op T`, which this lattice handles exactly.
    *  - Two different `TyKindVar`s — `None`; the elaborator surfaces the
    *    operand mismatch as a normal error.
    */
  protected def promote(a: Type, b: Type): Option[Type] =
    (a, b) match
      case (k1: TyKindVar, k2: TyKindVar) if k1 == k2 => Some(k1)
      case (k: TyKindVar, t) if isNumeric(t)          => Some(k)
      case (t, k: TyKindVar) if isNumeric(t)          => Some(k)
      case _ =>
        def rank(t: Type): Option[Int] = t match
          case TyInteger => Some(0)
          case TyReal    => Some(1)
          case TyComplex => Some(2)
          case _         => None
        for ra <- rank(a); rb <- rank(b) yield
          if ra >= rb then a else b

  /** True iff `t` is one of integer / real / complex, or a `TyKindVar`
    * whose constraint admits at least one numeric type (so the body
    * can be type-checked optimistically; monomorphization later sees a
    * concrete numeric).
    */
  protected def isNumeric(t: Type): Boolean = t match
    case TyInteger | TyReal | TyComplex => true
    case TyKindVar(_, c)                => c.admits(TyInteger) || c.admits(TyReal) || c.admits(TyComplex)
    case _                              => false

  /** Element type of an array, or None if `t` isn't an array type. */
  protected def elemOf(t: Type): Option[(Type, Int)] = t match
    case TyArray(e, r) => Some((e, r))
    case _             => None

  /** Pull the current (post-update) type of a Symbol from the table.
    * Stage 2 mutates [[SymbolTable]] when bindings get inferred, so
    * `sym.tpe` on a captured TVarRef may be stale.
    */
  protected def currentType(s: Symbol): Type =
    symbols.get(s.id).map(_.tpe).getOrElse(s.tpe)

  /** Update the symbol table to reflect `s`'s newly inferred type. */
  protected def setSymType(s: Symbol, t: Type): Symbol =
    val updated = s.copy(tpe = t)
    symbols.update(updated)
    updated

  /** Return the latest Symbol for `s.id` from the symbol table, falling
    * back to `s` if no entry exists. Used to refresh Symbols that were
    * captured in the typed AST during Stage 1 before their type was
    * inferred — without this, ref-style nodes (TVarRef, TInterpRef) carry
    * a stale `.sym.tpe` even after Stage 2 has refined the binding type.
    */
  protected def refreshSym(s: Symbol): Symbol =
    symbols.get(s.id).getOrElse(s)

  // ==========================================================================
  // Bidirectional arg inference
  // ==========================================================================

  /** Bidirectional inference for a single argument position.
    *
    * When the caller knows what type is expected at this position (because
    * the callee is a `TyFunc` with concrete param types, or a binding has
    * a declared type), the expected type is pushed down into the argument
    * before its body is inferred. This is what lets `xs.map(x -> x * 2)`
    * — where `x` carries no annotation — infer `x: integer` from the
    * callee's `(integer -> integer)` parameter type.
    *
    * Special-cased: `TLambda` with at least one TyUnknown param. For
    * everything else this just delegates to [[infExpr]].
    */
  protected def inferArg(arg: TExpr, expected: Type): TExpr = arg match
    case TLambda(params, body, p, _) =>
      expected match
        case TyFunc(eparams, _) if eparams.size == params.size =>
          params.zip(eparams).foreach { case (s, (pt, _)) =>
            if currentType(s) == TyUnknown && pt != TyUnknown then
              setSymType(s, pt)
          }
          val freshParams = params.map(refreshSym)
          val body2       = infExpr(body)
          val ty          = TyFunc(freshParams.map(s => (s.tpe, ParamMode.Read)), body2.tpe)
          TLambda(freshParams, body2, p, ty)
        case _ => infExpr(arg)

    // Deferred resolve: `val f = lam` left lam's params at TyUnknown.
    // First call site that gives a concrete TyFunc refines lam in place
    // — the lambda is re-inferred with pushed-down param types, the
    // deferred map is updated, and the Symbol's tpe is bumped to the
    // refined function type. Stage 3 swaps the refined value back into
    // the binding's `value` field. The refreshed TVarRef is returned so
    // the call site sees the now-concrete function type.
    case TVarRef(s, p, _) if deferredLambdas.contains(s.id) =>
      expected match
        case TyFunc(_, _) =>
          val lam      = deferredLambdas(s.id)
          val refined  = inferArg(lam, expected).asInstanceOf[TLambda]
          deferredLambdas(s.id) = refined
          val freshSym = setSymType(s, refined.tpe)
          TVarRef(freshSym, p, refined.tpe)
        case _ => infExpr(arg)

    // Empty array literal with an expected array type: take the expected
    // element/rank rather than running through `infExpr` (which would
    // error per spec §4.13's "annotation required" rule). This is the
    // happy path for `val x: [real] = []` / `f([])` where f wants `[T]`.
    case TArrayLit(Nil, p, _) =>
      expected match
        case TyArray(_, _) => TArrayLit(Nil, p, expected)
        case _             => infExpr(arg)

    // Non-empty array literal with an expected element type: push the
    // expected element down into each element via [[inferArg]] (which
    // recurses, so nested literals and sub-aggregates compose) and then
    // run each element through [[coerceTo]] so narrower numeric
    // literals fold up to the expected element type. This makes
    //   val xs: [complex] = [1.0, 0.0, 1.0, 0.0]
    // work uniformly without manual `1.0 + 0i` cosmetics.
    case TArrayLit(elems, p, _) =>
      expected match
        case TyArray(elemT, 1) if !hasKindVar(elemT) =>
          val coerced = elems.map { e =>
            val inferred = inferArg(e, elemT)
            coerceTo(inferred, elemT)
          }
          TArrayLit(coerced, p, TyArray(elemT, 1))
        case TyArray(elemT, 2) if !hasKindVar(elemT) =>
          // Rank-2 literal — each row is itself a `[elemT]` array, so
          // recurse with the rank-1 element type.
          val rowT = TyArray(elemT, 1)
          val coerced = elems.map(row => inferArg(row, rowT))
          TArrayLit(coerced, p, TyArray(elemT, 2))
        case _ => infExpr(arg)

    // Bare variant of a generic enum: a reference like `None` produces a
    // `TVarRef` whose declared type still mentions the enum's kind-vars.
    // When the surrounding context (a binding's declared type, a function
    // return type, a function-argument's expected type) pins the concrete
    // enum, push it down so monomorphization can mint the right spec.
    case TVarRef(s, p, _) if s.kind == SymKind.EnumVariant =>
      val refTy = currentType(s)
      (refTy, expected) match
        case (TyEnum(n1, _), e @ TyEnum(n2, _)) if n1 == n2 && hasKindVar(refTy) =>
          TVarRef(s, p, e)
        case _ => infExpr(arg)

    case _ => infExpr(arg)

  /** A lambda is "partially inferred" iff at least one param's type in
    * the symbol table is still TyUnknown. Used to decide whether a
    * `val f = lam` binding goes into [[deferredLambdas]].
    */
  protected def isPartiallyInferredLambda(lam: TLambda): Boolean =
    lam.params.exists(s => currentType(s) == TyUnknown)

  // ==========================================================================
  // Program-level inference
  // ==========================================================================

  protected def inferProgram(p: TProgram): TProgram =
    // Two-pass: first register every function's declared TyFunc shape
    // (params + declared return type, or TyUnknown if not declared),
    // then infer bodies. Without this, mutual recursion broke: when
    // inferring `def isEven(...)` whose body calls `isOdd` (defined
    // later), isOdd's symbol still carried its registration-time type
    // — usually TyUnknown — and the call site captured that stale
    // type. Codegen then emitted `call i64 @isOdd(...)` while the
    // body's other branch had the declared return type, producing
    // phi-node type mismatches.
    p.decls.foreach {
      case f: TFunDecl =>
        val params = f.params.map(pp =>
          (currentType(pp), paramModes.getOrElse(pp.id, ParamMode.Read)),
        )
        setSymType(f.sym, TyFunc(params, f.returnType))
      case _ => ()
    }
    val result = p.copy(decls = p.decls.map(inferDecl))
    // Stash every TFunDecl body so a later module's deferred purity
    // check can reach back through `def`s declared in the prelude or
    // in earlier modules. (The map is module-scope-agnostic — symbol
    // ids are globally unique via `SymbolTable.mint`.)
    result.decls.foreach {
      case f: TFunDecl => allFuncBodies(f.sym.id) = f.body
      case _ => ()
    }
    drainPendingConstPurityChecks()
    result

  protected def inferDecl(d: TDecl): TDecl = d match
    case t: TFunDecl     => inferFun(t)
    case t: TStructDecl  => t  // fields already typed
    case t: TEnumDecl    => t  // variants already typed in Pass B
    case t: TTopBinding  => inferTopBinding(t)
    case t: TModuleDecl  => t
    case t: TImportDecl  => t

  protected def inferFun(f: TFunDecl): TFunDecl =
    // Pre-register the function's TyFunc shape BEFORE inferring its
    // body so any recursive `TVarRef` to `f.sym` inside the body
    // resolves to the right return type. The declared return type is
    // used directly if present; otherwise we register TyUnknown for
    // the return and the second `setSymType` below refines it after
    // the body is inferred.
    val params = f.params.map(p => (currentType(p), paramModes.getOrElse(p.id, ParamMode.Read)))
    setSymType(f.sym, TyFunc(params, f.returnType))

    val body2 = infExpr(f.body)
    val ret =
      if f.returnType != TyUnknown then f.returnType
      else body2.tpe
    val sym2 = setSymType(f.sym, TyFunc(params, ret))
    f.copy(sym = sym2, body = body2, returnType = ret)

  protected def inferTopBinding(b: TTopBinding): TTopBinding =
    val declared = currentType(b.sym)
    val v2 =
      if declared != TyUnknown then inferArg(b.value, declared)
      else inferBindingValue(b.value)
    val (vCoerced, t) = declared match
      case TyUnknown => (v2, v2.tpe)
      case _         => (coerceTo(v2, declared), declared)
    val sym2 = setSymType(b.sym, t)
    registerDeferredLambda(sym2, vCoerced)
    // Spec §5.3: `const` bindings require a constant expression on the
    // RHS. Track every const Symbol id so later const refs to this one
    // can be recognised, and validate the value tree.
    if b.kind == BindingKind.Const then
      constSymIds += sym2.id
      validateConstExpr(vCoerced)
    b.copy(sym = sym2, value = vCoerced)

  protected def registerDeferredLambda(sym: Symbol, v: TExpr): Unit = v match
    case lam: TLambda if isPartiallyInferredLambda(lam) =>
      deferredLambdas(sym.id) = lam
    case _ => ()

  /** Infer the RHS of a `val` / `var` binding with no declared type.
    *
    * Special case for `val f = x -> body` where `body`'s typing depends
    * on `x`'s eventual type (e.g. `s -> s + "!"` — the `+` operator's
    * branch is selected by operand type). Running the body through
    * Stage 2 with `x: TyUnknown` would record real diagnostics like
    * "`+` requires numeric operands, got TyString" that stick in the
    * error list even after the later call-site refinement gives `x`
    * its concrete type. Roll those errors back when the lambda is
    * still partially-inferred after the first pass — the deferred-
    * resolve path re-runs body inference with the refined param
    * types and will report any genuine bugs there.
    */
  protected def inferBindingValue(v: TExpr): TExpr = v match
    case lam: TLambda if lam.params.exists(s => currentType(s) == TyUnknown) =>
      val errSnapshot = errors.size
      val result      = infExpr(lam)
      result match
        case refined: TLambda if isPartiallyInferredLambda(refined) =>
          // The lambda stayed partially inferred — body errors are
          // premature, drop them so refinement can speak authoritatively.
          if errors.size > errSnapshot then
            errors.remove(errSnapshot, errors.size - errSnapshot)
          refined
        case _ => result
    case _ => infExpr(v)

  /** Assignment-compatibility check used at val/var/const sites and at
    * `assignment-expression` sites. If `expected` is numeric and the
    * supplied value's type is also numeric and assignable via promotion,
    * we accept. Otherwise we require structural equality.
    */
  protected def checkAssignable(v: TExpr, expected: Type): Unit =
    val actual = v.tpe
    if actual == TyUnknown || expected == TyUnknown then return
    if actual == expected then return
    (actual, expected) match
      case (a, b) if isNumeric(a) && isNumeric(b) =>
        promote(a, b) match
          case Some(t) if t == b => () // a promotes up to b
          case _                  => err(s"cannot assign value of type $actual to $expected", v.pos)
      case _ =>
        err(s"cannot assign value of type $actual to $expected", v.pos)

  /** Like [[checkAssignable]] but returns the (possibly-coerced) value
    * instead of just validating. When `v` is narrower than `expected`
    * and they're both numeric, wrap `v` in the appropriate prelude
    * conversion (`to_real` / `to_complex`) so downstream codegen sees a
    * value of the expected type. When no coercion is needed, returns
    * `v` unchanged; when the assignment is invalid, emits the same
    * diagnostic as [[checkAssignable]] and returns `v` so elaboration
    * can continue.
    *
    * Used at val/top-binding, assign-target, and function-arg sites so
    * that mixed-type assignments like `val z: complex = 1.0` or
    * `f(1)` where `f: (real -> ...)` don't surface to the codegen as
    * type-mismatched stores.
    */
  protected def coerceTo(v: TExpr, expected: Type): TExpr =
    val actual = v.tpe
    if actual == TyUnknown || expected == TyUnknown then v
    else if actual == expected then v
    else if hasKindVar(expected) || hasKindVar(actual) then v  // generic — monomorph resolves
    else (actual, expected) match
      case (a, b) if isNumeric(a) && isNumeric(b) =>
        promote(a, b) match
          case Some(t) if t == b =>
            // Wrap in `to_<expected>(v)`. The prelude registers these
            // under [[SymKind.Prelude]] at startup, so they're always
            // in scope at the root level.
            synthCoerce(v, b)
          case _ =>
            err(s"cannot assign value of type $actual to $expected", v.pos)
            v
      case _ =>
        err(s"cannot assign value of type $actual to $expected", v.pos)
        v

  /** Wrap `v` in a synthetic call to the appropriate prelude conversion
    * function. Returns `v` unchanged if no conversion exists for the
    * target type or if the prelude name can't be resolved (defensive —
    * registerPrelude always installs these). Uses [[current]] to walk
    * the scope chain so prelude names resolve from anywhere.
    *
    * Constant folding: when `v` is a literal whose promotion result is
    * also expressible as a literal, fold to the literal directly so
    * neither the interpreter nor the AOT codegen has to round-trip
    * through the runtime conversion. Most common case is `val x: real
    * = 1` → `val x = 1.0` instead of `val x = to_real(1)`.
    *  - `TIntLit(n) → TyReal`     ⇒ `TRealLit(n.toDouble)`
    *  - `TBoolLit(b) → TyReal`    is not a valid promotion (rejected by
    *    [[promote]]), so no fold needed.
    *  - Promotions targeting `TyComplex` keep the wrapping `TCall` —
    *    Nex's AST has no complex-literal node, and clang's `-O1`
    *    constant folder collapses the resulting insertvalue chain into
    *    an aggregate constant anyway.
    */
  protected def synthCoerce(v: TExpr, target: Type): TExpr =
    // Compile-time literal fold for the most common case.
    (v, target) match
      case (TIntLit(n, p, _), TyReal) => return TRealLit(n.toDouble, p)
      case _ => ()

    val fnName = target match
      case TyReal    => "to_real"
      case TyComplex => "to_complex"
      case _         => return v
    current.lookup(fnName) match
      case Some(sym) => TCall(TVarRef(sym, v.pos, sym.tpe), List(v), v.pos, target)
      case None      => v

  // ==========================================================================
  // Expression inference
  // ==========================================================================

  protected def infExpr(e: TExpr): TExpr = e match
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit | _: TUnitLit | _: TIntrinsic => e

    case TInterpStringLit(parts, p, _) =>
      // `${...}` re-parse happens in Stage 1, so the parts list may
      // contain `TInterpExpr(x)` holding a freshly elaborated subtree.
      // Those subtrees need the same Stage-2 inference treatment any
      // other expression gets, or their types stay TyUnknown and the
      // mut-call-site check skips them. `TInterpRef` also needs a
      // refresh — its captured Symbol is a Stage-1 snapshot.
      val inferredParts = parts.map {
        case TInterpExpr(x, sp) => TInterpExpr(infExpr(x), sp)
        case TInterpRef(s, sp)  => TInterpRef(refreshSym(s), sp)
        case other          => other
      }
      TInterpStringLit(inferredParts, p, TyString)

    case TVarRef(s, p, _) =>
      // Refresh both `.sym` and `.tpe`. Stage 1 captured the symbol
      // before its type was inferred; if we only refresh `.tpe` then
      // `ref.sym.tpe` is stale and downstream walkers see the wrong
      // type. Same bug class as the for-loop loopVars staleness.
      val freshSym = refreshSym(s)
      TVarRef(freshSym, p, freshSym.tpe)

    case TBinOp(op, l, r, p, _) =>
      val ll = infExpr(l); val rr = infExpr(r)
      inferBinOp(op, ll, rr, p)

    case TUnaryOp(op, x, p, _) =>
      val xx = infExpr(x)
      inferUnaryOp(op, xx, p)

    case TJuxtapose(c, b, p, _) =>
      // Treat as multiplication for inference; Stage 3 will lower the node.
      val cc = infExpr(c); val bb = infExpr(b)
      inferBinOp("*", cc, bb, p) match
        case TBinOp("*", l, r, pp, t) => TJuxtapose(l, r, pp, t)
        case other                    => other // already an element-wise/broadcast form

    case TCall(callee, args, p, _) =>
      val cc = infExpr(callee)
      // Prelude HOFs (map/reduce/filter) have TyUnknown signatures in v0
      // because the type system has no type variables. But their shapes
      // are fixed and well-known, so we can hand-roll bidirectional
      // inference for them — push the array's element type into the
      // lambda arg and compute a sensible result type.
      cc match
        case TVarRef(s, _, _) if isPreludeHOF(s) =>
          inferPreludeHOFCall(s.name, cc, args, p)
        case TVarRef(s, _, _) if isPreludeRank1Only(s) =>
          inferPreludeRank1Call(s.name, cc, args, p)
        case TVarRef(s, _, _) if deferredLambdas.contains(s.id) =>
          // Direct call of a bound-then-called lambda: the lambda left its
          // params at TyUnknown, but the call-site args now carry concrete
          // types. Synthesize an expected `TyFunc(argTypes, TyUnknown)`
          // and route through `inferArg`, which handles deferred-lambda
          // refinement by re-running the lambda's body with pushed-down
          // param types. The refined TVarRef replaces `cc`; from there the
          // generic call path applies just like a regular function call.
          val aa = args.map(infExpr)
          val argTypes = aa.map(a => (a.tpe, ParamMode.Read))
          val refinedCc = inferArg(cc, TyFunc(argTypes, TyUnknown))
          inferCall(refinedCc, aa, p)
        case _ =>
          val aa = cc.tpe match
            case TyFunc(params, _) if params.size == args.size =>
              args.zip(params).map { case (a, (pt, _)) => inferArg(a, pt) }
            case _ =>
              args.map(infExpr)
          inferCall(cc, aa, p)

    case TIndex(arr, idx, p, _) =>
      val aa = infExpr(arr); val ii = idx.map(infExpr)
      inferIndex(aa, ii, p)

    case TField(r, name, p, _) =>
      val rr = infExpr(r)
      inferField(rr, name, p)

    case TTupleProj(r, idx, p, _) =>
      val rr = infExpr(r)
      val ty = rr.tpe match
        case TyTuple(elems) if idx < elems.size => elems(idx)
        case TyTuple(elems) =>
          err(s"tuple destructuring binds ${idx + 1} names but the value has only ${elems.size} elements", p)
          TyUnknown
        case TyUnknown => TyUnknown
        case other     =>
          err(s"tuple destructuring requires a tuple value, got $other", p)
          TyUnknown
      TTupleProj(rr, idx, p, ty)

    case TMethodCall(r, n, args, p, _) =>
      // Stage 3 will lower this into either a field access or a function
      // call `n(r, args...)`. Meanwhile, push expected param types into
      // unannotated lambda args so `xs.foo(x -> x + 1)` works. Three
      // shapes are handled here:
      //   1. `n` resolves to a top-level user function with a concrete
      //      TyFunc signature — push from `params.tail` into `args`.
      //   2. `n` is one of the prelude HOFs (map/reduce/filter) — use
      //      [[inferPreludeHOFCall]] with the receiver as the first arg.
      //   3. Otherwise infer args generically.
      val rr = infExpr(r)
      current.lookup(n) match
        case Some(sym) if isPreludeHOF(sym) =>
          // Reuse the prelude HOF path. The lowered call form is
          // `n(receiver, args...)`, so we synthesize that shape here and
          // let inferPreludeHOFCall do the push-down. We re-wrap as a
          // TMethodCall so Stage 3 can still recognize and dispatch it.
          val syntheticCallee = TVarRef(sym, p, currentType(sym))
          val tcall = inferPreludeHOFCall(n, syntheticCallee, r :: args, p).asInstanceOf[TCall]
          TMethodCall(rr, n, tcall.args.tail, p, tcall.tpe)
        case Some(sym) if isPreludeRank1Only(sym) =>
          // Same routing for the rank-1-only prelude helpers — `xs.dot(ys)`,
          // `xs.enumerate()`, `xs.zip(ys)` all need the rank guard.
          val syntheticCallee = TVarRef(sym, p, currentType(sym))
          val tcall = inferPreludeRank1Call(n, syntheticCallee, r :: args, p).asInstanceOf[TCall]
          TMethodCall(rr, n, tcall.args.tail, p, tcall.tpe)
        case Some(sym) =>
          val aa = currentType(sym) match
            case TyFunc(params, _) if params.size == args.size + 1 =>
              args.zip(params.tail).map { case (a, (pt, _)) => inferArg(a, pt) }
            case _ =>
              args.map(infExpr)
          TMethodCall(rr, n, aa, p, TyUnknown)
        case None =>
          TMethodCall(rr, n, args.map(infExpr), p, TyUnknown)

    case TLambda(params, body, p, _) =>
      // No expected type at this position — params keep whatever type they
      // had at Stage 1 (annotated or TyUnknown). Refresh the param Symbols
      // so downstream consumers don't see a stale `.tpe`; for bound-then-
      // called lambdas with annotations, the annotation already set the
      // type at Stage 1 mint time.
      val body2       = infExpr(body)
      val freshParams = params.map(refreshSym)
      val ty          = TyFunc(freshParams.map(s => (s.tpe, ParamMode.Read)), body2.tpe)
      TLambda(freshParams, body2, p, ty)

    case TTuple(elems, p, _) =>
      val es = elems.map(infExpr)
      TTuple(es, p, TyTuple(es.map(_.tpe)))

    case TArrayLit(Nil, p, _) =>
      // Spec §4.13: empty rank-1 literals require a type annotation. If
      // we reached `infExpr` for an empty literal that means no expected
      // type was pushed in — emit an error so the user is told to annotate.
      // [[inferArg]] handles the `val x: [real] = []` happy path before
      // this case ever fires.
      err("empty array literal requires a type annotation (e.g. `val x: [real] = []`)", p)
      TArrayLit(Nil, p, TyArray(TyUnknown, 1))

    case TArrayLit(elems, p, _) =>
      val es = elems.map(infExpr)
      val et = lubTypes(es.map(_.tpe))
      val (elemT, rank) = et match
        case TyArray(inner, 1) => (inner, 2) // rank-2: array of rank-1
        case other             => (other, 1)
      TArrayLit(es, p, TyArray(elemT, rank))

    case TIf(c, t, eO, p, _) =>
      val cc = infExpr(c)
      requireBool(cc, "if condition")
      val tt = infExpr(t)
      eO match
        case None =>
          // unit branch — both must be unit
          if tt.tpe != TyUnknown && tt.tpe != TyUnit then
            err(s"if-without-else branch must be unit, got ${tt.tpe}", tt.pos)
          TIf(cc, tt, None, p, TyUnit)
        case Some(eb) =>
          val ee = infExpr(eb)
          val ty = lubTypes(List(tt.tpe, ee.tpe))
          TIf(cc, tt, Some(ee), p, ty)

    case TFor(loopVars, iter, body, p, _) =>
      val it = infExpr(iter)
      // Pin the freshly-typed Symbols back into `loopVars` — `setSymType`
      // returns the new immutable Symbol with the updated `tpe`, and the
      // TFor field must hold those updated copies so anything walking the
      // typed AST (codegen, analysis) sees the right loop-var types.
      val typedLoopVars: List[Symbol] = it.tpe match
        case TyArray(elem, 1) if loopVars.size == 1 =>
          List(setSymType(loopVars.head, elem))
        case TyArray(TyTuple(elems), 1) if loopVars.size == elems.size =>
          // `for i, x in [(1, 1.5), (2, 2.5)] do ...` — destructure
          // tuple-typed elements across the loop vars one-to-one.
          loopVars.zip(elems).map { case (s, t) => setSymType(s, t) }
        case TyArray(TyTuple(elems), 1) =>
          err(s"for-loop tuple destructuring binds ${loopVars.size} names but each element has ${elems.size}", it.pos)
          loopVars
        case TyArray(elem, 1) =>
          // Single-array-of-non-tuple but multiple loop vars — user
          // wrote e.g. `for a, b in xs` over a non-tuple array.
          err(s"for-loop binds ${loopVars.size} names but each element is $elem, not a tuple", it.pos)
          loopVars
        case TyArray(_, r) =>
          // Rank-2+ for-iteration semantics aren't fixed in the spec
          // (row-wise vs flat). Reject at compile time so neither
          // backend silently picks a different interpretation; users
          // can opt in via `for row in m.map(r -> r)` once the spec
          // settles. The error references the rank to make the
          // diagnostic actionable.
          err(s"for-loop iteration over rank-$r arrays is not yet defined — flatten first or iterate explicitly", it.pos)
          loopVars
        case TyUnknown     => loopVars
        case other         =>
          err(s"for-loop iterable must be an array, got $other", it.pos)
          loopVars
      val bb = infExpr(body)
      TFor(typedLoopVars, it, bb, p, TyUnit)

    case TWhile(c, b, p, _) =>
      val cc = infExpr(c); requireBool(cc, "while condition")
      TWhile(cc, infExpr(b), p, TyUnit)

    case TReturn(v, p, _) =>
      TReturn(v.map(infExpr), p, TyUnit)

    case TAssign(t, v, p, _) =>
      val tt = infExpr(t); val vv = infExpr(v)
      val vvCoerced = if tt.tpe != TyUnknown then coerceTo(vv, tt.tpe) else vv
      TAssign(tt, vvCoerced, p, TyUnit)

    case TBlock(items, result, p, _) =>
      val its = items.map(inferBlockItem)
      val rr  = infExpr(result)
      TBlock(its, rr, p, rr.tpe)

    case TMatch(scrutinee, cases, p, _) =>
      val ts = infExpr(scrutinee)
      // The scrutinee's type drives both pattern typing and the
      // exhaustiveness check. Anything other than a [[TyEnum]] is a
      // hard error — bare types (int / real / string) can't be
      // discriminated in `match` yet.
      val enumTy = ts.tpe match
        case te: TyEnum => Some(te)
        case TyUnknown  => None
        case other      =>
          err(s"`match` scrutinee must be an enum, got $other", ts.pos)
          None
      val typedCases = cases.map { c =>
        val typedPat = typePattern(c.pat, enumTy)
        val body     = infExpr(c.body)
        TMatchCase(typedPat, body)
      }
      checkMatchExhaustiveness(typedCases, enumTy, p)
      val resTy = lubTypes(typedCases.map(_.body.tpe))
      TMatch(ts, typedCases, p, resTy)

    // Element-wise & matmul nodes don't appear in Stage 1 output; they
    // get introduced here. If we see them in a second-pass scenario,
    // pass through. TFusedLoop is similar — introduced by NexFusion
    // (Stage 4, post-lowering), never present during Stage 2 today,
    // but pass it through defensively in case the pipeline is rerun.
    case _: TElementWise | _: TBroadcast | _: TMap | _: TReduce | _: TMatMul | _: TFusedLoop | _: TFlatIndex | _: TSlice | _: TSlice2 | _: TAxisAllMark | _: TClone => e

    // TOpenSliceMark wraps user expressions in `Option[TExpr]` — recurse
    // into them so the inner `lo`/`hi`/`stride` get type-inferred (e.g.
    // `a[..-1]` needs the `TUnaryOp("-", TIntLit(1))` to flow through
    // typing to pick up `TyInteger`). The mark itself stays untyped;
    // `inferIndex` consumes it without inspecting its tpe.
    case TOpenSliceMark(lo, hi, inclusive, stride, pos, tpe) =>
      TOpenSliceMark(lo.map(infExpr), hi.map(infExpr), inclusive, stride.map(infExpr), pos, tpe)

  protected def inferBlockItem(i: TBlockItem): TBlockItem = i match
    case TBlockBinding(s, kind, v) =>
      val declared = currentType(s)
      val vv =
        if declared != TyUnknown then inferArg(v, declared)
        else inferBindingValue(v)
      val (vvCoerced, t) = declared match
        case TyUnknown => (vv, vv.tpe)
        case _         => (coerceTo(vv, declared), declared)
      val s2 = setSymType(s, t)
      registerDeferredLambda(s2, vvCoerced)
      TBlockBinding(s2, kind, vvCoerced)
    case TBlockExpr(x) => TBlockExpr(infExpr(x))

  // ==========================================================================
  // Inference helpers
  // ==========================================================================

  /** Least-upper-bound of a list of types under the promotion lattice
    * and structural equality. For numeric mixes, returns the highest type.
    * For mixed non-numeric / non-equal: returns the first non-unknown type
    * and emits no error (Stage 2 is permissive about array literal types
    * unless explicitly mismatched).
    */
  protected def lubTypes(ts: List[Type]): Type =
    val concrete = ts.filterNot(_ == TyUnknown)
    if concrete.isEmpty then TyUnknown
    else
      concrete.tail.foldLeft(concrete.head) { (acc, t) =>
        if acc == t then acc
        else (promote(acc, t)) match
          case Some(p) => p
          case None    => acc
      }

  protected def requireBool(e: TExpr, ctx: String): Unit =
    if e.tpe != TyUnknown && e.tpe != TyBool then
      err(s"$ctx must be bool, got ${e.tpe}", e.pos)

  protected def inferBinOp(op: String, l: TExpr, r: TExpr, p: Option[Position]): TExpr =
    val lt = l.tpe; val rt = r.tpe

    // matrix multiplication
    if op == "@" then
      return TMatMul(l, r, p, matMulType(lt, rt, p))

    // logical
    if op == "and" || op == "or" then
      requireBool(l, s"left operand of `$op`")
      requireBool(r, s"right operand of `$op`")
      return TBinOp(op, l, r, p, TyBool)

    // comparison
    if Set("==", "!=", "<", "<=", ">", ">=").contains(op) then
      return inferCompare(op, l, r, p)

    // range
    if op == ".." || op == "..=" then
      if lt != TyUnknown && lt != TyInteger then err("range bound must be integer", l.pos)
      if rt != TyUnknown && rt != TyInteger then err("range bound must be integer", r.pos)
      return TBinOp(op, l, r, p, TyArray(TyInteger, 1))

    // arithmetic with arrays → element-wise / broadcast
    if isArrayArith(op) then
      (elemOf(lt), elemOf(rt)) match
        case (Some((le, lr)), Some((re, rr))) if lr == rr =>
          val resultElem = promote(le, re).getOrElse(le)
          return TElementWise(op, l, r, p, TyArray(resultElem, lr))
        case (Some((_, _)), None) if isNumeric(rt) || rt == TyUnknown =>
          // `arr op scalar` — scalar is on the right.
          return TBroadcast(r, l, op, scalarFirst = false, p, lt)
        case (None, Some((_, _))) if isNumeric(lt) || lt == TyUnknown =>
          // `scalar op arr` — scalar is on the left.
          return TBroadcast(l, r, op, scalarFirst = true, p, rt)
        case _ => ()

    // string + string → string concat. Mixed string + value (interpreter
    // accepts via formatValue auto-promotion) would require a per-Type
    // value-to-string runtime; that case is currently rejected here.
    if op == "+" && lt == TyString && rt == TyString then
      return TBinOp(op, l, r, p, TyString)

    // scalar arithmetic
    val t = inferArith(op, lt, rt, p)
    TBinOp(op, l, r, p, t)

  protected def isArrayArith(op: String): Boolean =
    Set("+", "-", "*", "/", "%", "div", "^").contains(op)

  protected def inferArith(op: String, lt: Type, rt: Type, p: Option[Position]): Type =
    op match
      case "div" | "%" =>
        if lt != TyUnknown && lt != TyInteger then
          err(s"`$op` requires integer operands, got $lt", p)
        if rt != TyUnknown && rt != TyInteger then
          err(s"`$op` requires integer operands, got $rt", p)
        TyInteger
      case "/" =>
        // real division — promote both to at least real
        if !(isNumeric(lt) || lt == TyUnknown) then
          err(s"`/` requires numeric operands, got $lt", p)
        if !(isNumeric(rt) || rt == TyUnknown) then
          err(s"`/` requires numeric operands, got $rt", p)
        promote(lt, rt) match
          case Some(TyInteger) => TyReal
          case Some(t)         => t
          case None            => TyUnknown
      case _ =>
        if !(isNumeric(lt) || lt == TyUnknown) then
          err(s"`$op` requires numeric operands, got $lt", p)
        if !(isNumeric(rt) || rt == TyUnknown) then
          err(s"`$op` requires numeric operands, got $rt", p)
        promote(lt, rt).getOrElse(TyUnknown)

  protected def inferCompare(op: String, l: TExpr, r: TExpr, p: Option[Position]): TExpr =
    val lt = l.tpe; val rt = r.tpe
    // Array element-wise comparison
    (elemOf(lt), elemOf(rt)) match
      case (Some((_, lr)), Some((_, rr))) if lr == rr =>
        return TElementWise(op, l, r, p, TyArray(TyBool, lr))
      case (Some((_, lr)), None) if isNumeric(rt) || rt == TyUnknown =>
        return TBroadcast(r, l, op, scalarFirst = false, p, TyArray(TyBool, lr))
      case (None, Some((_, rr))) if isNumeric(lt) || lt == TyUnknown =>
        return TBroadcast(l, r, op, scalarFirst = true, p, TyArray(TyBool, rr))
      case _ => ()
    op match
      case "==" | "!=" =>
        // permissive: any equal pair, plus numeric pairs that promote,
        // plus an `Eq`-constrained kind var matched against itself or
        // any concrete type it admits.
        if lt == TyUnknown || rt == TyUnknown || lt == rt then ()
        else if isNumeric(lt) && isNumeric(rt) then ()
        else if eqCompatible(lt, rt) then ()
        else err(s"cannot compare $lt and $rt", p)
        TBinOp(op, l, r, p, TyBool)
      case _ =>
        // ordered comparison — ordered numerics, plus an `Ord`-constrained
        // kind var matched against itself or one of its admitted types.
        if lt == TyUnknown || rt == TyUnknown then ()
        else if (lt == TyInteger || lt == TyReal) && (rt == TyInteger || rt == TyReal) then ()
        else if ordCompatible(lt, rt) then ()
        else err(s"`$op` requires ordered operands, got $lt and $rt", p)
        TBinOp(op, l, r, p, TyBool)

  /** True iff a pair of operands is admissible under the structural
    * `Eq` constraint when at least one side is a kind variable.
    */
  private def eqCompatible(lt: Type, rt: Type): Boolean = (lt, rt) match
    case (k1: TyKindVar, k2: TyKindVar) => k1 == k2
    case (TyKindVar(_, c), t)           => c == KindConstraint.Eq && c.admits(t)
    case (t, TyKindVar(_, c))           => c == KindConstraint.Eq && c.admits(t)
    case _                              => false

  /** True iff a pair of operands is admissible under the `Ord`
    * constraint when at least one side is a kind variable.
    */
  private def ordCompatible(lt: Type, rt: Type): Boolean = (lt, rt) match
    case (k1: TyKindVar, k2: TyKindVar) => k1 == k2
    case (TyKindVar(_, c), t)           => c == KindConstraint.Ord && c.admits(t)
    case (t, TyKindVar(_, c))           => c == KindConstraint.Ord && c.admits(t)
    case _                              => false

  protected def matMulType(lt: Type, rt: Type, p: Option[Position]): Type =
    (lt, rt) match
      case (TyArray(le, 2), TyArray(re, 2)) => TyArray(promote(le, re).getOrElse(le), 2)
      case (TyArray(le, 2), TyArray(re, 1)) => TyArray(promote(le, re).getOrElse(le), 1)
      case (TyArray(le, 1), TyArray(re, 2)) => TyArray(promote(le, re).getOrElse(le), 1)
      case (TyArray(le, 1), TyArray(re, 1)) => promote(le, re).getOrElse(le)
      case (TyUnknown, _) | (_, TyUnknown)  => TyUnknown
      case _ =>
        err(s"`@` requires array operands, got $lt and $rt", p); TyUnknown

  protected def inferUnaryOp(op: String, x: TExpr, p: Option[Position]): TExpr =
    op match
      case "-" =>
        if !(isNumeric(x.tpe) || x.tpe == TyUnknown || (elemOf(x.tpe).exists(e => isNumeric(e._1)))) then
          err(s"unary `-` requires numeric operand, got ${x.tpe}", x.pos)
        TUnaryOp(op, x, p, x.tpe)
      case "not" =>
        requireBool(x, "operand of `not`")
        TUnaryOp(op, x, p, TyBool)
      case _ =>
        TUnaryOp(op, x, p, TyUnknown)

  // ==========================================================================
  // Call inference
  // ==========================================================================

  protected def inferCall(callee: TExpr, args: List[TExpr], p: Option[Position]): TExpr =
    // Overload resolution: if the callee is a TVarRef to a Function and
    // the same name binds multiple overloads in scope, score each one
    // against the actual arg types and pick the best. The chosen Symbol
    // replaces the callee's; from there the normal TyFunc path coerces
    // each argument to the matching formal type via `coerceTo`.
    val resolvedCallee = callee match
      case TVarRef(s, refPos, _) if s.kind == SymKind.Function =>
        val cands = current.lookupAll(s.name).filter(_.kind == SymKind.Function)
        if cands.size > 1 then
          // Use the post-update Symbol type via `currentType` so resolveOverload
          // sees the TyFunc Stage 2 registered (Pass A only saw TyUnknown).
          // The chosen Symbol stored on the new TVarRef is refreshed from the
          // SymbolTable so downstream readers of `.sym.tpe` see the fresh type.
          resolveOverload(cands, args, p) match
            case Some(chosen) =>
              val fresh = refreshSym(chosen)
              TVarRef(fresh, refPos, fresh.tpe)
            case None => callee
        else callee
      case _ => callee
    resolvedCallee match
      case TVarRef(s, _, _) if s.kind == SymKind.TypeName =>
        // Struct construction.
        currentType(s) match
          case ts @ TyStruct(_, fs) =>
            if fs.size != args.size then
              err(s"struct `${s.name}` expects ${fs.size} args, got ${args.size}", p)
              TCall(resolvedCallee, args, p, ts)
            else if fs.exists { case (_, ft) => hasKindVar(ft) } then
              inferGenericStructConstruct(resolvedCallee, s, ts, args, p)
            else
              val coercedArgs = fs.zip(args).map { case ((_, ft), a) => coerceTo(a, ft) }
              TCall(resolvedCallee, coercedArgs, p, ts)
          case _ => TCall(resolvedCallee, args, p, TyUnknown)

      case _ =>
        resolvedCallee.tpe match
          case TyFunc(params, ret) =>
            if params.size != args.size then
              err(s"function call expects ${params.size} args, got ${args.size}", p)
              TCall(resolvedCallee, args, p, ret)
            else if params.exists((pt, _) => hasKindVar(pt)) || hasKindVar(ret) then
              inferGenericCall(resolvedCallee, params, ret, args, p)
            else
              val coercedArgs = params.zip(args).map { case ((pt, _), a) => coerceTo(a, pt) }
              TCall(resolvedCallee, coercedArgs, p, ret)
          case _ =>
            // Fallback for prelude functions whose signatures aren't in
            // [[TyFunc]] form yet. We don't refine the param types here
            // (they'd need overload resolution), but we DO supply a
            // return type where we know one — so a containing function's
            // inferred return type isn't poisoned by TyUnknown bubbling
            // up from `print` / `assert` / etc.
            val ret = resolvedCallee match
              case TVarRef(s, _, _) if s.kind == SymKind.Prelude =>
                preludeReturnTypeFor(s.name, args)
              case _ => TyUnknown
            TCall(resolvedCallee, args, p, ret)
