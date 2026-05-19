package io.github.edadma.nex

import scala.collection.mutable
import scala.util.parsing.input.Position

/** Stage 2 type inference for the Nex elaborator. Walks the Stage-1
  * typed AST, fills in concrete types via bottom-up inference, applies
  * the integer→real→complex promotion lattice (§3.2), rewrites array
  * arithmetic into [[TElementWise]] / [[TBroadcast]] (§4.5), and rewrites
  * `@` into [[TMatMul]] (§4.6).
  *
  * Also houses the hand-rolled bidirectional inference for prelude
  * higher-order functions (map/reduce/filter/flatMap) and rank-1-only
  * helpers (dot/enumerate/zip), plus the per-name return-type lookup
  * for the rest of the prelude — these are too intertwined with the
  * general inferCall path to live in their own trait.
  */
protected trait NexElabInference extends NexElabState:

  // ==========================================================================
  // Promotion & type helpers
  // ==========================================================================

  /** Numeric promotion lattice (§3.2): integer → real → complex. Returns
    * the higher of the two types, or `None` if either is non-numeric.
    */
  protected def promote(a: Type, b: Type): Option[Type] =
    def rank(t: Type): Option[Int] = t match
      case TyInteger => Some(0)
      case TyReal    => Some(1)
      case TyComplex => Some(2)
      case _         => None
    for ra <- rank(a); rb <- rank(b) yield
      if ra >= rb then a else b

  /** True iff `t` is one of integer / real / complex. */
  protected def isNumeric(t: Type): Boolean = t match
    case TyInteger | TyReal | TyComplex => true
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
        case TyArray(elemT, 1) =>
          val coerced = elems.map { e =>
            val inferred = inferArg(e, elemT)
            coerceTo(inferred, elemT)
          }
          TArrayLit(coerced, p, TyArray(elemT, 1))
        case TyArray(elemT, 2) =>
          // Rank-2 literal — each row is itself a `[elemT]` array, so
          // recurse with the rank-1 element type.
          val rowT = TyArray(elemT, 1)
          val coerced = elems.map(row => inferArg(row, rowT))
          TArrayLit(coerced, p, TyArray(elemT, 2))
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
    p.copy(decls = p.decls.map(inferDecl))

  protected def inferDecl(d: TDecl): TDecl = d match
    case t: TFunDecl     => inferFun(t)
    case t: TStructDecl  => t  // fields already typed
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
      else infExpr(b.value)
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

  /** Spec §5.3: enforce that a `const` RHS is a constant expression.
    *
    * Allowed:
    *   - Numeric / boolean literals (incl. the parsed-out negative-juxt
    *     forms — these are integer/real lits in Stage 1).
    *   - Arithmetic, comparison, and logical binops: `+ - * / div % ^
    *     and or == != < <= > >=` (juxt is parsed as `*`).
    *   - Unary `-`, `not`.
    *   - References to other `const` bindings.
    *   - References to prelude constants (`pi`, `e`, `inf`, `nan`, `i`).
    *
    * Rejected: function calls (deferred to v1+), references to `val` /
    * `var`, lambdas, array/tuple/struct construction, control flow,
    * string literals, interpolated strings, every other expression
    * form. Each rejection emits an elaboration error rooted at the
    * offending node's position.
    */
  protected def validateConstExpr(e: TExpr): Unit =
    val allowedBinOps = Set(
      "+", "-", "*", "/", "div", "%", "^",
      "and", "or",
      "==", "!=", "<", "<=", ">", ">=",
    )
    e match
      case _: TIntLit | _: TRealLit | _: TBoolLit | _: TUnitLit => ()
      case TBinOp(op, l, r, p, _) =>
        if !allowedBinOps.contains(op) then
          err(s"const expression cannot use operator `$op`", p)
        else
          validateConstExpr(l); validateConstExpr(r)
      case TUnaryOp(op, x, p, _) =>
        if op != "-" && op != "not" then
          err(s"const expression cannot use unary `$op`", p)
        else validateConstExpr(x)
      case TJuxtapose(c, b, _, _) =>
        validateConstExpr(c); validateConstExpr(b)
      case TVarRef(s, p, _) =>
        s.kind match
          case SymKind.Prelude =>
            // Prelude constants are values (TyReal / TyComplex / TyBool /
            // TyInteger); prelude functions are TyFunc — those are
            // calls, not allowed in const expressions.
            s.tpe match
              case TyFunc(_, _) =>
                err(s"const expression cannot reference function `${s.name}`", p)
              case _ => ()
          case _ =>
            if !constSymIds.contains(s.id) then
              err(s"const expression cannot reference `${s.name}` — only other `const` bindings and prelude constants are allowed", p)
      case TCall(_, _, p, _) =>
        err("const expression cannot contain a function call (deferred to v1+)", p)
      case TLambda(_, _, p, _) =>
        err("const expression cannot be a lambda", p)
      case TArrayLit(_, p, _) =>
        err("const expression cannot construct an array", p)
      case TTuple(_, p, _) =>
        err("const expression cannot construct a tuple", p)
      case TIf(_, _, _, p, _) =>
        err("const expression cannot use `if`", p)
      case TStringLit(_, p, _) =>
        err("const expression cannot be a string literal in v0", p)
      case other =>
        err(s"const expression contains a non-constant form (${other.getClass.getSimpleName})", other.pos)

  protected def registerDeferredLambda(sym: Symbol, v: TExpr): Unit = v match
    case lam: TLambda if isPartiallyInferredLambda(lam) =>
      deferredLambdas(sym.id) = lam
    case _ => ()

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
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit | _: TUnitLit => e

    case TInterpStringLit(parts, p, _) =>
      // `${...}` re-parse happens in Stage 1, so the parts list may
      // contain `TInterpExpr(x)` holding a freshly elaborated subtree.
      // Those subtrees need the same Stage-2 inference treatment any
      // other expression gets, or their types stay TyUnknown and the
      // mut-call-site check skips them. `TInterpRef` also needs a
      // refresh — its captured Symbol is a Stage-1 snapshot.
      val inferredParts = parts.map {
        case TInterpExpr(x) => TInterpExpr(infExpr(x))
        case TInterpRef(s)  => TInterpRef(refreshSym(s))
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
        case TyArray(_, _) => loopVars // rank-2 iteration not yet defined
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

    // Element-wise & matmul nodes don't appear in Stage 1 output; they
    // get introduced here. If we see them in a second-pass scenario,
    // pass through. TFusedLoop is similar — introduced by NexFusion
    // (Stage 4, post-lowering), never present during Stage 2 today,
    // but pass it through defensively in case the pipeline is rerun.
    case _: TElementWise | _: TBroadcast | _: TMap | _: TReduce | _: TMatMul | _: TFusedLoop | _: TFlatIndex | _: TSlice | _: TSlice2 | _: TAxisAllMark | _: TClone => e

  protected def inferBlockItem(i: TBlockItem): TBlockItem = i match
    case TBlockBinding(s, kind, v) =>
      val declared = currentType(s)
      val vv =
        if declared != TyUnknown then inferArg(v, declared)
        else infExpr(v)
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
        // permissive: any equal pair, plus numeric pairs that promote
        if lt == TyUnknown || rt == TyUnknown || lt == rt then ()
        else if isNumeric(lt) && isNumeric(rt) then ()
        else err(s"cannot compare $lt and $rt", p)
        TBinOp(op, l, r, p, TyBool)
      case _ =>
        // ordered comparison — only on ordered numerics (no complex)
        if lt == TyUnknown || rt == TyUnknown then ()
        else if (lt == TyInteger || lt == TyReal) && (rt == TyInteger || rt == TyReal) then ()
        else err(s"`$op` requires ordered numeric operands, got $lt and $rt", p)
        TBinOp(op, l, r, p, TyBool)

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
  // Prelude HOF routing & inferCall
  // ==========================================================================

  /** Prelude higher-order functions whose shape is fixed enough for
    * bidirectional inference even though their registered signature is
    * TyUnknown. Limited to those that actually take a lambda — adding more
    * is cheap but needs a per-name entry in [[inferPreludeHOFCall]].
    */
  protected val preludeHOFNames: Set[String] = Set("map", "flatMap", "reduce", "filter")

  protected def isPreludeHOF(s: Symbol): Boolean =
    s.kind == SymKind.Prelude && preludeHOFNames.contains(s.name)

  /** Prelude functions whose runtime only accepts rank-1 input. Calls with
    * rank-2 (or higher) sources should error at elaborate time rather than
    * trap with an unhelpful runtime message. Each entry needs a matching
    * branch in [[inferPreludeRank1Call]] that supplies the result type.
    */
  protected val preludeRank1OnlyNames: Set[String] = Set("dot", "enumerate", "zip")

  protected def isPreludeRank1Only(s: Symbol): Boolean =
    s.kind == SymKind.Prelude && preludeRank1OnlyNames.contains(s.name)

  /** Reject rank-2+ array arguments and supply a concrete result type for
    * rank-1-only prelude functions. Without this, calls fall through to
    * the generic `inferCall` which returns `TyUnknown` and lets the
    * runtime trap with "expects rank-1" — fine for users who never made
    * the mistake, hostile to users who did.
    */
  protected def inferPreludeRank1Call(
    name: String,
    callee: TExpr,
    args: List[TExpr],
    p: Option[Position],
  ): TExpr =
    val aa = args.map(infExpr)

    def requireRank1(arg: TExpr, ctx: String): Unit = arg.tpe match
      case TyArray(_, r) if r > 1 =>
        err(s"$name $ctx requires a rank-1 array, got rank $r", p)
      case _ =>

    name match
      case "dot" if aa.size == 2 =>
        requireRank1(aa(0), "first argument")
        requireRank1(aa(1), "second argument")
        val elemT = elemOf(aa(0).tpe).map(_._1).getOrElse(TyUnknown)
        TCall(callee, aa, p, elemT)

      case "enumerate" if aa.size == 1 =>
        requireRank1(aa(0), "argument")
        val elemT = elemOf(aa(0).tpe).map(_._1).getOrElse(TyUnknown)
        TCall(callee, aa, p, TyArray(TyTuple(List(TyInteger, elemT)), 1))

      case "zip" if aa.size == 2 =>
        requireRank1(aa(0), "first argument")
        requireRank1(aa(1), "second argument")
        val aT = elemOf(aa(0).tpe).map(_._1).getOrElse(TyUnknown)
        val bT = elemOf(aa(1).tpe).map(_._1).getOrElse(TyUnknown)
        TCall(callee, aa, p, TyArray(TyTuple(List(aT, bT)), 1))

      case _ =>
        TCall(callee, aa, p, TyUnknown)

  /** Hand-rolled bidirectional inference for the prelude HOFs:
    *   - `map(arr, f)`       — f: (elem -> U); result: [U]
    *   - `reduce(arr, init, f)` — f: (U, elem) -> U; result: U
    *   - `filter(arr, f)`    — f: (elem -> bool); result: same array type
    *
    * Each branch infers non-lambda args via [[infExpr]], extracts the
    * array's element type, builds the expected `TyFunc` for the lambda
    * arg, and pushes it down via [[inferArg]]. The result type of the
    * call is computed when enough is known; otherwise stays TyUnknown.
    */
  protected def inferPreludeHOFCall(
    name: String,
    callee: TExpr,
    args: List[TExpr],
    p: Option[Position],
  ): TExpr = name match
    case "map" if args.size == 2 =>
      val arr           = infExpr(args.head)
      val (elemT, srcR) = elemOf(arr.tpe).getOrElse((TyUnknown, 1))
      val f             = inferArg(args(1), TyFunc(List((elemT, ParamMode.Read)), TyUnknown))
      // `mapArray` preserves the source's rank at runtime — VArray2 in
      // gives VArray2 out — so the result type must do the same. Hard-
      // coding rank-1 here would break shape-preservation downstream
      // (notably the fusion pass, which keys off this `tpe`).
      val outT          = f.tpe match
        case TyFunc(_, ret) if ret != TyUnknown => TyArray(ret, srcR)
        case _                                  => TyUnknown
      TCall(callee, List(arr, f), p, outT)

    // `flatMap(arr, f)` where `f: T -> [U]`. Result is `[U]` —
    // always rank-1 since the inner arrays are concatenated. Reject
    // rank-2 sources at elaborate time (semantics on a matrix would
    // be ambiguous — flat-iterate the elements? concat the rows?).
    case "flatMap" if args.size == 2 =>
      val arr   = infExpr(args.head)
      arr.tpe match
        case TyArray(_, r) if r > 1 =>
          err(s"flatMap requires a rank-1 array, got rank $r", p)
        case _ =>
      val elemT = elemOf(arr.tpe).map(_._1).getOrElse(TyUnknown)
      val f     = inferArg(
        args(1),
        TyFunc(List((elemT, ParamMode.Read)), TyArray(TyUnknown, 1)),
      )
      val outT = f.tpe match
        case TyFunc(_, TyArray(u, _)) if u != TyUnknown => TyArray(u, 1)
        case _                                          => TyUnknown
      TCall(callee, List(arr, f), p, outT)

    case "reduce" if args.size == 3 =>
      val arr   = infExpr(args.head)
      val init  = infExpr(args(1))
      val elemT = elemOf(arr.tpe).map(_._1).getOrElse(TyUnknown)
      val accT  = init.tpe
      val f     = inferArg(
        args(2),
        TyFunc(List((accT, ParamMode.Read), (elemT, ParamMode.Read)), TyUnknown),
      )
      TCall(callee, List(arr, init, f), p, accT)

    case "filter" if args.size == 2 =>
      val arr = infExpr(args.head)
      // `filter` semantics on rank-2 are ambiguous (filter rows? filter
      // elements and flatten?); the runtime only handles `VArray1`. Reject
      // rank-2+ at elaborate time rather than letting it trap later.
      arr.tpe match
        case TyArray(_, r) if r > 1 =>
          err(s"filter requires a rank-1 array, got rank $r", p)
        case _ =>
      val elemT = elemOf(arr.tpe).map(_._1).getOrElse(TyUnknown)
      val f     = inferArg(args(1), TyFunc(List((elemT, ParamMode.Read)), TyBool))
      TCall(callee, List(arr, f), p, TyArray(elemT, 1))

    case _ =>
      // Wrong arity for a known HOF — fall back to default inference and
      // let the runtime / future arity-check report it.
      val aa = args.map(infExpr)
      TCall(callee, aa, p, TyUnknown)

  protected def inferCall(callee: TExpr, args: List[TExpr], p: Option[Position]): TExpr =
    callee match
      case TVarRef(s, _, _) if s.kind == SymKind.TypeName =>
        // Struct construction.
        currentType(s) match
          case TyStruct(_, fs) =>
            if fs.size != args.size then
              err(s"struct `${s.name}` expects ${fs.size} args, got ${args.size}", p)
              TCall(callee, args, p, currentType(s))
            else
              val coercedArgs = fs.zip(args).map { case ((_, ft), a) => coerceTo(a, ft) }
              TCall(callee, coercedArgs, p, currentType(s))
          case _ => TCall(callee, args, p, TyUnknown)

      case _ =>
        callee.tpe match
          case TyFunc(params, ret) =>
            if params.size != args.size then
              err(s"function call expects ${params.size} args, got ${args.size}", p)
              TCall(callee, args, p, ret)
            else
              val coercedArgs = params.zip(args).map { case ((pt, _), a) => coerceTo(a, pt) }
              TCall(callee, coercedArgs, p, ret)
          case _ =>
            // Fallback for prelude functions whose signatures aren't in
            // [[TyFunc]] form yet. We don't refine the param types here
            // (they'd need overload resolution), but we DO supply a
            // return type where we know one — so a containing function's
            // inferred return type isn't poisoned by TyUnknown bubbling
            // up from `print` / `assert` / etc.
            val ret = callee match
              case TVarRef(s, _, _) if s.kind == SymKind.Prelude =>
                preludeReturnTypeFor(s.name, args)
              case _ => TyUnknown
            TCall(callee, args, p, ret)

  /** Lookup table for prelude functions whose return type is known
    * statically and doesn't depend on argument types. HOFs (map/reduce/
    * filter), Rank-1 calls (sum/dot/enumerate), construction (zeros/
    * ones), and scalar math (sqrt/sin) have their own inference paths
    * elsewhere — this table only covers the unit-returning side-effect
    * functions and conversions.
    */
  protected val preludeReturnType: Map[String, Type] = Map(
    "print"          -> TyUnit,
    "assert"         -> TyUnit,
    "assert_eq"      -> TyUnit,
    "assert_approx"  -> TyUnit,
    "assert_traps"   -> TyUnit,
    "format"         -> TyString,
    "to_integer"     -> TyInteger,
    "to_real"        -> TyReal,
    "to_complex"     -> TyComplex,
    "length"         -> TyInteger,
    "rows"           -> TyInteger,
    "cols"           -> TyInteger,
    // §10.2 scalar math — all return real regardless of arg type.
    "sqrt" -> TyReal, "cbrt" -> TyReal,
    "exp"  -> TyReal, "log"  -> TyReal, "log2" -> TyReal, "log10" -> TyReal,
    "sin"  -> TyReal, "cos"  -> TyReal, "tan"  -> TyReal,
    "asin" -> TyReal, "acos" -> TyReal, "atan" -> TyReal, "atan2" -> TyReal,
    "sinh" -> TyReal, "cosh" -> TyReal, "tanh" -> TyReal,
    "asinh"-> TyReal, "acosh"-> TyReal, "atanh"-> TyReal,
    "floor"-> TyReal, "ceil" -> TyReal, "round"-> TyReal, "trunc" -> TyReal,
    // §10.3 complex
    "arg"  -> TyReal,
  ).withDefaultValue(TyUnknown)

  /** Pick a return type for a prelude call. Most names are in
    * [[preludeReturnType]] directly; [[abs]] / [[sign]] / [[min]] /
    * [[max]] / [[conj]] depend on argument types, so they branch here.
    */
  protected def preludeReturnTypeFor(name: String, args: List[TExpr]): Type =
    name match
      case "abs" =>
        args.headOption.map(_.tpe) match
          case Some(TyInteger) => TyInteger
          case Some(TyReal)    => TyReal
          case Some(TyComplex) => TyReal // |z| is the modulus, a real
          case _               => TyUnknown
      case "sign" =>
        args.headOption.map(_.tpe) match
          case Some(TyInteger) => TyInteger
          case Some(TyReal)    => TyReal
          case _               => TyUnknown
      case "min" | "max" =>
        (args.headOption.map(_.tpe), args.lift(1).map(_.tpe)) match
          case (Some(TyInteger), Some(TyInteger)) => TyInteger
          case (Some(TyReal), _) | (_, Some(TyReal)) => TyReal
          case _ => TyUnknown
      case "conj" =>
        args.headOption.map(_.tpe) match
          case Some(TyComplex) => TyComplex
          case Some(t)         => t
          case None            => TyUnknown
      // Spec §10.2 line 32: sin, cos, exp, log, sqrt apply to real
      // AND complex. The complex case returns complex; everything
      // else returns real (with sqrt(negative real) handled by the
      // interpreter at runtime).
      case "sin" | "cos" | "tan" | "exp" | "log" | "log2" | "log10" if args.size == 1 =>
        args.head.tpe match
          case TyComplex => TyComplex
          case _         => TyReal
      case "sqrt" | "cbrt" if args.size == 1 =>
        args.head.tpe match
          case TyComplex => TyComplex
          case _         => TyReal
      // §10.5 construction. `fill(n, v)` shape depends on n's type:
      //   - `n: integer`            → `[T]`  where T = v.tpe
      //   - `n: (integer, integer)` → `[[T]]`
      // Without an arg-type-aware path, the default `TyUnknown` would
      // make `print(fill(8, 0+0i))` silently drop the value.
      case "fill" if args.size == 2 =>
        val elemT = args(1).tpe
        args.head.tpe match
          case TyInteger                              => TyArray(elemT, 1)
          case TyTuple(List(TyInteger, TyInteger))    => TyArray(elemT, 2)
          case _                                       => TyUnknown
      // `zeros` / `ones` always produce integer arrays in the
      // interpreter; the rank is determined by the arg shape.
      case "zeros" | "ones" if args.size == 1 =>
        args.head.tpe match
          case TyInteger                              => TyArray(TyInteger, 1)
          case TyTuple(List(TyInteger, TyInteger))    => TyArray(TyInteger, 2)
          case _                                       => TyUnknown
      // `identity(n)` is the rank-2 n×n identity matrix. Interpreter
      // builds integer cells (despite the spec's `[[real]]` signature
      // — a known v0 divergence); typed accordingly so the parity
      // tests print the integer form.
      case "identity" if args.size == 1 =>
        args.head.tpe match
          case TyInteger => TyArray(TyInteger, 2)
          case _         => TyUnknown
      // §10.4 rank-1 reductions — return the element type of the
      // (first) array argument. Without an arg-aware path, the static
      // default would be TyUnknown and downstream `print(sum(xs))`
      // would silently drop the value.
      case "sum" | "product" if args.size == 1 =>
        args.head.tpe match
          case TyArray(e, _) => e // sum/product walk all elements regardless of rank
          case _             => TyUnknown
      case "dot" if args.size == 2 =>
        args.head.tpe match
          case TyArray(e, 1) => e
          case _             => TyUnknown
      // §10.4 rank-1 builders — materialize a fresh array. range and
      // linspace are total-shape-known; enumerate/zip are handled in
      // inferPreludeRank1Call already.
      case "range" if args.size == 2 =>
        TyArray(TyInteger, 1)
      case "linspace" if args.size == 3 =>
        TyArray(TyReal, 1)
      // §10.4 rank-2 ops. shape returns a tuple whose arity matches the
      // source rank; transpose / matmul / diag / reshape / flatten compute
      // their result type from the argument shape.
      case "shape" if args.size == 1 =>
        args.head.tpe match
          case TyArray(_, 1) => TyTuple(List(TyInteger))
          case TyArray(_, 2) => TyTuple(List(TyInteger, TyInteger))
          case _             => TyUnknown
      case "transpose" if args.size == 1 =>
        args.head.tpe match
          case TyArray(e, 2) => TyArray(e, 2)
          case _             => TyUnknown
      case "matmul" if args.size == 2 =>
        // matmul follows the same shape rules as TMatMul (`@`). Defer
        // to matMulType for the lattice; it handles all 2x2 / 2x1 /
        // 1x2 / 1x1 combinations correctly.
        matMulType(args(0).tpe, args(1).tpe, None)
      case "diag" if args.size == 1 =>
        args.head.tpe match
          // diag(rank-1) → rank-2 n×n with the input on the diagonal.
          // (Interpreter doesn't yet implement diag(rank-2) → rank-1.)
          case TyArray(e, 1) => TyArray(e, 2)
          case _             => TyUnknown
      case "reshape" if args.size == 3 =>
        // Spec §10.4: `reshape(a: [T], rows: integer, cols: integer)` —
        // 3 args, NOT a tuple-shape. Result element type preserved.
        args.head.tpe match
          case TyArray(e, _) => TyArray(e, 2)
          case _             => TyUnknown
      case "flatten" if args.size == 1 =>
        args.head.tpe match
          case TyArray(e, _) => TyArray(e, 1)
          case _             => TyUnknown
      case "sum_axis" if args.size == 2 =>
        // §10.4: rank-2 matrix collapsed along axis 0 or 1. The result
        // is always rank-1; the axis value picks which axis goes away
        // but doesn't change the result element type or rank.
        args.head.tpe match
          case TyArray(e, 2) => TyArray(e, 1)
          case _             => TyUnknown
      case _ => preludeReturnType(name)

  // ==========================================================================
  // Index & field inference
  // ==========================================================================

  protected def inferIndex(arr: TExpr, idx: List[TExpr], p: Option[Position]): TExpr =
    // Spec §4.14 slicing detection.
    //
    // Three shapes get rewritten in this dispatch:
    //   - `a[lo..hi]` or `a[lo..=hi]` (single range, rank-1 source) →
    //     `TSlice(arr, lo, hi, inclusive)` rank-1 result.
    //   - 2-element index list on rank-2 source where any element is
    //     a range or the `:` axis-all marker → `TSlice2(arr, rowAx,
    //     colAx)`. Result rank = number of preserved axes (integer
    //     collapses, range / `:` preserves).
    //   - everything else → plain `TIndex` (the existing behaviour).

    /** Classify one position in the index list as an axis spec. */
    def asAxisSpec(e: TExpr): TAxisSpec = e match
      case _: TAxisAllMark => TAxisAll
      case TBinOp(op, lo, hi, _, _) if op == ".." || op == "..=" =>
        if lo.tpe != TyUnknown && lo.tpe != TyInteger then
          err(s"slice lower bound must be integer, got ${lo.tpe}", lo.pos)
        if hi.tpe != TyUnknown && hi.tpe != TyInteger then
          err(s"slice upper bound must be integer, got ${hi.tpe}", hi.pos)
        TAxisRange(lo, hi, inclusive = op == "..=")
      case other =>
        if other.tpe != TyUnknown && other.tpe != TyInteger then
          err(s"array index must be integer, got ${other.tpe}", other.pos)
        TAxisIndex(other)

    /** True iff the index element is a range or axis-all (i.e. would
      * trigger a slice rewrite). */
    def isSliceMarker(e: TExpr): Boolean = e match
      case _: TAxisAllMark                                       => true
      case TBinOp(op, _, _, _, _) if op == ".." || op == "..=" => true
      case _                                                     => false

    idx match
      // -- Rank-1 single-range slice --------------------------------
      case List(TBinOp(op, lo, hi, _, _)) if op == ".." || op == "..=" =>
        if lo.tpe != TyUnknown && lo.tpe != TyInteger then
          err(s"slice lower bound must be integer, got ${lo.tpe}", lo.pos)
        if hi.tpe != TyUnknown && hi.tpe != TyInteger then
          err(s"slice upper bound must be integer, got ${hi.tpe}", hi.pos)
        arr.tpe match
          case TyArray(e, 1) =>
            TSlice(arr, lo, hi, inclusive = op == "..=", p, TyArray(e, 1))
          case TyArray(_, r) =>
            err(s"rank-1 slice requires a rank-1 array, got rank $r", p)
            TSlice(arr, lo, hi, inclusive = op == "..=", p, TyUnknown)
          case TyUnknown =>
            TSlice(arr, lo, hi, inclusive = op == "..=", p, TyUnknown)
          case other =>
            err(s"cannot slice value of type $other", p)
            TSlice(arr, lo, hi, inclusive = op == "..=", p, TyUnknown)

      // -- Orphan `:` in single-position index → error -------------
      case List(_: TAxisAllMark) =>
        err("`:` requires a rank-2 receiver and a paired axis (e.g. `m[:, 1]`)", p)
        TUnitLit(p)

      // -- Rank-2 slicing: 2 positions, at least one is a slice marker
      case List(r, c) if isSliceMarker(r) || isSliceMarker(c) =>
        val rowAx = asAxisSpec(r)
        val colAx = asAxisSpec(c)
        val preservedAxes = List(rowAx, colAx).count {
          case _: TAxisIndex => false
          case _             => true
        }
        arr.tpe match
          case TyArray(e, 2) =>
            val resultT = preservedAxes match
              case 0 => e
              case 1 => TyArray(e, 1)
              case _ => TyArray(e, 2)
            TSlice2(arr, rowAx, colAx, p, resultT)
          case TyArray(_, r0) =>
            err(s"rank-2 slice requires a rank-2 array, got rank $r0", p)
            TSlice2(arr, rowAx, colAx, p, TyUnknown)
          case TyUnknown =>
            TSlice2(arr, rowAx, colAx, p, TyUnknown)
          case other =>
            err(s"cannot slice value of type $other", p)
            TSlice2(arr, rowAx, colAx, p, TyUnknown)

      // -- Plain integer indexing (single or two-index) ------------
      case _ =>
        idx.foreach {
          case _: TAxisAllMark =>
            err("`:` is not legal here — used outside a rank-2 index list", p)
          case i if i.tpe != TyUnknown && i.tpe != TyInteger =>
            err(s"array index must be integer, got ${i.tpe}", i.pos)
          case _ => ()
        }
        val ty = arr.tpe match
          case TyArray(e, 1) if idx.size == 1 => e
          case TyArray(e, 2) if idx.size == 2 => e
          case TyArray(e, 2) if idx.size == 1 => TyArray(e, 1) // row slice
          case TyUnknown                      => TyUnknown
          case other =>
            err(s"cannot index value of type $other", p); TyUnknown
        TIndex(arr, idx, p, ty)

  protected def inferField(r: TExpr, name: String, p: Option[Position]): TExpr =
    val ty = r.tpe match
      case TyStruct(_, fs) =>
        fs.find(_._1 == name).map(_._2) match
          case Some(t) => t
          case None    => err(s"no field `$name` on $r.tpe", p); TyUnknown
      case TyComplex if name == "re" || name == "im" => TyReal
      case TyUnknown => TyUnknown
      case other     => err(s"cannot access field `$name` on $other", p); TyUnknown
    TField(r, name, p, ty)
