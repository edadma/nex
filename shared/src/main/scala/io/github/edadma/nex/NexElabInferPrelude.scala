package io.github.edadma.nex

import scala.util.parsing.input.Position

/** Stage 2 sub-trait — `const`-expression validation, purity analysis
  * for `const`-callable prelude / user functions, the hand-rolled
  * bidirectional inference path for prelude HOFs (map/reduce/filter/
  * flatMap) and rank-1-only helpers (dot/enumerate/zip/view), and the
  * per-name return-type lookup table for the rest of the prelude.
  * Sibling of [[NexElabInference]]; entry points are routed through
  * [[NexElabState]]'s abstract decls so the expression dispatcher and
  * the top-binding inference path can reach them.
  */
protected trait NexElabInferPrelude extends NexElabState:

  // ==========================================================================
  // const-expression validation (spec §5.3)
  // ==========================================================================

  /** Resolve every TCall-in-const purity check accumulated by
    * [[validateConstExpr]] during this module's elaboration. Runs
    * after every function body has been fully typed, so a `const
    * NINE = square(3.0)` can refer to a `def square` declared
    * anywhere in the same module (above OR below) — or in the
    * source prelude / an already-elaborated upstream module. The
    * scalar-result check already ran eagerly at validation time
    * (when the call's tpe was known). Here we only verify the
    * callee is pure (no I/O, no var mutation, only pure-call
    * transitive closure). Each violation emits an error at the
    * original call site.
    */
  protected def drainPendingConstPurityChecks(): Unit =
    if pendingConstPurityChecks.isEmpty then return
    for (callee, pos) <- pendingConstPurityChecks do
      val current = symbols.get(callee.id).getOrElse(callee)
      if !isPureFn(current, allFuncBodies.toMap) then
        err(s"const expression can only call pure functions; `${current.name}` is impure (or its purity could not be determined)", pos)
    pendingConstPurityChecks.clear()

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
      case call @ TCall(callee, args, p, _) =>
        // Spec §5.3: function calls are valid in a const RHS as long
        // as the callee is a pure function (prelude or user-defined),
        // every argument is itself a constant expression, and the
        // result is a scalar. The result-type check is eager — the
        // const binding's symbol type freezes at the call's tpe at
        // this point, so we need it resolved (any TyUnknown here is
        // a forward-reference to a fn with no declared return type
        // and that frozen TyUnknown would break codegen later).
        // Purity is deferred: a user fn might be declared below the
        // const, so its body isn't yet typed. The pending list is
        // drained at the end of `inferProgram`.
        callee match
          case TVarRef(s, _, _) if s.kind == SymKind.Prelude || s.kind == SymKind.Function =>
            if call.tpe == TyUnknown then
              err(s"const expression's call to `${s.name}` has unresolved return type — declare the function's return type or move its `def` above the const", p)
            else if !isScalarConstType(call.tpe) then
              err(s"const expression's call result must be a scalar (integer / real / bool / complex), got ${call.tpe}", p)
            else
              args.foreach(validateConstExpr)
              pendingConstPurityChecks += ((s, p))
          case _ =>
            err("const expression's function call must target a named function (no higher-order or computed callees)", p)
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

  /** Result types a const binding's RHS may produce. Mirrors §5.3's
    * "compile-time constant" intent: scalars only — arrays / tuples /
    * structs / strings escape the inline-friendly model. Complex
    * values qualify (they're a 16-byte struct in codegen but a single
    * value semantically — `const Z = 3 + 4i` already works).
    */
  protected def isScalarConstType(t: Type): Boolean = t match
    case TyInteger | TyReal | TyBool | TyComplex => true
    case _ => false

  // ==========================================================================
  // Purity analysis
  // ==========================================================================

  /** Prelude functions known to be free of observable side effects.
    * `print` writes to stdout; the `assert_*` family inspects state
    * and can trap (a trap is observable). Everything else listed in
    * §10.2 / §10.3 / §10.7 is a pure mathematical transformation and
    * fair game for compile-time inlining or runtime startup eval.
    *
    * Array / matrix / I/O operations are deliberately omitted — even
    * if their bodies are pure, their results aren't scalar, so they
    * can never satisfy the const-result-type rule.
    */
  protected def isPurePreludeName(name: String): Boolean =
    purePreludeNames.contains(name)

  private val purePreludeNames: Set[String] = Set(
    "sqrt", "cbrt", "exp", "log", "log2", "log10",
    "sin", "cos", "tan", "asin", "acos", "atan", "atan2",
    "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
    "hypot",
    "floor", "ceil", "round", "trunc",
    "abs", "sign", "min", "max",
    "conj", "arg",
    "to_real", "to_integer", "to_complex",
  )

  /** Decide whether calling `sym` is side-effect-free. Prelude names
    * consult [[purePreludeNames]] directly. User functions
    * (`SymKind.Function`) walk their body via [[isPureBody]] using the
    * caller-supplied `funcBodies` index. Recursion is broken by
    * caching `None` mid-discovery (i.e., assume pure during the
    * recursive walk; the final verdict is whatever a fixed body walk
    * produces). Imports and any other kind are conservatively impure.
    */
  protected def isPureFn(sym: Symbol, funcBodies: Map[Int, TExpr]): Boolean =
    purityCache.get(sym.id) match
      case Some(Some(b)) => b
      case Some(None)    => true
      case None =>
        sym.kind match
          case SymKind.Prelude =>
            val pure = isPurePreludeName(sym.name)
            purityCache(sym.id) = Some(pure)
            pure
          case SymKind.Function =>
            funcBodies.get(sym.id) match
              case None =>
                purityCache(sym.id) = Some(false)
                false
              case Some(body) =>
                purityCache(sym.id) = None
                val pure = isPureBody(body, funcBodies)
                purityCache(sym.id) = Some(pure)
                pure
          case _ =>
            purityCache(sym.id) = Some(false)
            false

  /** Walk a typed expression tree and report whether every leaf is
    * side-effect-free. The shape mirrors the typed-AST forms a normal
    * user function can produce; anything not listed is conservatively
    * impure (forces the analyzer to be extended deliberately when
    * new node kinds appear).
    */
  protected def isPureBody(e: TExpr, funcBodies: Map[Int, TExpr]): Boolean = e match
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit | _: TUnitLit => true
    case TVarRef(_, _, _) => true
    case TBinOp(_, l, r, _, _) =>
      isPureBody(l, funcBodies) && isPureBody(r, funcBodies)
    case TUnaryOp(_, x, _, _) =>
      isPureBody(x, funcBodies)
    case TJuxtapose(c, b, _, _) =>
      isPureBody(c, funcBodies) && isPureBody(b, funcBodies)
    case TCall(callee, args, _, _) =>
      callee match
        case TVarRef(s, _, _) =>
          isPureFn(s, funcBodies) && args.forall(a => isPureBody(a, funcBodies))
        case _ =>
          false
    case TIf(c, t, eo, _, _) =>
      isPureBody(c, funcBodies) && isPureBody(t, funcBodies) && eo.forall(x => isPureBody(x, funcBodies))
    case TBlock(items, result, _, _) =>
      items.forall {
        case TBlockBinding(_, _, v) => isPureBody(v, funcBodies)
        case TBlockExpr(x)          => isPureBody(x, funcBodies)
      } && isPureBody(result, funcBodies)
    case TMatch(scrut, cases, _, _) =>
      isPureBody(scrut, funcBodies) && cases.forall(c => isPureBody(c.body, funcBodies))
    case TArrayLit(elems, _, _) =>
      elems.forall(x => isPureBody(x, funcBodies))
    case TTuple(elems, _, _) =>
      elems.forall(x => isPureBody(x, funcBodies))
    case TLambda(_, body, _, _) =>
      isPureBody(body, funcBodies)
    case _: TAssign =>
      false
    case TIntrinsic(opId, _, _) =>
      // Intrinsic bodies are foreign-call leaves. Today's only family
      // is `libm.*`, all pure mathematical transformations; an opId
      // outside that family would be a new bridge whose semantics
      // aren't known yet, so reject conservatively.
      opId.startsWith("libm.")
    case _ =>
      false

  // ==========================================================================
  // Prelude HOF routing
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
  protected val preludeRank1OnlyNames: Set[String] = Set("dot", "enumerate", "zip", "view")

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

      case "view" if aa.size == 2 =>
        // First arg: a rank-1 or rank-2 array. Second arg: a range
        // expression (typed `TyArray(TyInteger, 1)`); the codegen
        // unpacks its bounds rather than reading the materialised range.
        // Rank-1: borrows an element range. Rank-2: borrows a row
        // range (contiguous in row-major layout). Result type matches
        // the source.
        aa(0).tpe match
          case TyArray(_, r) if r > 2 =>
            err(s"view first argument requires a rank-1 or rank-2 array, got rank $r", p)
          case _ =>
        TCall(callee, aa, p, aa(0).tpe)

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
      // Wrong arity for a known HOF — surface a clear diagnostic at
      // elaboration time. Spec §10.4 fixes the signatures: map / filter
      // / flatMap are 2-arg; reduce is 3-arg (the 2-arg form is NOT in
      // the spec). Silently emitting the TCall let the AOT no-op while
      // the interpreter trapped at runtime — divergent and confusing.
      val expected = name match
        case "map" | "filter" | "flatMap" => 2
        case "reduce"                     => 3
        case _                            => -1
      if expected > 0 then
        err(s"$name expects $expected args, got ${args.size}", p)
      val aa = args.map(infExpr)
      TCall(callee, aa, p, TyUnknown)

  // ==========================================================================
  // Prelude return-type table
  // ==========================================================================

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
    "to_integer"     -> TyInteger,
    "to_real"        -> TyReal,
    "to_complex"     -> TyComplex,
    "length"         -> TyInteger,
    "rows"           -> TyInteger,
    "cols"           -> TyInteger,
    // §10.2 scalar math — the real-only siblings (cbrt, floor, ceil,
    // round, trunc, asin, acos, atan, atan2, sinh, cosh, tanh, asinh,
    // acosh, atanh) and the overload-by-signature ones (sqrt, exp, log,
    // log2, log10, sin, cos, tan — each has a real and a complex def
    // in `prelude/scalar.nex`) arrive as TyFunc-typed Function symbols.
    // They route through ordinary overload resolution, not this table.
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
          // Binary scalar form: min(a, b) / max(a, b).
          case (Some(TyInteger), Some(TyInteger)) => TyInteger
          case (Some(TyReal), _) | (_, Some(TyReal)) => TyReal
          // Spec §10.4 unary array form: min(a: [T]): T.
          case (Some(TyArray(elem, _)), None) => elem
          case _ => TyUnknown
      case "conj" =>
        args.headOption.map(_.tpe) match
          case Some(TyComplex) => TyComplex
          case Some(t)         => t
          case None            => TyUnknown
      // sqrt / exp / log / log2 / log10 / sin / cos / tan ship as
      // overload pairs (real and complex) in `prelude/scalar.nex` and
      // resolve through the normal overload path; nothing to dispatch
      // here.
      case "cbrt" if args.size == 1 => TyReal
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
      case "view" if args.size == 2 =>
        // view(a, lo..hi) — same element type and rank as the source.
        // The second arg is a range expression; range bounds typecheck
        // separately. Rank-1 takes an element range; rank-2 takes a
        // row range (the result is still a rank-2 matrix with fewer
        // rows). Sub-rectangle (rank-2, two ranges) is deferred.
        args.head.tpe match
          case TyArray(e, 1) => TyArray(e, 1)
          case TyArray(e, 2) => TyArray(e, 2)
          case _             => TyUnknown
      case _ => preludeReturnType(name)
