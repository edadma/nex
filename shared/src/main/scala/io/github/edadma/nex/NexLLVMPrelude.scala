package io.github.edadma.nex

/** Prelude dispatch: built-in functions marked by the elaborator with
  * [[SymKind.Prelude]] — libm bridges, `abs` / `sign` / `min` / `max`,
  * `length` / `rows` / `cols`, and the `assert*` family.
  */
protected trait NexLLVMPrelude extends NexLLVMState:

  // ---------------------------------------------------------------------------
  // Prelude calls. The elaborator marks built-ins with [[SymKind.Prelude]];
  // the unrecognised-name path falls back to a `notYet` diag and a zero so
  // the rest of the IR still parses. Special-cased here:
  //   - `length(arr)` / `rows(arr)` / `cols(arr)`
  // Other prelude functions (sqrt, sin, map, reduce, ...) will land in
  // later chunks.
  // ---------------------------------------------------------------------------

  protected def emitPreludeCall(name: String, args: List[TExpr], resultT: Type): String =
    (name, args) match
      case ("length", List(a)) => emitArrayLengthish(a, "len")
      case ("rows",   List(a)) => emitArrayLengthish(a, "rows")
      case ("cols",   List(a)) => emitArrayLengthish(a, "cols")

      // The scalar transcendentals (sqrt/exp/log/sin/cos/tan and their
      // companion entries) all live in `prelude/scalar.nex` as either
      // direct `@intrinsic("libm.X")` bridges or overloaded source-body
      // defs that compose those bridges (complex variants). The Prelude-
      // path dispatch below only handles names that vary their return
      // type by argument type (abs / conj / arg / min / max / sign /
      // length / rows / cols / shape / fill / zeros / ones / linspace /
      // identity / map / reduce / filter / range / enumerate / zip /
      // dot / sum / product / sum_axis / transpose / matmul / diag /
      // reshape / flatten / print / format / to_real / to_integer /
      // to_complex / assert*).

      // `abs` is overloaded — integer abs vs real fabs vs complex modulus.
      // The elaborator leaves the call's result type to follow the
      // argument's type.
      case ("abs", List(x)) =>
        x.tpe match
          case TyInteger =>
            val xv  = emitExpr(x)
            val reg = newReg()
            emitLine(s"  $reg = call i64 @llabs(i64 $xv)\n")
            reg
          case TyComplex =>
            // |z| = sqrt(re² + im²)
            val xv  = emitExpr(x)
            val re  = newReg()
            emitLine(s"  $re = extractvalue { double, double } $xv, 0\n")
            val im  = newReg()
            emitLine(s"  $im = extractvalue { double, double } $xv, 1\n")
            val rr  = newReg(); emitLine(s"  $rr = fmul double $re, $re\n")
            val ii  = newReg(); emitLine(s"  $ii = fmul double $im, $im\n")
            val sum = newReg(); emitLine(s"  $sum = fadd double $rr, $ii\n")
            val reg = newReg()
            emitLine(s"  $reg = call double @sqrt(double $sum)\n")
            reg
          case _ =>
            val xv  = liftToReal(x)
            val reg = newReg()
            emitLine(s"  $reg = call double @fabs(double $xv)\n")
            reg

      // Complex prelude functions (§10.3).
      case ("conj", List(z)) =>
        z.tpe match
          case TyComplex =>
            val zv = emitExpr(z)
            val re = newReg()
            emitLine(s"  $re = extractvalue { double, double } $zv, 0\n")
            val im = newReg()
            emitLine(s"  $im = extractvalue { double, double } $zv, 1\n")
            val nim = newReg()
            emitLine(s"  $nim = fneg double $im\n")
            val c0 = newReg()
            emitLine(s"  $c0 = insertvalue { double, double } undef, double $re, 0\n")
            val c1 = newReg()
            emitLine(s"  $c1 = insertvalue { double, double } $c0, double $nim, 1\n")
            c1
          case _ =>
            // conj of a real / int is itself — just emit it.
            emitExpr(z)

      case ("arg", List(z)) =>
        // arg(z) = atan2(im, re) per the interpreter. Reals/ints have
        // `im = 0`, so arg returns 0 for positive and π for negative.
        z.tpe match
          case TyComplex =>
            val zv = emitExpr(z)
            val re = newReg()
            emitLine(s"  $re = extractvalue { double, double } $zv, 0\n")
            val im = newReg()
            emitLine(s"  $im = extractvalue { double, double } $zv, 1\n")
            val reg = newReg()
            emitLine(s"  $reg = call double @atan2(double $im, double $re)\n")
            reg
          case _ =>
            val xv = liftToReal(z)
            // For a non-complex value: return 0 if >= 0, π otherwise.
            val ge = newReg()
            emitLine(s"  $ge = fcmp oge double $xv, 0.0\n")
            val sel = newReg()
            emitLine(s"  $sel = select i1 $ge, double 0.0, double 0x400921FB54442D18\n")
            sel

      // §10.7 to_complex(x) — wrap any numeric as a complex pair.
      case ("to_complex", List(x)) =>
        x.tpe match
          case TyComplex => emitExpr(x)
          case TyReal =>
            val xv = emitExpr(x)
            val c0 = newReg()
            emitLine(s"  $c0 = insertvalue { double, double } undef, double $xv, 0\n")
            val c1 = newReg()
            emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
            c1
          case TyInteger =>
            val xv = emitExpr(x)
            val rv = newReg()
            emitLine(s"  $rv = sitofp i64 $xv to double\n")
            val c0 = newReg()
            emitLine(s"  $c0 = insertvalue { double, double } undef, double $rv, 0\n")
            val c1 = newReg()
            emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
            c1
          case other =>
            notYet(s"to_complex from $other"); "0"

      // §10.7 to_real(x) — widen any numeric to double.
      case ("to_real", List(x)) =>
        x.tpe match
          case TyReal    => emitExpr(x)
          case TyInteger =>
            val xv = emitExpr(x)
            val rv = newReg()
            emitLine(s"  $rv = sitofp i64 $xv to double\n")
            rv
          case TyComplex =>
            // Convention: real part of a complex value (matches the
            // interpreter, which uses `asReal` returning the real
            // component of a complex).
            val xv = emitExpr(x)
            val re = newReg()
            emitLine(s"  $re = extractvalue { double, double } $xv, 0\n")
            re
          case other =>
            notYet(s"to_real from $other"); "0.0"

      // §10.7 to_integer(x) — truncate to i64.
      case ("to_integer", List(x)) =>
        x.tpe match
          case TyInteger => emitExpr(x)
          case TyReal =>
            val xv  = emitExpr(x)
            val reg = newReg()
            emitLine(s"  $reg = fptosi double $xv to i64\n")
            reg
          case TyComplex =>
            // Truncate the real part — same convention as to_real(complex).
            val xv  = emitExpr(x)
            val re  = newReg()
            emitLine(s"  $re = extractvalue { double, double } $xv, 0\n")
            val reg = newReg()
            emitLine(s"  $reg = fptosi double $re to i64\n")
            reg
          case other =>
            notYet(s"to_integer from $other"); "0"

      // sign(x) returns -1, 0, or +1 — integer or real result follows arg.
      case ("sign", List(x)) =>
        x.tpe match
          case TyInteger =>
            val xv  = emitExpr(x)
            val pos = newReg()
            emitLine(s"  $pos = icmp sgt i64 $xv, 0\n")
            val neg = newReg()
            emitLine(s"  $neg = icmp slt i64 $xv, 0\n")
            // zext (NOT sext) — we want true → 1, not true → -1.
            val s1  = newReg()
            emitLine(s"  $s1 = zext i1 $pos to i64\n")
            val s2  = newReg()
            emitLine(s"  $s2 = zext i1 $neg to i64\n")
            val r   = newReg()
            emitLine(s"  $r = sub i64 $s1, $s2\n")
            r
          case _ =>
            val xv = liftToReal(x)
            // Compare against 0.0 in both directions; pick 1.0 / -1.0 / 0.0.
            val pos = newReg()
            emitLine(s"  $pos = fcmp ogt double $xv, 0.0\n")
            val neg = newReg()
            emitLine(s"  $neg = fcmp olt double $xv, 0.0\n")
            val s1  = newReg()
            emitLine(s"  $s1 = select i1 $pos, double 1.0, double 0.0\n")
            val s2  = newReg()
            emitLine(s"  $s2 = select i1 $neg, double -1.0, double $s1\n")
            s2

      case ("min", List(a, b)) => emitMinMax(a, b, isMin = true)
      case ("max", List(a, b)) => emitMinMax(a, b, isMin = false)

      // §10.8 assertions
      case ("assert", List(c)) =>
        val cv = emitExpr(c)
        emitLine(s"  call void @__nex_assert(i1 $cv)\n")
        "void"
      case ("assert", List(c, msg)) =>
        // Two-arg form: thread the user message into the trap text so
        // a downstream `assert_traps(fn, "<substr>")` matches the
        // interpreter's behavior.
        val cv = emitExpr(c)
        val mv = emitExpr(msg)
        emitLine(s"  call void @__nex_assert_with_msg(i1 $cv, ptr $mv)\n")
        "void"
      case ("assert_eq", List(a, b)) =>
        emitAssertEq(a, b); "void"
      case ("assert_approx", List(a, b, eps)) =>
        (a.tpe, b.tpe) match
          case (TyArray(_, 1), TyArray(_, 1)) =>
            emitAssertApproxArr1(a, b, eps); "void"
          case (TyArray(_, 2), TyArray(_, 2)) =>
            emitAssertApproxArr2(a, b, eps); "void"
          case (TyComplex, TyComplex) =>
            emitAssertApproxComplex(a, b, eps); "void"
          case _ =>
            emitAssertApprox(a, b, eps); "void"
      case ("assert_traps", List(fn)) =>
        emitAssertTraps(fn, expectedSubstr = None); "void"
      case ("assert_traps", List(fn, sub)) =>
        emitAssertTraps(fn, expectedSubstr = Some(sub)); "void"

      // §10.4 rank-1 reductions — counted loop + accumulator. Element
      // type may be integer (i64), real (double), or complex
      // ({double, double}); the op is fixed (+ for sum, * for product,
      // a*b accumulated for dot).
      case ("sum",     List(arr))              => emitSumCall(arr, resultT)
      case ("product", List(arr))              => emitProductCall(arr, resultT)
      case ("dot",     List(a, b))             => emitDotCall(a, b, resultT)

      // §10.4 unary min/max on an array. The binary scalar form is
      // handled by the elaborator's regular call path (it generates a
      // TCall with two args and the prelude dispatcher above handles
      // that via the lifted scalar entry); only the array overload
      // routes here.
      case ("min", List(arr)) if isArrayType(arr.tpe) =>
        emitMinMaxCall(arr, resultT, isMin = true)
      case ("max", List(arr)) if isArrayType(arr.tpe) =>
        emitMinMaxCall(arr, resultT, isMin = false)

      // §10.4 rank-1 builders.
      case ("range",     List(lo, hi))         => emitRangeCall(lo, hi)
      case ("enumerate", List(arr))            => emitEnumerateCall(arr, resultT)
      case ("zip",       List(a, b))           => emitZipCall(a, b, resultT)
      case ("linspace",  List(lo, hi, n))      => emitLinspaceCall(lo, hi, n)

      // §10.4 array HOFs — direct inlined loops that dispatch each
      // iteration through the indirect-call closure helper. Works
      // identically for inline TLambda args and TVarRef closure
      // bindings (both emit a `{ptr, ptr}` value via emitExpr).
      case ("map",     List(arr, fn))          => emitMapCall(arr, fn, resultT)
      case ("flatMap", List(arr, fn))          => emitFlatMapCall(arr, fn, resultT)
      case ("reduce",  List(arr, init, fn))    => emitReduceCall(arr, init, fn, resultT)
      case ("filter",  List(arr, fn))          => emitFilterCall(arr, fn, resultT)

      // §10.5 array construction. `fill(n, v)` allocates a fresh rank-1
      // array of length `n` with every slot set to `v`; element type
      // comes from `v.tpe` and was already propagated into resultT by
      // the elaborator. `zeros(n)` / `ones(n)` are convenience wrappers
      // that lower through fill with a 0 / 1 integer constant.
      case ("fill",     List(n, v))            => emitFillCall(n, v, resultT)
      case ("zeros",    List(n))               => emitConstFill(n, "0", TyInteger, resultT)
      case ("ones",     List(n))               => emitConstFill(n, "1", TyInteger, resultT)
      case ("identity", List(n))               => emitIdentityCall(n, resultT)

      // view-style slicing (rank-1 contiguous). The range arg is a
      // `TBinOp("..", lo, hi)` (exclusive) or `("..=", lo, hi)`
      // (inclusive) — we unpack both bounds, lower an inclusive range
      // to the equivalent exclusive `hi+1`, then call the runtime
      // helper. The result descriptor borrows from the source.
      case ("view", List(arr, r))               => emitViewCall(arr, r, resultT)

      // §10.4 rank-2 ops.
      case ("shape",     List(a))              => emitShapeCall(a, resultT)
      case ("transpose", List(a))              => emitTransposeCall(a, resultT)
      case ("matmul",    List(a, b))           => emitMatMulCall(a, b, resultT)
      case ("diag",      List(a))              => emitDiagCall(a, resultT)
      case ("reshape",   List(a, r, c))        => emitReshapeCall(a, r, c, resultT)
      case ("flatten",   List(a))              => emitFlattenCall(a, resultT)
      case ("sum_axis",  List(m, ax))          => emitSumAxisCall(m, ax, resultT)

      case _ =>
        notYet(s"prelude `$name`/${args.size}"); "0"

  /** Lift an integer-typed expression to double via `sitofp`; pass-through
    * for double-typed expressions. Used by every libm bridge so callers
    * can write `sqrt(4)` and get `2.0`.
    */
  private def liftToReal(e: TExpr): String =
    val v = emitExpr(e)
    e.tpe match
      case TyInteger =>
        val r = newReg()
        emitLine(s"  $r = sitofp i64 $v to double\n")
        r
      case _ => v

  /** Emit `%r = call double @fn(double <x>)`. */
  private def emitLibmUnary(fn: String, x: TExpr): String =
    val xv  = liftToReal(x)
    val reg = newReg()
    emitLine(s"  $reg = call double @$fn(double $xv)\n")
    reg

  /** Pack two doubles into a `{ double, double }` aggregate matching
    * Nex's complex value layout. Used by the pair-arithmetic paths in
    * complex `+`/`*`/`/` (NexLLVMCodegen) and by complex reductions
    * here in NexLLVMPrelude.
    */
  protected def packComplexCD(re: String, im: String): String =
    val c0 = newReg()
    emitLine(s"  $c0 = insertvalue { double, double } undef, double $re, 0\n")
    val c1 = newReg()
    emitLine(s"  $c1 = insertvalue { double, double } $c0, double $im, 1\n")
    c1

  /** Extract `(re, im)` from a complex SSA value. */
  protected def unpackComplex(z: String): (String, String) =
    val re = newReg()
    emitLine(s"  $re = extractvalue { double, double } $z, 0\n")
    val im = newReg()
    emitLine(s"  $im = extractvalue { double, double } $z, 1\n")
    (re, im)

  /** Emit `min(a, b)` / `max(a, b)` using fcmp+select for reals and
    * icmp+select for integers. Mixed-type results promote both operands
    * to double.
    */
  private def emitMinMax(a: TExpr, b: TExpr, isMin: Boolean): String =
    val bothInt = a.tpe == TyInteger && b.tpe == TyInteger
    if bothInt then
      val av  = emitExpr(a)
      val bv  = emitExpr(b)
      val cmp = newReg()
      val op  = if isMin then "icmp sle" else "icmp sge"
      emitLine(s"  $cmp = $op i64 $av, $bv\n")
      val sel = newReg()
      emitLine(s"  $sel = select i1 $cmp, i64 $av, i64 $bv\n")
      sel
    else
      val av  = liftToReal(a)
      val bv  = liftToReal(b)
      val cmp = newReg()
      val op  = if isMin then "fcmp ole" else "fcmp oge"
      emitLine(s"  $cmp = $op double $av, $bv\n")
      val sel = newReg()
      emitLine(s"  $sel = select i1 $cmp, double $av, double $bv\n")
      sel

  /** Emit an equality check between two values, trapping on mismatch
    * with the `assert_eq` message — so a parent `assert_traps`
    * substring check can find "assert_eq". Supports integer / real /
    * bool (with optional sitofp promotion when types differ).
    */
  private def emitAssertEq(a: TExpr, b: TExpr): Unit =
    val isReal = a.tpe == TyReal || b.tpe == TyReal
    val eq = newReg()
    if isReal then
      val av = liftToReal(a)
      val bv = liftToReal(b)
      emitLine(s"  $eq = fcmp oeq double $av, $bv\n")
    else
      val av = emitExpr(a)
      val bv = emitExpr(b)
      val ty = llvmType(a.tpe)
      emitLine(s"  $eq = icmp eq $ty $av, $bv\n")
    emitTrapOnFalse(eq, "@.assert_eq_msg", "aeq")

  /** Common pattern: branch on a bool; trap-with-msg on false, fall
    * through on true. Used by `assert_eq` / `assert_approx` so each
    * carries its own message (vs. the generic `__nex_assert` form).
    */
  private def emitTrapOnFalse(cond: String, msgSymbol: String, labelPrefix: String): Unit =
    val okL   = freshLabel(s"$labelPrefix.ok")
    val failL = freshLabel(s"$labelPrefix.fail")
    emitTerminator(s"  br i1 $cond, label %$okL, label %$failL\n")
    startBlock(failL)
    emitLine(s"  call void @__nex_trap_with(ptr $msgSymbol)\n")
    emitTerminator(s"  unreachable\n")
    startBlock(okL)

  /** Emit `assert_traps(fn)` or `assert_traps(fn, expectedSubstr)` —
    * call the thunk and verify that it traps, optionally checking that
    * the stashed trap message contains a given substring.
    *
    * Uses libc `setjmp` / `longjmp` to catch the trap. The thread-local
    * `@__nex_trap_buf` global holds the jmp_buf chain head; `__nex_trap`
    * (the single trap-emission point) reads it on every trap and
    * `longjmp`s when set. After the thunk returns (or longjmp'd back to
    * us), the previous chain head is restored.
    *
    * Stack slots holding the previous buf and the thunk's closure
    * components are loaded/stored via `volatile` ops to keep them stable
    * across the `returns_twice` setjmp call. Without volatile the LLVM
    * optimizer is free to cache them in callee-saved registers that
    * `longjmp` will clobber.
    *
    * Two-arg form: after the trap is caught, load `@__nex_trap_msg`
    * (which `__nex_trap` deliberately preserves across longjmp), the
    * substring's data, and call libc `strstr`. A null result means the
    * substring was not present — trap with the matching interpreter
    * message so a parent `assert_traps` sees the same verdict.
    *
    * On normal-return path (no trap fired): trap with the
    * "expected trap" message, also routed through `__nex_trap_with`
    * so a parent `assert_traps` catches silently.
    */
  private def emitAssertTraps(fn: TExpr, expectedSubstr: Option[TExpr]): Unit =
    val cl = emitExpr(fn)

    val fnPtr = newReg()
    emitLine(s"  $fnPtr = extractvalue { ptr, ptr } $cl, 0\n")
    val envPtr = newReg()
    emitLine(s"  $envPtr = extractvalue { ptr, ptr } $cl, 1\n")

    // Evaluate the substring argument (if any) BEFORE setjmp, otherwise
    // its computation might involve allocations the longjmp would skip
    // past. We hold the descriptor across the setjmp via the same
    // volatile-stash pattern as the closure components.
    val substrPtr = expectedSubstr.map(emitExpr)

    val retT = fn.tpe match
      case TyFunc(_, r) => r
      case _            => TyUnit

    val buf         = newReg()
    val prevSlot    = newReg()
    val envSlot     = newReg()
    val fnPtrSlot   = newReg()
    val substrSlot  = if substrPtr.isDefined then Some(newReg()) else None
    emitLine(s"  $buf = alloca [256 x i64], align 16\n")
    emitLine(s"  $prevSlot = alloca ptr, align 8\n")
    emitLine(s"  $envSlot = alloca ptr, align 8\n")
    emitLine(s"  $fnPtrSlot = alloca ptr, align 8\n")
    substrSlot.foreach { ss => emitLine(s"  $ss = alloca ptr, align 8\n") }

    val prev = newReg()
    emitLine(s"  $prev = load ptr, ptr @__nex_trap_buf, align 8\n")
    emitLine(s"  store volatile ptr $prev, ptr $prevSlot, align 8\n")
    emitLine(s"  store volatile ptr $envPtr, ptr $envSlot, align 8\n")
    emitLine(s"  store volatile ptr $fnPtr, ptr $fnPtrSlot, align 8\n")
    (substrSlot, substrPtr) match
      case (Some(ss), Some(sp)) =>
        emitLine(s"  store volatile ptr $sp, ptr $ss, align 8\n")
      case _ => ()
    emitLine(s"  store ptr $buf, ptr @__nex_trap_buf, align 8\n")

    val sjRes = newReg()
    emitLine(s"  $sjRes = call i32 @setjmp(ptr $buf)\n")
    val isFirst = newReg()
    emitLine(s"  $isFirst = icmp eq i32 $sjRes, 0\n")

    val callL    = freshLabel("at.call")
    val caughtL  = freshLabel("at.caught")
    val mergeL   = freshLabel("at.done")
    emitTerminator(s"  br i1 $isFirst, label %$callL, label %$caughtL\n")

    startBlock(callL)
    val fnP2  = newReg()
    val envP2 = newReg()
    emitLine(s"  $fnP2  = load volatile ptr, ptr $fnPtrSlot, align 8\n")
    emitLine(s"  $envP2 = load volatile ptr, ptr $envSlot, align 8\n")
    val retLLT = llvmType(retT)
    val sig    = s"$retLLT (ptr)"
    retT match
      case TyUnit =>
        emitLine(s"  call $sig $fnP2(ptr $envP2)\n")
      case _ =>
        val dummy = newReg()
        emitLine(s"  $dummy = call $sig $fnP2(ptr $envP2)\n")
    // Got here without trapping — restore prev, dec env + substring,
    // trap with the "expected trap" message. Routing through
    // __nex_trap_with keeps the message buffered so an enclosing
    // assert_traps stays silent.
    val prevA = newReg()
    emitLine(s"  $prevA = load volatile ptr, ptr $prevSlot, align 8\n")
    emitLine(s"  store ptr $prevA, ptr @__nex_trap_buf, align 8\n")
    emitLine(s"  call void @__nex_env_dec(ptr $envP2)\n")
    substrSlot.foreach { ss =>
      val sp = newReg()
      emitLine(s"  $sp = load volatile ptr, ptr $ss, align 8\n")
      emitLine(s"  call void @__nex_str_dec(ptr $sp)\n")
    }
    emitLine(s"  call void @__nex_trap_with(ptr @.assert_traps_fail_msg)\n")
    emitTerminator(s"  unreachable\n")

    startBlock(caughtL)
    // longjmp brought us here — the trap fired. Restore prev buf.
    // The env captured by the closure is leaked: state inside the thunk
    // was abandoned mid-execution, and Nex's ARC scheme has no way to
    // unwind a partial computation. assert_traps is a test-only feature,
    // so this is acceptable.
    val prevB = newReg()
    emitLine(s"  $prevB = load volatile ptr, ptr $prevSlot, align 8\n")
    emitLine(s"  store ptr $prevB, ptr @__nex_trap_buf, align 8\n")

    // Two-arg form: pull the substring descriptor's C-string, strstr
    // against the stashed trap message, and emit a follow-up trap on
    // miss. The miss path uses a static message — full parity with the
    // interpreter's verbose form ("expected substring `X`, got `Y`")
    // would require building a runtime string and is deferred.
    substrSlot.foreach { ss =>
      val subDesc = newReg()
      emitLine(s"  $subDesc = load volatile ptr, ptr $ss, align 8\n")
      val subData = newReg()
      emitLine(s"  $subData = call ptr @__nex_str_data(ptr $subDesc)\n")
      val trapMsg = newReg()
      emitLine(s"  $trapMsg = load ptr, ptr @__nex_trap_msg, align 8\n")

      // strstr(needle, haystack) — but BSD/glibc strstr is
      // strstr(haystack, needle). We pass trap msg as haystack.
      val found = newReg()
      emitLine(s"  $found = call ptr @strstr(ptr $trapMsg, ptr $subData)\n")
      val ok = newReg()
      emitLine(s"  $ok = icmp ne ptr $found, null\n")

      val okL   = freshLabel("at.substr.ok")
      val failL = freshLabel("at.substr.fail")
      emitTerminator(s"  br i1 $ok, label %$okL, label %$failL\n")

      startBlock(failL)
      emitLine(s"  call void @__nex_str_dec(ptr $subDesc)\n")
      emitLine(s"  call void @__nex_trap_with(ptr @.assert_traps_substr_msg)\n")
      emitTerminator(s"  unreachable\n")

      startBlock(okL)
      emitLine(s"  call void @__nex_str_dec(ptr $subDesc)\n")
    }
    // Clear the stashed trap message now that we have processed it
    // (either matched the substring or there was no substring check).
    // Failing to clear would let an unrelated later abort() inherit a
    // stale message.
    emitLine(s"  store ptr null, ptr @__nex_trap_msg, align 8\n")
    emitTerminator(s"  br label %$mergeL\n")

    startBlock(mergeL)

  /** Emit `|a - b| <= eps` check, trapping on mismatch with the
    * `assert_approx` message. Mixed numeric types promote to double.
    */
  private def emitAssertApprox(a: TExpr, b: TExpr, eps: TExpr): Unit =
    val av = liftToReal(a)
    val bv = liftToReal(b)
    val ev = liftToReal(eps)
    val d  = newReg()
    emitLine(s"  $d = fsub double $av, $bv\n")
    val ad = newReg()
    emitLine(s"  $ad = call double @fabs(double $d)\n")
    val ok = newReg()
    emitLine(s"  $ok = fcmp ole double $ad, $ev\n")
    emitTrapOnFalse(ok, "@.assert_approx_msg", "aap")

  /** Emit `hypot(re(a) - re(b), im(a) - im(b)) <= eps` for complex
    * operands. Both operands carry the `{ double, double }` layout.
    * Traps with the standard assert_approx message on mismatch.
    */
  private def emitAssertApproxComplex(a: TExpr, b: TExpr, eps: TExpr): Unit =
    val av = emitExpr(a)
    val bv = emitExpr(b)
    val ev = liftToReal(eps)
    val ar = newReg(); emitLine(s"  $ar = extractvalue { double, double } $av, 0\n")
    val ai = newReg(); emitLine(s"  $ai = extractvalue { double, double } $av, 1\n")
    val br = newReg(); emitLine(s"  $br = extractvalue { double, double } $bv, 0\n")
    val bi = newReg(); emitLine(s"  $bi = extractvalue { double, double } $bv, 1\n")
    val dr = newReg(); emitLine(s"  $dr = fsub double $ar, $br\n")
    val di = newReg(); emitLine(s"  $di = fsub double $ai, $bi\n")
    val dist = newReg()
    emitLine(s"  $dist = call double @hypot(double $dr, double $di)\n")
    val ok = newReg()
    emitLine(s"  $ok = fcmp ole double $dist, $ev\n")
    emitTrapOnFalse(ok, "@.assert_approx_msg", "aapc")

  /** Emit a rank-1 element-wise `assert_approx`. Both arrays must have
    * the same length (traps on mismatch); each element pair is checked
    * with the appropriate per-element distance — scalar |x - y| for
    * integer / real / bool elements, `hypot(dr, di)` for complex.
    * Trap message is the standard assert_approx message; the array
    * descriptor shares are released at exit.
    */
  private def emitAssertApproxArr1(a: TExpr, b: TExpr, eps: TExpr): Unit =
    val elemA = arrayElem(a.tpe)
    val elemB = arrayElem(b.tpe)
    if elemA != elemB then
      notImpl(s"assert_approx on arrays of different element types ($elemA vs $elemB)")
    val elem  = elemA
    val esz   = elemSize(elem)
    val stT   = storageType(elem)
    val langT = llvmType(elem)

    val av  = emitExpr(a)
    val bv  = emitExpr(b)
    val ev  = liftToReal(eps)

    val aLen = newReg(); emitLine(s"  $aLen = call i64 @__nex_arr1_len(ptr $av)\n")
    val bLen = newReg(); emitLine(s"  $bLen = call i64 @__nex_arr1_len(ptr $bv)\n")
    val sameLen = newReg()
    emitLine(s"  $sameLen = icmp eq i64 $aLen, $bLen\n")
    emitTrapOnFalse(sameLen, "@.assert_approx_msg", "aapal")

    val aBuf = bufPtr(av, a.tpe)
    val bBuf = bufPtr(bv, b.tpe)

    emitCountingLoop(aLen, "aapa") { i =>
      val sA = newReg(); emitLine(s"  $sA = getelementptr inbounds $stT, ptr $aBuf, i64 $i\n")
      val sB = newReg(); emitLine(s"  $sB = getelementptr inbounds $stT, ptr $bBuf, i64 $i\n")
      val vA = loadElem(stT, sA, langT)
      val vB = loadElem(stT, sB, langT)
      val dist = elem match
        case TyComplex =>
          val ar = newReg(); emitLine(s"  $ar = extractvalue { double, double } $vA, 0\n")
          val ai = newReg(); emitLine(s"  $ai = extractvalue { double, double } $vA, 1\n")
          val br = newReg(); emitLine(s"  $br = extractvalue { double, double } $vB, 0\n")
          val bi = newReg(); emitLine(s"  $bi = extractvalue { double, double } $vB, 1\n")
          val dr = newReg(); emitLine(s"  $dr = fsub double $ar, $br\n")
          val di = newReg(); emitLine(s"  $di = fsub double $ai, $bi\n")
          val h  = newReg(); emitLine(s"  $h  = call double @hypot(double $dr, double $di)\n")
          h
        case TyReal =>
          val d  = newReg(); emitLine(s"  $d  = fsub double $vA, $vB\n")
          val ad = newReg(); emitLine(s"  $ad = call double @fabs(double $d)\n")
          ad
        case TyInteger =>
          val d  = newReg(); emitLine(s"  $d  = sub i64 $vA, $vB\n")
          val df = newReg(); emitLine(s"  $df = sitofp i64 $d to double\n")
          val ad = newReg(); emitLine(s"  $ad = call double @fabs(double $df)\n")
          ad
        case other =>
          notImpl(s"assert_approx on array of $other elements")
      val ok = newReg()
      emitLine(s"  $ok = fcmp ole double $dist, $ev\n")
      emitTrapOnFalse(ok, "@.assert_approx_msg", "aapae")
    }

    emitArrDec(av, a.tpe)
    emitArrDec(bv, b.tpe)

  /** Emit a rank-2 element-wise `assert_approx`. Mirrors the rank-1
    * emitter (same per-element distance computations) but checks both
    * `rows` and `cols` for the shape match and iterates `rows * cols`
    * positions over the flat buffer.
    */
  private def emitAssertApproxArr2(a: TExpr, b: TExpr, eps: TExpr): Unit =
    val elemA = arrayElem(a.tpe)
    val elemB = arrayElem(b.tpe)
    if elemA != elemB then
      notImpl(s"assert_approx on arrays of different element types ($elemA vs $elemB)")
    val elem  = elemA
    val stT   = storageType(elem)
    val langT = llvmType(elem)

    val av  = emitExpr(a)
    val bv  = emitExpr(b)
    val ev  = liftToReal(eps)

    val aRows = newReg(); emitLine(s"  $aRows = call i64 @__nex_arr2_rows(ptr $av)\n")
    val bRows = newReg(); emitLine(s"  $bRows = call i64 @__nex_arr2_rows(ptr $bv)\n")
    val aCols = newReg(); emitLine(s"  $aCols = call i64 @__nex_arr2_cols(ptr $av)\n")
    val bCols = newReg(); emitLine(s"  $bCols = call i64 @__nex_arr2_cols(ptr $bv)\n")
    val sameRows = newReg(); emitLine(s"  $sameRows = icmp eq i64 $aRows, $bRows\n")
    val sameCols = newReg(); emitLine(s"  $sameCols = icmp eq i64 $aCols, $bCols\n")
    val sameShape = newReg(); emitLine(s"  $sameShape = and i1 $sameRows, $sameCols\n")
    emitTrapOnFalse(sameShape, "@.assert_approx_msg", "aapa2s")

    val total = newReg(); emitLine(s"  $total = mul i64 $aRows, $aCols\n")

    val aBuf = bufPtr(av, a.tpe)
    val bBuf = bufPtr(bv, b.tpe)

    emitCountingLoop(total, "aapa2") { i =>
      val sA = newReg(); emitLine(s"  $sA = getelementptr inbounds $stT, ptr $aBuf, i64 $i\n")
      val sB = newReg(); emitLine(s"  $sB = getelementptr inbounds $stT, ptr $bBuf, i64 $i\n")
      val vA = loadElem(stT, sA, langT)
      val vB = loadElem(stT, sB, langT)
      val dist = elem match
        case TyComplex =>
          val ar = newReg(); emitLine(s"  $ar = extractvalue { double, double } $vA, 0\n")
          val ai = newReg(); emitLine(s"  $ai = extractvalue { double, double } $vA, 1\n")
          val br = newReg(); emitLine(s"  $br = extractvalue { double, double } $vB, 0\n")
          val bi = newReg(); emitLine(s"  $bi = extractvalue { double, double } $vB, 1\n")
          val dr = newReg(); emitLine(s"  $dr = fsub double $ar, $br\n")
          val di = newReg(); emitLine(s"  $di = fsub double $ai, $bi\n")
          val h  = newReg(); emitLine(s"  $h  = call double @hypot(double $dr, double $di)\n")
          h
        case TyReal =>
          val d  = newReg(); emitLine(s"  $d  = fsub double $vA, $vB\n")
          val ad = newReg(); emitLine(s"  $ad = call double @fabs(double $d)\n")
          ad
        case TyInteger =>
          val d  = newReg(); emitLine(s"  $d  = sub i64 $vA, $vB\n")
          val df = newReg(); emitLine(s"  $df = sitofp i64 $d to double\n")
          val ad = newReg(); emitLine(s"  $ad = call double @fabs(double $df)\n")
          ad
        case other =>
          notImpl(s"assert_approx on array of $other elements")
      val ok = newReg()
      emitLine(s"  $ok = fcmp ole double $dist, $ev\n")
      emitTrapOnFalse(ok, "@.assert_approx_msg", "aapa2e")
    }

    emitArrDec(av, a.tpe)
    emitArrDec(bv, b.tpe)

  /** Emit a length-ish call (length / rows / cols). The runtime helper
    * picked depends on the array's static rank.
    *
    *   - rank-1 + `len`  → `__nex_arr1_len`
    *   - rank-1 + `rows` → 1 (no helper call needed)
    *   - rank-1 + `cols` → length
    *   - rank-2 + `len`  → `rows * cols`
    *   - rank-2 + `rows` → `__nex_arr2_rows`
    *   - rank-2 + `cols` → `__nex_arr2_cols`
    */
  private def emitArrayLengthish(arr: TExpr, which: String): String =
    val rank = arrayRank(arr.tpe)
    val av   = emitExpr(arr)
    val r    = newReg()
    val result = (rank, which) match
      case (1, "len" | "cols") =>
        emitLine(s"  $r = call i64 @__nex_arr1_len(ptr $av)\n"); r
      case (1, "rows") =>
        "1"
      case (2, "rows") | (2, "len") =>
        // length(rank-2) follows the interpreter convention: it returns
        // the row count, not rows*cols. (The flat element count is
        // available internally as __nex_arr2_len for codegen helpers.)
        emitLine(s"  $r = call i64 @__nex_arr2_rows(ptr $av)\n"); r
      case (2, "cols") =>
        emitLine(s"  $r = call i64 @__nex_arr2_cols(ptr $av)\n"); r
      case _ =>
        notYet(s"$which for rank $rank"); "0"
    // Release the owning share we took on the input array.
    emitArrDec(av, arr.tpe)
    result

  // ---------------------------------------------------------------------------
  // §10.4 array higher-order functions.
  //
  // map / reduce / filter all share the same shape:
  //   1. Evaluate `arr` once into an owning SSA value.
  //   2. Evaluate the closure-typed `fn` once and split it into
  //      `{ fn_ptr, env_ptr }` so the loop body can dispatch directly.
  //   3. Iterate 0..len-1, invoking the closure per element.
  //   4. dec the source array (we owned a share).
  //
  // map and reduce work on rank-1 and rank-2 (shape preserved by map;
  // reduce folds over every element regardless of rank). filter is
  // rank-1 only — the spec doesn't define what filtering a matrix
  // even means. Ranks ≥3 surface a `notYet` diag.
  // ---------------------------------------------------------------------------

  /** Emit `map(arr, fn)` — allocate a result array of the same length
    * and store each `fn(arr[i])` into it. The closure's `{ptr, ptr}`
    * value is split into fn_ptr + env_ptr before the loop so the body
    * only needs an indirect call.
    */
  private def emitMapCall(arr: TExpr, fn: TExpr, resultT: Type): String =
    val rank = arrayRank(arr.tpe)
    if rank != 1 && rank != 2 then
      notYet(s"map on rank $rank")
      return "0"

    val srcElem = arrayElem(arr.tpe)
    val resElem = arrayElem(resultT)
    val resEsz  = elemSize(resElem)
    val srcStT  = storageType(srcElem)
    val srcLLT  = llvmType(srcElem)
    val resStT  = storageType(resElem)
    val resLLT  = llvmType(resElem)

    val arrV    = emitExpr(arr)
    val (fnPtr, envPtr) = splitClosure(fn)

    // Rank-1: len = arr.len; alloc(len, esz). Rank-2: rows, cols from
    // source; alloc rank-2 with the same shape, walk flat over
    // rows*cols.
    val (len, res) = rank match
      case 1 =>
        val l = newReg(); emitLine(s"  $l = call i64 @__nex_arr1_len(ptr $arrV)\n")
        val r = newReg(); emitLine(s"  $r = call ptr @__nex_arr1_alloc(i64 $l, i64 $resEsz)\n")
        (l, r)
      case _ =>
        val rows = newReg(); emitLine(s"  $rows = call i64 @__nex_arr2_rows(ptr $arrV)\n")
        val cols = newReg(); emitLine(s"  $cols = call i64 @__nex_arr2_cols(ptr $arrV)\n")
        val l    = newReg(); emitLine(s"  $l = mul i64 $rows, $cols\n")
        val r    = newReg(); emitLine(s"  $r = call ptr @__nex_arr2_alloc(i64 $rows, i64 $cols, i64 $resEsz)\n")
        (l, r)

    val srcBuf  = bufPtr(arrV, arr.tpe)
    val dstBuf  = bufPtr(res, resultT)

    emitCountingLoop(len, "hof.map") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $srcStT, ptr $srcBuf, i64 $i\n")
      val elem    = loadElem(srcStT, srcSlot, srcLLT)
      val y       = newReg()
      emitLine(s"  $y = call $resLLT (ptr, $srcLLT) $fnPtr(ptr $envPtr, $srcLLT $elem)\n")
      val dstSlot = newReg()
      emitLine(s"  $dstSlot = getelementptr inbounds $resStT, ptr $dstBuf, i64 $i\n")
      storeElem(resStT, y, dstSlot)
    }

    emitArrDec(arrV, arr.tpe)
    emitLine(s"  call void @__nex_env_dec(ptr $envPtr)\n")
    res

  /** Emit `reduce(arr, init, fn)` — fold the array left-to-right
    * starting from `init`. The accumulator lives in an alloca slot so
    * we can update-in-place each iteration without phi nodes.
    */
  private def emitReduceCall(arr: TExpr, init: TExpr, fn: TExpr, resultT: Type): String =
    val rank = arrayRank(arr.tpe)
    if rank != 1 && rank != 2 then
      notYet(s"reduce on rank $rank")
      return "0"
    val lenFn = if rank == 1 then "__nex_arr1_len" else "__nex_arr2_len"

    val srcElem = arrayElem(arr.tpe)
    val srcStT  = storageType(srcElem)
    val srcLLT  = llvmType(srcElem)
    val accLLT  = llvmType(resultT)

    val arrV = emitExpr(arr)
    val iv   = emitExpr(init)
    val (fnPtr, envPtr) = splitClosure(fn)
    val accSlot = newReg()
    emitLine(s"  $accSlot = alloca $accLLT\n")
    emitLine(s"  store $accLLT $iv, ptr $accSlot\n")
    val len  = newReg()
    emitLine(s"  $len = call i64 @$lenFn(ptr $arrV)\n")
    val srcBuf  = bufPtr(arrV, arr.tpe)

    emitCountingLoop(len, "hof.reduce") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $srcStT, ptr $srcBuf, i64 $i\n")
      val elem    = loadElem(srcStT, srcSlot, srcLLT)
      val accCur  = newReg()
      emitLine(s"  $accCur = load $accLLT, ptr $accSlot\n")
      val nextAcc = newReg()
      emitLine(s"  $nextAcc = call $accLLT (ptr, $accLLT, $srcLLT) $fnPtr(ptr $envPtr, $accLLT $accCur, $srcLLT $elem)\n")
      emitLine(s"  store $accLLT $nextAcc, ptr $accSlot\n")
    }

    emitArrDec(arrV, arr.tpe)
    emitLine(s"  call void @__nex_env_dec(ptr $envPtr)\n")
    val finalAcc = newReg()
    emitLine(s"  $finalAcc = load $accLLT, ptr $accSlot\n")
    finalAcc

  /** Emit `filter(arr, predicate)` — allocate a worst-case-sized result
    * array of length(arr), iterate and copy elements where the
    * predicate returns true, then truncate the descriptor's length
    * field to the actual count and `realloc` the buffer down to that
    * exact size so a sparse predicate doesn't leave the dropped tail
    * pinned as slack until the array dies.
    */
  private def emitFilterCall(arr: TExpr, fn: TExpr, resultT: Type): String =
    if arrayRank(arr.tpe) != 1 then
      notYet(s"filter on rank ${arrayRank(arr.tpe)} (only rank-1 supported)")
      return "0"

    val elem    = arrayElem(arr.tpe)
    val esz     = elemSize(elem)
    val stT     = storageType(elem)
    val langT   = llvmType(elem)

    val arrV = emitExpr(arr)
    val (fnPtr, envPtr) = splitClosure(fn)
    val len  = newReg()
    emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $arrV)\n")
    val res  = newReg()
    emitLine(s"  $res = call ptr @__nex_arr1_alloc(i64 $len, i64 $esz)\n")
    val countSlot = newReg()
    emitLine(s"  $countSlot = alloca i64\n")
    emitLine(s"  store i64 0, ptr $countSlot\n")
    // Tier-1 perf: hoist buffer pointers; both indices are provably
    // in-bounds (`i` ∈ 0..len; `cur` ∈ 0..i ≤ len).
    val srcBuf  = bufPtr(arrV, arr.tpe)
    val dstBuf  = bufPtr(res, resultT)

    emitCountingLoop(len, "hof.filter") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $stT, ptr $srcBuf, i64 $i\n")
      val v       = loadElem(stT, srcSlot, langT)
      val keep    = newReg()
      emitLine(s"  $keep = call i1 (ptr, $langT) $fnPtr(ptr $envPtr, $langT $v)\n")
      val keepL   = freshLabel("filter.keep")
      val skipL   = freshLabel("filter.skip")
      emitTerminator(s"  br i1 $keep, label %$keepL, label %$skipL\n")
      startBlock(keepL)
      val cur = newReg()
      emitLine(s"  $cur = load i64, ptr $countSlot\n")
      val dst = newReg()
      emitLine(s"  $dst = getelementptr inbounds $stT, ptr $dstBuf, i64 $cur\n")
      storeElem(stT, v, dst)
      val nx  = newReg()
      emitLine(s"  $nx = add i64 $cur, 1\n")
      emitLine(s"  store i64 $nx, ptr $countSlot\n")
      emitTerminator(s"  br label %$skipL\n")
      startBlock(skipL)
    }

    // Truncate the result descriptor's length field to the actual
    // count. The buffer keeps its over-allocated size but the array's
    // visible length matches what was retained — matches the
    // interpreter's `VArray1(out)` shape.
    val cnt   = newReg()
    emitLine(s"  $cnt = load i64, ptr $countSlot\n")
    val lenP  = newReg()
    emitLine(s"  $lenP = getelementptr inbounds %nex_arr1, ptr $res, i32 0, i32 1\n")
    emitLine(s"  store i64 $cnt, ptr $lenP\n")

    // Shrink the buffer to the kept-element count. realloc returns
    // either the same block trimmed in place or a fresh smaller one;
    // either way the buffer ptr in the descriptor is updated. Pass at
    // least one byte so realloc-to-zero stays well-defined across
    // implementations.
    val bufP   = newReg()
    emitLine(s"  $bufP = getelementptr inbounds %nex_arr1, ptr $res, i32 0, i32 2\n")
    val oldBuf = newReg()
    emitLine(s"  $oldBuf = load ptr, ptr $bufP\n")
    val raw    = newReg()
    emitLine(s"  $raw = mul i64 $cnt, $esz\n")
    val nz     = newReg()
    emitLine(s"  $nz = icmp eq i64 $raw, 0\n")
    val shrunkSize = newReg()
    emitLine(s"  $shrunkSize = select i1 $nz, i64 1, i64 $raw\n")
    val newBuf = newReg()
    emitLine(s"  $newBuf = call ptr @realloc(ptr $oldBuf, i64 $shrunkSize)\n")
    emitLine(s"  store ptr $newBuf, ptr $bufP\n")

    emitArrDec(arrV, arr.tpe)
    emitLine(s"  call void @__nex_env_dec(ptr $envPtr)\n")
    res

  /** Evaluate a closure-typed expression once and return its
    * `(fn_ptr, env_ptr)` extracted SSA names. Used by the three HOF
    * helpers so the loop body only emits one indirect call per
    * iteration rather than re-extracting per use.
    */
  private def splitClosure(fn: TExpr): (String, String) =
    val cl = emitExpr(fn)
    val fp = newReg()
    emitLine(s"  $fp = extractvalue { ptr, ptr } $cl, 0\n")
    val ep = newReg()
    emitLine(s"  $ep = extractvalue { ptr, ptr } $cl, 1\n")
    (fp, ep)

  // ---------------------------------------------------------------------------
  // §10.5 array construction — `fill(n, v)`, `zeros(n)`, `ones(n)`.
  //
  // Each emits an allocation followed by a counted store loop. The
  // value is evaluated ONCE outside the loop and reused across slots —
  // matches the interpreter's `mutable.ArrayBuffer.fill(n)(v)` semantics
  // (no per-element re-evaluation).
  // ---------------------------------------------------------------------------

  /** Emit `fill(n, v)` — allocate a rank-1 or rank-2 array and store
    * `v` into every slot. The shape comes from the `n` argument's
    * type: integer → rank-1 of length `n`; tuple (rows, cols) →
    * rank-2 of shape (rows, cols). `v` is evaluated once.
    */
  private def emitFillCall(n: TExpr, v: TExpr, resultT: Type): String =
    val rank = arrayRank(resultT)
    if rank != 1 && rank != 2 then
      notYet(s"fill(rank-$rank)")
      return "null"

    val elem = arrayElem(resultT)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val vVal = emitExpr(v)

    // Rank-1 path takes `n` directly. Rank-2 expects `n` to be a
    // (integer, integer) tuple — extract rows / cols via extractvalue
    // on the SSA tuple value, then compute the flat length for the
    // counting loop.
    val (arr, len) = rank match
      case 1 =>
        val nVal = emitExpr(n)
        val a    = newReg()
        emitLine(s"  $a = call ptr @__nex_arr1_alloc(i64 $nVal, i64 $esz)\n")
        (a, nVal)
      case _ =>
        val tup  = emitExpr(n)
        val rows = newReg(); emitLine(s"  $rows = extractvalue { i64, i64 } $tup, 0\n")
        val cols = newReg(); emitLine(s"  $cols = extractvalue { i64, i64 } $tup, 1\n")
        val a    = newReg()
        emitLine(s"  $a = call ptr @__nex_arr2_alloc(i64 $rows, i64 $cols, i64 $esz)\n")
        val l    = newReg(); emitLine(s"  $l = mul i64 $rows, $cols\n")
        (a, l)

    val buf = bufPtr(arr, resultT)
    emitCountingLoop(len, "fill") { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $i\n")
      storeElem(stT, vVal, slot)
    }
    arr

  /** Emit a fill-with-constant for `zeros(n)` / `ones(n)`. The constant
    * is materialised as an LLVM literal (no separate SSA), and the loop
    * shape matches [[emitFillCall]]. Rank-1 takes `n: integer`; rank-2
    * takes `n: (integer, integer)` and allocates rows × cols.
    */
  private def emitConstFill(n: TExpr, constStr: String, elemT: Type, resultT: Type): String =
    val rank = arrayRank(resultT)
    if rank != 1 && rank != 2 then
      notYet(s"zeros/ones rank-$rank")
      return "null"

    val elem = arrayElem(resultT)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)

    val (arr, len) = rank match
      case 1 =>
        val nVal = emitExpr(n)
        val a    = newReg()
        emitLine(s"  $a = call ptr @__nex_arr1_alloc(i64 $nVal, i64 $esz)\n")
        (a, nVal)
      case _ =>
        val tup  = emitExpr(n)
        val rows = newReg(); emitLine(s"  $rows = extractvalue { i64, i64 } $tup, 0\n")
        val cols = newReg(); emitLine(s"  $cols = extractvalue { i64, i64 } $tup, 1\n")
        val a    = newReg()
        emitLine(s"  $a = call ptr @__nex_arr2_alloc(i64 $rows, i64 $cols, i64 $esz)\n")
        val l    = newReg(); emitLine(s"  $l = mul i64 $rows, $cols\n")
        (a, l)

    val buf = bufPtr(arr, resultT)
    emitCountingLoop(len, "constfill") { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $i\n")
      storeElem(stT, constStr, slot)
    }
    arr

  /** Emit `identity(n)` — n × n integer identity matrix. Element type
    * matches the interpreter's VInt-typed cells (a v0 divergence from
    * the spec's `[[real]]` signature). Allocates n*n integer slots
    * with 0, then walks the diagonal writing 1.
    */
  private def emitIdentityCall(n: TExpr, resultT: Type): String =
    if arrayRank(resultT) != 2 then
      notYet(s"identity rank ${arrayRank(resultT)}"); return "null"
    val elem = arrayElem(resultT)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val nVal = emitExpr(n)
    val arr  = newReg()
    emitLine(s"  $arr = call ptr @__nex_arr2_alloc(i64 $nVal, i64 $nVal, i64 $esz)\n")
    val buf  = bufPtr(arr, resultT)
    val total = newReg(); emitLine(s"  $total = mul i64 $nVal, $nVal\n")

    // Zero-fill every slot.
    emitCountingLoop(total, "identity.zero") { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $i\n")
      storeElem(stT, "0", slot)
    }
    // Walk the diagonal: index i*n + i for i in 0..n.
    emitCountingLoop(nVal, "identity.diag") { i =>
      val nplus1 = newReg(); emitLine(s"  $nplus1 = add i64 $nVal, 1\n")
      val idx    = newReg(); emitLine(s"  $idx = mul i64 $i, $nplus1\n")
      val slot   = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $idx\n")
      storeElem(stT, "1", slot)
    }
    arr

  // ---------------------------------------------------------------------------
  // §10.4 rank-1 reductions: sum / product / dot.
  //
  // Each emits an alloca'd accumulator updated by a counted loop. The
  // op is fixed per function (no closure dispatch). Element types
  // supported: integer (i64), real (double), complex ({double, double}).
  // ---------------------------------------------------------------------------

  /** Emit `sum(arr)` — fold the array with +, walking every element
    * regardless of rank. Result type matches the element type. The
    * rank-1 and rank-2 paths share `emitReduceOver`; only the length
    * helper differs (`__nex_arr1_len` vs `__nex_arr2_len`, which the
    * helper picks via the rank).
    */
  private def emitSumCall(arr: TExpr, resultT: Type): String =
    arrayRank(arr.tpe) match
      case 1 | 2 => emitReduceOver(arr, resultT, "+", zeroOf(resultT))
      case other => notYet(s"sum on rank $other"); "0"

  /** Emit `product(arr)` — same shape as sum, identity 1, op `*`. */
  private def emitProductCall(arr: TExpr, resultT: Type): String =
    arrayRank(arr.tpe) match
      case 1 | 2 => emitReduceOver(arr, resultT, "*", oneOf(resultT))
      case other => notYet(s"product on rank $other"); "0"

  /** Emit `dot(a, b)` — inner product. Both arrays must be rank-1 with
    * the same element type; result is the element type. Two source
    * GEPs per iter, one accumulator update.
    */
  private def emitDotCall(a: TExpr, b: TExpr, resultT: Type): String =
    if arrayRank(a.tpe) != 1 || arrayRank(b.tpe) != 1 then
      notYet("dot requires two rank-1 arrays")
      return "0"
    val elem = arrayElem(a.tpe)
    val stT  = storageType(elem)
    val llT  = llvmType(elem)
    val accLLT = llvmType(resultT)

    val av    = emitExpr(a)
    val bv    = emitExpr(b)
    val len   = newReg(); emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $av)\n")
    val blen  = newReg(); emitLine(s"  $blen = call i64 @__nex_arr1_len(ptr $bv)\n")
    // dot(a, b) requires a.len == b.len — interpreter traps with
    // "dot: length mismatch"; without this check the AOT would either
    // truncate (if b shorter) or read OOB (if b longer).
    val mismatch = newReg()
    emitLine(s"  $mismatch = icmp ne i64 $len, $blen\n")
    val okL   = freshLabel("dot.ok")
    val failL = freshLabel("dot.fail")
    emitTerminator(s"  br i1 $mismatch, label %$failL, label %$okL\n")
    startBlock(failL)
    emitLine(s"  call void @__nex_trap_with(ptr @.dot_mismatch_msg)\n")
    emitTerminator(s"  unreachable\n")
    startBlock(okL)
    val aBuf  = bufPtr(av, a.tpe)
    val bBuf  = bufPtr(bv, b.tpe)
    val accSlot = newReg()
    emitLine(s"  $accSlot = alloca $accLLT\n")
    storeElem(stT, zeroOf(resultT), accSlot)

    emitCountingLoop(len, "hof.dot") { i =>
      val aS = newReg()
      emitLine(s"  $aS = getelementptr inbounds $stT, ptr $aBuf, i64 $i\n")
      val ae = loadElem(stT, aS, llT)
      val bS = newReg()
      emitLine(s"  $bS = getelementptr inbounds $stT, ptr $bBuf, i64 $i\n")
      val be = loadElem(stT, bS, llT)
      val prod = emitScalarBinOpSimple("*", ae, be, elem)
      val cur  = newReg()
      emitLine(s"  $cur = load $accLLT, ptr $accSlot\n")
      val nxt  = emitScalarBinOpSimple("+", cur, prod, elem)
      storeElem(stT, nxt, accSlot)
    }

    emitArrDec(av, a.tpe)
    emitArrDec(bv, b.tpe)
    val r = newReg()
    emitLine(s"  $r = load $accLLT, ptr $accSlot\n")
    r

  /** Lower `min(arr)` / `max(arr)` — the spec §10.4 unary array
    * overload. Initial accumulator is `arr[0]`; each subsequent
    * element is compared against the accumulator with `icmp`/`fcmp`
    * and `select`. Empty arrays trap (the accumulator has no value
    * to seed from).
    */
  private def emitMinMaxCall(arr: TExpr, resultT: Type, isMin: Boolean): String =
    val rank   = arrayRank(arr.tpe)
    val elem   = arrayElem(arr.tpe)
    val stT    = storageType(elem)
    val llT    = llvmType(elem)
    val accLLT = llvmType(resultT)
    val lenFn  = rank match
      case 1 => "__nex_arr1_len"
      case 2 => "__nex_arr2_len"
      case _ => notImpl(s"min/max on rank $rank array")
    val labelPrefix = if isMin then "hof.min" else "hof.max"
    val cmpOp = (elem, isMin) match
      case (TyInteger, true)  => "icmp slt"
      case (TyInteger, false) => "icmp sgt"
      case (TyReal,    true)  => "fcmp olt"
      case (TyReal,    false) => "fcmp ogt"
      case _ =>
        notImpl(s"min/max on array element type $elem")

    val arrV   = emitExpr(arr)
    val len    = newReg(); emitLine(s"  $len = call i64 @$lenFn(ptr $arrV)\n")
    // Empty-array trap. Spec §10.4 doesn't define min/max of [].
    val isEmpty = newReg()
    emitLine(s"  $isEmpty = icmp eq i64 $len, 0\n")
    val okL    = freshLabel(s"$labelPrefix.ok")
    val flL    = freshLabel(s"$labelPrefix.fail")
    emitTerminator(s"  br i1 $isEmpty, label %$flL, label %$okL\n")
    startBlock(flL)
    emitLine(s"  call void @__nex_trap_with(ptr @.minmax_empty_msg)\n")
    emitTerminator(s"  unreachable\n")
    startBlock(okL)

    val buf     = bufPtr(arrV, arr.tpe)
    // Seed the accumulator with arr[0], then loop 1..len-1.
    val firstSlot = newReg()
    emitLine(s"  $firstSlot = getelementptr inbounds $stT, ptr $buf, i64 0\n")
    val first   = loadElem(stT, firstSlot, llT)
    val accSlot = newReg()
    emitLine(s"  $accSlot = alloca $accLLT\n")
    storeElem(stT, first, accSlot)

    // Loop bound is `len - 1`, body indexes `i + 1`.
    val tail = newReg(); emitLine(s"  $tail = sub i64 $len, 1\n")
    emitCountingLoop(tail, labelPrefix) { i =>
      val j = newReg(); emitLine(s"  $j = add i64 $i, 1\n")
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $j\n")
      val e   = loadElem(stT, slot, llT)
      val cur = newReg()
      emitLine(s"  $cur = load $accLLT, ptr $accSlot\n")
      val pick = newReg()
      emitLine(s"  $pick = $cmpOp $accLLT $e, $cur\n")
      val sel  = newReg()
      emitLine(s"  $sel = select i1 $pick, $accLLT $e, $accLLT $cur\n")
      storeElem(stT, sel, accSlot)
    }

    emitArrDec(arrV, arr.tpe)
    val r = newReg()
    emitLine(s"  $r = load $accLLT, ptr $accSlot\n")
    r

  /** Shared loop for sum / product. Walks every element of a rank-1
    * or rank-2 array (row-major flat order for rank-2). The element
    * type and op are fixed per call; the accumulator lives in an
    * alloca slot that the loop updates in place.
    */
  private def emitReduceOver(arr: TExpr, resultT: Type, op: String, identity: String): String =
    val rank   = arrayRank(arr.tpe)
    val elem   = arrayElem(arr.tpe)
    val stT    = storageType(elem)
    val llT    = llvmType(elem)
    val accLLT = llvmType(resultT)
    val lenFn  = rank match
      case 1 => "__nex_arr1_len"
      case 2 => "__nex_arr2_len"
      case _ => "__nex_arr1_len" // unreached — caller filtered ranks
    val labelPrefix = op match
      case "+" => "hof.sum"
      case "*" => "hof.product"
      case _   => "hof.fold"

    val arrV    = emitExpr(arr)
    val len     = newReg(); emitLine(s"  $len = call i64 @$lenFn(ptr $arrV)\n")
    val buf     = bufPtr(arrV, arr.tpe)
    val accSlot = newReg()
    emitLine(s"  $accSlot = alloca $accLLT\n")
    storeElem(stT, identity, accSlot)

    emitCountingLoop(len, labelPrefix) { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $i\n")
      val e   = loadElem(stT, slot, llT)
      val cur = newReg()
      emitLine(s"  $cur = load $accLLT, ptr $accSlot\n")
      val nxt = emitScalarBinOpSimple(op, cur, e, elem)
      storeElem(stT, nxt, accSlot)
    }

    emitArrDec(arrV, arr.tpe)
    val r = newReg()
    emitLine(s"  $r = load $accLLT, ptr $accSlot\n")
    r

  /** Per-element scalar binop including the TyComplex pair-arithmetic
    * formulas. Reuses NexLLVMCodegen's emitScalarBinOp for the simple
    * cases via the trait-shared abstract; complex needs the dedicated
    * formulas (real + complex = complex etc.).
    */
  private def emitScalarBinOpSimple(op: String, lv: String, rv: String, t: Type): String =
    t match
      case TyComplex =>
        // Both operands already complex; pair-arithmetic.
        val (lre, lim) = unpackComplex(lv)
        val (rre, rim) = unpackComplex(rv)
        op match
          case "+" =>
            val re = newReg(); emitLine(s"  $re = fadd double $lre, $rre\n")
            val im = newReg(); emitLine(s"  $im = fadd double $lim, $rim\n")
            packComplexCD(re, im)
          case "*" =>
            val ac = newReg(); emitLine(s"  $ac = fmul double $lre, $rre\n")
            val bd = newReg(); emitLine(s"  $bd = fmul double $lim, $rim\n")
            val ad = newReg(); emitLine(s"  $ad = fmul double $lre, $rim\n")
            val bc = newReg(); emitLine(s"  $bc = fmul double $lim, $rre\n")
            val re = newReg(); emitLine(s"  $re = fsub double $ac, $bd\n")
            val im = newReg(); emitLine(s"  $im = fadd double $ad, $bc\n")
            packComplexCD(re, im)
          case other =>
            notYet(s"complex `$other` in reduction"); lv
      case _ =>
        // Scalar int / real binop via the shared instruction table.
        val (instr, _) = binOpInst(op, t)
        val reg = newReg()
        emitLine(s"  $reg = $instr ${llvmType(t)} $lv, $rv\n")
        reg

  /** Zero element for sum (identity for +): 0 / 0.0 / 0.0+0.0i. */
  private def zeroOf(t: Type): String = t match
    case TyInteger => "0"
    case TyReal    => "0.0"
    case TyComplex =>
      val c0 = newReg()
      emitLine(s"  $c0 = insertvalue { double, double } undef, double 0.0, 0\n")
      val c1 = newReg()
      emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
      c1
    case _ => "0"

  /** One element for product (identity for *): 1 / 1.0 / 1.0+0.0i. */
  private def oneOf(t: Type): String = t match
    case TyInteger => "1"
    case TyReal    => "1.0"
    case TyComplex =>
      val c0 = newReg()
      emitLine(s"  $c0 = insertvalue { double, double } undef, double 1.0, 0\n")
      val c1 = newReg()
      emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
      c1
    case _ => "1"

  // ---------------------------------------------------------------------------
  // §10.4 flatMap — concat-map: for each element, call f, append all
  // returned values to the output buffer. Worst-case sized buffer
  // grows as we go and is truncated at the end (same pattern as
  // emitFilterCall, but copying every returned element rather than
  // gating on a predicate). The closure's return type is `[T]` so
  // each call yields an array descriptor that we walk and copy from.
  // ---------------------------------------------------------------------------

  private def emitFlatMapCall(arr: TExpr, fn: TExpr, resultT: Type): String =
    if arrayRank(arr.tpe) != 1 then
      notYet(s"flatMap on rank ${arrayRank(arr.tpe)}")
      return "0"

    val srcElem = arrayElem(arr.tpe)
    val srcEsz  = elemSize(srcElem)
    val srcStT  = storageType(srcElem)
    val srcLLT  = llvmType(srcElem)
    val resElem = arrayElem(resultT)
    val resEsz  = elemSize(resElem)
    val resStT  = storageType(resElem)
    val resLLT  = llvmType(resElem)

    val arrV    = emitExpr(arr)
    val (fnPtr, envPtr) = splitClosure(fn)
    val srcLen  = newReg()
    emitLine(s"  $srcLen = call i64 @__nex_arr1_len(ptr $arrV)\n")
    val srcBuf  = bufPtr(arrV, arr.tpe)

    // Per-source-element loop:
    //   for i in 0..srcLen:
    //     subarr = call fnPtr(envPtr, srcBuf[i])
    //     subLen = __nex_arr1_len(subarr)
    //     for j in 0..subLen: out[count++] = subarr[j]
    //     dec subarr
    //
    // We DON'T know the total output length ahead of time. Approach:
    // 1) two passes — first counts total, second fills (calls f twice)
    // 2) grow-as-we-go — start with srcLen capacity, double when full
    // (1) double-calls the closure (might have side effects); (2) is
    // standard. Implement (2): start with an initial guess of
    // srcLen, realloc bigger if needed.
    //
    // For simplicity here use the simpler initial-overallocate of
    // (srcLen * 4) which is sufficient for most flatMaps where the
    // average expansion factor is small; truncate the descriptor to
    // the actual count at the end. Matches the interpreter's
    // ArrayBuffer.append semantics (no observable difference for
    // small programs; a follow-up can add real grow logic for
    // pathological cases).
    val initialCap = newReg()
    emitLine(s"  $initialCap = mul i64 $srcLen, 4\n")
    // Guard against initialCap=0 (empty source): allocate at least 1
    // slot to avoid malloc(0) UB; descriptor length gets set to 0
    // before the dec helper sees it.
    val isZero = newReg()
    emitLine(s"  $isZero = icmp eq i64 $initialCap, 0\n")
    val cap = newReg()
    emitLine(s"  $cap = select i1 $isZero, i64 1, i64 $initialCap\n")
    val res = newReg()
    emitLine(s"  $res = call ptr @__nex_arr1_alloc(i64 $cap, i64 $resEsz)\n")
    val dstBuf = bufPtr(res, resultT)
    val countSlot = newReg()
    emitLine(s"  $countSlot = alloca i64\n")
    emitLine(s"  store i64 0, ptr $countSlot\n")

    emitCountingLoop(srcLen, "hof.flatMap") { i =>
      // Call f(srcBuf[i]) — returns a rank-1 array descriptor (ptr).
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $srcStT, ptr $srcBuf, i64 $i\n")
      val srcE    = loadElem(srcStT, srcSlot, srcLLT)
      val subArr  = newReg()
      emitLine(s"  $subArr = call ptr (ptr, $srcLLT) $fnPtr(ptr $envPtr, $srcLLT $srcE)\n")
      val subLen  = newReg()
      emitLine(s"  $subLen = call i64 @__nex_arr1_len(ptr $subArr)\n")
      val subBuf  = bufPtr(subArr, resultT)
      // Inner copy loop.
      emitCountingLoop(subLen, "hof.flatMap.inner") { j =>
        val sSlot = newReg()
        emitLine(s"  $sSlot = getelementptr inbounds $resStT, ptr $subBuf, i64 $j\n")
        val sE    = loadElem(resStT, sSlot, resLLT)
        val cur   = newReg()
        emitLine(s"  $cur = load i64, ptr $countSlot\n")
        val dSlot = newReg()
        emitLine(s"  $dSlot = getelementptr inbounds $resStT, ptr $dstBuf, i64 $cur\n")
        storeElem(resStT, sE, dSlot)
        val nx    = newReg()
        emitLine(s"  $nx = add i64 $cur, 1\n")
        emitLine(s"  store i64 $nx, ptr $countSlot\n")
      }
      // Release the sub-array's owning share.
      emitArrDec(subArr, resultT)
    }

    // Truncate the descriptor's length field to the actual count.
    val finalCount = newReg()
    emitLine(s"  $finalCount = load i64, ptr $countSlot\n")
    val lenP = newReg()
    emitLine(s"  $lenP = getelementptr inbounds %nex_arr1, ptr $res, i32 0, i32 1\n")
    emitLine(s"  store i64 $finalCount, ptr $lenP\n")

    emitArrDec(arrV, arr.tpe)
    emitLine(s"  call void @__nex_env_dec(ptr $envPtr)\n")
    res

  // ---------------------------------------------------------------------------
  // §10.4 rank-1 builders: range / enumerate / zip / linspace.
  // ---------------------------------------------------------------------------

  /** Emit `range(lo, hi)` — integer half-open range. Length is
    * `max(0, hi - lo)`; element i is `lo + i`.
    */
  /** Emit `view(a, lo..hi)` — a non-copying borrow into `a`'s buffer.
    * The range argument is a `TBinOp("..", lo, hi)` (exclusive) or
    * `("..=", lo, hi)` (inclusive); we unpack both bounds, wrap any
    * negative indices against the source length, normalise inclusive
    * to exclusive (`hi+1`), and call `__nex_arr1_view`. The runtime
    * helper does its own bounds check and incs the owner refcount.
    *
    * Result is a fresh rank-1 descriptor whose `data` field aliases
    * the source's buffer at offset `lo*elem_size` and whose `owner`
    * field points at the source (or, for view-of-view, the root
    * owner — the runtime collapses chains).
    */
  private def emitViewCall(arr: TExpr, r: TExpr, resultT: Type): String =
    val (loE, hiE, inclusive) = r match
      case TBinOp(op, lo, hi, _, _) if op == ".." || op == "..=" =>
        (lo, hi, op == "..=")
      case _ =>
        notImpl(s"view requires a range argument, got ${r.tpe}")
    val esz = elemSize(arrayElem(arr.tpe))
    val av  = emitExpr(arr)
    val srcLen = newReg(); emitLine(s"  $srcLen = call i64 @__nex_arr1_len(ptr $av)\n")
    // Wrap negative bounds against srcLen, mirroring `__nex_arr1_slot`
    // and `wrapNegBound` in NexLLVMArrays. Replicated locally because
    // the sibling trait's protected helper isn't reachable from here.
    def wrapNegHere(raw: String): String =
      val isNeg = newReg(); emitLine(s"  $isNeg = icmp slt i64 $raw, 0\n")
      val wrapped = newReg(); emitLine(s"  $wrapped = add i64 $raw, $srcLen\n")
      val out = newReg(); emitLine(s"  $out = select i1 $isNeg, i64 $wrapped, i64 $raw\n")
      out
    val loV = wrapNegHere(emitExpr(loE))
    val hiRaw = wrapNegHere(emitExpr(hiE))
    val hiV =
      if inclusive then
        val r = newReg(); emitLine(s"  $r = add i64 $hiRaw, 1\n"); r
      else hiRaw
    val res = newReg()
    emitLine(s"  $res = call ptr @__nex_arr1_view(ptr $av, i64 $loV, i64 $hiV, i64 $esz)\n")
    emitArrDec(av, arr.tpe)
    res

  private def emitRangeCall(loE: TExpr, hiE: TExpr): String =
    val lo = emitExpr(loE)
    val hi = emitExpr(hiE)
    val diff = newReg()
    emitLine(s"  $diff = sub i64 $hi, $lo\n")
    val neg  = newReg()
    emitLine(s"  $neg = icmp slt i64 $diff, 0\n")
    val len  = newReg()
    emitLine(s"  $len = select i1 $neg, i64 0, i64 $diff\n")
    val arr  = newReg()
    emitLine(s"  $arr = call ptr @__nex_arr1_alloc(i64 $len, i64 8)\n")
    val buf  = bufPtr(arr, TyArray(TyInteger, 1))

    emitCountingLoop(len, "range") { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds i64, ptr $buf, i64 $i\n")
      val v    = newReg()
      emitLine(s"  $v = add i64 $lo, $i\n")
      emitLine(s"  store i64 $v, ptr $slot\n")
    }
    arr

  /** Emit `enumerate(arr)` — pairs `(index, value)`. */
  private def emitEnumerateCall(arr: TExpr, resultT: Type): String =
    val srcElem = arrayElem(arr.tpe)
    val srcStT  = storageType(srcElem)
    val srcLLT  = llvmType(srcElem)
    val outElem = arrayElem(resultT)
    val outSize = elemSize(outElem)
    val outTupTy = llvmType(outElem)

    val src    = emitExpr(arr)
    val len    = newReg(); emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $src)\n")
    val srcBuf = bufPtr(src, arr.tpe)
    val out    = newReg()
    emitLine(s"  $out = call ptr @__nex_arr1_alloc(i64 $len, i64 $outSize)\n")
    val outBuf = bufPtr(out, resultT)

    emitCountingLoop(len, "enumerate") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $srcStT, ptr $srcBuf, i64 $i\n")
      val srcE    = loadElem(srcStT, srcSlot, srcLLT)
      val acc0    = newReg()
      emitLine(s"  $acc0 = insertvalue $outTupTy undef, i64 $i, 0\n")
      val acc1    = newReg()
      emitLine(s"  $acc1 = insertvalue $outTupTy $acc0, $srcLLT $srcE, 1\n")
      val outSlot = newReg()
      emitLine(s"  $outSlot = getelementptr inbounds $outTupTy, ptr $outBuf, i64 $i\n")
      emitLine(s"  store $outTupTy $acc1, ptr $outSlot\n")
    }
    emitArrDec(src, arr.tpe)
    out

  /** Emit `zip(a, b)` — pairs of corresponding elements. Length is
    * `min(len(a), len(b))`.
    */
  private def emitZipCall(a: TExpr, b: TExpr, resultT: Type): String =
    val aElem = arrayElem(a.tpe)
    val bElem = arrayElem(b.tpe)
    val aStT  = storageType(aElem)
    val bStT  = storageType(bElem)
    val aLLT  = llvmType(aElem)
    val bLLT  = llvmType(bElem)
    val outElem = arrayElem(resultT)
    val outSize = elemSize(outElem)
    val outTupTy = llvmType(outElem)

    val av    = emitExpr(a)
    val bv    = emitExpr(b)
    val aLen  = newReg(); emitLine(s"  $aLen = call i64 @__nex_arr1_len(ptr $av)\n")
    val bLen  = newReg(); emitLine(s"  $bLen = call i64 @__nex_arr1_len(ptr $bv)\n")
    val cmp   = newReg(); emitLine(s"  $cmp = icmp slt i64 $aLen, $bLen\n")
    val len   = newReg(); emitLine(s"  $len = select i1 $cmp, i64 $aLen, i64 $bLen\n")
    val aBuf  = bufPtr(av, a.tpe)
    val bBuf  = bufPtr(bv, b.tpe)
    val out   = newReg()
    emitLine(s"  $out = call ptr @__nex_arr1_alloc(i64 $len, i64 $outSize)\n")
    val outBuf = bufPtr(out, resultT)

    emitCountingLoop(len, "zip") { i =>
      val aS = newReg()
      emitLine(s"  $aS = getelementptr inbounds $aStT, ptr $aBuf, i64 $i\n")
      val ae = loadElem(aStT, aS, aLLT)
      val bS = newReg()
      emitLine(s"  $bS = getelementptr inbounds $bStT, ptr $bBuf, i64 $i\n")
      val be = loadElem(bStT, bS, bLLT)
      val acc0 = newReg()
      emitLine(s"  $acc0 = insertvalue $outTupTy undef, $aLLT $ae, 0\n")
      val acc1 = newReg()
      emitLine(s"  $acc1 = insertvalue $outTupTy $acc0, $bLLT $be, 1\n")
      val outSlot = newReg()
      emitLine(s"  $outSlot = getelementptr inbounds $outTupTy, ptr $outBuf, i64 $i\n")
      emitLine(s"  store $outTupTy $acc1, ptr $outSlot\n")
    }
    emitArrDec(av, a.tpe)
    emitArrDec(bv, b.tpe)
    out

  /** Emit `linspace(lo, hi, n)` — n evenly-spaced reals from lo to hi.
    * When n > 1, step is `(hi - lo) / (n - 1)`; when n <= 1, step is 0
    * and the single element is `lo`.
    */
  private def emitLinspaceCall(loE: TExpr, hiE: TExpr, nE: TExpr): String =
    val lo = liftToReal(loE)
    val hi = liftToReal(hiE)
    val n  = emitExpr(nE)
    val arr = newReg()
    emitLine(s"  $arr = call ptr @__nex_arr1_alloc(i64 $n, i64 8)\n")
    val buf = bufPtr(arr, TyArray(TyReal, 1))

    val gt1     = newReg(); emitLine(s"  $gt1 = icmp sgt i64 $n, 1\n")
    val nm1     = newReg(); emitLine(s"  $nm1 = sub i64 $n, 1\n")
    val nm1d    = newReg(); emitLine(s"  $nm1d = sitofp i64 $nm1 to double\n")
    val span    = newReg(); emitLine(s"  $span = fsub double $hi, $lo\n")
    val rawStep = newReg(); emitLine(s"  $rawStep = fdiv double $span, $nm1d\n")
    val step    = newReg(); emitLine(s"  $step = select i1 $gt1, double $rawStep, double 0.0\n")

    emitCountingLoop(n, "linspace") { i =>
      val id   = newReg(); emitLine(s"  $id = sitofp i64 $i to double\n")
      val off  = newReg(); emitLine(s"  $off = fmul double $step, $id\n")
      val v    = newReg(); emitLine(s"  $v = fadd double $lo, $off\n")
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds double, ptr $buf, i64 $i\n")
      emitLine(s"  store double $v, ptr $slot\n")
    }
    arr

  // ---------------------------------------------------------------------------
  // §10.4 rank-2 ops.
  //
  // All of these read an existing array, allocate a result, and dec the
  // input's owning share before returning. The interpreter is the
  // reference — see the matching cases in NexInterpreter.callPrelude /
  // NexInterpreter.matMul for the exact element-wise semantics.
  // ---------------------------------------------------------------------------

  /** `shape(arr)` — returns a tuple of the array's dimensions:
    *   rank-1 →  `(n,)`        (one-element tuple — printed as `(n)`)
    *   rank-2 →  `(rows, cols)`
    */
  private def emitShapeCall(arr: TExpr, resultT: Type): String =
    val arrV = emitExpr(arr)
    val tupTy = llvmType(resultT)
    val result = arrayRank(arr.tpe) match
      case 1 =>
        val len = newReg()
        emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $arrV)\n")
        val t   = newReg()
        emitLine(s"  $t = insertvalue $tupTy undef, i64 $len, 0\n")
        t
      case 2 =>
        val rows = newReg(); emitLine(s"  $rows = call i64 @__nex_arr2_rows(ptr $arrV)\n")
        val cols = newReg(); emitLine(s"  $cols = call i64 @__nex_arr2_cols(ptr $arrV)\n")
        val t0   = newReg()
        emitLine(s"  $t0 = insertvalue $tupTy undef, i64 $rows, 0\n")
        val t1   = newReg()
        emitLine(s"  $t1 = insertvalue $tupTy $t0, i64 $cols, 1\n")
        t1
      case r => notYet(s"shape on rank $r"); "undef"
    emitArrDec(arrV, arr.tpe)
    result

  /** `transpose(m)` — rank-2 only. Allocates an `cols × rows` result and
    * walks the source row-major copying `src[i,j]` to `dst[j,i]`.
    * Source storage is row-major (`src[i*cols + j]`) per __nex_arr2_slot.
    */
  private def emitTransposeCall(arr: TExpr, resultT: Type): String =
    if arrayRank(arr.tpe) != 2 then
      notYet(s"transpose on rank ${arrayRank(arr.tpe)} (rank-2 only)")
      return "null"
    val elem = arrayElem(arr.tpe)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val llT  = llvmType(elem)
    val arrV = emitExpr(arr)
    val rows = newReg(); emitLine(s"  $rows = call i64 @__nex_arr2_rows(ptr $arrV)\n")
    val cols = newReg(); emitLine(s"  $cols = call i64 @__nex_arr2_cols(ptr $arrV)\n")
    // result is cols × rows, same element size
    val res  = newReg()
    emitLine(s"  $res = call ptr @__nex_arr2_alloc(i64 $cols, i64 $rows, i64 $esz)\n")
    val srcBuf = bufPtr(arrV, arr.tpe)
    val dstBuf = bufPtr(res, resultT)
    // for i in 0..rows: for j in 0..cols: dst[j*rows + i] = src[i*cols + j]
    emitCountingLoop(rows, "tp.r") { i =>
      emitCountingLoop(cols, "tp.c") { j =>
        val srcOff = newReg(); emitLine(s"  $srcOff = mul i64 $i, $cols\n")
        val srcK   = newReg(); emitLine(s"  $srcK = add i64 $srcOff, $j\n")
        val srcSlt = newReg(); emitLine(s"  $srcSlt = getelementptr inbounds $stT, ptr $srcBuf, i64 $srcK\n")
        val v      = loadElem(stT, srcSlt, llT)
        val dstOff = newReg(); emitLine(s"  $dstOff = mul i64 $j, $rows\n")
        val dstK   = newReg(); emitLine(s"  $dstK = add i64 $dstOff, $i\n")
        val dstSlt = newReg(); emitLine(s"  $dstSlt = getelementptr inbounds $stT, ptr $dstBuf, i64 $dstK\n")
        storeElem(stT, v, dstSlt)
      }
    }
    emitArrDec(arrV, arr.tpe)
    res

  /** `matmul(a, b)` — naive triple-loop matrix multiply. Mirrors the
    * interpreter's [[matMul]]: handles all four (rank, rank) combos.
    *   2×2 → result is rank-2 (m×p)
    *   2×1 → result is rank-1 (m)
    *   1×2 → result is rank-1 (p)
    *   1×1 → result is a scalar (dot product)
    */
  private def emitMatMulCall(aE: TExpr, bE: TExpr, resultT: Type): String =
    emitMatMulShared(aE, bE, resultT)

  /** Used by both `matmul(a, b)` and the `@` operator ([[TMatMul]] node).
    * Kept as a single implementation so the two paths can never drift.
    */
  protected def emitMatMulShared(aE: TExpr, bE: TExpr, resultT: Type): String =
    val ar = arrayRank(aE.tpe)
    val br = arrayRank(bE.tpe)
    val elem = arrayElem(resultT) match
      case TyUnknown =>
        // 1×1 case: result is a scalar; pick element type from the
        // inputs (interpreter promotes via addV/mulV — we use the
        // source element since matmul typing requires equal element
        // types anyway).
        arrayElem(aE.tpe)
      case e => e
    val stT = storageType(elem)
    val llT = llvmType(elem)
    val aV  = emitExpr(aE)
    val bV  = emitExpr(bE)
    val aBuf = bufPtr(aV, aE.tpe)
    val bBuf = bufPtr(bV, bE.tpe)

    val result = (ar, br) match
      case (2, 2) =>
        // a: m×n   b: n×p   →   c: m×p
        val m = newReg(); emitLine(s"  $m = call i64 @__nex_arr2_rows(ptr $aV)\n")
        val n = newReg(); emitLine(s"  $n = call i64 @__nex_arr2_cols(ptr $aV)\n")
        val p = newReg(); emitLine(s"  $p = call i64 @__nex_arr2_cols(ptr $bV)\n")
        val res = newReg()
        emitLine(s"  $res = call ptr @__nex_arr2_alloc(i64 $m, i64 $p, i64 ${elemSize(elem)})\n")
        val cBuf = bufPtr(res, resultT)
        emitCountingLoop(m, "mm.i") { i =>
          emitCountingLoop(p, "mm.j") { j =>
            val accSlot = newReg(); emitLine(s"  $accSlot = alloca $llT\n")
            storeElem(stT, zeroOf(elem), accSlot)
            emitCountingLoop(n, "mm.k") { k =>
              val aOff = newReg(); emitLine(s"  $aOff = mul i64 $i, $n\n")
              val aK   = newReg(); emitLine(s"  $aK = add i64 $aOff, $k\n")
              val aSlt = newReg(); emitLine(s"  $aSlt = getelementptr inbounds $stT, ptr $aBuf, i64 $aK\n")
              val av   = loadElem(stT, aSlt, llT)
              val bOff = newReg(); emitLine(s"  $bOff = mul i64 $k, $p\n")
              val bK   = newReg(); emitLine(s"  $bK = add i64 $bOff, $j\n")
              val bSlt = newReg(); emitLine(s"  $bSlt = getelementptr inbounds $stT, ptr $bBuf, i64 $bK\n")
              val bv   = loadElem(stT, bSlt, llT)
              val prod = emitScalarBinOpSimple("*", av, bv, elem)
              val cur  = loadElem(stT, accSlot, llT)
              val nxt  = emitScalarBinOpSimple("+", cur, prod, elem)
              storeElem(stT, nxt, accSlot)
            }
            val acc = loadElem(stT, accSlot, llT)
            val cOff = newReg(); emitLine(s"  $cOff = mul i64 $i, $p\n")
            val cK   = newReg(); emitLine(s"  $cK = add i64 $cOff, $j\n")
            val cSlt = newReg(); emitLine(s"  $cSlt = getelementptr inbounds $stT, ptr $cBuf, i64 $cK\n")
            storeElem(stT, acc, cSlt)
          }
        }
        res
      case (2, 1) =>
        // a: m×n   b: n   →   c: m
        val m = newReg(); emitLine(s"  $m = call i64 @__nex_arr2_rows(ptr $aV)\n")
        val n = newReg(); emitLine(s"  $n = call i64 @__nex_arr2_cols(ptr $aV)\n")
        val res = newReg()
        emitLine(s"  $res = call ptr @__nex_arr1_alloc(i64 $m, i64 ${elemSize(elem)})\n")
        val cBuf = bufPtr(res, resultT)
        emitCountingLoop(m, "mv.i") { i =>
          val accSlot = newReg(); emitLine(s"  $accSlot = alloca $llT\n")
          storeElem(stT, zeroOf(elem), accSlot)
          emitCountingLoop(n, "mv.k") { k =>
            val aOff = newReg(); emitLine(s"  $aOff = mul i64 $i, $n\n")
            val aK   = newReg(); emitLine(s"  $aK = add i64 $aOff, $k\n")
            val aSlt = newReg(); emitLine(s"  $aSlt = getelementptr inbounds $stT, ptr $aBuf, i64 $aK\n")
            val av   = loadElem(stT, aSlt, llT)
            val bSlt = newReg(); emitLine(s"  $bSlt = getelementptr inbounds $stT, ptr $bBuf, i64 $k\n")
            val bv   = loadElem(stT, bSlt, llT)
            val prod = emitScalarBinOpSimple("*", av, bv, elem)
            val cur  = loadElem(stT, accSlot, llT)
            val nxt  = emitScalarBinOpSimple("+", cur, prod, elem)
            storeElem(stT, nxt, accSlot)
          }
          val acc = loadElem(stT, accSlot, llT)
          val cSlt = newReg(); emitLine(s"  $cSlt = getelementptr inbounds $stT, ptr $cBuf, i64 $i\n")
          storeElem(stT, acc, cSlt)
        }
        res
      case (1, 2) =>
        // a: n     b: n×p   →   c: p
        val n = newReg(); emitLine(s"  $n = call i64 @__nex_arr1_len(ptr $aV)\n")
        val p = newReg(); emitLine(s"  $p = call i64 @__nex_arr2_cols(ptr $bV)\n")
        val res = newReg()
        emitLine(s"  $res = call ptr @__nex_arr1_alloc(i64 $p, i64 ${elemSize(elem)})\n")
        val cBuf = bufPtr(res, resultT)
        emitCountingLoop(p, "vm.j") { j =>
          val accSlot = newReg(); emitLine(s"  $accSlot = alloca $llT\n")
          storeElem(stT, zeroOf(elem), accSlot)
          emitCountingLoop(n, "vm.k") { k =>
            val aSlt = newReg(); emitLine(s"  $aSlt = getelementptr inbounds $stT, ptr $aBuf, i64 $k\n")
            val av   = loadElem(stT, aSlt, llT)
            val bOff = newReg(); emitLine(s"  $bOff = mul i64 $k, $p\n")
            val bK   = newReg(); emitLine(s"  $bK = add i64 $bOff, $j\n")
            val bSlt = newReg(); emitLine(s"  $bSlt = getelementptr inbounds $stT, ptr $bBuf, i64 $bK\n")
            val bv   = loadElem(stT, bSlt, llT)
            val prod = emitScalarBinOpSimple("*", av, bv, elem)
            val cur  = loadElem(stT, accSlot, llT)
            val nxt  = emitScalarBinOpSimple("+", cur, prod, elem)
            storeElem(stT, nxt, accSlot)
          }
          val acc = loadElem(stT, accSlot, llT)
          val cSlt = newReg(); emitLine(s"  $cSlt = getelementptr inbounds $stT, ptr $cBuf, i64 $j\n")
          storeElem(stT, acc, cSlt)
        }
        res
      case (1, 1) =>
        // Degenerate "dot product" form. Result is a scalar.
        val n = newReg(); emitLine(s"  $n = call i64 @__nex_arr1_len(ptr $aV)\n")
        val accSlot = newReg(); emitLine(s"  $accSlot = alloca $llT\n")
        storeElem(stT, zeroOf(elem), accSlot)
        emitCountingLoop(n, "dot.k") { k =>
          val aSlt = newReg(); emitLine(s"  $aSlt = getelementptr inbounds $stT, ptr $aBuf, i64 $k\n")
          val av   = loadElem(stT, aSlt, llT)
          val bSlt = newReg(); emitLine(s"  $bSlt = getelementptr inbounds $stT, ptr $bBuf, i64 $k\n")
          val bv   = loadElem(stT, bSlt, llT)
          val prod = emitScalarBinOpSimple("*", av, bv, elem)
          val cur  = loadElem(stT, accSlot, llT)
          val nxt  = emitScalarBinOpSimple("+", cur, prod, elem)
          storeElem(stT, nxt, accSlot)
        }
        loadElem(stT, accSlot, llT)
      case (ra, rb) =>
        notYet(s"matmul ranks $ra × $rb"); "null"

    emitArrDec(aV, aE.tpe)
    emitArrDec(bV, bE.tpe)
    result

  /** `diag(v)` — builds an n×n rank-2 matrix with `v` on the diagonal,
    * zero elsewhere. Interpreter doesn't yet implement the rank-2 →
    * rank-1 (extract-diagonal) direction; we follow suit.
    */
  private def emitDiagCall(arr: TExpr, resultT: Type): String =
    if arrayRank(arr.tpe) != 1 then
      notYet(s"diag on rank ${arrayRank(arr.tpe)} (rank-1 only)")
      return "null"
    val elem = arrayElem(arr.tpe)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val llT  = llvmType(elem)
    val arrV = emitExpr(arr)
    val n    = newReg(); emitLine(s"  $n = call i64 @__nex_arr1_len(ptr $arrV)\n")
    val res  = newReg()
    emitLine(s"  $res = call ptr @__nex_arr2_alloc(i64 $n, i64 $n, i64 $esz)\n")
    val dstBuf = bufPtr(res, resultT)
    // Zero-fill the n*n flat buffer first.
    val total  = newReg(); emitLine(s"  $total = mul i64 $n, $n\n")
    emitCountingLoop(total, "diag.zero") { k =>
      val slot = newReg(); emitLine(s"  $slot = getelementptr inbounds $stT, ptr $dstBuf, i64 $k\n")
      storeElem(stT, zeroOf(elem), slot)
    }
    val srcBuf = bufPtr(arrV, arr.tpe)
    // Stamp diagonal: dst[i*n + i] = src[i]
    emitCountingLoop(n, "diag.fill") { i =>
      val srcSlt = newReg(); emitLine(s"  $srcSlt = getelementptr inbounds $stT, ptr $srcBuf, i64 $i\n")
      val v      = loadElem(stT, srcSlt, llT)
      val off    = newReg(); emitLine(s"  $off = mul i64 $i, $n\n")
      val k      = newReg(); emitLine(s"  $k = add i64 $off, $i\n")
      val dstSlt = newReg(); emitLine(s"  $dstSlt = getelementptr inbounds $stT, ptr $dstBuf, i64 $k\n")
      storeElem(stT, v, dstSlt)
    }
    emitArrDec(arrV, arr.tpe)
    res

  /** `reshape(arr, rows, cols)` — interpret `arr` as a column-major flat
    * buffer of an `m × n` matrix and re-bake it into the internal
    * row-major storage. Mirror of the interpreter loop: walk the source
    * as `flat[c*rows + r]` for each (r, c) of the result. Source may be
    * rank-1 or rank-2; the interpreter `flatten1` accepts both.
    */
  private def emitReshapeCall(arr: TExpr, rowsE: TExpr, colsE: TExpr, resultT: Type): String =
    val elem = arrayElem(resultT)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val llT  = llvmType(elem)
    val arrV = emitExpr(arr)
    val rows = emitExpr(rowsE)
    val cols = emitExpr(colsE)
    val res  = newReg()
    emitLine(s"  $res = call ptr @__nex_arr2_alloc(i64 $rows, i64 $cols, i64 $esz)\n")
    val srcBuf = bufPtr(arrV, arr.tpe)
    val dstBuf = bufPtr(res, resultT)
    // for r in 0..rows: for c in 0..cols: dst[r*cols + c] = src[c*rows + r]
    emitCountingLoop(rows, "rs.r") { r =>
      emitCountingLoop(cols, "rs.c") { c =>
        val srcOff = newReg(); emitLine(s"  $srcOff = mul i64 $c, $rows\n")
        val srcK   = newReg(); emitLine(s"  $srcK = add i64 $srcOff, $r\n")
        val srcSlt = newReg(); emitLine(s"  $srcSlt = getelementptr inbounds $stT, ptr $srcBuf, i64 $srcK\n")
        val v      = loadElem(stT, srcSlt, llT)
        val dstOff = newReg(); emitLine(s"  $dstOff = mul i64 $r, $cols\n")
        val dstK   = newReg(); emitLine(s"  $dstK = add i64 $dstOff, $c\n")
        val dstSlt = newReg(); emitLine(s"  $dstSlt = getelementptr inbounds $stT, ptr $dstBuf, i64 $dstK\n")
        storeElem(stT, v, dstSlt)
      }
    }
    emitArrDec(arrV, arr.tpe)
    res

  /** `flatten(m)` — column-major flatten. Mirror of the interpreter:
    *   rank-1: clone (still a fresh rank-1)
    *   rank-2: walk columns first, rows inside — `out[idx++] = src[row*cols + col]`
    */
  private def emitFlattenCall(arr: TExpr, resultT: Type): String =
    val elem = arrayElem(arr.tpe)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val llT  = llvmType(elem)
    val arrV = emitExpr(arr)

    val result = arrayRank(arr.tpe) match
      case 1 =>
        val n   = newReg(); emitLine(s"  $n = call i64 @__nex_arr1_len(ptr $arrV)\n")
        val res = newReg()
        emitLine(s"  $res = call ptr @__nex_arr1_alloc(i64 $n, i64 $esz)\n")
        val srcBuf = bufPtr(arrV, arr.tpe)
        val dstBuf = bufPtr(res, resultT)
        emitCountingLoop(n, "fl.k") { k =>
          val srcSlt = newReg(); emitLine(s"  $srcSlt = getelementptr inbounds $stT, ptr $srcBuf, i64 $k\n")
          val v      = loadElem(stT, srcSlt, llT)
          val dstSlt = newReg(); emitLine(s"  $dstSlt = getelementptr inbounds $stT, ptr $dstBuf, i64 $k\n")
          storeElem(stT, v, dstSlt)
        }
        res
      case 2 =>
        val rows = newReg(); emitLine(s"  $rows = call i64 @__nex_arr2_rows(ptr $arrV)\n")
        val cols = newReg(); emitLine(s"  $cols = call i64 @__nex_arr2_cols(ptr $arrV)\n")
        val total = newReg(); emitLine(s"  $total = mul i64 $rows, $cols\n")
        val res  = newReg()
        emitLine(s"  $res = call ptr @__nex_arr1_alloc(i64 $total, i64 $esz)\n")
        val srcBuf = bufPtr(arrV, arr.tpe)
        val dstBuf = bufPtr(res, resultT)
        val idxSlot = newReg(); emitLine(s"  $idxSlot = alloca i64\n")
        emitLine(s"  store i64 0, ptr $idxSlot\n")
        emitCountingLoop(cols, "fl.col") { col =>
          emitCountingLoop(rows, "fl.row") { row =>
            val srcOff = newReg(); emitLine(s"  $srcOff = mul i64 $row, $cols\n")
            val srcK   = newReg(); emitLine(s"  $srcK = add i64 $srcOff, $col\n")
            val srcSlt = newReg(); emitLine(s"  $srcSlt = getelementptr inbounds $stT, ptr $srcBuf, i64 $srcK\n")
            val v      = loadElem(stT, srcSlt, llT)
            val idx    = newReg(); emitLine(s"  $idx = load i64, ptr $idxSlot\n")
            val dstSlt = newReg(); emitLine(s"  $dstSlt = getelementptr inbounds $stT, ptr $dstBuf, i64 $idx\n")
            storeElem(stT, v, dstSlt)
            val nxt    = newReg(); emitLine(s"  $nxt = add i64 $idx, 1\n")
            emitLine(s"  store i64 $nxt, ptr $idxSlot\n")
          }
        }
        res
      case r => notYet(s"flatten on rank $r"); "null"

    emitArrDec(arrV, arr.tpe)
    result

  /** Emit `sum_axis(m, axis)` — collapse one axis of a rank-2 matrix
    * into a rank-1 vector. axis=0 sums down columns (result len = cols);
    * axis=1 sums across rows (result len = rows). The axis value is a
    * runtime integer and we dispatch on it; anything outside {0, 1}
    * traps via @abort. Element type is preserved.
    */
  private def emitSumAxisCall(m: TExpr, axisE: TExpr, resultT: Type): String =
    if arrayRank(m.tpe) != 2 then
      notYet(s"sum_axis on rank ${arrayRank(m.tpe)} (only rank-2 supported)")
      return "0"
    val elem   = arrayElem(m.tpe)
    val esz    = elemSize(elem)
    val stT    = storageType(elem)
    val llT    = llvmType(elem)
    val accLLT = llT

    val mV   = emitExpr(m)
    val axV  = emitExpr(axisE)
    val rows = newReg(); emitLine(s"  $rows = call i64 @__nex_arr2_rows(ptr $mV)\n")
    val cols = newReg(); emitLine(s"  $cols = call i64 @__nex_arr2_cols(ptr $mV)\n")
    val buf  = bufPtr(mV, m.tpe)

    val resSlot = newReg()
    emitLine(s"  $resSlot = alloca ptr\n")

    val ax0L    = freshLabel("sumax.0")
    val ax1Try  = freshLabel("sumax.tryax1")
    val ax1L    = freshLabel("sumax.1")
    val badL    = freshLabel("sumax.bad")
    val doneL   = freshLabel("sumax.done")

    val isZero  = newReg()
    emitLine(s"  $isZero = icmp eq i64 $axV, 0\n")
    emitTerminator(s"  br i1 $isZero, label %$ax0L, label %$ax1Try\n")

    // axis = 0: result has `cols` slots, each is the sum over `rows`
    // elements at column j (source idx = i*cols + j).
    startBlock(ax0L)
    val res0  = newReg()
    emitLine(s"  $res0 = call ptr @__nex_arr1_alloc(i64 $cols, i64 $esz)\n")
    val dst0  = bufPtr(res0, resultT)
    emitCountingLoop(cols, "sumax.0.col") { j =>
      val accSlot = newReg()
      emitLine(s"  $accSlot = alloca $accLLT\n")
      storeElem(stT, zeroOf(elem), accSlot)
      emitCountingLoop(rows, "sumax.0.row") { i =>
        val rowOff = newReg(); emitLine(s"  $rowOff = mul i64 $i, $cols\n")
        val k      = newReg(); emitLine(s"  $k = add i64 $rowOff, $j\n")
        val slot   = newReg(); emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $k\n")
        val e      = loadElem(stT, slot, llT)
        val cur    = newReg(); emitLine(s"  $cur = load $accLLT, ptr $accSlot\n")
        val nxt    = emitScalarBinOpSimple("+", cur, e, elem)
        storeElem(stT, nxt, accSlot)
      }
      val accV  = newReg()
      emitLine(s"  $accV = load $accLLT, ptr $accSlot\n")
      val dstSl = newReg()
      emitLine(s"  $dstSl = getelementptr inbounds $stT, ptr $dst0, i64 $j\n")
      storeElem(stT, accV, dstSl)
    }
    emitLine(s"  store ptr $res0, ptr $resSlot\n")
    emitTerminator(s"  br label %$doneL\n")

    startBlock(ax1Try)
    val isOne = newReg()
    emitLine(s"  $isOne = icmp eq i64 $axV, 1\n")
    emitTerminator(s"  br i1 $isOne, label %$ax1L, label %$badL\n")

    // axis = 1: result has `rows` slots, each is the sum over `cols`
    // elements at row i (source idx = i*cols + j).
    startBlock(ax1L)
    val res1 = newReg()
    emitLine(s"  $res1 = call ptr @__nex_arr1_alloc(i64 $rows, i64 $esz)\n")
    val dst1 = bufPtr(res1, resultT)
    emitCountingLoop(rows, "sumax.1.row") { i =>
      val accSlot = newReg()
      emitLine(s"  $accSlot = alloca $accLLT\n")
      storeElem(stT, zeroOf(elem), accSlot)
      val rowOff = newReg(); emitLine(s"  $rowOff = mul i64 $i, $cols\n")
      emitCountingLoop(cols, "sumax.1.col") { j =>
        val k    = newReg(); emitLine(s"  $k = add i64 $rowOff, $j\n")
        val slot = newReg(); emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $k\n")
        val e    = loadElem(stT, slot, llT)
        val cur  = newReg(); emitLine(s"  $cur = load $accLLT, ptr $accSlot\n")
        val nxt  = emitScalarBinOpSimple("+", cur, e, elem)
        storeElem(stT, nxt, accSlot)
      }
      val accV  = newReg()
      emitLine(s"  $accV = load $accLLT, ptr $accSlot\n")
      val dstSl = newReg()
      emitLine(s"  $dstSl = getelementptr inbounds $stT, ptr $dst1, i64 $i\n")
      storeElem(stT, accV, dstSl)
    }
    emitLine(s"  store ptr $res1, ptr $resSlot\n")
    emitTerminator(s"  br label %$doneL\n")

    startBlock(badL)
    emitLine(s"  call void @__nex_trap_with(ptr @.sum_axis_msg)\n")
    emitTerminator(s"  unreachable\n")

    startBlock(doneL)
    val result = newReg()
    emitLine(s"  $result = load ptr, ptr $resSlot\n")
    emitArrDec(mV, m.tpe)
    result
