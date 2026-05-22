package io.github.edadma.nex

/** §10.8 assertion family for the LLVM backend.
  *
  * Each helper takes the AST nodes of an `assert*` call and emits the
  * matching trap-on-fail sequence. Failure paths route through
  * `__nex_trap_with`, which stashes the static trap-message symbol the
  * runtime's enclosing `assert_traps` sees on a successful longjmp; a
  * caller-supplied `assert_eq` or `assert_approx` message stays distinct
  * from the generic `__nex_assert` form so substring checks in
  * `assert_traps(fn, "...")` keep working.
  *
  * `emitAssertTraps` is the most involved member: it sets up
  * `setjmp`/`longjmp`, calls the thunk through its `{fn_ptr, env_ptr}`
  * pair, and either traps (if no trap fired) or — with the two-arg form —
  * runs a `strstr` over the stashed message text. Stack-slot loads use
  * `volatile` so the LLVM optimiser doesn't cache values across the
  * `returns_twice` setjmp call.
  */
protected trait NexLLVMAsserts extends NexLLVMState:

  protected def emitAssertEq(a: TExpr, b: TExpr): Unit =
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
  protected def emitAssertTraps(fn: TExpr, expectedSubstr: Option[TExpr]): Unit =
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
  protected def emitAssertApprox(a: TExpr, b: TExpr, eps: TExpr): Unit =
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
  protected def emitAssertApproxComplex(a: TExpr, b: TExpr, eps: TExpr): Unit =
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
  protected def emitAssertApproxArr1(a: TExpr, b: TExpr, eps: TExpr): Unit =
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
  protected def emitAssertApproxArr2(a: TExpr, b: TExpr, eps: TExpr): Unit =
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
