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

      // §10.2 scalar math — all unary fns lower to libm doubles. The
      // elaborator marks the result as TyReal; integer operands need
      // a sitofp lift before the call.
      case ("sqrt",  List(x)) => emitLibmUnary("sqrt",  x)
      case ("cbrt",  List(x)) => emitLibmUnary("cbrt",  x)
      case ("exp",   List(x)) => emitLibmUnary("exp",   x)
      case ("log",   List(x)) => emitLibmUnary("log",   x)
      case ("log2",  List(x)) => emitLibmUnary("log2",  x)
      case ("log10", List(x)) => emitLibmUnary("log10", x)
      case ("sin",   List(x)) => emitLibmUnary("sin",   x)
      case ("cos",   List(x)) => emitLibmUnary("cos",   x)
      case ("tan",   List(x)) => emitLibmUnary("tan",   x)
      case ("asin",  List(x)) => emitLibmUnary("asin",  x)
      case ("acos",  List(x)) => emitLibmUnary("acos",  x)
      case ("atan",  List(x)) => emitLibmUnary("atan",  x)
      case ("sinh",  List(x)) => emitLibmUnary("sinh",  x)
      case ("cosh",  List(x)) => emitLibmUnary("cosh",  x)
      case ("tanh",  List(x)) => emitLibmUnary("tanh",  x)
      case ("asinh", List(x)) => emitLibmUnary("__nex_asinh", x)
      case ("acosh", List(x)) => emitLibmUnary("__nex_acosh", x)
      case ("atanh", List(x)) => emitLibmUnary("__nex_atanh", x)
      case ("floor", List(x)) => emitLibmUnary("floor", x)
      case ("ceil",  List(x)) => emitLibmUnary("ceil",  x)
      case ("round", List(x)) => emitLibmUnary("round", x)
      case ("trunc", List(x)) => emitLibmUnary("trunc", x)
      case ("atan2", List(y, x)) =>
        val yv = liftToReal(y)
        val xv = liftToReal(x)
        val reg = newReg()
        emitLine(s"  $reg = call double @atan2(double $yv, double $xv)\n")
        reg

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
      case ("assert", List(c, _)) =>
        // Two-arg form: we drop the message and reuse the generic trap
        // for v0. Matches the interpreter's print-then-abort contract.
        val cv = emitExpr(c)
        emitLine(s"  call void @__nex_assert(i1 $cv)\n")
        "void"
      case ("assert_eq", List(a, b)) =>
        emitAssertEq(a, b); "void"
      case ("assert_approx", List(a, b, eps)) =>
        emitAssertApprox(a, b, eps); "void"

      // §10.4 array HOFs — direct inlined loops that dispatch each
      // iteration through the chunk-9 closure call helper. Works
      // identically for inline TLambda args and TVarRef closure
      // bindings (both emit a `{ptr, ptr}` value via emitExpr).
      case ("map",    List(arr, fn))           => emitMapCall(arr, fn, resultT)
      case ("reduce", List(arr, init, fn))     => emitReduceCall(arr, init, fn, resultT)
      case ("filter", List(arr, fn))           => emitFilterCall(arr, fn, resultT)

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

  /** Emit an equality check between two values, aborting via
    * `__nex_assert(false)` on mismatch. Supports integer / real / bool
    * (with optional sitofp promotion when types differ).
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
    emitLine(s"  call void @__nex_assert(i1 $eq)\n")

  /** Emit `|a - b| <= eps` check, aborting on mismatch. Mixed numeric
    * types promote to double.
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
    emitLine(s"  call void @__nex_assert(i1 $ok)\n")

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
  // §10.4 array higher-order functions (chunk 11).
  //
  // map / reduce / filter all share the same shape:
  //   1. Evaluate `arr` once into an owning SSA value.
  //   2. Evaluate the closure-typed `fn` once and split it into
  //      `{ fn_ptr, env_ptr }` so the loop body can dispatch directly.
  //   3. Iterate 0..len-1, invoking the closure per element.
  //   4. dec the source array (we owned a share).
  //
  // Rank-1 only for now — rank-2 map would preserve shape but the
  // interpreter's filter is rank-1 only too, so the spec target is the
  // rank-1 path. Rank-2 surfaces a `notYet` diag.
  // ---------------------------------------------------------------------------

  /** Emit `map(arr, fn)` — allocate a result array of the same length
    * and store each `fn(arr[i])` into it. The closure's `{ptr, ptr}`
    * value is split into fn_ptr + env_ptr before the loop so the body
    * only needs an indirect call.
    */
  private def emitMapCall(arr: TExpr, fn: TExpr, resultT: Type): String =
    if arrayRank(arr.tpe) != 1 then
      notYet(s"map on rank ${arrayRank(arr.tpe)} (only rank-1 supported)")
      return "0"

    val srcElem = arrayElem(arr.tpe)
    val resElem = arrayElem(resultT)
    val srcEsz  = elemSize(srcElem)
    val resEsz  = elemSize(resElem)
    val srcStT  = storageType(srcElem)
    val srcLLT  = llvmType(srcElem)
    val resStT  = storageType(resElem)
    val resLLT  = llvmType(resElem)

    val arrV    = emitExpr(arr)
    val (fnPtr, envPtr) = splitClosure(fn)
    val len     = newReg()
    emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $arrV)\n")
    val res     = newReg()
    emitLine(s"  $res = call ptr @__nex_arr1_alloc(i64 $len, i64 $resEsz)\n")

    emitCountingLoop(len, "hof.map") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = call ptr @__nex_arr1_slot(ptr $arrV, i64 $i, i64 $srcEsz)\n")
      val elem    = loadElem(srcStT, srcSlot, srcLLT)
      val y       = newReg()
      emitLine(s"  $y = call $resLLT (ptr, $srcLLT) $fnPtr(ptr $envPtr, $srcLLT $elem)\n")
      val dstSlot = newReg()
      emitLine(s"  $dstSlot = call ptr @__nex_arr1_slot(ptr $res, i64 $i, i64 $resEsz)\n")
      storeElem(resStT, y, dstSlot)
    }

    emitArrDec(arrV, arr.tpe)
    res

  /** Emit `reduce(arr, init, fn)` — fold the array left-to-right
    * starting from `init`. The accumulator lives in an alloca slot so
    * we can update-in-place each iteration without phi nodes.
    */
  private def emitReduceCall(arr: TExpr, init: TExpr, fn: TExpr, resultT: Type): String =
    if arrayRank(arr.tpe) != 1 then
      notYet(s"reduce on rank ${arrayRank(arr.tpe)} (only rank-1 supported)")
      return "0"

    val srcElem = arrayElem(arr.tpe)
    val srcEsz  = elemSize(srcElem)
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
    emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $arrV)\n")

    emitCountingLoop(len, "hof.reduce") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = call ptr @__nex_arr1_slot(ptr $arrV, i64 $i, i64 $srcEsz)\n")
      val elem    = loadElem(srcStT, srcSlot, srcLLT)
      val accCur  = newReg()
      emitLine(s"  $accCur = load $accLLT, ptr $accSlot\n")
      val nextAcc = newReg()
      emitLine(s"  $nextAcc = call $accLLT (ptr, $accLLT, $srcLLT) $fnPtr(ptr $envPtr, $accLLT $accCur, $srcLLT $elem)\n")
      emitLine(s"  store $accLLT $nextAcc, ptr $accSlot\n")
    }

    emitArrDec(arrV, arr.tpe)
    val finalAcc = newReg()
    emitLine(s"  $finalAcc = load $accLLT, ptr $accSlot\n")
    finalAcc

  /** Emit `filter(arr, predicate)` — allocate a worst-case-sized result
    * array of length(arr), iterate and copy elements where the
    * predicate returns true, then truncate the descriptor's length
    * field to the actual count. The over-allocation costs at most a
    * pointer's-worth of unused memory and avoids a two-pass approach.
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

    emitCountingLoop(len, "hof.filter") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = call ptr @__nex_arr1_slot(ptr $arrV, i64 $i, i64 $esz)\n")
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
      emitLine(s"  $dst = call ptr @__nex_arr1_slot(ptr $res, i64 $cur, i64 $esz)\n")
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

    emitArrDec(arrV, arr.tpe)
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
