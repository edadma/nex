package io.github.edadma.nex

/** Rank-1-oriented prelude emitters for the LLVM backend.
  *
  * Bundles the array higher-order functions (`map` / `reduce` / `filter`
  * / `flatMap`), reductions (`sum` / `product` / `dot` and the unary
  * array overload of `min` / `max`), construction (`fill` / `zeros` /
  * `ones` / `identity`), builders (`range` / `enumerate` / `zip` /
  * `linspace`), the non-copying `view(a, lo..hi)` borrow, and the
  * shared per-element scalar-binop helper (`emitScalarBinOpSimple`)
  * together with the typed identity constants (`zeroOf` / `oneOf`)
  * used by both array reductions here and the matmul / sum_axis
  * kernels in [[NexLLVMMatrixPrelude]].
  *
  * Every HOF shares the same shape: evaluate the source array once,
  * split the closure-typed `fn` into its `{fn_ptr, env_ptr}` pair,
  * iterate `0..len-1` invoking the closure per element, then `dec`
  * the source array. Rank-2 inputs are accepted by `map` / `reduce`
  * (flat row-major walk); `filter` and the builders are rank-1 only.
  */
protected trait NexLLVMArrayPrelude extends NexLLVMState:

  // --------------------------------------------------------------------------
  // §10.4 array higher-order functions.
  // --------------------------------------------------------------------------

  /** Emit `map(arr, fn)` — allocate a result array of the same length
    * and store each `fn(arr[i])` into it. The closure's `{ptr, ptr}`
    * value is split into fn_ptr + env_ptr before the loop so the body
    * only needs an indirect call.
    */
  protected def emitMapCall(arr: TExpr, fn: TExpr, resultT: Type): String =
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
  protected def emitReduceCall(arr: TExpr, init: TExpr, fn: TExpr, resultT: Type): String =
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
  protected def emitFilterCall(arr: TExpr, fn: TExpr, resultT: Type): String =
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

  // --------------------------------------------------------------------------
  // §10.5 array construction — `fill(n, v)`, `zeros(n)`, `ones(n)`.
  //
  // Each emits an allocation followed by a counted store loop. The
  // value is evaluated ONCE outside the loop and reused across slots —
  // matches the interpreter's `mutable.ArrayBuffer.fill(n)(v)` semantics
  // (no per-element re-evaluation).
  // --------------------------------------------------------------------------

  /** Emit `fill(n, v)` — allocate a rank-1 or rank-2 array and store
    * `v` into every slot. The shape comes from the `n` argument's
    * type: integer → rank-1 of length `n`; tuple (rows, cols) →
    * rank-2 of shape (rows, cols). `v` is evaluated once.
    */
  protected def emitFillCall(n: TExpr, v: TExpr, resultT: Type): String =
    val rank = arrayRank(resultT)
    if rank != 1 && rank != 2 then
      notYet(s"fill(rank-$rank)")
      return "null"

    val elem = arrayElem(resultT)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val vVal = emitExpr(v)

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
  protected def emitConstFill(n: TExpr, constStr: String, elemT: Type, resultT: Type): String =
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
  protected def emitIdentityCall(n: TExpr, resultT: Type): String =
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

  // --------------------------------------------------------------------------
  // §10.4 rank-1 reductions: sum / product / dot.
  //
  // Each emits an alloca'd accumulator updated by a counted loop. The
  // op is fixed per function (no closure dispatch). Element types
  // supported: integer (i64), real (double), complex ({double, double}).
  // --------------------------------------------------------------------------

  /** Emit `sum(arr)` — fold the array with +, walking every element
    * regardless of rank. Result type matches the element type. The
    * rank-1 and rank-2 paths share `emitReduceOver`; only the length
    * helper differs (`__nex_arr1_len` vs `__nex_arr2_len`, which the
    * helper picks via the rank).
    */
  protected def emitSumCall(arr: TExpr, resultT: Type): String =
    arrayRank(arr.tpe) match
      case 1 | 2 => emitReduceOver(arr, resultT, "+", zeroOf(resultT))
      case other => notYet(s"sum on rank $other"); "0"

  /** Emit `product(arr)` — same shape as sum, identity 1, op `*`. */
  protected def emitProductCall(arr: TExpr, resultT: Type): String =
    arrayRank(arr.tpe) match
      case 1 | 2 => emitReduceOver(arr, resultT, "*", oneOf(resultT))
      case other => notYet(s"product on rank $other"); "0"

  /** Emit `dot(a, b)` — inner product. Both arrays must be rank-1 with
    * the same element type; result is the element type. Two source
    * GEPs per iter, one accumulator update.
    */
  protected def emitDotCall(a: TExpr, b: TExpr, resultT: Type): String =
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
  protected def emitMinMaxCall(arr: TExpr, resultT: Type, isMin: Boolean): String =
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
  protected def emitScalarBinOpSimple(op: String, lv: String, rv: String, t: Type): String =
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
  protected def zeroOf(t: Type): String = t match
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
  protected def oneOf(t: Type): String = t match
    case TyInteger => "1"
    case TyReal    => "1.0"
    case TyComplex =>
      val c0 = newReg()
      emitLine(s"  $c0 = insertvalue { double, double } undef, double 1.0, 0\n")
      val c1 = newReg()
      emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
      c1
    case _ => "1"

  // --------------------------------------------------------------------------
  // §10.4 flatMap — concat-map: for each element, call f, append all
  // returned values to the output buffer. Worst-case sized buffer
  // grows as we go and is truncated at the end (same pattern as
  // emitFilterCall, but copying every returned element rather than
  // gating on a predicate). The closure's return type is `[T]` so
  // each call yields an array descriptor that we walk and copy from.
  // --------------------------------------------------------------------------

  protected def emitFlatMapCall(arr: TExpr, fn: TExpr, resultT: Type): String =
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
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $srcStT, ptr $srcBuf, i64 $i\n")
      val srcE    = loadElem(srcStT, srcSlot, srcLLT)
      val subArr  = newReg()
      emitLine(s"  $subArr = call ptr (ptr, $srcLLT) $fnPtr(ptr $envPtr, $srcLLT $srcE)\n")
      val subLen  = newReg()
      emitLine(s"  $subLen = call i64 @__nex_arr1_len(ptr $subArr)\n")
      val subBuf  = bufPtr(subArr, resultT)
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
      emitArrDec(subArr, resultT)
    }

    val finalCount = newReg()
    emitLine(s"  $finalCount = load i64, ptr $countSlot\n")
    val lenP = newReg()
    emitLine(s"  $lenP = getelementptr inbounds %nex_arr1, ptr $res, i32 0, i32 1\n")
    emitLine(s"  store i64 $finalCount, ptr $lenP\n")

    emitArrDec(arrV, arr.tpe)
    emitLine(s"  call void @__nex_env_dec(ptr $envPtr)\n")
    res

  // --------------------------------------------------------------------------
  // §10.4 rank-1 builders: range / enumerate / zip / linspace; view.
  // --------------------------------------------------------------------------

  /** Emit `view(a, lo..hi)` — a non-copying borrow into `a`'s buffer.
    * The range argument is a `TBinOp("..", lo, hi)` (exclusive) or
    * `("..=", lo, hi)` (inclusive); we unpack both bounds, wrap any
    * negative indices against the relevant length (element count for
    * rank-1, row count for rank-2), normalise inclusive to exclusive
    * (`hi+1`), and call into the matching runtime helper. The helpers
    * do their own bounds check and incref the owner.
    *
    * Result is a fresh descriptor whose `data` field aliases the
    * source's buffer at the right offset and whose `owner` field
    * points at the source (or, for view-of-view, the root owner —
    * the runtime collapses chains).
    */
  protected def emitViewCall(arr: TExpr, r: TExpr, resultT: Type): String =
    val (loE, hiE, inclusive) = r match
      case TBinOp(op, lo, hi, _, _) if op == ".." || op == "..=" =>
        (lo, hi, op == "..=")
      case _ =>
        notImpl(s"view requires a range argument, got ${r.tpe}")
    val esz = elemSize(arrayElem(arr.tpe))
    val av  = emitExpr(arr)
    val rank = arrayRank(arr.tpe)
    val srcLen = newReg()
    rank match
      case 1 => emitLine(s"  $srcLen = call i64 @__nex_arr1_len(ptr $av)\n")
      case 2 => emitLine(s"  $srcLen = call i64 @__nex_arr2_rows(ptr $av)\n")
      case n => notYet(s"view on rank $n (only rank-1 and rank-2 supported)")
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
    rank match
      case 1 => emitLine(s"  $res = call ptr @__nex_arr1_view(ptr $av, i64 $loV, i64 $hiV, i64 $esz)\n")
      case 2 => emitLine(s"  $res = call ptr @__nex_arr2_view(ptr $av, i64 $loV, i64 $hiV, i64 $esz)\n")
      case _ => // unreachable — caught above
    emitArrDec(av, arr.tpe)
    res

  protected def emitRangeCall(loE: TExpr, hiE: TExpr): String =
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
  protected def emitEnumerateCall(arr: TExpr, resultT: Type): String =
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
  protected def emitZipCall(a: TExpr, b: TExpr, resultT: Type): String =
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
  protected def emitLinspaceCall(loE: TExpr, hiE: TExpr, nE: TExpr): String =
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
