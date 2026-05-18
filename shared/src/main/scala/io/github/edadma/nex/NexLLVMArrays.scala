package io.github.edadma.nex

/** Array codegen — literal construction, indexing, element-wise ops,
  * broadcasts, slices, clone, flat-index, fused loops.
  */
protected trait NexLLVMArrays extends NexLLVMState:

  // ---------------------------------------------------------------------------
  // Arrays — §8.5 ARC descriptors. Rank-1 uses %nex_arr1; rank-2 uses
  // %nex_arr2. Element buffer layout is row-major (matches the interpreter's
  // VArray2 flat buf). Each emitted helper takes / returns a `ptr` to the
  // descriptor.
  // ---------------------------------------------------------------------------

  /** Lower a `TArrayLit` to an alloc + per-element store. The result is a
    * fresh array with refcount = 1 (set by the runtime alloc helper).
    *
    * Rank-1 case: every elem evaluates to a scalar, store into `data[i]`.
    *
    * Rank-2 case: outer `TArrayLit` is a list of inner `TArrayLit`s of the
    * same length. Rows = outer length; cols = inner length. The flat buffer
    * is filled row-major: `data[i*cols + j] = elems(i).elems(j)`.
    */
  protected def emitArrayLit(elems: List[TExpr], t: Type): String =
    val rank   = arrayRank(t)
    val elem   = arrayElem(t)
    val esz    = elemSize(elem)
    val stType = storageType(elem)

    rank match
      case 1 =>
        val len  = elems.size
        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $len, i64 $esz)\n")
        if elems.nonEmpty then
          val dpr = newReg()
          emitLine(s"  $dpr = getelementptr inbounds %nex_arr1, ptr $desc, i32 0, i32 2\n")
          val buf = newReg()
          emitLine(s"  $buf = load ptr, ptr $dpr\n")
          for (e, i) <- elems.zipWithIndex do
            val v    = emitExpr(e)
            val slot = newReg()
            emitLine(s"  $slot = getelementptr inbounds $stType, ptr $buf, i64 $i\n")
            storeElem(stType, v, slot)
        desc

      case 2 =>
        // Validate inner shape — every elem should itself be a rank-1
        // `TArrayLit` of common length. The elaborator's type inference
        // accepts this shape; surface a clear diag if something else slipped
        // through.
        val rowExprs = elems.collect { case TArrayLit(inner, _, _) => inner }
        if rowExprs.size != elems.size then
          notYet("rank-2 array literal with non-literal rows")
          return "null"
        val rows = elems.size
        val cols = if rows == 0 then 0 else rowExprs.head.size

        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr2_alloc(i64 $rows, i64 $cols, i64 $esz)\n")
        if rows > 0 && cols > 0 then
          val dpr = newReg()
          emitLine(s"  $dpr = getelementptr inbounds %nex_arr2, ptr $desc, i32 0, i32 3\n")
          val buf = newReg()
          emitLine(s"  $buf = load ptr, ptr $dpr\n")
          for (row, i) <- rowExprs.zipWithIndex do
            for (e, j) <- row.zipWithIndex do
              val v       = emitExpr(e)
              val flatIdx = i * cols + j
              val slot    = newReg()
              emitLine(s"  $slot = getelementptr inbounds $stType, ptr $buf, i64 $flatIdx\n")
              storeElem(stType, v, slot)
        desc

      case other =>
        notYet(s"array literal of rank $other"); "null"

  /** Lower `arr[i]` (rank-1) or `arr[i, j]` (rank-2) to a slot-fetch via the
    * runtime helper and a load of the stored type. Bounds checks are inside
    * `__nex_arr*_slot` and abort on overflow.
    */
  protected def emitIndex(arr: TExpr, indices: List[TExpr], resultT: Type): String =
    val rank  = arrayRank(arr.tpe)
    val elem  = arrayElem(arr.tpe)
    val esz   = elemSize(elem)
    val stT   = storageType(elem)
    val arrV  = emitExpr(arr)

    val result = (rank, indices) match
      case (1, List(idx)) =>
        val iv   = emitExpr(idx)
        val slot = newReg()
        emitLine(s"  $slot = call ptr @__nex_arr1_slot(ptr $arrV, i64 $iv, i64 $esz)\n")
        loadElem(stT, slot, llvmType(elem))
      case (2, List(i, j)) =>
        val iv   = emitExpr(i)
        val jv   = emitExpr(j)
        val slot = newReg()
        emitLine(s"  $slot = call ptr @__nex_arr2_slot(ptr $arrV, i64 $iv, i64 $jv, i64 $esz)\n")
        loadElem(stT, slot, llvmType(elem))
      case (r, ixs) =>
        notYet(s"index of rank $r with ${ixs.size} indices"); "0"
    // If the element is itself an array (nested), the loaded value needs
    // an inc since we shared it out of the slot — but v0 doesn't support
    // nested arrays via TArrayLit, so skip for now.
    emitArrDec(arrV, arr.tpe)
    result

  /** Store a value of LLVM type `t` (the *language* type) into a buffer slot
    * whose stored type is `storageT`. For bool, the i1 value is widened to
    * an i8 on store. Otherwise this is a straight `store T v, ptr slot`.
    */
  protected def storeElem(storageT: String, value: String, slot: String): Unit =
    storageT match
      case "i8" =>
        // bool: widen i1 → i8 before storing
        val widened = newReg()
        emitLine(s"  $widened = zext i1 $value to i8\n")
        emitLine(s"  store i8 $widened, ptr $slot\n")
      case t =>
        emitLine(s"  store $t $value, ptr $slot\n")

  /** Load from a buffer slot whose stored type is `storageT`, producing a
    * value of language type `langT`. For bool, the i8 is truncated to i1.
    */
  protected def loadElem(storageT: String, slot: String, langT: String): String =
    storageT match
      case "i8" =>
        val raw = newReg()
        emitLine(s"  $raw = load i8, ptr $slot\n")
        val out = newReg()
        emitLine(s"  $out = icmp ne i8 $raw, 0\n")
        out
      case t =>
        val r = newReg()
        emitLine(s"  $r = load $t, ptr $slot\n")
        r

  // ---------------------------------------------------------------------------
  // Element-wise ops, broadcasts, slices, clone, flat index, fused loop.
  // ---------------------------------------------------------------------------

  /** Open an iteration loop over `0..len` with a fresh counter slot and
    * call `genBody(i)` inside the body block. `genBody` receives the
    * SSA register for the current counter value and is responsible for
    * emitting all of the body's instructions before returning. The
    * returned exit label is left as the current block.
    *
    * Used by [[emitElementWise]] / [[emitBroadcast]] / slice / clone /
    * print-array — every per-element loop has the same shape.
    */
  protected def emitCountingLoop(len: String, prefix: String)(genBody: String => Unit): Unit =
    val iSlot = newReg()
    emitLine(s"  $iSlot = alloca i64\n")
    emitLine(s"  store i64 0, ptr $iSlot\n")
    val condL = freshLabel(s"$prefix.cond")
    val bodyL = freshLabel(s"$prefix.body")
    val exitL = freshLabel(s"$prefix.exit")
    emitTerminator(s"  br label %$condL\n")
    startBlock(condL)
    val cur = newReg()
    emitLine(s"  $cur = load i64, ptr $iSlot\n")
    val ok  = newReg()
    emitLine(s"  $ok = icmp slt i64 $cur, $len\n")
    emitTerminator(s"  br i1 $ok, label %$bodyL, label %$exitL\n")
    startBlock(bodyL)
    genBody(cur)
    if currentBlock.isDefined then
      val nx = newReg()
      emitLine(s"  $nx = add i64 $cur, 1\n")
      emitLine(s"  store i64 $nx, ptr $iSlot\n")
      emitTerminator(s"  br label %$condL\n")
    startBlock(exitL)

  /** Emit a `length` lookup for either rank of array via the runtime
    * helpers. For rank-2 this returns the FLAT (rows*cols) element count
    * — different from the public-facing `length` builtin which follows
    * the interpreter's "length = rows" convention.
    */
  protected def flatLengthOf(arrV: String, t: Type): String =
    val r = newReg()
    arrayRank(t) match
      case 1 => emitLine(s"  $r = call i64 @__nex_arr1_len(ptr $arrV)\n"); r
      case 2 => emitLine(s"  $r = call i64 @__nex_arr2_len(ptr $arrV)\n"); r
      case other => notYet(s"flat length of rank $other"); "0"

  /** Allocate a fresh array shaped like `model` (same rank, same
    * dimensions). Returns the new descriptor's SSA value, with
    * `refcount = 1`. Element size comes from the result element type
    * (which may differ from the model's, e.g. a relational op result
    * has bool elements over reals).
    */
  protected def allocLike(model: String, modelT: Type, resultElem: Type): String =
    val esz = elemSize(resultElem)
    val desc = newReg()
    arrayRank(modelT) match
      case 1 =>
        val lenR = newReg()
        emitLine(s"  $lenR = call i64 @__nex_arr1_len(ptr $model)\n")
        emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $lenR, i64 $esz)\n")
      case 2 =>
        val rowsR = newReg()
        emitLine(s"  $rowsR = call i64 @__nex_arr2_rows(ptr $model)\n")
        val colsR = newReg()
        emitLine(s"  $colsR = call i64 @__nex_arr2_cols(ptr $model)\n")
        emitLine(s"  $desc = call ptr @__nex_arr2_alloc(i64 $rowsR, i64 $colsR, i64 $esz)\n")
      case other => notYet(s"alloc-like of rank $other")
    desc

  /** Return a pointer to the flat element buffer of an array descriptor. */
  protected def bufPtr(desc: String, t: Type): String =
    val r = newReg()
    arrayRank(t) match
      case 1 =>
        val dp = newReg()
        emitLine(s"  $dp = getelementptr inbounds %nex_arr1, ptr $desc, i32 0, i32 2\n")
        emitLine(s"  $r = load ptr, ptr $dp\n")
        r
      case 2 =>
        val dp = newReg()
        emitLine(s"  $dp = getelementptr inbounds %nex_arr2, ptr $desc, i32 0, i32 3\n")
        emitLine(s"  $r = load ptr, ptr $dp\n")
        r
      case other => notYet(s"buf ptr of rank $other"); "null"

  /** Emit `op` between two scalar values of the same Nex type, returning
    * the SSA register of the result. Reuses the existing [[binOpInst]]
    * table. Result type is implied by the operands (same as the
    * elementwise spec — Stage 2 lifts only valid op/operand combos).
    */
  protected def emitScalarBinOp(op: String, lv: String, rv: String, opT: Type): String =
    op match
      case "and" | "or" =>
        // For element-wise paths these get evaluated eagerly (no
        // short-circuit possible at element level).
        val reg = newReg()
        val instr = if op == "and" then "and" else "or"
        emitLine(s"  $reg = $instr i1 $lv, $rv\n")
        reg
      case _ =>
        val (instr, _) = binOpInst(op, opT)
        val reg = newReg()
        emitLine(s"  $reg = $instr ${llvmType(opT)} $lv, $rv\n")
        reg

  /** Lower `lhs ⊙ rhs` element-wise when both sides are array-typed.
    * Allocates a fresh result of the same shape, iterates the flat
    * buffer, applies the scalar op per element. Both operands are
    * dec'd before returning the fresh array.
    */
  protected def emitElementWise(op: String, lhs: TExpr, rhs: TExpr, resultT: Type): String =
    val elemL  = arrayElem(lhs.tpe)
    val elemR  = arrayElem(rhs.tpe)
    val resE   = arrayElem(resultT)
    val stL    = storageType(elemL)
    val stR    = storageType(elemR)
    val stRes  = storageType(resE)
    val langL  = llvmType(elemL)
    val langR  = llvmType(elemR)
    val esRes  = elemSize(resE)

    val lv = emitExpr(lhs)
    val rv = emitExpr(rhs)

    val desc = allocLike(lv, lhs.tpe, resE)
    val lenR = flatLengthOf(lv, lhs.tpe)
    val lBuf = bufPtr(lv, lhs.tpe)
    val rBuf = bufPtr(rv, rhs.tpe)
    val oBuf = bufPtr(desc, resultT)

    emitCountingLoop(lenR, "ew") { i =>
      val lSlot = newReg()
      emitLine(s"  $lSlot = getelementptr inbounds $stL, ptr $lBuf, i64 $i\n")
      val ll = loadElem(stL, lSlot, langL)
      val rSlot = newReg()
      emitLine(s"  $rSlot = getelementptr inbounds $stR, ptr $rBuf, i64 $i\n")
      val rr = loadElem(stR, rSlot, langR)
      val out = emitScalarBinOp(op, ll, rr, elemL)
      val oSlot = newReg()
      emitLine(s"  $oSlot = getelementptr inbounds $stRes, ptr $oBuf, i64 $i\n")
      storeElem(stRes, out, oSlot)
    }

    emitArrDec(lv, lhs.tpe)
    emitArrDec(rv, rhs.tpe)
    desc

  /** Lower a scalar × array (or array × scalar) broadcast. */
  protected def emitBroadcast(scalar: TExpr, arr: TExpr, op: String, scalarFirst: Boolean, resultT: Type): String =
    val elem  = arrayElem(arr.tpe)
    val resE  = arrayElem(resultT)
    val stE   = storageType(elem)
    val stRes = storageType(resE)
    val langE = llvmType(elem)

    val sv = emitExpr(scalar)
    val av = emitExpr(arr)

    val desc = allocLike(av, arr.tpe, resE)
    val lenR = flatLengthOf(av, arr.tpe)
    val aBuf = bufPtr(av, arr.tpe)
    val oBuf = bufPtr(desc, resultT)

    emitCountingLoop(lenR, "bc") { i =>
      val aSlot = newReg()
      emitLine(s"  $aSlot = getelementptr inbounds $stE, ptr $aBuf, i64 $i\n")
      val e = loadElem(stE, aSlot, langE)
      val (l, r) = if scalarFirst then (sv, e) else (e, sv)
      val out = emitScalarBinOp(op, l, r, elem)
      val oSlot = newReg()
      emitLine(s"  $oSlot = getelementptr inbounds $stRes, ptr $oBuf, i64 $i\n")
      storeElem(stRes, out, oSlot)
    }

    emitArrDec(av, arr.tpe)
    desc

  /** Lower `arr[lo..hi]` / `arr[lo..=hi]` (rank-1) to a fresh array of
    * `hi-lo` (or `hi-lo+1`) elements copied from the source.
    */
  protected def emitSlice(arr: TExpr, lo: TExpr, hi: TExpr, inclusive: Boolean, resultT: Type): String =
    val elem = arrayElem(arr.tpe)
    val stE  = storageType(elem)
    val langE = llvmType(elem)
    val esz  = elemSize(elem)

    val av  = emitExpr(arr)
    val loV = emitExpr(lo)
    val hiV = emitExpr(hi)

    // Slice length: hi - lo (exclusive) or hi - lo + 1 (inclusive).
    val rawLen = newReg()
    emitLine(s"  $rawLen = sub i64 $hiV, $loV\n")
    val length = if inclusive then
      val r = newReg()
      emitLine(s"  $r = add i64 $rawLen, 1\n")
      r
    else rawLen

    val desc = newReg()
    emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $length, i64 $esz)\n")
    val srcBuf = bufPtr(av, arr.tpe)
    val outBuf = bufPtr(desc, resultT)

    emitCountingLoop(length, "slice1") { i =>
      val srcIdx = newReg()
      emitLine(s"  $srcIdx = add i64 $loV, $i\n")
      val sSlot = newReg()
      emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $srcIdx\n")
      val v = loadElem(stE, sSlot, langE)
      val oSlot = newReg()
      emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $i\n")
      storeElem(stE, v, oSlot)
    }

    emitArrDec(av, arr.tpe)
    desc

  /** Lower a rank-2 axis-spec slice. The result rank depends on how
    * many axes are preserved; rank-0 (scalar) currently surfaces a diag.
    */
  protected def emitSlice2(arr: TExpr, rowAx: TAxisSpec, colAx: TAxisSpec, resultT: Type): String =
    // Compute the (loR, hiR, isRangeR) for the row axis, similar for col.
    val elem = arrayElem(arr.tpe)
    val stE  = storageType(elem)
    val langE = llvmType(elem)
    val esz  = elemSize(elem)

    val av = emitExpr(arr)
    val rowsAll = newReg()
    emitLine(s"  $rowsAll = call i64 @__nex_arr2_rows(ptr $av)\n")
    val colsAll = newReg()
    emitLine(s"  $colsAll = call i64 @__nex_arr2_cols(ptr $av)\n")

    // For each axis: (loStart, loEnd, preserved?, isSingleton?).
    // `preserved` means this axis contributes to the result rank.
    def axis(spec: TAxisSpec, total: String): (String, String, Boolean, Option[String]) = spec match
      case TAxisAll => ("0", total, true, None)
      case TAxisIndex(idx) =>
        val iv = emitExpr(idx)
        val hi = newReg()
        emitLine(s"  $hi = add i64 $iv, 1\n")
        (iv, hi, false, Some(iv))
      case TAxisRange(lo, hi, inclusive) =>
        val loV = emitExpr(lo)
        val hiV = emitExpr(hi)
        val end = if inclusive then
          val r = newReg(); emitLine(s"  $r = add i64 $hiV, 1\n"); r
        else hiV
        (loV, end, true, None)

    val (rLo, rEnd, rPres, _) = axis(rowAx, rowsAll)
    val (cLo, cEnd, cPres, _) = axis(colAx, colsAll)
    val rLen = newReg()
    emitLine(s"  $rLen = sub i64 $rEnd, $rLo\n")
    val cLen = newReg()
    emitLine(s"  $cLen = sub i64 $cEnd, $cLo\n")

    val srcBuf = bufPtr(av, arr.tpe)

    val result = (rPres, cPres) match
      case (true, true) =>
        // Both axes preserved → rank-2 result.
        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr2_alloc(i64 $rLen, i64 $cLen, i64 $esz)\n")
        val outBuf = bufPtr(desc, resultT)
        emitCountingLoop(rLen, "slice2r") { i =>
          emitCountingLoop(cLen, "slice2c") { j =>
            val sr = newReg(); emitLine(s"  $sr = add i64 $rLo, $i\n")
            val sc = newReg(); emitLine(s"  $sc = add i64 $cLo, $j\n")
            val srcFlat = newReg(); emitLine(s"  $srcFlat = mul i64 $sr, $colsAll\n")
            val srcIdx  = newReg(); emitLine(s"  $srcIdx = add i64 $srcFlat, $sc\n")
            val sSlot = newReg()
            emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $srcIdx\n")
            val v = loadElem(stE, sSlot, langE)
            val outFlat = newReg(); emitLine(s"  $outFlat = mul i64 $i, $cLen\n")
            val outIdx  = newReg(); emitLine(s"  $outIdx = add i64 $outFlat, $j\n")
            val oSlot = newReg()
            emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $outIdx\n")
            storeElem(stE, v, oSlot)
          }
        }
        desc

      case (true, false) =>
        // Row axis preserved, col collapsed → rank-1 of rLen elements.
        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $rLen, i64 $esz)\n")
        val outBuf = bufPtr(desc, resultT)
        emitCountingLoop(rLen, "slice2rc") { i =>
          val sr = newReg(); emitLine(s"  $sr = add i64 $rLo, $i\n")
          val sFlat = newReg(); emitLine(s"  $sFlat = mul i64 $sr, $colsAll\n")
          val sIdx  = newReg(); emitLine(s"  $sIdx = add i64 $sFlat, $cLo\n")
          val sSlot = newReg()
          emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $sIdx\n")
          val v = loadElem(stE, sSlot, langE)
          val oSlot = newReg()
          emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $i\n")
          storeElem(stE, v, oSlot)
        }
        desc

      case (false, true) =>
        // Col axis preserved, row collapsed → rank-1 of cLen elements.
        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $cLen, i64 $esz)\n")
        val outBuf = bufPtr(desc, resultT)
        emitCountingLoop(cLen, "slice2cr") { j =>
          val sc = newReg(); emitLine(s"  $sc = add i64 $cLo, $j\n")
          val sFlat = newReg(); emitLine(s"  $sFlat = mul i64 $rLo, $colsAll\n")
          val sIdx  = newReg(); emitLine(s"  $sIdx = add i64 $sFlat, $sc\n")
          val sSlot = newReg()
          emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $sIdx\n")
          val v = loadElem(stE, sSlot, langE)
          val oSlot = newReg()
          emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $j\n")
          storeElem(stE, v, oSlot)
        }
        desc

      case (false, false) =>
        // Both axes collapsed → scalar; should have been TIndex, not TSlice2.
        notYet("rank-2 slice with both axes collapsed")
        "null"

    emitArrDec(av, arr.tpe)
    result

  /** Lower [[TClone]] — deep-copy the source descriptor and its buffer
    * into a fresh allocation. Source's owning share is released.
    */
  protected def emitClone(arr: TExpr, resultT: Type): String =
    // Tier-1 perf optimization: lower deep-copy to a single
    // `@llvm.memcpy.p0.p0.i64` instead of a per-element store loop.
    // The src and dst buffers are guaranteed non-aliasing (the
    // destination came from a fresh malloc); LLVM constant-folds
    // the size when the length is statically known.
    val elem = arrayElem(arr.tpe)
    val esz  = elemSize(elem)

    val src  = emitExpr(arr)
    val desc = allocLike(src, arr.tpe, elem)
    val len  = flatLengthOf(src, arr.tpe)
    val sBuf = bufPtr(src, arr.tpe)
    val oBuf = bufPtr(desc, resultT)

    val bytes = newReg()
    emitLine(s"  $bytes = mul i64 $len, $esz\n")
    emitLine(s"  call void @llvm.memcpy.p0.p0.i64(ptr $oBuf, ptr $sBuf, i64 $bytes, i1 false)\n")

    emitArrDec(src, arr.tpe)
    desc

  /** Lower [[TFlatIndex]] — single flat-index access regardless of rank.
    * Used inside [[TFusedLoop]] bodies (introduced by the fusion pass),
    * where the index is the loop counter and the bound is the source
    * array's own length. Both invariants are encoded in the AST shape
    * — fusion never produces a TFlatIndex against an unrelated index
    * — so the bounds check would always succeed. Skip it: emit a
    * direct GEP+load on the data buffer instead of the bounds-checked
    * `__nex_arr*_slot` helper. Tier-1 perf optimization.
    */
  protected def emitFlatIndex(arr: TExpr, idx: TExpr, resultT: Type): String =
    val elem  = arrayElem(arr.tpe)
    val stE   = storageType(elem)
    val langE = llvmType(elem)
    val av    = emitExpr(arr)
    val iv    = emitExpr(idx)
    val buf   = bufPtr(av, arr.tpe)
    val slot  = newReg()
    emitLine(s"  $slot = getelementptr inbounds $stE, ptr $buf, i64 $iv\n")
    val v = loadElem(stE, slot, langE)
    emitArrDec(av, arr.tpe)
    v

  /** Lower [[TFusedLoop]] — the fusion-pass output. Allocates a fresh
    * array of `length` elements, then iterates 0..length binding
    * `loopVar` to the flat index and storing `body` into the buffer.
    * `cols=None` → rank-1; `cols=Some(c)` → rank-2 with rows = length/c.
    */
  protected def emitFusedLoop(loopVar: Symbol, length: TExpr, body: TExpr, cols: Option[TExpr], resultT: Type): String =
    val resElem = arrayElem(resultT)
    val stE     = storageType(resElem)
    val esz     = elemSize(resElem)
    val len     = emitExpr(length)

    val desc = cols match
      case None =>
        val d = newReg()
        emitLine(s"  $d = call ptr @__nex_arr1_alloc(i64 $len, i64 $esz)\n")
        d
      case Some(cExpr) =>
        val c = emitExpr(cExpr)
        val rows = newReg()
        emitLine(s"  $rows = sdiv i64 $len, $c\n")
        val d = newReg()
        emitLine(s"  $d = call ptr @__nex_arr2_alloc(i64 $rows, i64 $c, i64 $esz)\n")
        d

    val outBuf = bufPtr(desc, resultT)

    // The loop variable is a per-function Symbol — register a fresh slot
    // for it so the body's TVarRef(loopVar) reads the current index.
    val ivSlot = newReg()
    emitLine(s"  $ivSlot = alloca i64\n")
    locals(loopVar.id) = ivSlot

    emitCountingLoop(len, "fused") { i =>
      emitLine(s"  store i64 $i, ptr $ivSlot\n")
      val v = emitExpr(body)
      val oSlot = newReg()
      emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $i\n")
      storeElem(stE, v, oSlot)
    }
    desc
