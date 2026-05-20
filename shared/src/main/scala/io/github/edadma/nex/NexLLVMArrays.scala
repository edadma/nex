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
      case (2, List(i)) =>
        // Spec §4.14: `m[i]` on a rank-2 array returns row `i` as a
        // freshly-owned rank-1 array. Negative indices wrap from the end
        // (matching the rank-1 / rank-2 element-slot helpers). Bounds-
        // checked after wrap; out-of-range routes through the OOB trap.
        val iv      = emitExpr(i)
        val rows    = newReg(); emitLine(s"  $rows = call i64 @__nex_arr2_rows(ptr $arrV)\n")
        val cols    = newReg(); emitLine(s"  $cols = call i64 @__nex_arr2_cols(ptr $arrV)\n")
        val isNeg   = newReg(); emitLine(s"  $isNeg = icmp slt i64 $iv, 0\n")
        val wrapped = newReg(); emitLine(s"  $wrapped = add i64 $iv, $rows\n")
        val ii      = newReg(); emitLine(s"  $ii = select i1 $isNeg, i64 $wrapped, i64 $iv\n")
        val negI    = newReg(); emitLine(s"  $negI = icmp slt i64 $ii, 0\n")
        val geRows  = newReg(); emitLine(s"  $geRows = icmp sge i64 $ii, $rows\n")
        val bad     = newReg(); emitLine(s"  $bad = or i1 $negI, $geRows\n")
        val okL     = freshLabel("row.ok")
        val flL     = freshLabel("row.fail")
        emitTerminator(s"  br i1 $bad, label %$flL, label %$okL\n")
        startBlock(flL)
        emitLine(s"  call void @__nex_trap_with(ptr @.slice_oob_msg)\n")
        emitTerminator(s"  unreachable\n")
        startBlock(okL)
        // Allocate the row + memcpy from the source row's flat offset.
        val desc   = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $cols, i64 $esz)\n")
        val srcBuf = bufPtr(arrV, arr.tpe)
        val dstBuf = bufPtr(desc, resultT)
        val flat   = newReg(); emitLine(s"  $flat = mul i64 $ii, $cols\n")
        val srcRow = newReg()
        emitLine(s"  $srcRow = getelementptr inbounds $stT, ptr $srcBuf, i64 $flat\n")
        val bytes  = newReg(); emitLine(s"  $bytes = mul i64 $cols, $esz\n")
        emitLine(s"  call void @llvm.memcpy.p0.p0.i64(ptr $dstBuf, ptr $srcRow, i64 $bytes, i1 false)\n")
        desc
      case (r, ixs) =>
        notImpl(s"index of rank $r with ${ixs.size} indices")
    // When the element is itself refcounted (string, nested array), the
    // loaded value is a borrowed share from the slot. Inc so the caller
    // has its own owning share — without this, a `print(arr[i])` would
    // dec the slot's share to zero and free the inner descriptor while
    // the array still claims to own it.
    if isRefCountedType(elem) then emitArrInc(result, elem)
    emitArrDec(arrV, arr.tpe)
    result

  /** Negative-index wrap for a slice / axis bound (spec §4.14). If
    * `raw < 0`, returns `raw + extent`; otherwise returns `raw`
    * unchanged. The caller's bounds check runs against the wrapped
    * value, so an over-negative input (e.g. `lo = -10` on a 3-element
    * array → wrapped to -7) still trips the negative-bound clause and
    * traps. Mirrors the interpreter's `wrapNeg` helper.
    */
  protected def wrapNegBound(raw: String, extent: String): String =
    val isNeg = newReg()
    emitLine(s"  $isNeg = icmp slt i64 $raw, 0\n")
    val wrapped = newReg()
    emitLine(s"  $wrapped = add i64 $raw, $extent\n")
    val out = newReg()
    emitLine(s"  $out = select i1 $isNeg, i64 $wrapped, i64 $raw\n")
    out

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
    * table for the integer / real / bool cases; complex operands route
    * through the component-decomposition path
    * ([[toComplex]] / [[emitComplexArith]] / [[packComplex]]) so we
    * don't emit invalid IR like `fadd { double, double } ...`. Result
    * type is implied by the operands (same as the elementwise spec).
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
      case _ if opT == TyComplex =>
        // Complex elements need componentwise arithmetic / comparison.
        // The scalar TBinOp emission path already does this for
        // top-level binops; element-wise + broadcast over `[complex]`
        // now share the same lowering so the per-element op produces
        // valid IR.
        val (lre, lim) = toComplex(lv, TyComplex)
        val (rre, rim) = toComplex(rv, TyComplex)
        op match
          case "+" | "-" | "*" | "/" =>
            emitComplexArith(op, lre, lim, rre, rim)
          case "==" =>
            // (lre == rre) and (lim == rim). Real `==` on complex
            // components: IEEE `oeq` (NaN comparisons fail, matching
            // the scalar real path in `binOpInst`).
            val reEq = newReg(); emitLine(s"  $reEq = fcmp oeq double $lre, $rre\n")
            val imEq = newReg(); emitLine(s"  $imEq = fcmp oeq double $lim, $rim\n")
            val both = newReg(); emitLine(s"  $both = and i1 $reEq, $imEq\n")
            both
          case "!=" =>
            // Negate the equality fold: any-component-differs. Use
            // `une` to match real-`!=` semantics (NaN-aware).
            val reNe = newReg(); emitLine(s"  $reNe = fcmp une double $lre, $rre\n")
            val imNe = newReg(); emitLine(s"  $imNe = fcmp une double $lim, $rim\n")
            val any  = newReg(); emitLine(s"  $any = or i1 $reNe, $imNe\n")
            any
          case other =>
            notYet(s"complex elementwise `$other`")
            packComplex("0.0", "0.0")
      case _ =>
        val (instr, _) = binOpInst(op, opT)
        val reg = newReg()
        emitLine(s"  $reg = $instr ${llvmType(opT)} $lv, $rv\n")
        reg

  /** Promote a scalar IR value of type `from` up the numeric lattice to
    * `to`. No-op when types match. The valid coercions follow the
    * promotion order in spec §3.2: `integer → real → complex`. Used by
    * the element-wise and broadcast lowerings so a mixed-numeric op
    * (`int_scalar * real_array`, `[int] + [real]`) doesn't emit an IR
    * shape mismatch (e.g. `fmul double 2, %t` where `2` is an integer
    * literal).
    */
  protected def liftScalarTo(sv: String, from: Type, to: Type): String =
    if from == to then sv
    else (from, to) match
      case (TyInteger, TyReal) =>
        val r = newReg()
        emitLine(s"  $r = sitofp i64 $sv to double\n")
        r
      case (TyInteger, TyComplex) =>
        val r1 = newReg()
        emitLine(s"  $r1 = sitofp i64 $sv to double\n")
        val c0 = newReg()
        emitLine(s"  $c0 = insertvalue { double, double } undef, double $r1, 0\n")
        val c1 = newReg()
        emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
        c1
      case (TyReal, TyComplex) =>
        val c0 = newReg()
        emitLine(s"  $c0 = insertvalue { double, double } undef, double $sv, 0\n")
        val c1 = newReg()
        emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
        c1
      case _ => sv

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

    // For mixed-element arrays (e.g. `[int] + [real]`), the per-element
    // op runs on the result element type; promote each loaded element
    // up the numeric lattice to that type before the binop. Comparisons
    // are special — they keep operating on the common numeric type,
    // never on bool, so promote to that common type instead of the
    // result-element type (which would be TyBool).
    val opElem =
      if Set("==", "!=", "<", "<=", ">", ">=").contains(op) then
        // Both sides are promoted to whichever is higher in the lattice.
        if elemL == TyComplex || elemR == TyComplex then TyComplex
        else if elemL == TyReal || elemR == TyReal then TyReal
        else elemL
      else resE
    emitCountingLoop(lenR, "ew") { i =>
      val lSlot = newReg()
      emitLine(s"  $lSlot = getelementptr inbounds $stL, ptr $lBuf, i64 $i\n")
      val ll0 = loadElem(stL, lSlot, langL)
      val rSlot = newReg()
      emitLine(s"  $rSlot = getelementptr inbounds $stR, ptr $rBuf, i64 $i\n")
      val rr0 = loadElem(stR, rSlot, langR)
      val ll = liftScalarTo(ll0, elemL, opElem)
      val rr = liftScalarTo(rr0, elemR, opElem)
      val out = emitScalarBinOp(op, ll, rr, opElem)
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

    val sv0 = emitExpr(scalar)
    // Promote the scalar IR value up to the array's element type if
    // they differ (e.g. `int_literal * [real]` arrives with
    // `scalar.tpe = TyInteger` and `elem = TyReal`; without this lift
    // the per-element fmul receives an i64 immediate as a double
    // operand and clang rejects the IR).
    val sv = liftScalarTo(sv0, scalar.tpe, elem)
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
    *
    * Traps on out-of-bounds bounds (lo < 0, hi < lo, or hi past the
    * array's length) to match the interpreter; without the check, an
    * OOB slice would silently read past the buffer.
    */
  protected def emitSlice(arr: TExpr, lo: TExpr, hi: TExpr, inclusive: Boolean, resultT: Type): String =
    val elem = arrayElem(arr.tpe)
    val stE  = storageType(elem)
    val langE = llvmType(elem)
    val esz  = elemSize(elem)

    val av    = emitExpr(arr)
    val loRaw = emitExpr(lo)
    val hiRaw = emitExpr(hi)

    val srcLen = newReg()
    emitLine(s"  $srcLen = call i64 @__nex_arr1_len(ptr $av)\n")
    val loV = wrapNegBound(loRaw, srcLen)
    val hiV = wrapNegBound(hiRaw, srcLen)

    // Bounds check: lo < 0, hi < lo, or hi exceeds size (for
    // exclusive: hi > size; for inclusive: hi >= size). The trap
    // routes through __nex_trap_with so an enclosing assert_traps
    // catches. The check runs against the wrapped values so an over-
    // negative input (e.g. lo=-10 on a 3-element array → wrapped to
    // -7) still trips the `lo < 0` clause.
    val negLo = newReg()
    emitLine(s"  $negLo = icmp slt i64 $loV, 0\n")
    val hiLtLo = newReg()
    emitLine(s"  $hiLtLo = icmp slt i64 $hiV, $loV\n")
    val hiBad = newReg()
    if inclusive then
      emitLine(s"  $hiBad = icmp sge i64 $hiV, $srcLen\n")
    else
      emitLine(s"  $hiBad = icmp sgt i64 $hiV, $srcLen\n")
    val any01 = newReg()
    emitLine(s"  $any01 = or i1 $negLo, $hiLtLo\n")
    val any = newReg()
    emitLine(s"  $any = or i1 $any01, $hiBad\n")
    val okL   = freshLabel("sl1.ok")
    val failL = freshLabel("sl1.fail")
    emitTerminator(s"  br i1 $any, label %$failL, label %$okL\n")
    startBlock(failL)
    emitLine(s"  call void @__nex_trap_with(ptr @.slice_oob_msg)\n")
    emitTerminator(s"  unreachable\n")
    startBlock(okL)

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
    // Each branch traps on OOB bounds against the matching extent
    // (rows or cols), mirroring the interpreter's per-axis checks.
    def axis(spec: TAxisSpec, total: String, label: String): (String, String, Boolean, Option[String]) = spec match
      case TAxisAll => ("0", total, true, None)
      case TAxisIndex(idx) =>
        val ivRaw = emitExpr(idx)
        val iv    = wrapNegBound(ivRaw, total)
        val neg = newReg()
        emitLine(s"  $neg = icmp slt i64 $iv, 0\n")
        val ge  = newReg()
        emitLine(s"  $ge  = icmp sge i64 $iv, $total\n")
        val bad = newReg()
        emitLine(s"  $bad = or i1 $neg, $ge\n")
        val okL = freshLabel(s"$label.ix.ok")
        val flL = freshLabel(s"$label.ix.fail")
        emitTerminator(s"  br i1 $bad, label %$flL, label %$okL\n")
        startBlock(flL)
        emitLine(s"  call void @__nex_trap_with(ptr @.axis_oob_msg)\n")
        emitTerminator(s"  unreachable\n")
        startBlock(okL)
        val hi = newReg()
        emitLine(s"  $hi = add i64 $iv, 1\n")
        (iv, hi, false, Some(iv))
      case TAxisRange(lo, hi, inclusive) =>
        val loRaw = emitExpr(lo)
        val hiRaw = emitExpr(hi)
        val loV = wrapNegBound(loRaw, total)
        val hiV = wrapNegBound(hiRaw, total)
        val negLo = newReg()
        emitLine(s"  $negLo = icmp slt i64 $loV, 0\n")
        val hiLtLo = newReg()
        emitLine(s"  $hiLtLo = icmp slt i64 $hiV, $loV\n")
        val hiBad = newReg()
        if inclusive then
          emitLine(s"  $hiBad = icmp sge i64 $hiV, $total\n")
        else
          emitLine(s"  $hiBad = icmp sgt i64 $hiV, $total\n")
        val any01 = newReg()
        emitLine(s"  $any01 = or i1 $negLo, $hiLtLo\n")
        val any = newReg()
        emitLine(s"  $any = or i1 $any01, $hiBad\n")
        val okL = freshLabel(s"$label.rg.ok")
        val flL = freshLabel(s"$label.rg.fail")
        emitTerminator(s"  br i1 $any, label %$flL, label %$okL\n")
        startBlock(flL)
        emitLine(s"  call void @__nex_trap_with(ptr @.slice_oob_msg)\n")
        emitTerminator(s"  unreachable\n")
        startBlock(okL)
        val end = if inclusive then
          val r = newReg(); emitLine(s"  $r = add i64 $hiV, 1\n"); r
        else hiV
        (loV, end, true, None)

    val (rLo, rEnd, rPres, _) = axis(rowAx, rowsAll, "sl2r")
    val (cLo, cEnd, cPres, _) = axis(colAx, colsAll, "sl2c")
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

  /** Lower a rank-1 slice-assign: `xs[lo..hi] = rhs`. Spec §4.14 lvalue
    * form (Fortran-90 array-section assignment). Bounds-checks the slice
    * against the destination, length-checks RHS against the slice length,
    * then per-element refcount-aware copies the elements from RHS into
    * the destination's underlying buffer. Both descriptor shares (LHS
    * and RHS) are released at exit.
    */
  protected def emitSliceAssign(arr: TExpr, lo: TExpr, hi: TExpr, inclusive: Boolean, value: TExpr): Unit =
    val elem  = arrayElem(arr.tpe)
    val stE   = storageType(elem)
    val langE = llvmType(elem)
    val esz   = elemSize(elem)

    val av    = emitExpr(arr)
    val loRaw = emitExpr(lo)
    val hiRaw = emitExpr(hi)
    val rv    = emitExpr(value)

    val dstLen = newReg()
    emitLine(s"  $dstLen = call i64 @__nex_arr1_len(ptr $av)\n")
    val loV = wrapNegBound(loRaw, dstLen)
    val hiV = wrapNegBound(hiRaw, dstLen)
    val negLo = newReg()
    emitLine(s"  $negLo = icmp slt i64 $loV, 0\n")
    val hiLtLo = newReg()
    emitLine(s"  $hiLtLo = icmp slt i64 $hiV, $loV\n")
    val hiBad = newReg()
    if inclusive then
      emitLine(s"  $hiBad = icmp sge i64 $hiV, $dstLen\n")
    else
      emitLine(s"  $hiBad = icmp sgt i64 $hiV, $dstLen\n")
    val any01 = newReg()
    emitLine(s"  $any01 = or i1 $negLo, $hiLtLo\n")
    val any = newReg()
    emitLine(s"  $any = or i1 $any01, $hiBad\n")
    val okL   = freshLabel("sla1.ok")
    val failL = freshLabel("sla1.fail")
    emitTerminator(s"  br i1 $any, label %$failL, label %$okL\n")
    startBlock(failL)
    emitLine(s"  call void @__nex_trap_with(ptr @.slice_oob_msg)\n")
    emitTerminator(s"  unreachable\n")
    startBlock(okL)

    val rawLen = newReg()
    emitLine(s"  $rawLen = sub i64 $hiV, $loV\n")
    val length = if inclusive then
      val r = newReg()
      emitLine(s"  $r = add i64 $rawLen, 1\n")
      r
    else rawLen

    val rhsLen = newReg()
    emitLine(s"  $rhsLen = call i64 @__nex_arr1_len(ptr $rv)\n")
    val mismatch = newReg()
    emitLine(s"  $mismatch = icmp ne i64 $rhsLen, $length\n")
    val okL2   = freshLabel("sla1.lenok")
    val failL2 = freshLabel("sla1.lenfail")
    emitTerminator(s"  br i1 $mismatch, label %$failL2, label %$okL2\n")
    startBlock(failL2)
    emitLine(s"  call void @__nex_trap_with(ptr @.slice_assign_len_msg)\n")
    emitTerminator(s"  unreachable\n")
    startBlock(okL2)

    val dstBuf = bufPtr(av, arr.tpe)
    val srcBuf = bufPtr(rv, value.tpe)

    emitCountingLoop(length, "sla1") { i =>
      val dIdx = newReg()
      emitLine(s"  $dIdx = add i64 $loV, $i\n")
      val dSlot = newReg()
      emitLine(s"  $dSlot = getelementptr inbounds $stE, ptr $dstBuf, i64 $dIdx\n")
      val sSlot = newReg()
      emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $i\n")
      val newV = loadElem(stE, sSlot, langE)
      if isRefCountedType(elem) then
        val oldV = loadElem(stE, dSlot, langE)
        emitArrInc(newV, elem)
        emitArrDec(oldV, elem)
      storeElem(stE, newV, dSlot)
    }

    emitArrDec(rv, value.tpe)
    emitArrDec(av, arr.tpe)

  /** Lower a rank-2 slice-assign: `m[axes...] = rhs`. Reuses the same
    * per-axis (lo, hi, preserved) decomposition as [[emitSlice2]], then
    * walks the slice region writing from RHS. RHS shape rules:
    *   - both axes preserved → RHS is rank-2 with matching rows/cols.
    *   - exactly one axis preserved → RHS is rank-1 of matching length.
    *   - both collapsed → never reaches here (TIndex path).
    */
  protected def emitSlice2Assign(arr: TExpr, rowAx: TAxisSpec, colAx: TAxisSpec, value: TExpr): Unit =
    val elem  = arrayElem(arr.tpe)
    val stE   = storageType(elem)
    val langE = llvmType(elem)

    val av = emitExpr(arr)
    val rowsAll = newReg()
    emitLine(s"  $rowsAll = call i64 @__nex_arr2_rows(ptr $av)\n")
    val colsAll = newReg()
    emitLine(s"  $colsAll = call i64 @__nex_arr2_cols(ptr $av)\n")

    def axis(spec: TAxisSpec, total: String, label: String): (String, String, Boolean) = spec match
      case TAxisAll => ("0", total, true)
      case TAxisIndex(idx) =>
        val ivRaw = emitExpr(idx)
        val iv    = wrapNegBound(ivRaw, total)
        val neg = newReg()
        emitLine(s"  $neg = icmp slt i64 $iv, 0\n")
        val ge  = newReg()
        emitLine(s"  $ge  = icmp sge i64 $iv, $total\n")
        val bad = newReg()
        emitLine(s"  $bad = or i1 $neg, $ge\n")
        val okL = freshLabel(s"$label.ix.ok")
        val flL = freshLabel(s"$label.ix.fail")
        emitTerminator(s"  br i1 $bad, label %$flL, label %$okL\n")
        startBlock(flL)
        emitLine(s"  call void @__nex_trap_with(ptr @.axis_oob_msg)\n")
        emitTerminator(s"  unreachable\n")
        startBlock(okL)
        val hi = newReg()
        emitLine(s"  $hi = add i64 $iv, 1\n")
        (iv, hi, false)
      case TAxisRange(lo, hi, inclusive) =>
        val loRaw = emitExpr(lo)
        val hiRaw = emitExpr(hi)
        val loV   = wrapNegBound(loRaw, total)
        val hiV   = wrapNegBound(hiRaw, total)
        val negLo = newReg()
        emitLine(s"  $negLo = icmp slt i64 $loV, 0\n")
        val hiLtLo = newReg()
        emitLine(s"  $hiLtLo = icmp slt i64 $hiV, $loV\n")
        val hiBad = newReg()
        if inclusive then
          emitLine(s"  $hiBad = icmp sge i64 $hiV, $total\n")
        else
          emitLine(s"  $hiBad = icmp sgt i64 $hiV, $total\n")
        val any01 = newReg()
        emitLine(s"  $any01 = or i1 $negLo, $hiLtLo\n")
        val any = newReg()
        emitLine(s"  $any = or i1 $any01, $hiBad\n")
        val okL = freshLabel(s"$label.rg.ok")
        val flL = freshLabel(s"$label.rg.fail")
        emitTerminator(s"  br i1 $any, label %$flL, label %$okL\n")
        startBlock(flL)
        emitLine(s"  call void @__nex_trap_with(ptr @.slice_oob_msg)\n")
        emitTerminator(s"  unreachable\n")
        startBlock(okL)
        val end = if inclusive then
          val r = newReg(); emitLine(s"  $r = add i64 $hiV, 1\n"); r
        else hiV
        (loV, end, true)

    val (rLo, rEnd, rPres) = axis(rowAx, rowsAll, "sla2r")
    val (cLo, cEnd, cPres) = axis(colAx, colsAll, "sla2c")
    val rLen = newReg()
    emitLine(s"  $rLen = sub i64 $rEnd, $rLo\n")
    val cLen = newReg()
    emitLine(s"  $cLen = sub i64 $cEnd, $cLo\n")

    val rv     = emitExpr(value)
    val dstBuf = bufPtr(av, arr.tpe)

    def writeSlot(rIdx: String, cIdx: String, newV: String): Unit =
      val flat = newReg(); emitLine(s"  $flat = mul i64 $rIdx, $colsAll\n")
      val idx  = newReg(); emitLine(s"  $idx  = add i64 $flat, $cIdx\n")
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stE, ptr $dstBuf, i64 $idx\n")
      if isRefCountedType(elem) then
        val oldV = loadElem(stE, slot, langE)
        emitArrInc(newV, elem)
        emitArrDec(oldV, elem)
      storeElem(stE, newV, slot)

    (rPres, cPres) match
      case (true, true) =>
        // rank-2 RHS — shape check rows*cols.
        val srcRows = newReg()
        emitLine(s"  $srcRows = call i64 @__nex_arr2_rows(ptr $rv)\n")
        val srcCols = newReg()
        emitLine(s"  $srcCols = call i64 @__nex_arr2_cols(ptr $rv)\n")
        val mr = newReg(); emitLine(s"  $mr = icmp ne i64 $srcRows, $rLen\n")
        val mc = newReg(); emitLine(s"  $mc = icmp ne i64 $srcCols, $cLen\n")
        val mm = newReg(); emitLine(s"  $mm = or i1 $mr, $mc\n")
        val okL = freshLabel("sla2.sok")
        val flL = freshLabel("sla2.sfail")
        emitTerminator(s"  br i1 $mm, label %$flL, label %$okL\n")
        startBlock(flL)
        emitLine(s"  call void @__nex_trap_with(ptr @.slice_assign_shape_msg)\n")
        emitTerminator(s"  unreachable\n")
        startBlock(okL)
        val srcBuf = bufPtr(rv, value.tpe)
        emitCountingLoop(rLen, "sla2r") { i =>
          emitCountingLoop(cLen, "sla2c") { j =>
            val dr = newReg(); emitLine(s"  $dr = add i64 $rLo, $i\n")
            val dc = newReg(); emitLine(s"  $dc = add i64 $cLo, $j\n")
            val sFlat = newReg(); emitLine(s"  $sFlat = mul i64 $i, $cLen\n")
            val sIdx  = newReg(); emitLine(s"  $sIdx  = add i64 $sFlat, $j\n")
            val sSlot = newReg()
            emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $sIdx\n")
            val newV = loadElem(stE, sSlot, langE)
            writeSlot(dr, dc, newV)
          }
        }
      case (true, false) | (false, true) =>
        // rank-1 RHS — flat length = rLen * cLen (one of them is 1).
        val sliceLen = newReg()
        emitLine(s"  $sliceLen = mul i64 $rLen, $cLen\n")
        val rhsLen = newReg()
        emitLine(s"  $rhsLen = call i64 @__nex_arr1_len(ptr $rv)\n")
        val mm = newReg()
        emitLine(s"  $mm = icmp ne i64 $rhsLen, $sliceLen\n")
        val okL = freshLabel("sla2.lok")
        val flL = freshLabel("sla2.lfail")
        emitTerminator(s"  br i1 $mm, label %$flL, label %$okL\n")
        startBlock(flL)
        emitLine(s"  call void @__nex_trap_with(ptr @.slice_assign_len_msg)\n")
        emitTerminator(s"  unreachable\n")
        startBlock(okL)
        val srcBuf = bufPtr(rv, value.tpe)
        val kSlot = newReg()
        emitLine(s"  $kSlot = alloca i64\n")
        emitLine(s"  store i64 0, ptr $kSlot\n")
        emitCountingLoop(rLen, "sla2rk") { i =>
          emitCountingLoop(cLen, "sla2ck") { j =>
            val dr = newReg(); emitLine(s"  $dr = add i64 $rLo, $i\n")
            val dc = newReg(); emitLine(s"  $dc = add i64 $cLo, $j\n")
            val k = newReg(); emitLine(s"  $k = load i64, ptr $kSlot\n")
            val sSlot = newReg()
            emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $k\n")
            val newV = loadElem(stE, sSlot, langE)
            writeSlot(dr, dc, newV)
            val k1 = newReg(); emitLine(s"  $k1 = add i64 $k, 1\n")
            emitLine(s"  store i64 $k1, ptr $kSlot\n")
          }
        }
      case (false, false) =>
        notYet("rank-2 slice-assign with both axes collapsed")

    emitArrDec(rv, value.tpe)
    emitArrDec(av, arr.tpe)

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
