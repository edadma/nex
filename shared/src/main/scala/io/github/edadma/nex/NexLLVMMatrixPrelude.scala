package io.github.edadma.nex

/** §10.4 rank-2 prelude emitters for the LLVM backend:
  *   shape / transpose / matmul / diag / reshape / flatten / sum_axis.
  *
  * Each helper reads an existing array, allocates a result, and
  * `dec`s the input's owning share before returning. The interpreter
  * remains the reference for element-wise semantics — these emitters
  * mirror the matching cases in [[NexInterpArrays]].
  *
  * `emitMatMulShared` is the joint implementation behind both the
  * `matmul(a, b)` prelude call and the `@` operator's `TMatMul` AST
  * node; keeping a single implementation means the two paths can never
  * drift. The scalar per-element kernels (`emitScalarBinOpSimple`,
  * `zeroOf`) used here live in [[NexLLVMArrayPrelude]] and are
  * reached through their abstract declarations in [[NexLLVMState]].
  */
protected trait NexLLVMMatrixPrelude extends NexLLVMState:

  /** `shape(arr)` — returns a tuple of the array's dimensions:
    *   rank-1 →  `(n,)`        (one-element tuple — printed as `(n)`)
    *   rank-2 →  `(rows, cols)`
    */
  protected def emitShapeCall(arr: TExpr, resultT: Type): String =
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
  protected def emitTransposeCall(arr: TExpr, resultT: Type): String =
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
    // for i in 0..rows: for j in 0..cols: dst[j*rows + i] = src[i*rowStride + j]
    emitCountingLoop(rows, "tp.r") { i =>
      emitCountingLoop(cols, "tp.c") { j =>
        val srcSlt = emitArr2ElemGep(arrV, srcBuf, i, j, stT)
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
  protected def emitMatMulCall(aE: TExpr, bE: TExpr, resultT: Type): String =
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
              val aSlt = emitArr2ElemGep(aV, aBuf, i, k, stT)
              val av   = loadElem(stT, aSlt, llT)
              val bSlt = emitArr2ElemGep(bV, bBuf, k, j, stT)
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
            val aSlt = emitArr2ElemGep(aV, aBuf, i, k, stT)
            val av   = loadElem(stT, aSlt, llT)
            val bSlt = emitArr1ElemGep(bV, bBuf, k, stT)
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
            val aSlt = emitArr1ElemGep(aV, aBuf, k, stT)
            val av   = loadElem(stT, aSlt, llT)
            val bSlt = emitArr2ElemGep(bV, bBuf, k, j, stT)
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
          val aSlt = emitArr1ElemGep(aV, aBuf, k, stT)
          val av   = loadElem(stT, aSlt, llT)
          val bSlt = emitArr1ElemGep(bV, bBuf, k, stT)
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
  protected def emitDiagCall(arr: TExpr, resultT: Type): String =
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
      val srcSlt = emitArr1ElemGep(arrV, srcBuf, i, stT)
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
  protected def emitReshapeCall(arr: TExpr, rowsE: TExpr, colsE: TExpr, resultT: Type): String =
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
    // Source is treated as a flat 1D buffer of `m*n` elements
    // (column-major), so use the rank-aware flat GEP helper so sub-rect
    // rank-2 sources still index the right physical slot.
    emitCountingLoop(rows, "rs.r") { r =>
      emitCountingLoop(cols, "rs.c") { c =>
        val srcOff = newReg(); emitLine(s"  $srcOff = mul i64 $c, $rows\n")
        val srcK   = newReg(); emitLine(s"  $srcK = add i64 $srcOff, $r\n")
        val srcSlt = emitArrElemGep(arrV, srcBuf, srcK, stT, arr.tpe)
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
  protected def emitFlattenCall(arr: TExpr, resultT: Type): String =
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
          val srcSlt = emitArr1ElemGep(arrV, srcBuf, k, stT)
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
            val srcSlt = emitArr2ElemGep(arrV, srcBuf, row, col, stT)
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
  protected def emitSumAxisCall(m: TExpr, axisE: TExpr, resultT: Type): String =
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
        val slot   = emitArr2ElemGep(mV, buf, i, j, stT)
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
      emitCountingLoop(cols, "sumax.1.col") { j =>
        val slot = emitArr2ElemGep(mV, buf, i, j, stT)
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
