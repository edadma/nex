package io.github.edadma.nex

/** Array surface for [[NexMLIRCodegen]]. Owns every emitter that
  * produces a `tensor<...>` (or operates on one) — type helpers,
  * static + dynamic slicing with negative-index wrap and OOB traps,
  * static + dynamic builders (`range` / `zeros` / `ones` /
  * `linspace`), element-wise / broadcast arithmetic and comparison,
  * matmul (rank-1×1, rank-2×1, rank-2×2), reductions (sum / product /
  * min / max / sum_axis), and the rank-shape builders
  * (transpose / diag / identity / flatten / reshape).
  *
  * Self-typed on `NexMLIRCodegen` for access to shared state
  * ([[out]], [[env]], [[fresh]], …) and the dispatch helpers
  * ([[scalarText]], [[scalarBinop]], [[promoteIntToReal]],
  * [[zeroLit]], [[notYet]]).
  */
trait NexMLIRArrays:
  self: NexMLIRCodegen =>

  /** Bufferize a tensor to a memref and extract its aligned data
    * pointer as a raw `i64`. `bufferization.to_buffer` is the LLVM 22
    * spelling (formerly `bufferization.to_memref`); the rest of the
    * pipeline already lowers memref ops to LLVM dialect, so the
    * resulting `i64` is the actual data address at runtime.
    */
  protected def emitTensorPointer(srcReg: String, ty: MTensor): String =
    val mrefT = memrefText(ty)
    val mReg  = fresh("mref")
    out.append(s"  $mReg = bufferization.to_buffer $srcReg : ${ty.text} to $mrefT\n")
    val idxReg = fresh("pidx")
    out.append(s"  $idxReg = memref.extract_aligned_pointer_as_index $mReg : $mrefT -> index\n")
    val ptrReg = fresh("pi")
    out.append(s"  $ptrReg = arith.index_castui $idxReg : index to i64\n")
    ptrReg

  /** Memref text for a tensor: same element + shape, just `memref<...>`
    * instead of `tensor<...>`.
    */
  protected def memrefText(ty: MTensor): String =
    val dims =
      if ty.shape.isEmpty then ""
      else ty.shape.map(d => if d < 0 then "?" else d.toString).mkString("", "x", "x")
    s"memref<$dims${scalarText(ty.elem)}>"

  /** Recover the `i64` length of a tensor's `axis`-th dimension. For
    * static dims emits an `arith.constant`; for dynamic dims uses
    * `tensor.dim` and casts the resulting `index` to i64.
    */
  protected def tensorDimAsI64(tReg: String, ty: MTensor, axis: Int): String =
    val d = ty.shape(axis)
    if d >= 0 then
      val r = fresh("dimc")
      out.append(s"  $r = arith.constant $d : i64\n")
      r
    else
      val axisR = fresh("daxis")
      out.append(s"  $axisR = arith.constant $axis : index\n")
      val dimR = fresh("dim")
      out.append(s"  $dimR = tensor.dim $tReg, $axisR : ${ty.text}\n")
      val i64R = fresh("dimi")
      out.append(s"  $i64R = arith.index_castui $dimR : index to i64\n")
      i64R

  /** Recover an `index`-typed dimension length for a tensor axis. Used
    * by index-wrap + bounds-trap sites that need an index extent
    * without bouncing through `i64`. Static dims emit a constant;
    * dynamic dims use `tensor.dim`.
    */
  protected def tensorDimAsIndex(tReg: String, ty: MTensor, axis: Int): String =
    val d = ty.shape(axis)
    if d >= 0 then
      val r = fresh("dimx")
      out.append(s"  $r = arith.constant $d : index\n")
      r
    else
      val axisR = fresh("daxis")
      out.append(s"  $axisR = arith.constant $axis : index\n")
      val r = fresh("dim")
      out.append(s"  $r = tensor.dim $tReg, $axisR : ${ty.text}\n")
      r

  /** Emit `tensor.empty(...)` for either a static or dynamic shape.
    * `dynSizes` carries the SSA names of `index`-typed values for each
    * `-1` in `ty.shape`, in shape order.
    */
  protected def emitTensorEmpty(ty: MTensor, dynSizes: List[String] = Nil): String =
    require(
      ty.shape.count(_ < 0) == dynSizes.size,
      s"tensor.empty: ${dynSizes.size} dyn sizes for shape ${ty.shape}",
    )
    val r = fresh("init")
    val args = if dynSizes.isEmpty then "" else dynSizes.mkString(", ")
    out.append(s"  $r = tensor.empty($args) : ${ty.text}\n")
    r

  /** Emit `tensor.empty(...)` shaped like `srcTy` but with element type
    * `outElem`. For each `-1` axis in the source, recovers the runtime
    * size via `tensor.dim` against `srcReg`. Used by the rank-N HOFs
    * (`map`, `broadcast`, element-wise) where the output shape mirrors
    * the input.
    */
  protected def emitTensorEmptyLike(srcReg: String, srcTy: MTensor, outElem: Type): (String, MTensor) =
    val outTy = MTensor(outElem, srcTy.shape)
    val dynSizes = srcTy.shape.zipWithIndex.collect { case (d, axis) if d < 0 =>
      val axisR = fresh("axis")
      out.append(s"  $axisR = arith.constant $axis : index\n")
      val dimR = fresh("dim")
      out.append(s"  $dimR = tensor.dim $srcReg, $axisR : ${srcTy.text}\n")
      dimR
    }
    (emitTensorEmpty(outTy, dynSizes), outTy)

  protected def arrayPrintHelper(elemT: Type, rank: Int): String = (elemT, rank) match
    case (TyInteger, 1) => "nex_print_array_1d_i64"
    case (TyReal,    1) => "nex_print_array_1d_f64"
    case (TyBool,    1) => "nex_print_array_1d_bool"
    case (TyInteger, 2) => "nex_print_array_2d_i64"
    case (TyReal,    2) => "nex_print_array_2d_f64"
    case (TyBool,    2) => "nex_print_array_2d_bool"
    case _              => notYet(s"array print helper for $elemT rank-$rank")

  /** True when an axis spec can be fully resolved at compile time:
    * `TAxisAll`, a literal `TAxisIndex`, or a `TAxisRange` whose
    * bounds are literals and stride is omitted. Used as the dispatch
    * gate between the static-size [[emitRank2Slice]] fast path and
    * the dynamic-size [[emitRank2SliceDynamic]] path.
    */
  protected def isStaticAxis(spec: TAxisSpec): Boolean = spec match
    case TAxisAll                                                                       => true
    case TAxisIndex(TIntLit(_, _, _))                                                   => true
    case TAxisRange(Some(TIntLit(_, _, _)), Some(TIntLit(_, _, _)), _, None)            => true
    case TAxisRange(None, Some(TIntLit(_, _, _)), _, None)                              => true
    case _                                                                              => false

  /** Resolved spec for one axis of a rank-2 slice. `offset` and `size`
    * are the corresponding entries in the `tensor.extract_slice`
    * offsets/sizes lists; `collapsed` is true when this axis was a
    * single integer index (in which case the result tensor drops one
    * rank — MLIR's `extract_slice` handles this via its
    * rank-reducing form when the static size is 1).
    */
  protected case class AxisSlice(offset: Int, size: Int, collapsed: Boolean)

  /** Resolve a `TAxisSpec` to its concrete (offset, size, collapsed)
    * triple given the corresponding source dimension extent. Only
    * literal-bound axes are accepted — anything else surfaces as
    * `notYet`.
    */
  protected def axisToSlice(spec: TAxisSpec, dim: Int): AxisSlice = spec match
    case TAxisAll =>
      AxisSlice(offset = 0, size = dim, collapsed = false)
    case TAxisIndex(TIntLit(v, _, _)) =>
      AxisSlice(offset = v.toInt, size = 1, collapsed = true)
    case TAxisRange(Some(TIntLit(lo, _, _)), Some(TIntLit(hi, _, _)), inclusive, None) =>
      val end = if inclusive then hi.toInt + 1 else hi.toInt
      AxisSlice(offset = lo.toInt, size = math.max(0, end - lo.toInt), collapsed = false)
    case TAxisRange(None, Some(TIntLit(hi, _, _)), inclusive, None) =>
      val end = if inclusive then hi.toInt + 1 else hi.toInt
      AxisSlice(offset = 0, size = math.max(0, end), collapsed = false)
    case other =>
      notYet(s"rank-2 slice axis spec: ${other.getClass.getSimpleName} with non-literal bound")

  /** Rank-2 `tensor.extract_slice` with literal offset/size on both
    * axes. Output rank is `2 - (number of collapsed axes)`. When
    * both axes are collapsed the result is a scalar — but that
    * shape isn't reachable here because the elaborator emits a
    * `TIndex` (not a `TSlice2`) for the two-integer-index form.
    */
  protected def emitRank2Slice(av: MlirVal, srcTy: MTensor, rowSpec: AxisSlice, colSpec: AxisSlice): MlirVal =
    val outShape = List(rowSpec, colSpec).collect {
      case s if !s.collapsed => s.size
    }
    val outTy = MTensor(srcTy.elem, outShape)
    if outShape.contains(0) then
      val r = fresh("emp")
      out.append(s"  $r = tensor.empty() : ${outTy.text}\n")
      return MlirVal(r, outTy)
    val r = fresh("sl2")
    out.append(
      s"  $r = tensor.extract_slice ${av.reg}[${rowSpec.offset}, ${colSpec.offset}] [${rowSpec.size}, ${colSpec.size}] [1, 1] : ${srcTy.text} to ${outTy.text}\n",
    )
    MlirVal(r, outTy)

  /** Resolved axis for the dynamic-rank-2 path. `offset`, `size`, and
    * `stride` are SSA `index` values (or literal strings for the
    * trivial 0 / 1 / size-1 cases). `collapsed` carries the
    * rank-reducing flag for `TAxisIndex`. The result type's
    * corresponding entry is built from these in
    * [[emitRank2SliceDynamic]] — collapsed axes drop entirely; the
    * remaining axes are `?` since their size flows in via the SSA
    * operand.
    */
  protected case class AxisSliceD(
      offset:    String,
      size:      String,
      stride:    String,
      collapsed: Boolean,
  )

  /** Resolve a `TAxisSpec` to its concrete operand triple for the
    * rank-2 dynamic-bound slice path. Mirrors
    * [[emitRank1SliceDynamic]] for one axis at a time; the source
    * dimension may itself be dynamic in which case the default `hi`
    * comes from `tensor.dim`.
    */
  protected def emitAxisSliceDynamic(
      spec:     TAxisSpec,
      srcTy:    MTensor,
      srcReg:   String,
      axisIdx:  Int,
      c0Idx:    String,
      c1Idx:    String,
  ): AxisSliceD =
    val srcDim = srcTy.shape(axisIdx)
    def srcDimIdx(): String =
      if srcDim >= 0 then
        val r = fresh("sdim")
        out.append(s"  $r = arith.constant $srcDim : index\n")
        r
      else
        val ax = fresh("daxis")
        out.append(s"  $ax = arith.constant $axisIdx : index\n")
        val r = fresh("sdim")
        out.append(s"  $r = tensor.dim $srcReg, $ax : ${srcTy.text}\n")
        r

    spec match
      case TAxisAll =>
        AxisSliceD(c0Idx, srcDimIdx(), c1Idx, collapsed = false)

      case TAxisIndex(e) =>
        val total = srcDimIdx()
        val v = emitExpr(e)
        val rawR = fresh("airaw")
        out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
        val offsetR = wrapNegBound(rawR, total)
        emitAxisIndexTrap(offsetR, total, c0Idx)
        AxisSliceD(offsetR, "1", "1", collapsed = true)

      case TAxisRange(loE, hiE, inclusive, strideE) =>
        val total = srcDimIdx()
        val loIdx = loE match
          case Some(e) =>
            val v = emitExpr(e)
            val rawR = fresh("loraw")
            out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
            wrapNegBound(rawR, total)
          case None => c0Idx
        val hiIdx = hiE match
          case Some(e) =>
            val v = emitExpr(e)
            val rawR = fresh("hiraw")
            out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
            val wrapped = wrapNegBound(rawR, total)
            if inclusive then
              val r = fresh("hix1")
              out.append(s"  $r = arith.addi $wrapped, $c1Idx : index\n")
              r
            else wrapped
          case None =>
            if inclusive then
              val r = fresh("hix1")
              out.append(s"  $r = arith.addi $total, $c1Idx : index\n")
              r
            else total
        val strideIdx = strideE match
          case Some(e) =>
            val v = emitExpr(e)
            val r = fresh("stx")
            out.append(s"  $r = arith.index_cast ${v.reg} : i64 to index\n")
            r
          case None => c1Idx
        emitSliceBoundsTrap(loIdx, hiIdx, total, strideIdx, c0Idx, strideE.isDefined)
        val rawSpan = fresh("span")
        out.append(s"  $rawSpan = arith.subi $hiIdx, $loIdx : index\n")
        val spanC = fresh("spanc")
        out.append(s"  $spanC = arith.maxsi $rawSpan, $c0Idx : index\n")
        val strideM1 = fresh("stm1")
        out.append(s"  $strideM1 = arith.subi $strideIdx, $c1Idx : index\n")
        val numer = fresh("num")
        out.append(s"  $numer = arith.addi $spanC, $strideM1 : index\n")
        val sz = fresh("asz")
        out.append(s"  $sz = arith.divui $numer, $strideIdx : index\n")
        AxisSliceD(loIdx, sz, strideIdx, collapsed = false)

  /** Rank-2 slice with runtime axes — runtime bounds, open-ended
    * forms, and stride all flow through here. Each axis resolves to
    * an SSA `(offset, size, stride)` triple via
    * [[emitAxisSliceDynamic]]; the result type drops collapsed axes
    * and uses `?` for the rest. The static-bound fast path in
    * [[emitRank2Slice]] still handles the all-literal case for
    * tighter IR.
    */
  protected def emitRank2SliceDynamic(
      av:    MlirVal,
      srcTy: MTensor,
      rowAx: TAxisSpec,
      colAx: TAxisSpec,
  ): MlirVal =
    val c0Idx = fresh("c0")
    out.append(s"  $c0Idx = arith.constant 0 : index\n")
    val c1Idx = fresh("c1")
    out.append(s"  $c1Idx = arith.constant 1 : index\n")

    val rowAxis = emitAxisSliceDynamic(rowAx, srcTy, av.reg, 0, c0Idx, c1Idx)
    val colAxis = emitAxisSliceDynamic(colAx, srcTy, av.reg, 1, c0Idx, c1Idx)

    val outShape = List(rowAxis, colAxis).filterNot(_.collapsed).map(_ => -1)
    val outTy    = MTensor(srcTy.elem, outShape)

    val r = fresh("dsl2")
    out.append(
      s"  $r = tensor.extract_slice ${av.reg}" +
        s"[${rowAxis.offset}, ${colAxis.offset}] " +
        s"[${rowAxis.size}, ${colAxis.size}] " +
        s"[${rowAxis.stride}, ${colAxis.stride}] : ${srcTy.text} to ${outTy.text}\n",
    )
    MlirVal(r, outTy)

  /** Rank-1 slice `a[lo..hi]` / `a[lo..=hi]` with literal bounds.
    * Lowers to `tensor.extract_slice` with a static offset / size /
    * unit stride, which produces a freshly-allocated tensor of the
    * sliced length. Empty slices (computed length <= 0) collapse to
    * `tensor.empty() : tensor<0xT>` — printing walks zero elements
    * and emits `[]\n`.
    */
  protected def emitRank1Slice(av: MlirVal, srcTy: MTensor, offset: Int, len: Int): MlirVal =
    val outTy = MTensor(srcTy.elem, List(len))
    if len == 0 then
      val r = fresh("emp")
      out.append(s"  $r = tensor.empty() : ${outTy.text}\n")
      return MlirVal(r, outTy)
    val r = fresh("sl")
    out.append(s"  $r = tensor.extract_slice ${av.reg}[$offset] [$len] [1] : ${srcTy.text} to ${outTy.text}\n")
    MlirVal(r, outTy)

  /** Rank-1 slice with any combination of open-ended bounds, runtime
    * bounds, and stride. Always produces `tensor<?xT>`:
    *
    *   - Open `lo` (`a[..hi]`) defaults to 0; open `hi` (`a[lo..]`) to
    *     the source length recovered via `tensor.dim`.
    *   - `[..= ]` (inclusive) bumps the upper bound by one.
    *   - Stride defaults to 1; runtime stride uses ceil-divide for the
    *     output length: `(max(0, span) + stride - 1) / stride`.
    *
    * Negative-bound wrap (spec §4.14): `lo`/`hi` < 0 maps to
    * `bound + len` before the bounds check, so `a[-3..len]` selects
    * the last three elements. The check itself trips `lo < 0` after
    * wrap (over-negative input) or `hi > len`, routing through
    * `nex_trap_slice_oob`. Stride <= 0 also traps.
    */
  protected def emitRank1SliceDynamic(
      av:        MlirVal,
      srcTy:     MTensor,
      loE:       Option[TExpr],
      hiE:       Option[TExpr],
      inclusive: Boolean,
      strideE:   Option[TExpr],
  ): MlirVal =
    val outTy = MTensor(srcTy.elem, List(-1))

    val c0Idx = fresh("c0")
    out.append(s"  $c0Idx = arith.constant 0 : index\n")
    val c1Idx = fresh("c1")
    out.append(s"  $c1Idx = arith.constant 1 : index\n")

    val srcLenIdx =
      if srcTy.shape.head >= 0 then
        val r = fresh("srclen")
        out.append(s"  $r = arith.constant ${srcTy.shape.head} : index\n")
        r
      else
        val axisR = fresh("axis")
        out.append(s"  $axisR = arith.constant 0 : index\n")
        val r = fresh("srclen")
        out.append(s"  $r = tensor.dim ${av.reg}, $axisR : ${srcTy.text}\n")
        r

    val loIdx = loE match
      case Some(e) =>
        val v = emitExpr(e)
        val rawR = fresh("loraw")
        out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
        wrapNegBound(rawR, srcLenIdx)
      case None => c0Idx

    val hiIdx = hiE match
      case Some(e) =>
        val v = emitExpr(e)
        val rawR = fresh("hiraw")
        out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
        val wrapped = wrapNegBound(rawR, srcLenIdx)
        if inclusive then
          val r = fresh("hix1")
          out.append(s"  $r = arith.addi $wrapped, $c1Idx : index\n")
          r
        else wrapped
      case None =>
        if inclusive then
          val r = fresh("hix1")
          out.append(s"  $r = arith.addi $srcLenIdx, $c1Idx : index\n")
          r
        else srcLenIdx

    val strideIdx = strideE match
      case Some(e) =>
        val v = emitExpr(e)
        val r = fresh("stx")
        out.append(s"  $r = arith.index_cast ${v.reg} : i64 to index\n")
        r
      case None => c1Idx

    emitSliceBoundsTrap(loIdx, hiIdx, srcLenIdx, strideIdx, c0Idx, strideE.isDefined)

    val rawSpan = fresh("span")
    out.append(s"  $rawSpan = arith.subi $hiIdx, $loIdx : index\n")
    val spanClamped = fresh("spanc")
    out.append(s"  $spanClamped = arith.maxsi $rawSpan, $c0Idx : index\n")
    val strideM1 = fresh("stm1")
    out.append(s"  $strideM1 = arith.subi $strideIdx, $c1Idx : index\n")
    val numerator = fresh("num")
    out.append(s"  $numerator = arith.addi $spanClamped, $strideM1 : index\n")
    val sliceLen = fresh("slen")
    out.append(s"  $sliceLen = arith.divui $numerator, $strideIdx : index\n")

    val r = fresh("dsl")
    out.append(
      s"  $r = tensor.extract_slice ${av.reg}[$loIdx] [$sliceLen] [$strideIdx] : ${srcTy.text} to ${outTy.text}\n",
    )
    MlirVal(r, outTy)

  /** Negative-bound wrap (spec §4.14): if `raw < 0` return `raw + extent`,
    * otherwise `raw`. Mirrors the LLVM backend's `wrapNegBound`. The
    * caller's bounds check still runs on the result, so an
    * over-negative input (e.g. `lo = -10` on a length-3 array)
    * still trips the `< 0` clause and traps.
    */
  protected def wrapNegBound(raw: String, extent: String): String =
    val c0 = fresh("wc0")
    out.append(s"  $c0 = arith.constant 0 : index\n")
    val isNeg = fresh("wneg")
    out.append(s"  $isNeg = arith.cmpi slt, $raw, $c0 : index\n")
    val wrapped = fresh("wwrap")
    out.append(s"  $wrapped = arith.addi $raw, $extent : index\n")
    val out0 = fresh("wout")
    out.append(s"  $out0 = arith.select $isNeg, $wrapped, $raw : index\n")
    out0

  /** Emit the rank-1 slice bounds-check trap. Conditions match the LLVM
    * backend: post-wrap `lo < 0` (over-negative), `hi < lo`, `hi >
    * srcLen`, plus stride `<= 0` when the slice was user-stride'd.
    * Branch to `nex_trap_slice_oob` on failure; the function exits.
    */
  protected def emitSliceBoundsTrap(
      loIdx:    String,
      hiIdx:    String,
      srcLen:   String,
      stride:   String,
      c0Idx:    String,
      hasStride: Boolean,
  ): Unit =
    val negLo = fresh("nlo")
    out.append(s"  $negLo = arith.cmpi slt, $loIdx, $c0Idx : index\n")
    val hiLtLo = fresh("hlt")
    out.append(s"  $hiLtLo = arith.cmpi slt, $hiIdx, $loIdx : index\n")
    val hiBad = fresh("hbad")
    out.append(s"  $hiBad = arith.cmpi sgt, $hiIdx, $srcLen : index\n")
    val any01 = fresh("any1")
    out.append(s"  $any01 = arith.ori $negLo, $hiLtLo : i1\n")
    val any02 = fresh("any2")
    out.append(s"  $any02 = arith.ori $any01, $hiBad : i1\n")
    val any = if hasStride then
      val sBad = fresh("sbad")
      out.append(s"  $sBad = arith.cmpi sle, $stride, $c0Idx : index\n")
      val a = fresh("any")
      out.append(s"  $a = arith.ori $any02, $sBad : i1\n")
      a
    else any02
    out.append(s"  scf.if $any {\n")
    out.append(s"    func.call @nex_trap_slice_oob() : () -> ()\n")
    out.append(s"    scf.yield\n")
    out.append(s"  }\n")

  /** Axis-index trap for the rank-2 `TAxisIndex` case: after wrap,
    * `iv < 0` or `iv >= total` routes through `nex_trap_axis_oob`.
    */
  protected def emitAxisIndexTrap(iv: String, total: String, c0Idx: String): Unit =
    val neg = fresh("aneg")
    out.append(s"  $neg = arith.cmpi slt, $iv, $c0Idx : index\n")
    val ge = fresh("age")
    out.append(s"  $ge = arith.cmpi sge, $iv, $total : index\n")
    val bad = fresh("abad")
    out.append(s"  $bad = arith.ori $neg, $ge : i1\n")
    out.append(s"  scf.if $bad {\n")
    out.append(s"    func.call @nex_trap_axis_oob() : () -> ()\n")
    out.append(s"    scf.yield\n")
    out.append(s"  }\n")

  /** Element-wise binop via `linalg.map` over a fresh `tensor.empty()`
    * output. Both operands must already have the same tensor type.
    *
    * LLVM 22 quirk: the `linalg.map` block arity is `inputs + outputs`,
    * not `inputs` as the dialect docs suggest. The out argument is
    * unused — we discard its value and yield only the computed
    * element — but the verifier requires it.
    */
  protected def emitElementWiseBinop(op: String, lv: MlirVal, rv: MlirVal, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val opName = scalarBinop(op, elemT)
    val (initR, _) = emitTensorEmptyLike(lv.reg, ty, elemT)
    val outR = fresh("ew")
    out.append(
      s"  $outR = linalg.map ins(${lv.reg}, ${rv.reg} : ${ty.text}, ${ty.text}) outs($initR : ${ty.text})\n",
    )
    out.append(s"    (%a: $scalar, %b: $scalar, %_o: $scalar) {\n")
    out.append(s"      %s = $opName %a, %b : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Generalised `@` operator covering the three rank combinations
    * `NexInterpreter` recognises:
    *
    *   - rank-2 × rank-2: `linalg.matmul` → rank-2 result
    *   - rank-2 × rank-1: `linalg.matvec` → rank-1 result
    *   - rank-1 × rank-1: `linalg.dot`    → 0-d result, then extract
    *
    * Element types must match and the contracted dimension must agree.
    * Every variant uses the same zero-fill init pattern since the
    * linalg op accumulates into its output.
    */
  protected def emitMatMul(lv: MlirVal, rv: MlirVal): MlirVal =
    (lv.ty, rv.ty) match
      case (MTensor(elemL, List(n, k)), MTensor(elemR, List(k2, m))) if elemL == elemR && k == k2 =>
        val outTy = MTensor(elemL, List(n, m))
        val initR = emitZeroInit(outTy)
        val mmR   = fresh("mm")
        out.append(
          s"  $mmR = linalg.matmul ins(${lv.reg}, ${rv.reg} : ${lv.ty.text}, ${rv.ty.text}) outs($initR : ${outTy.text}) -> ${outTy.text}\n",
        )
        MlirVal(mmR, outTy)
      case (MTensor(elemL, List(n, k)), MTensor(elemR, List(k2))) if elemL == elemR && k == k2 =>
        val outTy = MTensor(elemL, List(n))
        val initR = emitZeroInit(outTy)
        val mvR   = fresh("mv")
        out.append(
          s"  $mvR = linalg.matvec ins(${lv.reg}, ${rv.reg} : ${lv.ty.text}, ${rv.ty.text}) outs($initR : ${outTy.text}) -> ${outTy.text}\n",
        )
        MlirVal(mvR, outTy)
      case (MTensor(elemL, List(k)), MTensor(elemR, List(k2))) if elemL == elemR && k == k2 =>
        val outTy = MTensor(elemL, Nil)
        val initR = emitZeroInit(outTy)
        val dotR  = fresh("dot")
        out.append(
          s"  $dotR = linalg.dot ins(${lv.reg}, ${rv.reg} : ${lv.ty.text}, ${rv.ty.text}) outs($initR : ${outTy.text}) -> ${outTy.text}\n",
        )
        val scalarR = fresh("dot_s")
        out.append(s"  $scalarR = tensor.extract $dotR[] : ${outTy.text}\n")
        MlirVal(scalarR, MScalar(elemL))
      case (lt, rt) =>
        notYet(s"matmul shape: $lt @ $rt")
  /** Zero-filled output tensor for the linalg.{matmul, matvec, dot}
    * family. `linalg.fill` over a `tensor.empty()` is the canonical
    * way to materialise the accumulator they reduce into.
    */
  protected def emitZeroInit(ty: MTensor): String =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val zeroR  = fresh("zero")
    out.append(s"  $zeroR = arith.constant ${zeroLit(elemT)} : $scalar\n")
    val emptyR = fresh("empty")
    out.append(s"  $emptyR = tensor.empty() : ${ty.text}\n")
    val initR  = fresh("init")
    out.append(s"  $initR = linalg.fill ins($zeroR : $scalar) outs($emptyR : ${ty.text}) -> ${ty.text}\n")
    initR
  /** Sum-reduce a tensor of any rank to a 0-d tensor, then extract
    * the scalar. Matches `NexInterpreter`'s rule that `sum` walks
    * every element regardless of rank — for rank-N input we reduce
    * along all N dimensions in one `linalg.reduce` and the output
    * is 0-d. Init value is the element-type zero; reducer is the
    * element-type add.
    */
  protected def emitSumReduce(srcReg: String, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val outTy  = MTensor(elemT, Nil)
    val initER = fresh("init_e")
    out.append(s"  $initER = arith.constant ${zeroLit(elemT)} : $scalar\n")
    val initR = fresh("init")
    out.append(s"  $initR = tensor.from_elements $initER : ${outTy.text}\n")
    val dims  = ty.shape.indices.mkString(", ")
    val sumTR = fresh("sum_t")
    out.append(
      s"  $sumTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("+", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val sumR = fresh("sum")
    out.append(s"  $sumR = tensor.extract $sumTR[] : ${outTy.text}\n")
    MlirVal(sumR, MScalar(elemT))
  /** Scalar-against-tensor broadcast for arithmetic ops. Emits one
    * `linalg.map` over the tensor; the body closes over the scalar
    * SSA value from the parent scope (MLIR's dominance rules let
    * region bodies reference parent-scope values). `scalarFirst`
    * matters for non-commutative ops: `100 - xs` and `xs - 100`
    * differ.
    */
  protected def emitBroadcast(op: String, sv: MlirVal, av: MlirVal, ty: MTensor, scalarFirst: Boolean): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val opName = scalarBinop(op, elemT)
    val (initR, _) = emitTensorEmptyLike(av.reg, ty, elemT)
    val outR = fresh("bc")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${ty.text})\n",
    )
    out.append(s"    (%a: $scalar, %_o: $scalar) {\n")
    if scalarFirst then
      out.append(s"      %s = $opName ${sv.reg}, %a : $scalar\n")
    else
      out.append(s"      %s = $opName %a, ${sv.reg} : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    MlirVal(outR, ty)
  /** Element-wise arithmetic on tensors of matching shape but mismatched
    * element types (e.g. `[int] + [real]`). The wider numeric type is
    * the result element type; each loaded element is promoted to that
    * type via `arith.sitofp` inside the region body before the binop
    * runs. Mirrors `emitElementWiseComparison` but yields a numeric
    * tensor rather than a bool tensor.
    */
  protected def emitElementWiseMixed(op: String, lv: MlirVal, rv: MlirVal, lt: MTensor, rt: MTensor): MlirVal =
    val lElem    = lt.elem
    val rElem    = rt.elem
    val commonT  = if lElem == TyReal || rElem == TyReal then TyReal else TyInteger
    val commonS  = scalarText(commonT)
    val opName   = scalarBinop(op, commonT)
    val (initR, outTy) = emitTensorEmptyLike(lv.reg, lt, commonT)
    val outR = fresh("ewm")
    out.append(
      s"  $outR = linalg.map ins(${lv.reg}, ${rv.reg} : ${lt.text}, ${rt.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(lElem)}, %b: ${scalarText(rElem)}, %_o: $commonS) {\n")
    val aName = promoteInRegion("%a", lElem, commonT)
    val bName = promoteInRegion("%b", rElem, commonT)
    out.append(s"      %s = $opName $aName, $bName : $commonS\n")
    out.append(s"      linalg.yield %s : $commonS\n")
    out.append("    }\n")
    MlirVal(outR, outTy)
  /** Scalar-against-tensor arithmetic broadcast when the scalar's
    * element type doesn't match the tensor's (e.g. `2 + [1.0, 2.0]`).
    * The scalar is promoted once outside the map body if needed; the
    * per-element promotion of the loaded tensor element happens
    * inside the region body. Output element type is the wider common
    * type. `scalarFirst` controls operand order for non-commutative
    * ops.
    */
  protected def emitBroadcastMixed(
      op: String,
      sv: MlirVal,
      av: MlirVal,
      ty: MTensor,
      scalarFirst: Boolean,
      scalarElem: Type,
      arrElem: Type,
  ): MlirVal =
    val commonT = if scalarElem == TyReal || arrElem == TyReal then TyReal else TyInteger
    val commonS = scalarText(commonT)
    val opName  = scalarBinop(op, commonT)
    val svPromoted =
      if scalarElem == commonT then sv.reg
      else
        val r = fresh("ps")
        out.append(s"  $r = arith.sitofp ${sv.reg} : ${scalarText(scalarElem)} to $commonS\n")
        r
    val (initR, outTy) = emitTensorEmptyLike(av.reg, ty, commonT)
    val outR = fresh("bcm")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(arrElem)}, %_o: $commonS) {\n")
    val elemName = promoteInRegion("%a", arrElem, commonT)
    val (lhs, rhs) = if scalarFirst then (svPromoted, elemName) else (elemName, svPromoted)
    out.append(s"      %s = $opName $lhs, $rhs : $commonS\n")
    out.append(s"      linalg.yield %s : $commonS\n")
    out.append("    }\n")
    MlirVal(outR, outTy)
  /** Element-wise comparison: `xs < ys`, `xs == ys`, etc. Output element
    * type is always `i1`. Mixed-element-type inputs (`[int] < [real]`)
    * are handled by promoting each loaded element up to the wider numeric
    * type inside the region body, then dispatching to `arith.cmpi` /
    * `arith.cmpf` with the same predicate scheme as scalar comparisons
    * (ordered for everything except `!=`, which uses `une` so NaN-vs-NaN
    * is correctly true).
    */
  protected def emitElementWiseComparison(op: String, lv: MlirVal, rv: MlirVal, lt: MTensor, rt: MTensor): MlirVal =
    val lElem    = lt.elem
    val rElem    = rt.elem
    val commonT  = if lElem == TyReal || rElem == TyReal then TyReal else TyInteger
    val commonS  = scalarText(commonT)
    val (initR, outTy) = emitTensorEmptyLike(lv.reg, lt, TyBool)
    val outR = fresh("ewcmp")
    out.append(
      s"  $outR = linalg.map ins(${lv.reg}, ${rv.reg} : ${lt.text}, ${rt.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(lElem)}, %b: ${scalarText(rElem)}, %_o: i1) {\n")
    val aName = promoteInRegion("%a", lElem, commonT)
    val bName = promoteInRegion("%b", rElem, commonT)
    val pred  = comparisonPredicate(op, commonT)
    val cmp   = if commonT == TyInteger then "arith.cmpi" else "arith.cmpf"
    out.append(s"      %s = $cmp $pred, $aName, $bName : $commonS\n")
    out.append(s"      linalg.yield %s : i1\n")
    out.append("    }\n")
    MlirVal(outR, outTy)
  /** Scalar-against-tensor comparison broadcast: `xs < 5`, `2 < xs`,
    * etc. Output is a `tensor<...xi1>` with the tensor's shape. The
    * scalar is promoted once outside the map body if its type doesn't
    * match the common comparison type; the per-element promotion of
    * the loaded tensor element happens inside the region body.
    * `scalarFirst` carries through to the operand order of the cmp op
    * — matters for non-symmetric predicates (`<`, `<=`, `>`, `>=`).
    */
  protected def emitBroadcastComparison(
      op: String,
      sv: MlirVal,
      av: MlirVal,
      ty: MTensor,
      scalarFirst: Boolean,
      scalarElem: Type,
      arrElem: Type,
  ): MlirVal =
    val commonT = if scalarElem == TyReal || arrElem == TyReal then TyReal else TyInteger
    val commonS = scalarText(commonT)
    val svPromoted =
      if scalarElem == commonT then sv.reg
      else
        val r = fresh("ps")
        out.append(s"  $r = arith.sitofp ${sv.reg} : ${scalarText(scalarElem)} to $commonS\n")
        r
    val (initR, outTy) = emitTensorEmptyLike(av.reg, ty, TyBool)
    val outR = fresh("bccmp")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(arrElem)}, %_o: i1) {\n")
    val elemName = promoteInRegion("%a", arrElem, commonT)
    val pred     = comparisonPredicate(op, commonT)
    val cmp      = if commonT == TyInteger then "arith.cmpi" else "arith.cmpf"
    val (lhs, rhs) = if scalarFirst then (svPromoted, elemName) else (elemName, svPromoted)
    out.append(s"      %s = $cmp $pred, $lhs, $rhs : $commonS\n")
    out.append(s"      linalg.yield %s : i1\n")
    out.append("    }\n")
    MlirVal(outR, outTy)
  /** `product(arr)` (spec §10.4): rank-N multiplicative reduction.
    * Same shape as `emitSumReduce` but seeds the accumulator with 1
    * and uses the type-specific multiply op (`muli` for int, `mulf`
    * for real). Walks every element regardless of rank.
    */
  protected def emitProductReduce(srcReg: String, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val outTy  = MTensor(elemT, Nil)
    val oneR   = fresh("init_e")
    val oneLit = elemT match
      case TyInteger => "1"
      case TyReal    => "1.0"
      case other     => notYet(s"one literal for $other")
    out.append(s"  $oneR = arith.constant $oneLit : $scalar\n")
    val initR = fresh("init")
    out.append(s"  $initR = tensor.from_elements $oneR : ${outTy.text}\n")
    val dims  = ty.shape.indices.mkString(", ")
    val prodTR = fresh("prod_t")
    out.append(
      s"  $prodTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("*", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val prodR = fresh("prod")
    out.append(s"  $prodR = tensor.extract $prodTR[] : ${outTy.text}\n")
    MlirVal(prodR, MScalar(elemT))
  /** `diag(arr)` (rank-1 → rank-2): build an n×n matrix with `arr` on
    * the diagonal and zeros elsewhere. Lowers to `linalg.fill` of 0
    * over a fresh n×n tensor, then a `linalg.map` over a rank-1
    * sequence of [0..n) that scatters `arr[i]` to position `(i, i)`
    * via `tensor.insert`. Output is a fresh tensor.
    *
    * Implemented as a `linalg.map` over the n×n output that uses
    * `linalg.index` to recover (i, j) and `arith.cmpi eq` plus
    * `arith.select` to either pick `arr[i]` or zero.
    */
  protected def emitDiag(av: MlirVal, srcTy: MTensor, n: Int): MlirVal =
    val elemT = srcTy.elem
    val s     = scalarText(elemT)
    val zero  = zeroLit(elemT)
    val outTy = MTensor(elemT, List(n, n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("diag")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val iR    = fresh("i")
    out.append(s"      $iR = linalg.index 0 : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = linalg.index 1 : index\n")
    val cmpR  = fresh("eq")
    out.append(s"      $cmpR = arith.cmpi eq, $iR, $jR : index\n")
    val zR    = fresh("z")
    out.append(s"      $zR = arith.constant $zero : $s\n")
    val eltR  = fresh("elt")
    out.append(s"      $eltR = tensor.extract ${av.reg}[$iR] : ${srcTy.text}\n")
    val selR  = fresh("sel")
    out.append(s"      $selR = arith.select $cmpR, $eltR, $zR : $s\n")
    out.append(s"      linalg.yield $selR : $s\n")
    out.append("    }\n")
    MlirVal(outR, outTy)
  /** `identity(n)`: n×n integer identity matrix. Same shape as `diag`
    * but the diagonal value is the constant 1. Element type is
    * integer (matches the interpreter — the spec says real-typed but
    * v0 has a known divergence).
    */
  protected def emitIdentity(n: Int): MlirVal =
    val outTy = MTensor(TyInteger, List(n, n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("id")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: i64) {\n")
    val iR    = fresh("i")
    out.append(s"      $iR = linalg.index 0 : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = linalg.index 1 : index\n")
    val cmpR  = fresh("eq")
    out.append(s"      $cmpR = arith.cmpi eq, $iR, $jR : index\n")
    val oneR  = fresh("one")
    out.append(s"      $oneR = arith.constant 1 : i64\n")
    val zR    = fresh("z")
    out.append(s"      $zR = arith.constant 0 : i64\n")
    val selR  = fresh("sel")
    out.append(s"      $selR = arith.select $cmpR, $oneR, $zR : i64\n")
    out.append(s"      linalg.yield $selR : i64\n")
    out.append("    }\n")
    MlirVal(outR, outTy)
  /** `sum_axis(m, axis)` (spec §10.4): rank-2 reduction along one axis,
    * producing a rank-1 result. axis=0 sums down each column (output
    * shape = cols); axis=1 sums across each row (output shape = rows).
    * Lowers to `linalg.reduce ... dimensions = [axis]` with a `+`
    * accumulator body — same kernel as the all-dims `emitSumReduce`,
    * just with a non-empty output shape.
    */
  protected def emitSumAxis(av: MlirVal, srcTy: MTensor, axis: Int): MlirVal =
    val elemT     = srcTy.elem
    val scalar    = scalarText(elemT)
    val outShape  = srcTy.shape.zipWithIndex.collect { case (d, i) if i != axis => d }
    val outTy     = MTensor(elemT, outShape)
    val initER    = fresh("init_e")
    out.append(s"  $initER = arith.constant ${zeroLit(elemT)} : $scalar\n")
    val initR     = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val filledR   = fresh("filled")
    out.append(s"  $filledR = linalg.fill ins($initER : $scalar) outs($initR : ${outTy.text}) -> ${outTy.text}\n")
    val outR      = fresh("axis")
    out.append(
      s"  $outR = linalg.reduce ins(${av.reg} : ${srcTy.text}) outs($filledR : ${outTy.text}) dimensions = [$axis]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("+", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    MlirVal(outR, outTy)
  /** `flatten(xs)` on a rank-1 array: identity, but materialise a fresh
    * tensor to match the interpreter's "flatten always copies"
    * semantics. A `linalg.copy`-style identity map does the job.
    */
  protected def emitFlatten1D(av: MlirVal): MlirVal =
    val ty    = av.ty.asInstanceOf[MTensor]
    val elemT = ty.elem
    val s     = scalarText(elemT)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val outR  = fresh("flat")
    out.append(s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${ty.text})\n")
    out.append(s"    (%a: $s, %_o: $s) {\n")
    out.append(s"      linalg.yield %a : $s\n")
    out.append("    }\n")
    MlirVal(outR, ty)
  /** `flatten(m)` on a rank-2 array — Nex spec §10.4 says this is
    * **column-major**: `flat[i + r*j] = m[i, j]` for an r×c matrix.
    * We materialise a fresh rank-1 of length r·c via `linalg.map`
    * whose body uses `linalg.index 0` plus a divmod against `r` to
    * recover the source `(i, j)`, then `tensor.extract %m[i, j]`.
    */
  protected def emitFlatten2D(av: MlirVal, srcTy: MTensor, rows: Int, cols: Int): MlirVal =
    val elemT = srcTy.elem
    val s     = scalarText(elemT)
    val total = rows * cols
    val outTy = MTensor(elemT, List(total))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("flat")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val rowsC = fresh("rows")
    out.append(s"      $rowsC = arith.constant $rows : index\n")
    val iR    = fresh("i")
    out.append(s"      $iR = arith.remui $idxR, $rowsC : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = arith.divui $idxR, $rowsC : index\n")
    val eltR  = fresh("elt")
    out.append(s"      $eltR = tensor.extract ${av.reg}[$iR, $jR] : ${srcTy.text}\n")
    out.append(s"      linalg.yield $eltR : $s\n")
    out.append("    }\n")
    MlirVal(outR, outTy)
  /** `reshape(flat, r, c)` — column-major inverse of `flatten`. The
    * input is rank-1; the output is rank-2 with `m[i, j] = flat[i + r*j]`.
    * Same shape as `emitFlatten2D` but iterates the (rows, cols) output
    * grid via two `linalg.index` calls and computes the flat source
    * index by hand. The elaborator rejects shape mismatches before
    * we get here, so `length(flat)` is guaranteed to equal `r*c`.
    */
  protected def emitReshape(av: MlirVal, srcTy: MTensor, rows: Int, cols: Int): MlirVal =
    val elemT = srcTy.elem
    val s     = scalarText(elemT)
    val outTy = MTensor(elemT, List(rows, cols))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("rs")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val iR    = fresh("i")
    out.append(s"      $iR = linalg.index 0 : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = linalg.index 1 : index\n")
    val rowsC = fresh("rows")
    out.append(s"      $rowsC = arith.constant $rows : index\n")
    val mul   = fresh("mul")
    out.append(s"      $mul = arith.muli $rowsC, $jR : index\n")
    val flat  = fresh("flat")
    out.append(s"      $flat = arith.addi $iR, $mul : index\n")
    val eltR  = fresh("elt")
    out.append(s"      $eltR = tensor.extract ${av.reg}[$flat] : ${srcTy.text}\n")
    out.append(s"      linalg.yield $eltR : $s\n")
    out.append("    }\n")
    MlirVal(outR, outTy)
  /** Rank-2 `.transpose()` / `transpose(m)`. Output shape swaps the
    * row and column dimensions; element type is unchanged. Lowers to
    * a single `linalg.transpose` with permutation `[1, 0]`, which
    * `--convert-linalg-to-loops` reduces to a nested counting loop
    * that does `out[j, i] = in[i, j]`.
    */
  protected def emitTranspose(srcReg: String, ty: MTensor, rows: Int, cols: Int): MlirVal =
    val outTy = MTensor(ty.elem, List(cols, rows))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("tr")
    out.append(
      s"  $outR = linalg.transpose ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) permutation = [1, 0]\n",
    )
    MlirVal(outR, outTy)
  /** Literal `range(lo, hi)`: integer half-open range with statically
    * known length. Length zero (when `hi <= lo`) lowers to an empty
    * `tensor<0xi64>`. Otherwise we walk the iteration domain via
    * `linalg.index` and add the loaded position to the literal `lo`.
    */
  protected def emitRangeCall(lo: Long, hi: Long): MlirVal =
    val len = math.max(0L, hi - lo).toInt
    val ty  = MTensor(TyInteger, List(len))
    if len == 0 then
      val r = fresh("rng")
      out.append(s"  $r = tensor.empty() : ${ty.text}\n")
      return MlirVal(r, ty)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val loConst = fresh("lo")
    out.append(s"  $loConst = arith.constant $lo : i64\n")
    val outR = fresh("rng")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: i64) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI  = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val sumR  = fresh("sum")
    out.append(s"      $sumR = arith.addi $loConst, $idxI : i64\n")
    out.append(s"      linalg.yield $sumR : i64\n")
    out.append("    }\n")
    MlirVal(outR, ty)
  /** Literal `zeros(n)` / `ones(n)`. Always integer-typed per the
    * elaborator (matches the interpreter; the spec's real-typed
    * signature is a v0 divergence shared across backends). Emits a
    * `linalg.fill` over a fresh empty tensor.
    */
  protected def emitConstFill(name: String, n: Int): MlirVal =
    val ty    = MTensor(TyInteger, List(n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val v     = if name == "zeros" then 0 else 1
    val cR    = fresh("c")
    out.append(s"  $cR = arith.constant $v : i64\n")
    val outR  = fresh(name)
    out.append(s"  $outR = linalg.fill ins($cR : i64) outs($initR : ${ty.text}) -> ${ty.text}\n")
    MlirVal(outR, ty)
  /** Literal `linspace(lo, hi, n)`: real array of length n with values
    * `lo + i * (hi - lo) / (n - 1)`. We compute the step at codegen
    * time and emit a `linalg.map` that yields `lo + step * i`. When
    * `n == 1`, every element collapses to `lo` (`step` is a 0/0 NaN
    * otherwise); special-case to avoid emitting NaN in the IR.
    */
  protected def emitLinspaceCall(lo: Double, hi: Double, n: Int): MlirVal =
    val ty    = MTensor(TyReal, List(n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val loC   = fresh("lo")
    out.append(s"  $loC = arith.constant ${formatReal(lo)} : f64\n")
    val step  = if n <= 1 then 0.0 else (hi - lo) / (n - 1)
    val stepC = fresh("step")
    out.append(s"  $stepC = arith.constant ${formatReal(step)} : f64\n")
    val outR  = fresh("ls")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: f64) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI  = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val idxF  = fresh("idxf")
    out.append(s"      $idxF = arith.sitofp $idxI : i64 to f64\n")
    val mulR  = fresh("mul")
    out.append(s"      $mulR = arith.mulf $stepC, $idxF : f64\n")
    val addR  = fresh("add")
    out.append(s"      $addR = arith.addf $loC, $mulR : f64\n")
    out.append(s"      linalg.yield $addR : f64\n")
    out.append("    }\n")
    MlirVal(outR, ty)
  /** Pull a `Double` out of an `TIntLit` or `TRealLit`. Used by the
    * `linspace` dispatch where the elaborator may leave `0` (int) or
    * `0.0` (real) untouched on the lo/hi arguments — both meanings
    * are valid sources.
    */
  protected def realLitValue(e: TExpr): Double = e match
    case TIntLit(v, _, _)  => v.toDouble
    case TRealLit(v, _, _) => v
    case other             => notYet(s"non-literal linspace bound: ${other.getClass.getSimpleName}")
  /** True when the given Nex type is a rank-1 integer array — the
    * surface form a value-position range expression takes. Used to
    * gate the `..` / `..=` value-position handler so scalar binops
    * (which can't reach `..` anyway, but as a defensive guard) still
    * fall through to the generic dispatch.
    */
  protected def isRangeArrayType(tpe: Type): Boolean = tpe match
    case TyArray(TyInteger, 1) => true
    case _                     => false
  /** Lower `range(lo, hi)` / `lo..hi` / `lo..=hi` to a rank-1 integer
    * tensor. Literal bounds get the static-shape `tensor<NxI64>` path;
    * any other operand shape falls through to the dynamic path that
    * computes the length at runtime and emits `tensor<?xI64>`.
    */
  protected def emitRangeDispatch(loE: TExpr, hiE: TExpr, inclusive: Boolean): MlirVal =
    (loE, hiE) match
      case (TIntLit(lo, _, _), TIntLit(hi, _, _)) =>
        val end = if inclusive then hi + 1 else hi
        emitRangeCall(lo, end)
      case _ =>
        val loV   = emitExpr(loE)
        val hiRaw = emitExpr(hiE)
        val hiV   =
          if inclusive then
            val one = fresh("one")
            out.append(s"  $one = arith.constant 1 : i64\n")
            val bumped = fresh("hi1")
            out.append(s"  $bumped = arith.addi ${hiRaw.reg}, $one : i64\n")
            MlirVal(bumped, MScalar(TyInteger))
          else hiRaw
        emitRangeDynamic(loV, hiV)
  /** Lower `zeros(n)` / `ones(n)` to a rank-1 integer tensor. Literal
    * length keeps the static `tensor.empty()` + `linalg.fill` shape;
    * runtime length emits `tensor<?xI64>` with a dynamic size operand.
    */
  protected def emitConstFillDispatch(name: String, nE: TExpr): MlirVal = nE match
    case TIntLit(n, _, _) => emitConstFill(name, n.toInt)
    case _                => emitConstFillDynamic(name, emitExpr(nE))
  /** Lower `linspace(lo, hi, n)` to a rank-1 real tensor. Literal `n`
    * folds the step constant at codegen time; runtime `n` keeps the
    * same shape with a runtime-computed step and a select that
    * collapses to `lo` when `n <= 1` (matches the literal special-case).
    */
  protected def emitLinspaceDispatch(loE: TExpr, hiE: TExpr, nE: TExpr): MlirVal =
    nE match
      case TIntLit(n, _, _) =>
        val lo = realLitValue(loE)
        val hi = realLitValue(hiE)
        emitLinspaceCall(lo, hi, n.toInt)
      case _ =>
        val loV = emitExpr(loE)
        val hiV = emitExpr(hiE)
        val nV  = emitExpr(nE)
        emitLinspaceDynamic(loV, hiV, nV)
  /** Dynamic `range(lo, hi)` (half-open) producing `tensor<?xi64>`.
    * Length is `max(0, hi - lo)` computed at runtime; `tensor.empty`
    * takes the size as an index operand. The body of the `linalg.map`
    * adds the `linalg.index 0` to the captured `lo` so the result is
    * the iota sequence offset by `lo`.
    */
  protected def emitRangeDynamic(loV: MlirVal, hiV: MlirVal): MlirVal =
    val ty       = MTensor(TyInteger, List(-1))
    val rawR     = fresh("rdiff")
    out.append(s"  $rawR = arith.subi ${hiV.reg}, ${loV.reg} : i64\n")
    val zeroR    = fresh("z64")
    out.append(s"  $zeroR = arith.constant 0 : i64\n")
    val sizeI    = fresh("size")
    out.append(s"  $sizeI = arith.maxsi $rawR, $zeroR : i64\n")
    val sizeIdx  = fresh("sidx")
    out.append(s"  $sizeIdx = arith.index_cast $sizeI : i64 to index\n")
    val initR    = emitTensorEmpty(ty, List(sizeIdx))
    val outR     = fresh("rng")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: i64) {\n")
    val idxR     = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI     = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val sumR     = fresh("sum")
    out.append(s"      $sumR = arith.addi ${loV.reg}, $idxI : i64\n")
    out.append(s"      linalg.yield $sumR : i64\n")
    out.append("    }\n")
    MlirVal(outR, ty)
  /** Dynamic `zeros(n)` / `ones(n)` producing `tensor<?xi64>`.
    */
  protected def emitConstFillDynamic(name: String, nV: MlirVal): MlirVal =
    val ty      = MTensor(TyInteger, List(-1))
    val sizeIdx = fresh("sidx")
    out.append(s"  $sizeIdx = arith.index_cast ${nV.reg} : i64 to index\n")
    val initR   = emitTensorEmpty(ty, List(sizeIdx))
    val fillV   = if name == "zeros" then 0 else 1
    val cR      = fresh("c")
    out.append(s"  $cR = arith.constant $fillV : i64\n")
    val outR    = fresh(name)
    out.append(s"  $outR = linalg.fill ins($cR : i64) outs($initR : ${ty.text}) -> ${ty.text}\n")
    MlirVal(outR, ty)
  /** Dynamic `linspace(lo, hi, n)` producing `tensor<?xf64>`. The
    * step is `(hi - lo) / (n - 1)` when `n > 1`, else 0.0 — chosen
    * via `arith.select` to keep the `arith.divf` out of the
    * divide-by-zero path's user-visible result (NaN/Inf still gets
    * computed but is immediately discarded). Both `lo` and `hi` are
    * promoted from i64 to f64 if needed before entering the body.
    */
  protected def emitLinspaceDynamic(loV: MlirVal, hiV: MlirVal, nV: MlirVal): MlirVal =
    val ty      = MTensor(TyReal, List(-1))
    val loF     = if loV.ty == MScalar(TyInteger) then promoteIntToReal(loV) else loV
    val hiF     = if hiV.ty == MScalar(TyInteger) then promoteIntToReal(hiV) else hiV
    val sizeIdx = fresh("sidx")
    out.append(s"  $sizeIdx = arith.index_cast ${nV.reg} : i64 to index\n")
    val initR   = emitTensorEmpty(ty, List(sizeIdx))
    val one64   = fresh("one64")
    out.append(s"  $one64 = arith.constant 1 : i64\n")
    val gt1     = fresh("gt1")
    out.append(s"  $gt1 = arith.cmpi sgt, ${nV.reg}, $one64 : i64\n")
    val nm1     = fresh("nm1")
    out.append(s"  $nm1 = arith.subi ${nV.reg}, $one64 : i64\n")
    val nm1f    = fresh("nm1f")
    out.append(s"  $nm1f = arith.sitofp $nm1 : i64 to f64\n")
    val diff    = fresh("diff")
    out.append(s"  $diff = arith.subf ${hiF.reg}, ${loF.reg} : f64\n")
    val rawStep = fresh("rstep")
    out.append(s"  $rawStep = arith.divf $diff, $nm1f : f64\n")
    val zeroF   = fresh("zerof")
    out.append(s"  $zeroF = arith.constant 0.000000e+00 : f64\n")
    val stepR   = fresh("step")
    out.append(s"  $stepR = arith.select $gt1, $rawStep, $zeroF : f64\n")
    val outR    = fresh("ls")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: f64) {\n")
    val idxR    = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI    = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val idxF    = fresh("idxf")
    out.append(s"      $idxF = arith.sitofp $idxI : i64 to f64\n")
    val mulR    = fresh("mul")
    out.append(s"      $mulR = arith.mulf $stepR, $idxF : f64\n")
    val addR    = fresh("add")
    out.append(s"      $addR = arith.addf ${loF.reg}, $mulR : f64\n")
    out.append(s"      linalg.yield $addR : f64\n")
    out.append("    }\n")
    MlirVal(outR, ty)
  /** Predicate string for `arith.cmpi` / `arith.cmpf`. Integers use
    * signed predicates; reals use ordered (`oeq`, `olt`, …) for every
    * op except `!=`, where `une` makes `nan != nan` true — matching
    * IEEE 754 and the spec §10.2.
    */
  protected def comparisonPredicate(op: String, t: Type): String = (op, t) match
    case ("==", TyInteger) => "eq"
    case ("!=", TyInteger) => "ne"
    case ("<",  TyInteger) => "slt"
    case ("<=", TyInteger) => "sle"
    case (">",  TyInteger) => "sgt"
    case (">=", TyInteger) => "sge"
    case ("==", TyReal)    => "oeq"
    case ("!=", TyReal)    => "une"
    case ("<",  TyReal)    => "olt"
    case ("<=", TyReal)    => "ole"
    case (">",  TyReal)    => "ogt"
    case (">=", TyReal)    => "oge"
    case _                 => notYet(s"comparison predicate $op on $t")
  /** Emit a `sitofp` promotion inside a linalg.map / linalg.reduce
    * region body (indented at the region depth) and return the new
    * SSA name. Returns `srcName` unchanged when no promotion is
    * needed.
    */
  protected def promoteInRegion(srcName: String, fromT: Type, toT: Type): String =
    if fromT == toT then srcName
    else (fromT, toT) match
      case (TyInteger, TyReal) =>
        val r = fresh("pr")
        out.append(s"      $r = arith.sitofp $srcName : i64 to f64\n")
        r
      case _ => notYet(s"in-region promote $fromT to $toT")
  /** Reduce a rank-1 tensor with the `min` or `max` prelude. Mirrors
    * `emitSumReduce` but seeds the accumulator with `arr[0]` rather
    * than a zero sentinel — matches `NexInterpreter`'s
    * `b.tail.foldLeft(b.head)` and dodges the need for an
    * IEEE-defined +∞ / -∞ constant in MLIR text.
    *
    * Reducer ops:
    *   - int  → `arith.minsi` / `arith.maxsi`
    *   - real → `arith.minimumf` / `arith.maximumf`
    *     (IEEE 754-2019 spelling, NaN-propagating like the
    *      `arith.cmpf` predicates we use elsewhere)
    *
    * Empty-array case is the caller's problem — the elaborator either
    * rejects empty literals or accepts them with a trap at runtime;
    * `tensor.extract` on an empty rank-1 tensor is UB by MLIR rules,
    * which matches NexInterpreter's `trap("min: empty array", ...)`.
    */
  protected def emitMinMaxReduce(name: String, srcReg: String, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val outTy  = MTensor(elemT, Nil)
    val zeroIdx = fresh("z_idx")
    out.append(s"  $zeroIdx = arith.constant 0 : index\n")
    val initER = fresh("init_e")
    out.append(s"  $initER = tensor.extract $srcReg[$zeroIdx] : ${ty.text}\n")
    val initR = fresh("init")
    out.append(s"  $initR = tensor.from_elements $initER : ${outTy.text}\n")
    val opName = (name, elemT) match
      case ("min", TyInteger) => "arith.minsi"
      case ("max", TyInteger) => "arith.maxsi"
      case ("min", TyReal)    => "arith.minimumf"
      case ("max", TyReal)    => "arith.maximumf"
      case _                  => notYet(s"$name reducer on $elemT")
    val dims  = ty.shape.indices.mkString(", ")
    val redTR = fresh(s"${name}_t")
    out.append(
      s"  $redTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = $opName %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val redR = fresh(name)
    out.append(s"  $redR = tensor.extract $redTR[] : ${outTy.text}\n")
    MlirVal(redR, MScalar(elemT))
