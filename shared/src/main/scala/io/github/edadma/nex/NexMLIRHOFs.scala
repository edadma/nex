package io.github.edadma.nex

/** Higher-order-function surface for [[NexMLIRCodegen]] — prelude
  * `map` / `filter` / `flatMap` / `reduce` with inline-lambda
  * lowering, plus the fused-loop emitter and the
  * `staticLength` / `staticTensorOf` helpers the fusion path needs.
  *
  * Self-typed on `NexMLIRCodegen` to share state and to call back
  * into the visitor ([[emitExpr]]) for lambda bodies plus the
  * tensor-shape helpers in [[NexMLIRArrays]].
  */
trait NexMLIRHOFs:
  self: NexMLIRCodegen =>

  /** `map(arr, lambda)` with an inline single-param lambda. The body
    * inlines into a `linalg.map` region: the input operand becomes
    * the lambda's parameter (registered in env), the body is emitted
    * via `emitExpr`, and the result is yielded. Output tensor type
    * derives from the elaborator's result element type — supports
    * `[int] map → [real]` since the body can promote internally.
    * Works for any rank: `linalg.map` walks the shape regardless,
    * and `srcTy.text`/`outTy.text` encode the full shape.
    */
  protected def emitMapInlineLambda(av: MlirVal, srcTy: MTensor, lam: TLambda, outElem: Type): MlirVal =
    val inElem        = srcTy.elem
    val inS           = scalarText(inElem)
    val outS          = scalarText(outElem)
    val (initR, outTy) = emitTensorEmptyLike(av.reg, srcTy, outElem)
    val outR   = fresh("hofmap")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${srcTy.text}) outs($initR : ${outTy.text})\n",
    )
    val paramName = fresh("p")
    out.append(s"    ($paramName: $inS, %_o: $outS) {\n")
    val paramSym  = lam.params.head
    val prev      = env.get(paramSym.id)
    env(paramSym.id) = MlirVal(paramName, MScalar(inElem))
    val bv = emitExpr(lam.body)
    out.append(s"      linalg.yield ${bv.reg} : $outS\n")
    out.append("    }\n")
    prev match
      case Some(v) => env(paramSym.id) = v
      case None    => env.remove(paramSym.id)
    MlirVal(outR, outTy)

  /** `filter(arr, x -> pred)` with an inline one-param predicate. Output
    * length depends on how many elements satisfy the predicate, so the
    * result has dynamic shape `tensor<?xT>` and is built in two passes:
    *
    *   1. Count matches into an i64 carried as an `scf.for` iter_arg.
    *   2. Allocate `tensor.empty(%count)` and walk again, inserting each
    *      matched element at the next write position via `tensor.insert`.
    *      Both the output tensor and the write cursor are iter_args.
    *
    * The predicate body is emitted twice — once per pass — using the
    * same lambda parameter symbol bound to a fresh per-pass element
    * extract. Predicates are pure (Nex value-level expressions), so
    * re-emission is semantically safe.
    */
  protected def emitFilterInlineLambda(av: MlirVal, srcTy: MTensor, lam: TLambda): MlirVal =
    val elemT = srcTy.elem
    val elemS = scalarText(elemT)
    val outTy = MTensor(elemT, List(-1))

    val c0Idx = fresh("c0")
    out.append(s"  $c0Idx = arith.constant 0 : index\n")
    val c1Idx = fresh("c1")
    out.append(s"  $c1Idx = arith.constant 1 : index\n")
    val c0I64 = fresh("c0i")
    out.append(s"  $c0I64 = arith.constant 0 : i64\n")
    val c1I64 = fresh("c1i")
    out.append(s"  $c1I64 = arith.constant 1 : i64\n")

    val lenIdx =
      if srcTy.shape.head >= 0 then
        val r = fresh("flen")
        out.append(s"  $r = arith.constant ${srcTy.shape.head} : index\n")
        r
      else
        val axisR = fresh("axis")
        out.append(s"  $axisR = arith.constant 0 : index\n")
        val r = fresh("flen")
        out.append(s"  $r = tensor.dim ${av.reg}, $axisR : ${srcTy.text}\n")
        r

    val paramSym = lam.params.head
    val prev     = env.get(paramSym.id)

    val ivPass1   = fresh("fi1")
    val accName   = fresh("acc")
    val countOut  = fresh("count")
    out.append(
      s"  $countOut = scf.for $ivPass1 = $c0Idx to $lenIdx step $c1Idx iter_args($accName = $c0I64) -> (i64) {\n",
    )
    val elt1 = fresh("elt")
    out.append(s"    $elt1 = tensor.extract ${av.reg}[$ivPass1] : ${srcTy.text}\n")
    env(paramSym.id) = MlirVal(elt1, MScalar(elemT))
    val pred1 = emitExpr(lam.body)
    val nextAcc = fresh("nacc")
    out.append(s"    $nextAcc = scf.if ${pred1.reg} -> (i64) {\n")
    val plus1 = fresh("plus1")
    out.append(s"      $plus1 = arith.addi $accName, $c1I64 : i64\n")
    out.append(s"      scf.yield $plus1 : i64\n")
    out.append("    } else {\n")
    out.append(s"      scf.yield $accName : i64\n")
    out.append("    }\n")
    out.append(s"    scf.yield $nextAcc : i64\n")
    out.append("  }\n")

    val countIdx = fresh("countidx")
    out.append(s"  $countIdx = arith.index_cast $countOut : i64 to index\n")
    val outInit  = emitTensorEmpty(outTy, List(countIdx))

    val ivPass2  = fresh("fi2")
    val outIter  = fresh("oit")
    val wposIter = fresh("wp")
    val resPair  = fresh("res")
    out.append(
      s"  $resPair:2 = scf.for $ivPass2 = $c0Idx to $lenIdx step $c1Idx " +
        s"iter_args($outIter = $outInit, $wposIter = $c0Idx) -> (${outTy.text}, index) {\n",
    )
    val elt2 = fresh("elt")
    out.append(s"    $elt2 = tensor.extract ${av.reg}[$ivPass2] : ${srcTy.text}\n")
    env(paramSym.id) = MlirVal(elt2, MScalar(elemT))
    val pred2 = emitExpr(lam.body)
    val nrPair = fresh("nr")
    out.append(s"    $nrPair:2 = scf.if ${pred2.reg} -> (${outTy.text}, index) {\n")
    val inserted = fresh("ins")
    out.append(s"      $inserted = tensor.insert $elt2 into $outIter[$wposIter] : ${outTy.text}\n")
    val nwpos = fresh("nwp")
    out.append(s"      $nwpos = arith.addi $wposIter, $c1Idx : index\n")
    out.append(s"      scf.yield $inserted, $nwpos : ${outTy.text}, index\n")
    out.append("    } else {\n")
    out.append(s"      scf.yield $outIter, $wposIter : ${outTy.text}, index\n")
    out.append("    }\n")
    out.append(s"    scf.yield $nrPair#0, $nrPair#1 : ${outTy.text}, index\n")
    out.append("  }\n")

    prev match
      case Some(v) => env(paramSym.id) = v
      case None    => env.remove(paramSym.id)

    MlirVal(s"$resPair#0", outTy)

  /** `flatMap(arr, x -> [...])` on rank-1. The lambda returns a
    * rank-1 array each call; the result concatenates them. Output
    * length is the sum of inner lengths, so the codegen mirrors
    * [[emitFilterInlineLambda]] with an extra dimension of nesting:
    *
    *   1. Pass 1 walks the input, emits the lambda body once per
    *      iteration, reads the inner tensor's first dim via
    *      `tensor.dim`, and accumulates the i64 sum.
    *   2. Pass 2 allocates `tensor<?xU>` of that size and walks
    *      again. For each iteration it emits the lambda body a
    *      second time, then runs a nested `scf.for` over the inner
    *      tensor that copies element-by-element into the output at
    *      `wpos + j`. The output tensor flows through the nested
    *      loop as an iter_arg; `wpos` advances by the inner length
    *      after each outer iteration.
    *
    * Two re-emissions of the lambda body (vs. one for filter) is
    * accepted: lambda bodies are pure Nex value-level expressions.
    */
  protected def emitFlatMapInlineLambda(
      av:      MlirVal,
      srcTy:   MTensor,
      lam:     TLambda,
      outElem: Type,
  ): MlirVal =
    val srcElemT = srcTy.elem
    val outTy    = MTensor(outElem, List(-1))

    val c0Idx = fresh("c0")
    out.append(s"  $c0Idx = arith.constant 0 : index\n")
    val c1Idx = fresh("c1")
    out.append(s"  $c1Idx = arith.constant 1 : index\n")
    val c0I64 = fresh("c0i")
    out.append(s"  $c0I64 = arith.constant 0 : i64\n")

    val lenIdx =
      if srcTy.shape.head >= 0 then
        val r = fresh("flen")
        out.append(s"  $r = arith.constant ${srcTy.shape.head} : index\n")
        r
      else
        val axisR = fresh("axis")
        out.append(s"  $axisR = arith.constant 0 : index\n")
        val r = fresh("flen")
        out.append(s"  $r = tensor.dim ${av.reg}, $axisR : ${srcTy.text}\n")
        r

    val paramSym = lam.params.head
    val prev     = env.get(paramSym.id)

    val ivPass1  = fresh("fi1")
    val accName  = fresh("acc")
    val totalOut = fresh("total")
    out.append(
      s"  $totalOut = scf.for $ivPass1 = $c0Idx to $lenIdx step $c1Idx iter_args($accName = $c0I64) -> (i64) {\n",
    )
    val elt1 = fresh("elt")
    out.append(s"    $elt1 = tensor.extract ${av.reg}[$ivPass1] : ${srcTy.text}\n")
    env(paramSym.id) = MlirVal(elt1, MScalar(srcElemT))
    val inner1 = emitExpr(lam.body)
    val innerTy1 = inner1.ty match
      case t @ MTensor(_, List(_)) => t
      case other =>
        notYet(s"flatMap lambda body did not produce rank-1 tensor: $other")
    val innerLenIdx1 = fresh("ilen")
    if innerTy1.shape.head >= 0 then
      out.append(s"    $innerLenIdx1 = arith.constant ${innerTy1.shape.head} : index\n")
    else
      val iaxis = fresh("iaxis")
      out.append(s"    $iaxis = arith.constant 0 : index\n")
      out.append(s"    $innerLenIdx1 = tensor.dim ${inner1.reg}, $iaxis : ${innerTy1.text}\n")
    val innerLenI64 = fresh("ileni")
    out.append(s"    $innerLenI64 = arith.index_castui $innerLenIdx1 : index to i64\n")
    val newAcc = fresh("nacc")
    out.append(s"    $newAcc = arith.addi $accName, $innerLenI64 : i64\n")
    out.append(s"    scf.yield $newAcc : i64\n")
    out.append("  }\n")

    val totalIdx = fresh("totalidx")
    out.append(s"  $totalIdx = arith.index_cast $totalOut : i64 to index\n")
    val outInit  = emitTensorEmpty(outTy, List(totalIdx))

    val ivPass2  = fresh("fi2")
    val outIter  = fresh("oit")
    val wposIter = fresh("wp")
    val resPair  = fresh("res")
    out.append(
      s"  $resPair:2 = scf.for $ivPass2 = $c0Idx to $lenIdx step $c1Idx " +
        s"iter_args($outIter = $outInit, $wposIter = $c0Idx) -> (${outTy.text}, index) {\n",
    )
    val elt2 = fresh("elt")
    out.append(s"    $elt2 = tensor.extract ${av.reg}[$ivPass2] : ${srcTy.text}\n")
    env(paramSym.id) = MlirVal(elt2, MScalar(srcElemT))
    val inner2 = emitExpr(lam.body)
    val innerTy2 = inner2.ty.asInstanceOf[MTensor]
    val innerLenIdx2 = fresh("ilen2")
    if innerTy2.shape.head >= 0 then
      out.append(s"    $innerLenIdx2 = arith.constant ${innerTy2.shape.head} : index\n")
    else
      val iaxis = fresh("iaxis2")
      out.append(s"    $iaxis = arith.constant 0 : index\n")
      out.append(s"    $innerLenIdx2 = tensor.dim ${inner2.reg}, $iaxis : ${innerTy2.text}\n")

    val ivInner  = fresh("ij")
    val outInner = fresh("oin")
    val outAfter = fresh("oaft")
    out.append(
      s"    $outAfter = scf.for $ivInner = $c0Idx to $innerLenIdx2 step $c1Idx " +
        s"iter_args($outInner = $outIter) -> (${outTy.text}) {\n",
    )
    val v = fresh("v")
    out.append(s"      $v = tensor.extract ${inner2.reg}[$ivInner] : ${innerTy2.text}\n")
    val destIdx = fresh("dst")
    out.append(s"      $destIdx = arith.addi $wposIter, $ivInner : index\n")
    val ins = fresh("ins")
    out.append(s"      $ins = tensor.insert $v into $outInner[$destIdx] : ${outTy.text}\n")
    out.append(s"      scf.yield $ins : ${outTy.text}\n")
    out.append("    }\n")

    val newWpos = fresh("nwp")
    out.append(s"    $newWpos = arith.addi $wposIter, $innerLenIdx2 : index\n")
    out.append(s"    scf.yield $outAfter, $newWpos : ${outTy.text}, index\n")
    out.append("  }\n")

    prev match
      case Some(v) => env(paramSym.id) = v
      case None    => env.remove(paramSym.id)

    MlirVal(s"$resPair#0", outTy)

  /** `reduce(arr, init, lambda)` with an inline two-param lambda.
    * Nex spec §10.4: the lambda is `(acc, x) -> body`. Lowers to
    * `linalg.reduce` over every axis of the input, with `init` seeded
    * into a 0-d output via `tensor.from_elements`. The lambda's `acc`
    * binds to the reduce body's accumulator and `x` to the input
    * element; the body emits and yields. All-dims reduce means the
    * lambda fires once per element regardless of rank — same flat
    * left-to-right traversal the interpreter and LLVM backend use.
    */
  protected def emitReduceInlineLambda(av: MlirVal, srcTy: MTensor, iv: MlirVal, lam: TLambda): MlirVal =
    val accSym  = lam.params(0)
    val elemSym = lam.params(1)
    val accTy   = iv.ty match
      case s: MScalar => s
      case other      => notYet(s"reduce with non-scalar init $other")
    val outTy   = MTensor(accTy.elem, Nil)
    val initR   = fresh("init")
    out.append(s"  $initR = tensor.from_elements ${iv.reg} : ${outTy.text}\n")
    val outTR   = fresh("hofred_t")
    val inS     = scalarText(srcTy.elem)
    val accS    = scalarText(accTy.elem)
    val dims    = srcTy.shape.indices.mkString(", ")
    out.append(
      s"  $outTR = linalg.reduce ins(${av.reg} : ${srcTy.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    val inName  = fresh("p")
    val accName = fresh("a")
    out.append(s"    ($inName: $inS, $accName: $accS) {\n")
    val prevAcc  = env.get(accSym.id)
    val prevElem = env.get(elemSym.id)
    env(accSym.id)  = MlirVal(accName, accTy)
    env(elemSym.id) = MlirVal(inName, MScalar(srcTy.elem))
    val bv = emitExpr(lam.body)
    out.append(s"      linalg.yield ${bv.reg} : $accS\n")
    out.append("    }\n")
    prevAcc match { case Some(v) => env(accSym.id) = v; case None => env.remove(accSym.id) }
    prevElem match { case Some(v) => env(elemSym.id) = v; case None => env.remove(elemSym.id) }
    val outR = fresh("hofred")
    out.append(s"  $outR = tensor.extract $outTR[] : ${outTy.text}\n")
    MlirVal(outR, accTy)

  /** Resolve an Nex length expression to a compile-time integer when
    * possible. Recognises:
    *   - `TIntLit(n)` directly.
    *   - `length(arr)` where `arr` evaluates (via the env or a fresh
    *     emit, free of side effects) to a tensor with known static
    *     shape — the outer dim is the length.
    *   - `a * b` where both sides are static integers (used by the
    *     fusion pass for `rows * cols` on rank-2 sources).
    * Returns `None` otherwise so callers can fall back to `notYet`.
    */
  protected def staticLength(e: TExpr): Option[Int] = e match
    case TIntLit(n, _, _) => Some(n.toInt)
    case TCall(TVarRef(s, _, _), List(arr), _, _) if s.kind == SymKind.Prelude && s.name == "length" =>
      staticTensorOf(arr).map(_.shape.head)
    case TBinOp("*", l, r, _, _) =>
      for li <- staticLength(l); ri <- staticLength(r) yield li * ri
    case _ => None

  /** Find the static `MTensor` type of an expression that's already
    * sitting in `env` (a `TVarRef` to a let-bound or var-bound array).
    * Used by [[staticLength]] to read the source shape without emitting
    * any side-effecting ops.
    */
  protected def staticTensorOf(e: TExpr): Option[MTensor] = e match
    case TVarRef(s, _, _) =>
      env.get(s.id).map(_.ty).collect { case t: MTensor => t }
    case _ => None

  /** Rank-1 fused loop. The fusion pass already inlined the lambda body
    * with the param substituted to a `TFlatIndex(src, loopVar)` shape;
    * here we emit a `linalg.map` over a fresh `tensor<NxT_out>`, bind
    * the loop var symbol to the iteration index (cast to i64), and
    * `emitExpr(body)` inside the region. The body's `TFlatIndex` will
    * tensor.extract from the source tensor as needed.
    */
  protected def emitFusedLoop1D(loopVar: Symbol, n: Int, elemT: Type, body: TExpr): MlirVal =
    val outTy = MTensor(elemT, List(n))
    val s     = scalarText(elemT)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("fl")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val ivR   = fresh("ivi")
    out.append(s"      $ivR = arith.index_castui $idxR : index to i64\n")
    val prev  = env.get(loopVar.id)
    env(loopVar.id) = MlirVal(ivR, MScalar(TyInteger))
    val bv    = emitExpr(body)
    out.append(s"      linalg.yield ${bv.reg} : $s\n")
    out.append("    }\n")
    prev match
      case Some(v) => env(loopVar.id) = v
      case None    => env.remove(loopVar.id)
    MlirVal(outR, outTy)
