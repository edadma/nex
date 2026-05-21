package io.github.edadma.nex

import scala.collection.mutable
import scala.util.parsing.input.Position

/** Stage 4: array-fusion rewrite (opt-in).
  *
  * Rewrites rank-1 [[TElementWise]] and [[TBroadcast]] nodes into
  * [[TFusedLoop]] form. The point of the pass is to give the AOT backend
  * a single uniform array-producer node that can be emitted as one loop
  * instead of one loop per arithmetic operator. The interpreter has eval
  * cases for both shapes so running this pass has no semantic effect on
  * interpreter output. The chain-inlining rewrite then collapses nested
  * fused loops into one.
  *
  * Not auto-wired into [[NexElaborator.elaborate]] — call it explicitly
  * via `NexFusion(program).fuseProgram(program)`. That keeps existing
  * Stage 2 / Stage 3 tests (which pin TElementWise / TBroadcast shapes)
  * green while letting fusion-aware tests opt in.
  *
  * @param symbols the program's symbol table; new loop-index and
  *                temp-binding symbols are minted here.
  */
class NexFusion(symbols: SymbolTable):

  /** Look up the prelude `length` function once — used to build the
    * `length(temp)` expression that feeds each TFusedLoop's `length` slot.
    * Throws if no prelude is registered (shouldn't happen with a
    * normally-elaborated program).
    */
  private lazy val preludeLength: Symbol =
    symbols.all
      .find(s => s.kind == SymKind.Prelude && s.name == "length")
      .getOrElse(sys.error("NexFusion: prelude `length` not in symbol table"))

  private lazy val preludeRows: Symbol =
    symbols.all
      .find(s => s.kind == SymKind.Prelude && s.name == "rows")
      .getOrElse(sys.error("NexFusion: prelude `rows` not in symbol table"))

  private lazy val preludeCols: Symbol =
    symbols.all
      .find(s => s.kind == SymKind.Prelude && s.name == "cols")
      .getOrElse(sys.error("NexFusion: prelude `cols` not in symbol table"))

  /** Symbol-id → fused TLambda for `val f = x -> body` bindings discovered
    * during the walk. Populated as we process top-level [[TTopBinding]]s in
    * order, and as we walk block items inside function bodies. Consumed by
    * [[fuseMap]] when the lambda argument to `map(...)` is a [[TVarRef]] —
    * named-lambda chasing. Sequential population means forward references
    * at the top level are NOT chased (the binding has to lexically precede
    * the call site within the program); the unfused TCall is still emitted
    * in that case — fusion is an optimisation, not a soundness requirement.
    */
  private val lambdaBindings = mutable.Map.empty[Int, TLambda]

  def fuseProgram(p: TProgram): TProgram =
    p.copy(decls = p.decls.map(fuseDecl))

  private def fuseDecl(d: TDecl): TDecl = d match
    case f: TFunDecl    => f.copy(body = fuseExpr(f.body))
    case b: TTopBinding =>
      val fv = fuseExpr(b.value)
      fv match
        case lam: TLambda => lambdaBindings(b.sym.id) = lam
        case _            =>
      b.copy(value = fv)
    case other          => other

  /** Walk the typed AST, recursively fusing children first (bottom-up),
    * then applying the rewrite rule at the current node if it matches.
    * Bottom-up so the chain-inlining rule can spot nested TFusedLoops.
    */
  private def fuseExpr(e: TExpr): TExpr = e match
    case TElementWise(op, l, r, p, t) =>
      val ll = fuseExpr(l); val rr = fuseExpr(r)
      fuseElementWise(op, ll, rr, p, t).getOrElse(TElementWise(op, ll, rr, p, t))

    case TBroadcast(s, a, op, sf, p, t) =>
      val ss = fuseExpr(s); val aa = fuseExpr(a)
      fuseBroadcast(ss, aa, op, sf, p, t).getOrElse(TBroadcast(ss, aa, op, sf, p, t))

    // Prelude `map(arr, lambda)` with an inline TLambda → fuse to a
    // single loop with the lambda body inlined. Only fires when the
    // lambda is an inline TLambda — `map(arr, f)` where `f` is a
    // TVarRef is left alone (the value behind the binding isn't
    // visible here without a deferred-resolve map).
    case TCall(callee @ TVarRef(s, _, _), args, p, t)
        if s.kind == SymKind.Prelude && s.name == "map" && args.size == 2 =>
      val arr = fuseExpr(args.head)
      val fn  = fuseExpr(args(1))
      fuseMap(arr, fn, p, t).getOrElse(TCall(callee, List(arr, fn), p, t))

    // -- recurse ----------------------------------------------------------
    case TBinOp(op, l, r, p, t)        => TBinOp(op, fuseExpr(l), fuseExpr(r), p, t)
    case TUnaryOp(op, x, p, t)         => TUnaryOp(op, fuseExpr(x), p, t)
    case TJuxtapose(c, b, p, t)        => TJuxtapose(fuseExpr(c), fuseExpr(b), p, t)
    case TCall(c, args, p, t)          => TCall(fuseExpr(c), args.map(fuseExpr), p, t)
    case TIndex(a, i, p, t)            => TIndex(fuseExpr(a), i.map(fuseExpr), p, t)
    case TField(r, n, p, t)            => TField(fuseExpr(r), n, p, t)
    case TTupleProj(r, idx, p, t)      => TTupleProj(fuseExpr(r), idx, p, t)
    case TMethodCall(r, n, args, p, t) => TMethodCall(fuseExpr(r), n, args.map(fuseExpr), p, t)
    case TLambda(params, body, p, t)   => TLambda(params, fuseExpr(body), p, t)
    case TTuple(es, p, t)              => TTuple(es.map(fuseExpr), p, t)
    case TArrayLit(es, p, t)           => TArrayLit(es.map(fuseExpr), p, t)
    case TIf(c, th, el, p, t)          => TIf(fuseExpr(c), fuseExpr(th), el.map(fuseExpr), p, t)
    case TFor(vs, it, b, p, t)         => TFor(vs, fuseExpr(it), fuseExpr(b), p, t)
    case TWhile(c, b, p, t)            => TWhile(fuseExpr(c), fuseExpr(b), p, t)
    case TReturn(v, p, t)              => TReturn(v.map(fuseExpr), p, t)
    case TAssign(tgt, v, p, t)         => TAssign(fuseExpr(tgt), fuseExpr(v), p, t)
    case TBlock(items, r, p, t)        =>
      val its = items.map {
        case TBlockBinding(s, k, v) =>
          val fv = fuseExpr(v)
          fv match
            case lam: TLambda => lambdaBindings(s.id) = lam
            case _            =>
          TBlockBinding(s, k, fv)
        case TBlockExpr(x) => TBlockExpr(fuseExpr(x))
      }
      TBlock(its, fuseExpr(r), p, t)
    case TMap(a, f, p, t)              => TMap(fuseExpr(a), fuseExpr(f), p, t)
    case TReduce(a, i, f, p, t)        => TReduce(fuseExpr(a), fuseExpr(i), fuseExpr(f), p, t)
    case TMatMul(l, r, p, t)           => TMatMul(fuseExpr(l), fuseExpr(r), p, t)
    case TMatch(s, cs, p, t)           =>
      TMatch(fuseExpr(s), cs.map(c => TMatchCase(c.pat, fuseExpr(c.body))), p, t)
    case TFusedLoop(lv, len, b, cols, p, t) => TFusedLoop(lv, fuseExpr(len), fuseExpr(b), cols.map(fuseExpr), p, t)
    case TFlatIndex(a, i, p, t)        => TFlatIndex(fuseExpr(a), fuseExpr(i), p, t)
    case TClone(a, p, t)               => TClone(fuseExpr(a), p, t)
    case TSlice(a, lo, hi, inc, st, p, t)  => TSlice(fuseExpr(a), lo.map(fuseExpr), hi.map(fuseExpr), inc, st.map(fuseExpr), p, t)
    case TSlice2(a, rAx, cAx, p, t)    =>
      def fuseAxis(s: TAxisSpec): TAxisSpec = s match
        case TAxisAll              => TAxisAll
        case TAxisIndex(e)         => TAxisIndex(fuseExpr(e))
        case TAxisRange(lo, hi, i, st) => TAxisRange(lo.map(fuseExpr), hi.map(fuseExpr), i, st.map(fuseExpr))
      TSlice2(fuseExpr(a), fuseAxis(rAx), fuseAxis(cAx), p, t)
    case _: TAxisAllMark =>
      sys.error("internal: TAxisAllMark survived to fusion pass; should be Stage-2-only")
    case _: TOpenSliceMark =>
      sys.error("internal: TOpenSliceMark survived to fusion pass; should be Stage-2-only")
    case TInterpStringLit(parts, p, t) =>
      val ps = parts.map {
        case TInterpExpr(x) => TInterpExpr(fuseExpr(x))
        case other          => other
      }
      TInterpStringLit(ps, p, t)
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit
       | _: TUnitLit | _: TVarRef | _: TIntrinsic => e

  /** Rule 1a: `lhs op rhs` (both arrays, rank-1 OR rank-2) → fused loop.
    * For rank-2 the loop is FLAT — single index 0..rows*cols-1 — with the
    * result wrapped in a [[TFusedLoop]] carrying `cols = Some(c)` so the
    * interpreter materialises a [[VArray2]] of the right shape. Chain
    * inlining: each operand that's already a fused subexpression (a
    * `TBlock` wrapping a `TFusedLoop`) has its bindings hoisted to the
    * outer block and its body inlined in place of `arr[i]` — collapsing
    * nested fused loops into one regardless of rank.
    */
  private def fuseElementWise(
    op: String,
    l: TExpr,
    r: TExpr,
    pos: Option[Position],
    tpe: Type,
  ): Option[TExpr] =
    arrayInfo(tpe).map { case (elemT, rank) =>
      val iSym = symbols.mint("$fused_i", TyInteger, SymKind.Local)
      val iRef = TVarRef(iSym, pos, TyInteger)
      val (lBindings, lElem, lLen, lCols) = sourceOperand(l, iSym, iRef, pos)
      val (rBindings, rElem, _, _)        = sourceOperand(r, iSym, iRef, pos)
      val body                            = TBinOp(op, lElem, rElem, pos, elemT)
      val cols                            = if rank == 2 then lCols else None
      TBlock(
        items  = lBindings ++ rBindings,
        result = TFusedLoop(iSym, lLen, body, cols, pos, tpe),
        pos    = pos,
        tpe    = tpe,
      )
    }

  /** Rule 1b: broadcast (`scalar op arr` or `arr op scalar`, rank-1 OR
    * rank-2) → fused loop. `scalarFirst` is preserved so the body builds
    * the operands in the right order. Chain inlining applies to `arr`
    * (the scalar is always evaluated once via a temp). For rank-2 the
    * loop iterates flat and the result wraps in a rank-2 array via cols.
    */
  private def fuseBroadcast(
    scalar: TExpr,
    arr: TExpr,
    op: String,
    scalarFirst: Boolean,
    pos: Option[Position],
    tpe: Type,
  ): Option[TExpr] =
    arrayInfo(tpe).map { case (elemT, rank) =>
      val iSym = symbols.mint("$fused_i", TyInteger, SymKind.Local)
      val iRef = TVarRef(iSym, pos, TyInteger)
      val sSym = symbols.mint("$fused_s", scalar.tpe, SymKind.Local)
      val sRef = TVarRef(sSym, pos, scalar.tpe)
      val (aBindings, aElem, aLen, aCols) = sourceOperand(arr, iSym, iRef, pos)
      val body =
        if scalarFirst then TBinOp(op, sRef, aElem, pos, elemT)
        else                TBinOp(op, aElem, sRef, pos, elemT)
      val cols = if rank == 2 then aCols else None
      TBlock(
        items  = TBlockBinding(sSym, BindingKind.Val, scalar) :: aBindings,
        result = TFusedLoop(iSym, aLen, body, cols, pos, tpe),
        pos    = pos,
        tpe    = tpe,
      )
    }

  /** Rule 1c: `map(arr, fn)` → fused loop, with two ways to resolve `fn`:
    *
    *  - **Inline lambda:** `map(arr, x -> body)` — the lambda's body is
    *    inlined with its single param substituted to the indexed access on
    *    the array temp.
    *
    *  - **Named lambda:** `map(arr, f)` where `f` is a [[TVarRef]] whose
    *    binding (top-level [[TTopBinding]] or block-level [[TBlockBinding]])
    *    was registered in [[lambdaBindings]] earlier in the walk. The
    *    looked-up lambda is treated identically to an inline lambda from
    *    here on. The original `val f = ...` stays in the program — fusion
    *    duplicates the body at each call site, it does not consume the
    *    binding.
    *
    * If `arr` is itself a fused subexpression, chain inlining via
    * [[sourceOperand]] collapses both into a single loop, so e.g.
    * `map(2 * a + b, x -> x * 10)` collapses to one loop.
    *
    * Limited to single-param lambdas — `map` only takes `(elem -> U)`.
    * Returns `None` when `fn` is neither an inline TLambda nor a TVarRef
    * pointing at a registered lambda (e.g. a function-typed value passed
    * across module boundaries).
    */
  private def fuseMap(
    arr: TExpr,
    fn: TExpr,
    pos: Option[Position],
    tpe: Type,
  ): Option[TExpr] =
    val resolved: Option[TLambda] = fn match
      case lam: TLambda                                  => Some(lam)
      case TVarRef(sym, _, _)                            => lambdaBindings.get(sym.id)
      case _                                             => None

    (resolved, arrayInfo(tpe)) match
      case (Some(lam), Some((_, rank))) if lam.params.size == 1 =>
        val iSym = symbols.mint("$fused_i", TyInteger, SymKind.Local)
        val iRef = TVarRef(iSym, pos, TyInteger)
        val (aBindings, aElem, aLen, aCols) = sourceOperand(arr, iSym, iRef, pos)
        val paramId = lam.params.head.id
        val body    = subst(lam.body, paramId, aElem)
        val cols    = if rank == 2 then aCols else None
        Some(
          TBlock(
            items  = aBindings,
            result = TFusedLoop(iSym, aLen, body, cols, pos, tpe),
            pos    = pos,
            tpe    = tpe,
          )
        )
      case _ => None

  /** Process one source-array operand of a fused loop. Returns:
    *   - `bindings`: items to splice into the outer TBlock (either a
    *     single new temp binding, or the hoisted bindings from an inner
    *     fused subexpression).
    *   - `elemExpr`: the expression to use in the body in place of
    *     `arr[outerIdx]`. For a non-fused operand it's `TFlatIndex(tmp,
    *     outerIdx)`; for a fused operand it's the inner body with the
    *     inner loop variable substituted to `outerIdx`.
    *   - `lenExpr`: the loop's total flat length. Rank-1 → `length(tmp)`;
    *     rank-2 → `rows(tmp) * cols(tmp)`. Element-wise requires matching
    *     shapes; we use the first operand's length as canonical (the
    *     second is checked at runtime by the original semantics — future
    *     work for the fused path).
    *   - `colsExpr`: `Some(cols(tmp))` if the source is rank-2, `None`
    *     otherwise. Only used when the OUTER result is rank-2.
    */
  private def sourceOperand(
    e: TExpr,
    outerIdx: Symbol,
    outerIdxRef: TExpr,
    pos: Option[Position],
  ): (List[TBlockItem], TExpr, TExpr, Option[TExpr]) =
    asFusedSource(e) match
      case Some((innerBindings, innerIdx, innerLen, innerCols, innerBody)) =>
        val accessExpr = subst(innerBody, innerIdx.id, outerIdxRef)
        val lenExpr    = subst(innerLen,  innerIdx.id, outerIdxRef)
        val colsExpr   = innerCols.map(subst(_, innerIdx.id, outerIdxRef))
        (innerBindings, accessExpr, lenExpr, colsExpr)
      case None =>
        val tmp           = symbols.mint("$fused_a", e.tpe, SymKind.Local)
        val ref           = TVarRef(tmp, pos, e.tpe)
        val (elemT, rank) = arrayInfo(e.tpe).getOrElse((TyUnknown, 1))
        val access        = TFlatIndex(ref, outerIdxRef, pos, elemT)
        val len =
          if rank == 1 then lengthOf(ref, pos)
          else TBinOp("*", rowsOf(ref, pos), colsOf(ref, pos), pos, TyInteger)
        val cols = if rank == 2 then Some(colsOf(ref, pos)) else None
        (List(TBlockBinding(tmp, BindingKind.Val, e)), access, len, cols)

  /** Match the TBlock(bindings, TFusedLoop) shape produced by an earlier
    * fuseElementWise / fuseBroadcast / fuseMap call. Returns the bindings,
    * inner loop variable, inner length, inner cols (None for rank-1, Some
    * for rank-2), and inner body if so; otherwise None.
    */
  private def asFusedSource(e: TExpr): Option[(List[TBlockItem], Symbol, TExpr, Option[TExpr], TExpr)] = e match
    case TBlock(items, TFusedLoop(lv, len, body, cols, _, _), _, _) =>
      Some((items, lv, len, cols, body))
    case _ => None

  /** Capture-free substitution: replace every TVarRef whose Symbol id
    * matches `fromId` with `to`. Loop variables are uniquely minted by
    * this pass so capture is not a concern; this is a straightforward
    * structural walk over the typed AST.
    */
  private def subst(e: TExpr, fromId: Int, to: TExpr): TExpr = e match
    case TVarRef(s, _, _) if s.id == fromId => to
    case _: TVarRef                         => e
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit | _: TUnitLit | _: TIntrinsic => e
    case TBinOp(op, l, r, p, t)        => TBinOp(op, subst(l, fromId, to), subst(r, fromId, to), p, t)
    case TUnaryOp(op, x, p, t)         => TUnaryOp(op, subst(x, fromId, to), p, t)
    case TJuxtapose(c, b, p, t)        => TJuxtapose(subst(c, fromId, to), subst(b, fromId, to), p, t)
    case TCall(c, args, p, t)          => TCall(subst(c, fromId, to), args.map(subst(_, fromId, to)), p, t)
    case TIndex(a, i, p, t)            => TIndex(subst(a, fromId, to), i.map(subst(_, fromId, to)), p, t)
    case TField(r, n, p, t)            => TField(subst(r, fromId, to), n, p, t)
    case TTupleProj(r, idx, p, t)      => TTupleProj(subst(r, fromId, to), idx, p, t)
    case TMethodCall(r, n, args, p, t) => TMethodCall(subst(r, fromId, to), n, args.map(subst(_, fromId, to)), p, t)
    case TLambda(params, body, p, t)   => TLambda(params, subst(body, fromId, to), p, t)
    case TTuple(es, p, t)              => TTuple(es.map(subst(_, fromId, to)), p, t)
    case TArrayLit(es, p, t)           => TArrayLit(es.map(subst(_, fromId, to)), p, t)
    case TIf(c, th, el, p, t)          => TIf(subst(c, fromId, to), subst(th, fromId, to), el.map(subst(_, fromId, to)), p, t)
    case TFor(vs, it, b, p, t)         => TFor(vs, subst(it, fromId, to), subst(b, fromId, to), p, t)
    case TWhile(c, b, p, t)            => TWhile(subst(c, fromId, to), subst(b, fromId, to), p, t)
    case TReturn(v, p, t)              => TReturn(v.map(subst(_, fromId, to)), p, t)
    case TAssign(tgt, v, p, t)         => TAssign(subst(tgt, fromId, to), subst(v, fromId, to), p, t)
    case TBlock(items, r, p, t)        =>
      val its = items.map {
        case TBlockBinding(s, k, v) => TBlockBinding(s, k, subst(v, fromId, to))
        case TBlockExpr(x)          => TBlockExpr(subst(x, fromId, to))
      }
      TBlock(its, subst(r, fromId, to), p, t)
    case TElementWise(op, l, r, p, t)  => TElementWise(op, subst(l, fromId, to), subst(r, fromId, to), p, t)
    case TBroadcast(s, a, op, sf, p, t)=> TBroadcast(subst(s, fromId, to), subst(a, fromId, to), op, sf, p, t)
    case TMap(a, f, p, t)              => TMap(subst(a, fromId, to), subst(f, fromId, to), p, t)
    case TReduce(a, i, f, p, t)        => TReduce(subst(a, fromId, to), subst(i, fromId, to), subst(f, fromId, to), p, t)
    case TMatMul(l, r, p, t)           => TMatMul(subst(l, fromId, to), subst(r, fromId, to), p, t)
    case TMatch(s, cs, p, t)           =>
      TMatch(subst(s, fromId, to), cs.map(c => TMatchCase(c.pat, subst(c.body, fromId, to))), p, t)
    case TFusedLoop(lv, len, b, cols, p, t) =>
      // The inner loop's own loopVar shadows ours (uniquely minted, but
      // be defensive): don't substitute under a binder for the same id.
      if lv.id == fromId then e
      else TFusedLoop(lv, subst(len, fromId, to), subst(b, fromId, to), cols.map(subst(_, fromId, to)), p, t)
    case TFlatIndex(a, i, p, t)        => TFlatIndex(subst(a, fromId, to), subst(i, fromId, to), p, t)
    case TClone(a, p, t)               => TClone(subst(a, fromId, to), p, t)
    case TSlice(a, lo, hi, inc, st, p, t)  => TSlice(subst(a, fromId, to), lo.map(subst(_, fromId, to)), hi.map(subst(_, fromId, to)), inc, st.map(subst(_, fromId, to)), p, t)
    case TSlice2(a, rAx, cAx, p, t)    =>
      def substAxis(s: TAxisSpec): TAxisSpec = s match
        case TAxisAll              => TAxisAll
        case TAxisIndex(x)         => TAxisIndex(subst(x, fromId, to))
        case TAxisRange(lo, hi, i, st) => TAxisRange(lo.map(subst(_, fromId, to)), hi.map(subst(_, fromId, to)), i, st.map(subst(_, fromId, to)))
      TSlice2(subst(a, fromId, to), substAxis(rAx), substAxis(cAx), p, t)
    case _: TAxisAllMark => e
    case _: TOpenSliceMark => e
    case TInterpStringLit(parts, p, t) =>
      val ps = parts.map {
        case TInterpExpr(x) => TInterpExpr(subst(x, fromId, to))
        case other          => other
      }
      TInterpStringLit(ps, p, t)

  /** Build a `length(arr)` call against the prelude `length` symbol. */
  private def lengthOf(arr: TExpr, pos: Option[Position]): TExpr =
    TCall(
      TVarRef(preludeLength, pos, preludeLength.tpe),
      List(arr),
      pos,
      TyInteger,
    )

  /** Build a `rows(arr)` call against the prelude `rows` symbol. Used for
    * rank-2 sources to compute the total flat element count `rows * cols`. */
  private def rowsOf(arr: TExpr, pos: Option[Position]): TExpr =
    TCall(
      TVarRef(preludeRows, pos, preludeRows.tpe),
      List(arr),
      pos,
      TyInteger,
    )

  /** Build a `cols(arr)` call against the prelude `cols` symbol. Used for
    * rank-2 sources both for the flat element count and to thread the
    * column dimension into [[TFusedLoop.cols]]. */
  private def colsOf(arr: TExpr, pos: Option[Position]): TExpr =
    TCall(
      TVarRef(preludeCols, pos, preludeCols.tpe),
      List(arr),
      pos,
      TyInteger,
    )

  /** Returns `Some((elem, rank))` iff the type is a rank-1 OR rank-2 array
    * — otherwise None. Bool-result fusion (comparison broadcast) flows
    * through the same path; the body's TBinOp returns bool but the result
    * array still has a known element type.
    */
  private def arrayInfo(t: Type): Option[(Type, Int)] = t match
    case TyArray(elem, 1) => Some((elem, 1))
    case TyArray(elem, 2) => Some((elem, 2))
    case _                => None
