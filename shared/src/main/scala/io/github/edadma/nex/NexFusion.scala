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
  * interpreter output — chunk 1 is groundwork; chunk 2 will add the
  * chain-inlining rule that actually fuses nested loops.
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

  def fuseProgram(p: TProgram): TProgram =
    p.copy(decls = p.decls.map(fuseDecl))

  private def fuseDecl(d: TDecl): TDecl = d match
    case f: TFunDecl    => f.copy(body = fuseExpr(f.body))
    case b: TTopBinding => b.copy(value = fuseExpr(b.value))
    case other          => other

  /** Walk the typed AST, recursively fusing children first (bottom-up),
    * then applying the rewrite rule at the current node if it matches.
    * Bottom-up so chunk 2's chain-inlining rule can spot nested TFusedLoops.
    */
  private def fuseExpr(e: TExpr): TExpr = e match
    case TElementWise(op, l, r, p, t) =>
      val ll = fuseExpr(l); val rr = fuseExpr(r)
      fuseElementWise(op, ll, rr, p, t).getOrElse(TElementWise(op, ll, rr, p, t))

    case TBroadcast(s, a, op, sf, p, t) =>
      val ss = fuseExpr(s); val aa = fuseExpr(a)
      fuseBroadcast(ss, aa, op, sf, p, t).getOrElse(TBroadcast(ss, aa, op, sf, p, t))

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
        case TBlockBinding(s, k, v) => TBlockBinding(s, k, fuseExpr(v))
        case TBlockExpr(x)          => TBlockExpr(fuseExpr(x))
      }
      TBlock(its, fuseExpr(r), p, t)
    case TMap(a, f, p, t)              => TMap(fuseExpr(a), fuseExpr(f), p, t)
    case TReduce(a, i, f, p, t)        => TReduce(fuseExpr(a), fuseExpr(i), fuseExpr(f), p, t)
    case TMatMul(l, r, p, t)           => TMatMul(fuseExpr(l), fuseExpr(r), p, t)
    case TFusedLoop(lv, len, b, p, t)  => TFusedLoop(lv, fuseExpr(len), fuseExpr(b), p, t)
    case TInterpStringLit(parts, p, t) =>
      val ps = parts.map {
        case TInterpExpr(x) => TInterpExpr(fuseExpr(x))
        case other          => other
      }
      TInterpStringLit(ps, p, t)
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit
       | _: TUnitLit | _: TVarRef => e

  /** Rule 1a: rank-1 `lhs op rhs` (both arrays) → fused loop. With chain
    * inlining: each operand that's already a fused subexpression
    * (`TBlock` wrapping a `TFusedLoop`) has its bindings hoisted to the
    * outer block and its body inlined in place of `arr[i]` — collapsing
    * nested fused loops into one.
    */
  private def fuseElementWise(
    op: String,
    l: TExpr,
    r: TExpr,
    pos: Option[Position],
    tpe: Type,
  ): Option[TExpr] =
    elemTypeIfRank1(tpe).map { elemT =>
      val iSym = symbols.mint("$fused_i", TyInteger, SymKind.Local)
      val iRef = TVarRef(iSym, pos, TyInteger)
      val (lBindings, lElem, lLen) = sourceOperand(l, iSym, iRef, pos)
      val (rBindings, rElem, _)    = sourceOperand(r, iSym, iRef, pos)
      val body                     = TBinOp(op, lElem, rElem, pos, elemT)
      TBlock(
        items  = lBindings ++ rBindings,
        result = TFusedLoop(iSym, lLen, body, pos, tpe),
        pos    = pos,
        tpe    = tpe,
      )
    }

  /** Rule 1b: rank-1 broadcast (`scalar op arr` or `arr op scalar`) → fused
    * loop. `scalarFirst` is preserved so the body builds the operands in
    * the right order. Chain inlining applies to `arr` (the scalar is
    * always evaluated once via a temp).
    */
  private def fuseBroadcast(
    scalar: TExpr,
    arr: TExpr,
    op: String,
    scalarFirst: Boolean,
    pos: Option[Position],
    tpe: Type,
  ): Option[TExpr] =
    elemTypeIfRank1(tpe).map { elemT =>
      val iSym = symbols.mint("$fused_i", TyInteger, SymKind.Local)
      val iRef = TVarRef(iSym, pos, TyInteger)
      val sSym = symbols.mint("$fused_s", scalar.tpe, SymKind.Local)
      val sRef = TVarRef(sSym, pos, scalar.tpe)
      val (aBindings, aElem, aLen) = sourceOperand(arr, iSym, iRef, pos)
      val body =
        if scalarFirst then TBinOp(op, sRef, aElem, pos, elemT)
        else                TBinOp(op, aElem, sRef, pos, elemT)
      TBlock(
        items  = TBlockBinding(sSym, BindingKind.Val, scalar) :: aBindings,
        result = TFusedLoop(iSym, aLen, body, pos, tpe),
        pos    = pos,
        tpe    = tpe,
      )
    }

  /** Process one source-array operand of a fused loop. Returns:
    *   - `bindings`: items to splice into the outer TBlock (either a
    *     single new temp binding, or the hoisted bindings from an inner
    *     fused subexpression).
    *   - `elemExpr`: the expression to use in the body in place of
    *     `arr[outerIdx]`. For a non-fused operand it's `tmp[outerIdx]`;
    *     for a fused operand it's the inner body with the inner loop
    *     variable substituted to `outerIdx`.
    *   - `lenExpr`: the loop's length. Element-wise requires matching
    *     shapes; we use the first operand's length as the canonical one
    *     (the second is checked at runtime, future work).
    */
  private def sourceOperand(
    e: TExpr,
    outerIdx: Symbol,
    outerIdxRef: TExpr,
    pos: Option[Position],
  ): (List[TBlockItem], TExpr, TExpr) =
    asFusedSource(e) match
      case Some((innerBindings, innerIdx, innerLen, innerBody)) =>
        val accessExpr = subst(innerBody, innerIdx.id, outerIdxRef)
        val lenExpr    = subst(innerLen,  innerIdx.id, outerIdxRef)
        (innerBindings, accessExpr, lenExpr)
      case None =>
        val tmp    = symbols.mint("$fused_a", e.tpe, SymKind.Local)
        val ref    = TVarRef(tmp, pos, e.tpe)
        val elemT  = elemTypeIfRank1(e.tpe).getOrElse(TyUnknown)
        val access = TIndex(ref, List(outerIdxRef), pos, elemT)
        (List(TBlockBinding(tmp, BindingKind.Val, e)), access, lengthOf(ref, pos))

  /** Match the TBlock(bindings, TFusedLoop) shape produced by an earlier
    * fuseElementWise / fuseBroadcast call. Returns the bindings, inner
    * loop variable, inner length, and inner body if so; otherwise None.
    */
  private def asFusedSource(e: TExpr): Option[(List[TBlockItem], Symbol, TExpr, TExpr)] = e match
    case TBlock(items, TFusedLoop(lv, len, body, _, _), _, _) =>
      Some((items, lv, len, body))
    case _ => None

  /** Capture-free substitution: replace every TVarRef whose Symbol id
    * matches `fromId` with `to`. Loop variables are uniquely minted by
    * this pass so capture is not a concern; this is a straightforward
    * structural walk over the typed AST.
    */
  private def subst(e: TExpr, fromId: Int, to: TExpr): TExpr = e match
    case TVarRef(s, _, _) if s.id == fromId => to
    case _: TVarRef                         => e
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit | _: TUnitLit => e
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
    case TFusedLoop(lv, len, b, p, t)  =>
      // The inner loop's own loopVar shadows ours (uniquely minted, but
      // be defensive): don't substitute under a binder for the same id.
      if lv.id == fromId then e
      else TFusedLoop(lv, subst(len, fromId, to), subst(b, fromId, to), p, t)
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

  /** Returns `Some(elem)` iff the type is a rank-1 array — otherwise None.
    * Rank-2 fusion is chunk 3; bool-result fusion (comparison broadcast)
    * is also rank-1 but the body's TBinOp returns bool — handled by the
    * same path.
    */
  private def elemTypeIfRank1(t: Type): Option[Type] = t match
    case TyArray(elem, 1) => Some(elem)
    case _                => None
