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

  /** Rule 1a: rank-1 `lhs op rhs` (both arrays) → fused loop.
    *
    *   TBlock(
    *     [val $l_tmp = lhs; val $r_tmp = rhs],
    *     TFusedLoop(idx, length($l_tmp),
    *                TBinOp(op, $l_tmp[idx], $r_tmp[idx])))
    *
    * Returns `None` if the node isn't a rank-1 element-wise (rank-2, bool
    * comparison, or non-numeric element types are left alone for now).
    */
  private def fuseElementWise(
    op: String,
    l: TExpr,
    r: TExpr,
    pos: Option[Position],
    tpe: Type,
  ): Option[TExpr] =
    elemTypeIfRank1(tpe).map { elemT =>
      val lSym = symbols.mint("$fused_l", l.tpe, SymKind.Local)
      val rSym = symbols.mint("$fused_r", r.tpe, SymKind.Local)
      val iSym = symbols.mint("$fused_i", TyInteger,  SymKind.Local)
      val lRef = TVarRef(lSym, pos, l.tpe)
      val rRef = TVarRef(rSym, pos, r.tpe)
      val iRef = TVarRef(iSym, pos, TyInteger)
      val body = TBinOp(
        op,
        TIndex(lRef, List(iRef), pos, elemT),
        TIndex(rRef, List(iRef), pos, elemT),
        pos,
        elemT,
      )
      TBlock(
        items = List(
          TBlockBinding(lSym, BindingKind.Val, l),
          TBlockBinding(rSym, BindingKind.Val, r),
        ),
        result = TFusedLoop(iSym, lengthOf(lRef, pos), body, pos, tpe),
        pos = pos,
        tpe = tpe,
      )
    }

  /** Rule 1b: rank-1 broadcast (`scalar op arr` or `arr op scalar`) → fused
    * loop. `scalarFirst` is preserved so the body builds the operands in
    * the right order.
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
      val sSym = symbols.mint("$fused_s", scalar.tpe, SymKind.Local)
      val aSym = symbols.mint("$fused_a", arr.tpe,    SymKind.Local)
      val iSym = symbols.mint("$fused_i", TyInteger,    SymKind.Local)
      val sRef = TVarRef(sSym, pos, scalar.tpe)
      val aRef = TVarRef(aSym, pos, arr.tpe)
      val iRef = TVarRef(iSym, pos, TyInteger)
      val elemRef = TIndex(aRef, List(iRef), pos, elemT)
      val body =
        if scalarFirst then TBinOp(op, sRef,    elemRef, pos, elemT)
        else                TBinOp(op, elemRef, sRef,    pos, elemT)
      TBlock(
        items = List(
          TBlockBinding(sSym, BindingKind.Val, scalar),
          TBlockBinding(aSym, BindingKind.Val, arr),
        ),
        result = TFusedLoop(iSym, lengthOf(aRef, pos), body, pos, tpe),
        pos = pos,
        tpe = tpe,
      )
    }

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
