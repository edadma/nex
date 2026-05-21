package io.github.edadma.nex

import scala.collection.mutable
import scala.util.parsing.input.Position

/** Spec §8.2 / §8.3 / §4.11 — uniqueness for `var` arrays, auto-clone
  * insertion, and unique closure capture. Runs after Stage 3 lowering as
  * the elaborator's final pass.
  *
  * V0 strategy: a conservative whole-function-body reference count. For
  * each top-level binding's value and each function body:
  *
  *   1. Count every [[TVarRef]] referencing a `var` array binding.
  *   2. At every **move site** whose source is a bare [[TVarRef]] to a
  *      var-array binding with `refCount > 1`, wrap the source in
  *      [[TClone]]. (`> 1` means there's at least one OTHER reference
  *      somewhere in the body — the move could be from a still-live
  *      binding, so a fresh buffer is needed.)
  *
  * Move sites:
  *
  *   - **Call arg in a `mut` position.** `mut_fn(a)` — callee mutates,
  *     so it consumes `a`. The mode is read from `callee.tpe`'s
  *     [[TyFunc]] params.
  *   - **Var-decl RHS that is a bare var-array reference.** `var b = a`
  *     — moves `a` into `b`. Block-level via [[TBlockBinding]], top-level
  *     via [[TTopBinding]].
  *   - **Assign value that is a bare var-array reference.** `b = a` where
  *     `b` is mutable. Same logic as var-decl: the original `a` is
  *     consumed.
  *   - **Return value.** `return a` consumes `a`. Subsequent code is
  *     dead but our reference count makes no distinction (over-clones
  *     for safety).
  *
  * Spec §8.3 rule 3 ("clone the *earlier* use so the *later* use can be
  * a move") is a later optimisation; v0 always clones the move itself,
  * which is correct but sometimes redundant.
  *
  * §4.11 — Unique closure capture is NOT yet enforced (see the handoff
  * memo). The scaffolding here will grow that check as a follow-up.
  */
class NexLifetime(
    mutableSymIds: Int => Boolean,
    symbolType:    Int => Type,
    symbolName:    Int => String        = id => s"#$id",
    err:           (String, Option[Position]) => Unit = (_, _) => (),
):

  /** Set of var-array Symbol ids that have already been captured by some
    * earlier-walked [[TLambda]]. A second capture of the same id surfaces
    * the §4.11 uniqueness error. Reset at the start of each `rewrite`.
    */
  private val capturedBy = mutable.Map.empty[Int, Option[Position]]

  /** True iff `symId` names a `var` binding whose current (post-Stage-2)
    * type is a rank-1 or rank-2 array. Only those are unique-owned per
    * spec §8.2.
    */
  private def isVarArray(symId: Int): Boolean =
    mutableSymIds(symId) && (symbolType(symId) match
      case TyArray(_, _) => true
      case _             => false
    )

  /** Entry point — transforms an entire elaborated program in place,
    * returning a new [[TProgram]] with move sites wrapped in [[TClone]]
    * where appropriate.
    */
  def rewrite(p: TProgram): TProgram =
    capturedBy.clear()
    p.copy(
      decls    = p.decls.map(rewriteDecl),
      auxDecls = p.auxDecls.map(rewriteDecl),
    )

  private def rewriteDecl(d: TDecl): TDecl = d match
    case f: TFunDecl    => f.copy(body = transformContainer(f.body))
    case b: TTopBinding => b.copy(value = transformContainer(b.value))
    case other          => other

  /** Run the reference-count + rewrite pair on a single function-body or
    * top-binding-value subtree. Nested [[TLambda]] bodies recurse on
    * their own because closure captures count as moves in the OUTER
    * scope, and conversely the lambda's body itself is a fresh function-
    * body scope with its own reference counts.
    */
  private def transformContainer(body: TExpr): TExpr =
    val refs = collectRefs(body)
    rewriteSubtree(body, refs)

  // --------------------------------------------------------------------------
  // Phase 1: reference count for every var-array binding
  // --------------------------------------------------------------------------

  private def collectRefs(e: TExpr): mutable.Map[Int, Int] =
    val m = mutable.Map.empty[Int, Int].withDefaultValue(0)
    countRefs(e, m)
    m

  private def countRefs(e: TExpr, m: mutable.Map[Int, Int]): Unit =
    e match
      case TVarRef(s, _, _) if isVarArray(s.id) => m(s.id) += 1
      case _                                    => ()
    walkChildren(e, x => countRefs(x, m))

  // --------------------------------------------------------------------------
  // Phase 2: rewrite move sites
  // --------------------------------------------------------------------------

  private def rewriteSubtree(e: TExpr, refs: mutable.Map[Int, Int]): TExpr =
    e match
      // -- move sites ----------------------------------------------------
      case TCall(callee, args, p, t) =>
        val modes: List[ParamMode] = callee.tpe match
          case TyFunc(ps, _) if ps.size == args.size => ps.map(_._2)
          case _                                      => List.fill(args.size)(ParamMode.Read)
        val newArgs = args.zip(modes).map { case (a, m) =>
          val ra = rewriteSubtree(a, refs)
          m match
            case ParamMode.Mut  => cloneIfNeeded(ra, refs)
            case ParamMode.Read => ra
        }
        TCall(rewriteSubtree(callee, refs), newArgs, p, t)

      case TReturn(Some(v), p, t) =>
        TReturn(Some(cloneIfNeeded(rewriteSubtree(v, refs), refs)), p, t)

      case TAssign(target, value, p, t) =>
        val t2 = rewriteSubtree(target, refs)
        val v2 = rewriteSubtree(value, refs)
        // Source is consumed iff target is a mutable l-value (var binding
        // or mut parameter). The call-site check (validateModes) already
        // ensures non-mutable targets fail earlier, so this is a safe
        // narrowing.
        val v3 = t2 match
          case TVarRef(ts, _, _) if mutableSymIds(ts.id) => cloneIfNeeded(v2, refs)
          case _                                          => v2
        TAssign(t2, v3, p, t)

      case TBlock(items, r, p, t) =>
        val newItems = items.map {
          case TBlockBinding(s, BindingKind.Var, v) =>
            TBlockBinding(s, BindingKind.Var, cloneIfNeeded(rewriteSubtree(v, refs), refs))
          case TBlockBinding(s, k, v) =>
            TBlockBinding(s, k, rewriteSubtree(v, refs))
          case TBlockExpr(x) => TBlockExpr(rewriteSubtree(x, refs))
        }
        TBlock(newItems, rewriteSubtree(r, refs), p, t)

      // -- structural recursion -----------------------------------------
      case TBinOp(op, l, r, p, t)         => TBinOp(op, rewriteSubtree(l, refs), rewriteSubtree(r, refs), p, t)
      case TUnaryOp(op, x, p, t)          => TUnaryOp(op, rewriteSubtree(x, refs), p, t)
      case TJuxtapose(c, b, p, t)         => TJuxtapose(rewriteSubtree(c, refs), rewriteSubtree(b, refs), p, t)
      case TElementWise(op, l, r, p, t)   => TElementWise(op, rewriteSubtree(l, refs), rewriteSubtree(r, refs), p, t)
      case TBroadcast(s, a, op, sf, p, t) => TBroadcast(rewriteSubtree(s, refs), rewriteSubtree(a, refs), op, sf, p, t)
      case TMap(a, f, p, t)               => TMap(rewriteSubtree(a, refs), rewriteSubtree(f, refs), p, t)
      case TReduce(a, i, f, p, t)         => TReduce(rewriteSubtree(a, refs), rewriteSubtree(i, refs), rewriteSubtree(f, refs), p, t)
      case TMatMul(l, r, p, t)            => TMatMul(rewriteSubtree(l, refs), rewriteSubtree(r, refs), p, t)
      case TFusedLoop(lv, len, b, cols, p, t) =>
        TFusedLoop(lv, rewriteSubtree(len, refs), rewriteSubtree(b, refs), cols.map(rewriteSubtree(_, refs)), p, t)
      case TFlatIndex(a, i, p, t)         => TFlatIndex(rewriteSubtree(a, refs), rewriteSubtree(i, refs), p, t)
      case TClone(a, p, t)                => TClone(rewriteSubtree(a, refs), p, t)
      case TIndex(a, idx, p, t)           => TIndex(rewriteSubtree(a, refs), idx.map(rewriteSubtree(_, refs)), p, t)
      case TSlice(a, lo, hi, inc, st, p, t)   =>
        TSlice(rewriteSubtree(a, refs), lo.map(rewriteSubtree(_, refs)), hi.map(rewriteSubtree(_, refs)), inc, st.map(rewriteSubtree(_, refs)), p, t)
      case TSlice2(a, rAx, cAx, p, t)     =>
        def rwAx(ax: TAxisSpec): TAxisSpec = ax match
          case TAxisAll              => TAxisAll
          case TAxisIndex(e)         => TAxisIndex(rewriteSubtree(e, refs))
          case TAxisRange(lo, hi, i, st) => TAxisRange(lo.map(rewriteSubtree(_, refs)), hi.map(rewriteSubtree(_, refs)), i, st.map(rewriteSubtree(_, refs)))
        TSlice2(rewriteSubtree(a, refs), rwAx(rAx), rwAx(cAx), p, t)
      case TField(r, n, p, t)             => TField(rewriteSubtree(r, refs), n, p, t)
      case TTupleProj(r, idx, p, t)       => TTupleProj(rewriteSubtree(r, refs), idx, p, t)
      case TMethodCall(r, n, args, p, t)  => TMethodCall(rewriteSubtree(r, refs), n, args.map(rewriteSubtree(_, refs)), p, t)
      case lam @ TLambda(ps, body, p, t) =>
        // §4.11: a var-array binding may be captured by at most one
        // closure. Find this lambda's free var-array refs (those bound
        // OUTSIDE the lambda) and record them; on second capture of the
        // same id, surface an error pointing at the first capture site.
        val captures = findLambdaCaptures(lam)
        captures.foreach { (id, refPos) =>
          capturedBy.get(id) match
            case Some(prev) =>
              val where = prev.map(pp => s" at line ${pp.line}").getOrElse("")
              err(
                s"var-array `${symbolName(id)}` already captured by another closure$where (§4.11 forbids multiple closure capture of the same var binding)",
                refPos,
              )
            case None =>
              capturedBy(id) = p
        }
        // Lambda body is a separate function scope — re-run the analysis
        // inside (its own var bindings and refs are independent of the
        // enclosing function's). Captures from outside still hit the
        // outer refs map because the analysis there counts every TVarRef
        // anywhere in the outer body, including inside this lambda.
        TLambda(ps, transformContainer(body), p, t)
      case TTuple(es, p, t)               => TTuple(es.map(rewriteSubtree(_, refs)), p, t)
      case TArrayLit(es, p, t)            => TArrayLit(es.map(rewriteSubtree(_, refs)), p, t)
      case TIf(c, th, el, p, t)           => TIf(rewriteSubtree(c, refs), rewriteSubtree(th, refs), el.map(rewriteSubtree(_, refs)), p, t)
      case TFor(vs, it, b, p, t)          => TFor(vs, rewriteSubtree(it, refs), rewriteSubtree(b, refs), p, t)
      case TWhile(c, b, p, t)             => TWhile(rewriteSubtree(c, refs), rewriteSubtree(b, refs), p, t)
      case TMatch(s, cs, p, t)            =>
        TMatch(rewriteSubtree(s, refs), cs.map(c => TMatchCase(c.pat, rewriteSubtree(c.body, refs))), p, t)
      case TReturn(None, p, t)            => TReturn(None, p, t)
      case TInterpStringLit(parts, p, t)  =>
        TInterpStringLit(parts.map {
          case TInterpExpr(x) => TInterpExpr(rewriteSubtree(x, refs))
          case other          => other
        }, p, t)
      // Pure leaves
      case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit
         | _: TUnitLit | _: TVarRef | _: TAxisAllMark | _: TOpenSliceMark | _: TIntrinsic => e

  /** If `e` is a bare [[TVarRef]] to a var-array binding that has any
    * other reference in the body (count > 1), wrap it in [[TClone]] so
    * the move site gets a fresh buffer. Otherwise return `e` unchanged.
    */
  private def cloneIfNeeded(e: TExpr, refs: mutable.Map[Int, Int]): TExpr =
    e match
      case v @ TVarRef(s, _, _) if isVarArray(s.id) && refs.getOrElse(s.id, 0) > 1 =>
        TClone(v, v.pos, v.tpe)
      case _ => e

  // --------------------------------------------------------------------------
  // Closure-capture analysis (§4.11)
  // --------------------------------------------------------------------------

  /** Find the var-array Symbol ids that `lambda` captures — references
    * inside its body to bindings declared OUTSIDE its scope. Returns a
    * map from captured id to a representative source position (the
    * first TVarRef site, for error reporting).
    *
    * Scope tracking adds the lambda's params plus any inner block-level
    * vals/vars and `for` loop variables to the `bound` set as we
    * descend. Nested lambdas don't pollute the outer set — their params
    * are scoped to the nested lambda.
    */
  private def findLambdaCaptures(lambda: TLambda): Map[Int, Option[Position]] =
    val bound = mutable.Set.empty[Int]
    lambda.params.foreach(p => bound += p.id)
    val frees = mutable.Map.empty[Int, Option[Position]]
    collectLambdaFrees(lambda.body, bound, frees)
    frees.toMap

  private def collectLambdaFrees(
      e: TExpr,
      bound: mutable.Set[Int],
      frees: mutable.Map[Int, Option[Position]],
  ): Unit = e match
    case TVarRef(s, p, _) if isVarArray(s.id) && !bound.contains(s.id) =>
      // Record only the FIRST free-ref position; subsequent reads
      // resolve to the same binding and the first position is enough
      // for the error report.
      if !frees.contains(s.id) then frees(s.id) = p
    case TLambda(ps, body, _, _) =>
      val sub = mutable.Set.empty[Int] ++= bound
      ps.foreach(p => sub += p.id)
      collectLambdaFrees(body, sub, frees)
    case TBlock(items, r, _, _) =>
      val sub = mutable.Set.empty[Int] ++= bound
      items.foreach {
        case TBlockBinding(s, _, v) =>
          collectLambdaFrees(v, sub, frees)
          sub += s.id
        case TBlockExpr(x) => collectLambdaFrees(x, sub, frees)
      }
      collectLambdaFrees(r, sub, frees)
    case TFor(vs, it, body, _, _) =>
      collectLambdaFrees(it, bound, frees)
      val sub = mutable.Set.empty[Int] ++= bound
      vs.foreach(s => sub += s.id)
      collectLambdaFrees(body, sub, frees)
    case TMatch(s, cases, _, _) =>
      collectLambdaFrees(s, bound, frees)
      cases.foreach { c =>
        val sub = mutable.Set.empty[Int] ++= bound
        collectPatternBindings(c.pat).foreach(id => sub += id)
        collectLambdaFrees(c.body, sub, frees)
      }
    case _ =>
      // No new bindings introduced — just recurse.
      walkChildren(e, x => collectLambdaFrees(x, bound, frees))

  private def collectPatternBindings(p: TPattern): List[Int] = p match
    case TVarPat(s, _)         => List(s.id)
    case TVariantPat(_, sub, _) => sub.flatMap(collectPatternBindings)
    case _                     => Nil

  // --------------------------------------------------------------------------
  // Shared child-walker (used by both reference counting and any future
  // analyses that don't need per-node rewriting).
  // --------------------------------------------------------------------------

  private def walkChildren(e: TExpr, f: TExpr => Unit): Unit =
    e match
      case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit
         | _: TUnitLit | _: TVarRef | _: TAxisAllMark | _: TOpenSliceMark | _: TIntrinsic => ()
      case TBinOp(_, l, r, _, _)            => f(l); f(r)
      case TUnaryOp(_, x, _, _)             => f(x)
      case TJuxtapose(c, b, _, _)           => f(c); f(b)
      case TElementWise(_, l, r, _, _)      => f(l); f(r)
      case TBroadcast(s, a, _, _, _, _)     => f(s); f(a)
      case TMap(a, fn, _, _)                => f(a); f(fn)
      case TReduce(a, i, fn, _, _)          => f(a); f(i); f(fn)
      case TMatMul(l, r, _, _)              => f(l); f(r)
      case TFusedLoop(_, len, b, cols, _, _) =>
        f(len); f(b); cols.foreach(f)
      case TFlatIndex(a, i, _, _)           => f(a); f(i)
      case TClone(a, _, _)                  => f(a)
      case TCall(c, args, _, _)             => f(c); args.foreach(f)
      case TIndex(a, idx, _, _)             => f(a); idx.foreach(f)
      case TSlice(a, lo, hi, _, st, _, _)   =>
        f(a); lo.foreach(f); hi.foreach(f); st.foreach(f)
      case TSlice2(a, rAx, cAx, _, _)       =>
        f(a)
        def go(ax: TAxisSpec): Unit = ax match
          case TAxisAll                  => ()
          case TAxisIndex(e)             => f(e)
          case TAxisRange(lo, hi, _, st) => lo.foreach(f); hi.foreach(f); st.foreach(f)
        go(rAx); go(cAx)
      case TField(r, _, _, _)               => f(r)
      case TTupleProj(r, _, _, _)           => f(r)
      case TMethodCall(r, _, args, _, _)    => f(r); args.foreach(f)
      case TLambda(_, body, _, _)           => f(body)
      case TTuple(es, _, _)                 => es.foreach(f)
      case TArrayLit(es, _, _)              => es.foreach(f)
      case TIf(c, th, el, _, _)             => f(c); f(th); el.foreach(f)
      case TFor(_, it, b, _, _)             => f(it); f(b)
      case TWhile(c, b, _, _)               => f(c); f(b)
      case TMatch(s, cases, _, _)           => f(s); cases.foreach(c => f(c.body))
      case TReturn(v, _, _)                 => v.foreach(f)
      case TAssign(t, v, _, _)              => f(t); f(v)
      case TBlock(items, r, _, _)           =>
        items.foreach {
          case TBlockBinding(_, _, v) => f(v)
          case TBlockExpr(x)          => f(x)
        }
        f(r)
      case TInterpStringLit(parts, _, _)    =>
        parts.foreach {
          case TInterpExpr(x) => f(x)
          case _              => ()
        }
