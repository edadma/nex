package io.github.edadma.nex

import scala.util.parsing.input.Position

/** Stage 3 sugar lowering + Stage 3.5 mode validation for the Nex
  * elaborator. Runs after Stage 2 type inference; rewrites the typed AST
  * to drop sugar the interpreter and codegen would otherwise need to
  * handle, then walks the lowered tree to enforce §6.4 parameter mode
  * rules.
  *
  * Sugar rewritten here:
  *   - [[TJuxtapose]]   → [[TBinOp]] (`*`), or [[TBroadcast]] when one
  *     side is an array (§4.4 + §4.5 interaction)
  *   - [[TMethodCall]]  → [[TField]] (when `name` matches a struct field)
  *     or [[TCall]] (`name(receiver, args...)`) per §4.9
  *
  * Sugar items deferred to later stages:
  *   - Tuple destructuring (`val a, b = pair`) — needs new typed-AST
  *     projection node
  *   - Interpolated `${...}` raw text re-parsing
  */
protected trait NexElabLowering extends NexElabState:

  protected def lowerProgram(p: TProgram): TProgram =
    p.copy(decls = p.decls.map(lowerDecl))

  protected def lowerDecl(d: TDecl): TDecl = d match
    case f: TFunDecl =>
      val body2 = lowerExpr(f.body)
      validateModes(f, body2)
      f.copy(body = body2)
    case b: TTopBinding =>
      // If this binding's lambda was refined by a later call site (see
      // [[deferredLambdas]]), swap the refined version in *before*
      // lowering — the old value still has TyUnknown params. Also
      // refresh the binding's sym from the symbol table so downstream
      // readers (`b.sym.tpe`) see the post-refinement type rather than
      // the snapshot taken at binding time.
      val v = deferredLambdas.get(b.sym.id).getOrElse(b.value)
      b.copy(sym = refreshSym(b.sym), value = lowerExpr(v))
    case other          => other

  protected def lowerExpr(e: TExpr): TExpr = e match
    // -- juxtaposition lowering -------------------------------------------
    case TJuxtapose(c, b, p, t) =>
      val cc = lowerExpr(c); val bb = lowerExpr(b)
      (elemOf(cc.tpe), elemOf(bb.tpe)) match
        case (None, Some(_)) => TBroadcast(cc, bb, "*", scalarFirst = true, p, bb.tpe)
        case (Some(_), None) => TBroadcast(bb, cc, "*", scalarFirst = false, p, cc.tpe)
        case _               => TBinOp("*", cc, bb, p, t)

    // -- method-call dispatch (§4.9) --------------------------------------
    case mc @ TMethodCall(r, name, args, p, _) =>
      val rr     = lowerExpr(r)
      val argsLow = args.map(lowerExpr)
      lowerMethodCall(rr, name, argsLow, p, mc.tpe)

    // -- recurse ----------------------------------------------------------
    case TBinOp(op, l, r, p, t)        => TBinOp(op, lowerExpr(l), lowerExpr(r), p, t)
    case TUnaryOp(op, x, p, t)         => TUnaryOp(op, lowerExpr(x), p, t)
    case TCall(c, args, p, t)          => TCall(lowerExpr(c), args.map(lowerExpr), p, t)
    case TIndex(a, i, p, t)            => TIndex(lowerExpr(a), i.map(lowerExpr), p, t)
    case TSlice(a, lo, hi, inc, p, t)  => TSlice(lowerExpr(a), lowerExpr(lo), lowerExpr(hi), inc, p, t)
    case TSlice2(a, rAx, cAx, p, t)    =>
      def lowAxis(s: TAxisSpec): TAxisSpec = s match
        case TAxisAll              => TAxisAll
        case TAxisIndex(e)         => TAxisIndex(lowerExpr(e))
        case TAxisRange(lo, hi, i) => TAxisRange(lowerExpr(lo), lowerExpr(hi), i)
      TSlice2(lowerExpr(a), lowAxis(rAx), lowAxis(cAx), p, t)
    case _: TAxisAllMark =>
      sys.error("internal: TAxisAllMark survived Stage 2; should have been consumed by inferIndex")
    case TField(r, n, p, t)            => TField(lowerExpr(r), n, p, t)
    case TTupleProj(r, idx, p, t)      => TTupleProj(lowerExpr(r), idx, p, t)
    case TLambda(params, body, p, t)   => TLambda(params, lowerExpr(body), p, t)
    case TTuple(es, p, t)              => TTuple(es.map(lowerExpr), p, t)
    case TArrayLit(es, p, t)           => TArrayLit(es.map(lowerExpr), p, t)
    case TIf(c, th, el, p, t)          => TIf(lowerExpr(c), lowerExpr(th), el.map(lowerExpr), p, t)
    case TFor(vs, it, b, p, t)         => TFor(vs, lowerExpr(it), lowerExpr(b), p, t)
    case TWhile(c, b, p, t)            => TWhile(lowerExpr(c), lowerExpr(b), p, t)
    case TReturn(v, p, t)              => TReturn(v.map(lowerExpr), p, t)
    case TAssign(tgt, v, p, t)         => TAssign(lowerExpr(tgt), lowerExpr(v), p, t)
    case TBlock(items, r, p, t)        =>
      val its = items.map {
        case TBlockBinding(s, k, v) =>
          // Same deferred-lambda swap + sym refresh as the TTopBinding
          // case in lowerDecl — block-level `val f = lam` may have been
          // refined by a later call site.
          val v2 = deferredLambdas.get(s.id).getOrElse(v)
          TBlockBinding(refreshSym(s), k, lowerExpr(v2))
        case TBlockExpr(x) => TBlockExpr(lowerExpr(x))
      }
      TBlock(its, lowerExpr(r), p, t)
    case TElementWise(op, l, r, p, t)  => TElementWise(op, lowerExpr(l), lowerExpr(r), p, t)
    case TBroadcast(s, a, op, sf, p, t) => TBroadcast(lowerExpr(s), lowerExpr(a), op, sf, p, t)
    case TMap(a, f, p, t)              => TMap(lowerExpr(a), lowerExpr(f), p, t)
    case TReduce(a, i, f, p, t)        => TReduce(lowerExpr(a), lowerExpr(i), lowerExpr(f), p, t)
    case TMatMul(l, r, p, t)           => TMatMul(lowerExpr(l), lowerExpr(r), p, t)
    case TInterpStringLit(parts, p, t) =>
      // Recurse into `${...}` subtrees so juxt-lowering, method-call
      // dispatch, etc. happen there too. Same rationale as Stage 2.
      val lowered = parts.map {
        case TInterpExpr(x) => TInterpExpr(lowerExpr(x))
        case other          => other
      }
      TInterpStringLit(lowered, p, t)
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit
       | _: TUnitLit | _: TVarRef => e
    // TFusedLoop / TFlatIndex are post-lowering; if a re-lower pass ever
    // runs over a fused tree, recurse into their sub-expressions.
    case TFusedLoop(lv, len, body, cols, p, t) =>
      TFusedLoop(lv, lowerExpr(len), lowerExpr(body), cols.map(lowerExpr), p, t)
    case TFlatIndex(arr, idx, p, t) =>
      TFlatIndex(lowerExpr(arr), lowerExpr(idx), p, t)
    case TClone(arr, p, t) =>
      TClone(lowerExpr(arr), p, t)

  /** Per §4.9: `e.name(args)` is:
    *   1. field access if `e`'s type has a field `name` (and `args` is empty)
    *   2. function call `name(e, args)` if a function `name` is in scope
    *      whose first parameter type matches `e`'s type
    *   3. otherwise an error
    */
  protected def lowerMethodCall(
    r: TExpr,
    name: String,
    args: List[TExpr],
    p: Option[Position],
    originalTpe: Type,
  ): TExpr =
    // (1) field access if no args and the receiver has the field
    r.tpe match
      case TyStruct(_, fs) if args.isEmpty && fs.exists(_._1 == name) =>
        return TField(r, name, p, fs.find(_._1 == name).get._2)
      case TyComplex if args.isEmpty && (name == "re" || name == "im") =>
        return TField(r, name, p, TyReal)
      case _ => ()

    // (2) function call sugar — look up `name` in scope
    current.lookup(name) match
      case Some(sym) =>
        val calleeT = currentType(sym)
        val callee  = TVarRef(sym, p, calleeT)
        val tcall   = inferCall(callee, r :: args, p)
        // inferCall returns TyUnknown for prelude HOFs (TyUnknown sigs).
        // Stage 2 already computed the right result type on the TMethodCall
        // via inferPreludeHOFCall — fall back to that so downstream sees
        // the inferred type rather than losing it on the way through Stage 3.
        tcall match
          case c: TCall if c.tpe == TyUnknown && originalTpe != TyUnknown =>
            c.copy(tpe = originalTpe)
          case other => other
      case None =>
        err(s"no method or function `$name` on receiver of type ${r.tpe}", p)
        TCall(TVarRef(symbols.mint(name, TyUnknown, SymKind.Local), p), r :: args, p, TyUnknown)

  // ==========================================================================
  // Mode validation (§6.4)
  // ==========================================================================

  /** A read-mode parameter must not appear as a mutation target inside
    * the function body. v0 mode rules: by default every parameter is
    * read-mode unless declared `mut`. The check here is conservative:
    * any [[TAssign]] whose target ultimately resolves to a read param
    * symbol is an error.
    */
  protected def validateModes(f: TFunDecl, body: TExpr): Unit =
    val readParams: Set[Int] =
      f.params.iterator
        .filter(s => paramModes.get(s.id).contains(ParamMode.Read))
        .map(_.id).toSet
    walkForMutations(body, readParams, f.params.map(s => s.id -> s.name).toMap)

  protected def walkForMutations(e: TExpr, reads: Set[Int], names: Map[Int, String]): Unit =
    e match
      case TAssign(target, _, _, _) =>
        rootSym(target).foreach { s =>
          if reads.contains(s.id) then
            err(s"cannot mutate read-mode parameter `${names(s.id)}` (declare it `mut` per §6.4)", e.pos)
        }
        walkChildren(e, reads, names)
      case c: TCall =>
        checkCallSiteModes(c)
        walkChildren(e, reads, names)
      case _ =>
        walkChildren(e, reads, names)

  /** Per §6.4: a `mut`-mode parameter requires the call site to pass an
    * expression that the caller is allowed to hand out for mutation. The
    * argument must root in a `var` binding or a `mut` parameter — literals,
    * read-mode params, val/const bindings, and arbitrary expressions are
    * rejected. The callee type is read from `callee.tpe`; lambdas always
    * have all-read params (TLambda sets that explicitly), so this check
    * only fires for `def`-style functions with `mut` declarations.
    */
  protected def checkCallSiteModes(c: TCall): Unit =
    c.callee.tpe match
      case TyFunc(params, _) if params.exists(_._2 == ParamMode.Mut) =>
        // Zip up to the shorter side — arity mismatch is reported elsewhere.
        params.zip(c.args).foreach { case ((_, mode), arg) =>
          if mode == ParamMode.Mut then
            rootSym(arg) match
              case Some(s) if mutableSymIds.contains(s.id) => ()
              case Some(s) =>
                err(s"cannot pass `${s.name}` to a `mut`-mode parameter — only `var` bindings and `mut` parameters are accepted", arg.pos)
              case None =>
                err("a `mut`-mode parameter requires an l-value argument (name, field, index, or tuple projection)", arg.pos)
        }
      case _ => ()

  /** The "root" symbol of an l-value: the variable at the base of a chain
    * of `.field` / `[idx]` / `.0`-style tuple projections. */
  protected def rootSym(e: TExpr): Option[Symbol] = e match
    case TVarRef(s, _, _)         => Some(s)
    case TField(r, _, _, _)       => rootSym(r)
    case TIndex(r, _, _, _)       => rootSym(r)
    case TTupleProj(r, _, _, _)   => rootSym(r)
    case _                        => None

  protected def walkChildren(e: TExpr, reads: Set[Int], names: Map[Int, String]): Unit =
    e match
      case TBinOp(_, l, r, _, _)       => walkForMutations(l, reads, names); walkForMutations(r, reads, names)
      case TUnaryOp(_, x, _, _)        => walkForMutations(x, reads, names)
      case TJuxtapose(c, b, _, _)      => walkForMutations(c, reads, names); walkForMutations(b, reads, names)
      case TCall(c, args, _, _)        => walkForMutations(c, reads, names); args.foreach(a => walkForMutations(a, reads, names))
      case TIndex(a, idx, _, _)        => walkForMutations(a, reads, names); idx.foreach(i => walkForMutations(i, reads, names))
      case TSlice(a, lo, hi, _, _, _)  =>
        walkForMutations(a, reads, names); walkForMutations(lo, reads, names); walkForMutations(hi, reads, names)
      case TSlice2(a, rAx, cAx, _, _)  =>
        walkForMutations(a, reads, names)
        List(rAx, cAx).foreach {
          case TAxisIndex(e)         => walkForMutations(e, reads, names)
          case TAxisRange(lo, hi, _) => walkForMutations(lo, reads, names); walkForMutations(hi, reads, names)
          case TAxisAll              => ()
        }
      case _: TAxisAllMark             => ()
      case TField(r, _, _, _)          => walkForMutations(r, reads, names)
      case TTupleProj(r, _, _, _)      => walkForMutations(r, reads, names)
      case TMethodCall(r, _, args, _,_) => walkForMutations(r, reads, names); args.foreach(a => walkForMutations(a, reads, names))
      case TLambda(_, body, _, _)      => walkForMutations(body, reads, names)
      case TTuple(es, _, _)            => es.foreach(x => walkForMutations(x, reads, names))
      case TArrayLit(es, _, _)         => es.foreach(x => walkForMutations(x, reads, names))
      case TIf(c, th, el, _, _)        =>
        walkForMutations(c, reads, names); walkForMutations(th, reads, names); el.foreach(walkForMutations(_, reads, names))
      case TFor(_, it, body, _, _)     => walkForMutations(it, reads, names); walkForMutations(body, reads, names)
      case TWhile(c, b, _, _)          => walkForMutations(c, reads, names); walkForMutations(b, reads, names)
      case TReturn(v, _, _)            => v.foreach(walkForMutations(_, reads, names))
      case TAssign(t, v, _, _)         => walkForMutations(t, reads, names); walkForMutations(v, reads, names)
      case TBlock(items, r, _, _)      =>
        items.foreach {
          case TBlockBinding(_, _, v) => walkForMutations(v, reads, names)
          case TBlockExpr(x)          => walkForMutations(x, reads, names)
        }
        walkForMutations(r, reads, names)
      case TElementWise(_, l, r, _, _) => walkForMutations(l, reads, names); walkForMutations(r, reads, names)
      case TBroadcast(s, a, _, _, _, _) => walkForMutations(s, reads, names); walkForMutations(a, reads, names)
      case TMap(a, f, _, _)            => walkForMutations(a, reads, names); walkForMutations(f, reads, names)
      case TReduce(a, i, f, _, _)      => walkForMutations(a, reads, names); walkForMutations(i, reads, names); walkForMutations(f, reads, names)
      case TMatMul(l, r, _, _)         => walkForMutations(l, reads, names); walkForMutations(r, reads, names)
      case TInterpStringLit(parts, _, _) =>
        parts.foreach {
          case TInterpExpr(x) => walkForMutations(x, reads, names)
          case _              => ()
        }
      case _                           => ()
