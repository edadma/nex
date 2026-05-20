package io.github.edadma.nex

import scala.collection.mutable
import scala.util.parsing.input.Position

/** Stage 3-β.2 — monomorphization. The elaborator's Stage 2 left every
  * generic call site (a `TCall` whose `callee.tpe` is a `TyFunc`
  * containing one or more `TyKindVar`) with a substituted return type
  * but the callee still pointing at the generic template's `Symbol`.
  * Backends (interpreter, LLVM) cannot execute `TyKindVar` — it has no
  * runtime representation.
  *
  * This pass walks the program, collects every reachable
  * `(genericSymId, List[concreteTypeArgs])` pair, mints a fresh
  * specialized `TFunDecl` for each unique pair, and rewrites each
  * matching call site's `TVarRef` to point at the specialized clone.
  * The generic templates are dropped from the output entirely — they
  * have no executable form.
  *
  * Specialization recurses: a generic function whose body calls another
  * generic function discovers new `(gid, typeArgs)` pairs as its body
  * is walked. A worklist drains until no fresh pair is enqueued.
  * The kind-constraint set is finite and call sites are well-typed, so
  * the worklist always terminates.
  *
  * Body cloning gives every locally-bound symbol (parameter,
  * block-binding, for-loop var, lambda param) a fresh `Symbol.id` per
  * specialization, and rewrites every `TVarRef` inside the body to
  * reference the cloned symbol. Symbols defined outside the body
  * (other top-level functions, prelude entries, etc.) are left
  * untouched, so cross-decl references keep working.
  */
class NexMonomorphize(symbols: SymbolTable):

  // ==========================================================================
  // Kind-variable helpers (mirror of NexElabInference's helpers)
  // ==========================================================================

  private def hasKindVar(t: Type): Boolean = t match
    case _: TyKindVar  => true
    case TyArray(e, _) => hasKindVar(e)
    case TyTuple(es)   => es.exists(hasKindVar)
    case TyFunc(ps, r) => ps.exists((pt, _) => hasKindVar(pt)) || hasKindVar(r)
    case _             => false

  private def substituteKindVars(t: Type, subs: Map[String, Type]): Type = t match
    case TyKindVar(name, _) => subs.getOrElse(name, t)
    case TyArray(e, r)      => TyArray(substituteKindVars(e, subs), r)
    case TyTuple(es)        => TyTuple(es.map(substituteKindVars(_, subs)))
    case TyFunc(ps, r) =>
      TyFunc(
        ps.map { case (pt, m) => (substituteKindVars(pt, subs), m) },
        substituteKindVars(r, subs),
      )
    case _ => t

  /** Re-derive the kind-variable substitution for a call site, by
    * matching the generic's formal parameter types against the
    * concrete actual-argument types. Returns the type arguments in the
    * order the generic declared them (so the mangling is stable).
    */
  private def deriveSubstitution(generic: TFunDecl, args: List[TExpr]): (Map[String, Type], List[Type]) =
    val subs = mutable.Map.empty[String, Type]
    generic.params.zip(args).foreach { case (formal, actual) =>
      matchType(formal.tpe, actual.tpe, subs)
    }
    val typeArgs = collectTypeParamNames(generic).map(name => subs(name))
    (subs.toMap, typeArgs)

  private def matchType(formal: Type, actual: Type, subs: mutable.Map[String, Type]): Unit =
    (formal, actual) match
      case (TyKindVar(name, _), t) =>
        // β.1 already validated the constraint and joined the variable
        // across multiple binding sites; here we just record the first
        // concrete type we see for each name.
        if !subs.contains(name) then subs(name) = t
      case (TyArray(e1, r1), TyArray(e2, r2)) if r1 == r2 =>
        matchType(e1, e2, subs)
      case (TyTuple(es1), TyTuple(es2)) if es1.size == es2.size =>
        es1.zip(es2).foreach((a, b) => matchType(a, b, subs))
      case (TyFunc(p1, r1), TyFunc(p2, r2)) if p1.size == p2.size =>
        p1.zip(p2).foreach { case ((a, _), (b, _)) => matchType(a, b, subs) }
        matchType(r1, r2, subs)
      case _ => ()

  /** Walk a generic's signature in declaration order, returning every
    * `TyKindVar` name in the order it first appears. This is the order
    * used to mangle the specialized symbol name so it's stable across
    * runs.
    */
  private def collectTypeParamNames(generic: TFunDecl): List[String] =
    val out  = mutable.ListBuffer.empty[String]
    val seen = mutable.Set.empty[String]
    def go(t: Type): Unit = t match
      case TyKindVar(name, _) =>
        if !seen(name) then
          seen += name
          out += name
      case TyArray(e, _) => go(e)
      case TyTuple(es)   => es.foreach(go)
      case TyFunc(ps, r) =>
        ps.foreach((pt, _) => go(pt))
        go(r)
      case _ => ()
    generic.params.foreach(p => go(p.tpe))
    go(generic.returnType)
    out.toList

  /** Render a `Type` for use inside a specialized symbol name. Keep it
    * terse and ASCII so the resulting names are readable in IR
    * dumps and error messages.
    */
  private def mangleType(t: Type): String = t match
    case TyInteger        => "integer"
    case TyReal           => "real"
    case TyComplex        => "complex"
    case TyBool           => "bool"
    case TyString         => "string"
    case TyUnit           => "unit"
    case TyArray(e, 1)    => s"array_${mangleType(e)}"
    case TyArray(e, r)    => s"array${r}_${mangleType(e)}"
    case TyTuple(es)      => es.map(mangleType).mkString("tup_", "_", "")
    case TyStruct(n, _)   => n
    case TyFunc(ps, r)    => ps.map((pt, _) => mangleType(pt)).mkString("fn_", "_", s"_to_${mangleType(r)}")
    case TyKindVar(n, _)  => n  // shouldn't reach here for a specialized clone
    case TyUnknown        => "unknown"

  // ==========================================================================
  // Specialization registry
  // ==========================================================================

  /** (genericSymId, typeArgs) → specialized function Symbol. Keyed by
    * value so that two structurally-equal type-argument lists collapse
    * to the same clone.
    */
  private val specs = mutable.Map.empty[(Int, List[Type]), Symbol]

  /** genericSymId → original `TFunDecl`. Populated up-front. */
  private val templates = mutable.Map.empty[Int, TFunDecl]

  /** Pairs queued for body specialization. Drained at the end of
    * [[rewrite]].
    */
  private val worklist = mutable.Queue.empty[(Int, List[Type])]

  /** Newly-emitted specialized decls, in mint order. */
  private val emitted = mutable.ListBuffer.empty[TFunDecl]

  /** True iff the function's signature mentions any `TyKindVar`. */
  private def isGenericDecl(d: TDecl): Boolean = d match
    case f: TFunDecl =>
      f.params.exists(p => hasKindVar(p.tpe)) || hasKindVar(f.returnType)
    case _ => false

  // ==========================================================================
  // Entry point
  // ==========================================================================

  def rewrite(p: TProgram): TProgram =
    specs.clear()
    templates.clear()
    worklist.clear()
    emitted.clear()

    val (allGenerics, allNonGenerics) = (p.decls ::: p.auxDecls).partition(isGenericDecl)
    allGenerics.foreach {
      case f: TFunDecl => templates(f.sym.id) = f
      case _           => ()
    }

    // Walk non-generic decls. Each TCall to a generic registers a
    // specialization (lazily minted) and rewrites callee.sym to point
    // at the specialized clone. Generic templates are dropped from
    // both lists — they have no executable form.
    val rewrittenDecls    = p.decls.filterNot(isGenericDecl).map(rewriteDecl)
    val rewrittenAuxDecls = p.auxDecls.filterNot(isGenericDecl).map(rewriteDecl)

    // Drain the worklist. Each iteration specializes one (gid, args)
    // body, which may queue more specializations through nested
    // generic calls.
    while worklist.nonEmpty do
      val (gid, typeArgs) = worklist.dequeue()
      val generic = templates(gid)
      val specSym = specs((gid, typeArgs))
      val specDecl = specializeBody(generic, typeArgs, specSym)
      emitted += specDecl

    p.copy(
      decls    = rewrittenDecls ++ emitted.toList,
      auxDecls = rewrittenAuxDecls,
    )

  /** Get-or-mint the specialized symbol for `(gid, typeArgs)`. First
    * visit queues the pair for body specialization.
    */
  private def getOrMintSpec(gid: Int, typeArgs: List[Type]): Symbol =
    specs.getOrElseUpdate(
      (gid, typeArgs), {
        val generic     = templates(gid)
        val baseName    = generic.sym.name
        val argMangling = typeArgs.map(mangleType).mkString("$")
        val mangledName = s"$baseName$$$argMangling"
        // Compute the specialized TyFunc signature so the new symbol
        // carries a usable type immediately. The substitution map maps
        // each declared type-param name to its concrete argument.
        val paramNames = collectTypeParamNames(generic)
        val substMap   = paramNames.zip(typeArgs).toMap
        val specParamTypes = generic.params.map(p =>
          (substituteKindVars(p.tpe, substMap), ParamMode.Read),
        )
        val specRet  = substituteKindVars(generic.returnType, substMap)
        val specType = TyFunc(specParamTypes, specRet)
        val sym      = symbols.mint(mangledName, specType, SymKind.Function)
        worklist.enqueue((gid, typeArgs))
        sym
      },
    )

  // ==========================================================================
  // Body specialization
  // ==========================================================================

  /** Clone a generic function's body with the given concrete type
    * arguments. Local symbols (params, block-bindings, for-loop vars,
    * lambda params) get fresh Symbol ids so they don't collide with
    * the original or with other specializations. Outer references stay
    * the same.
    */
  private def specializeBody(
      generic:  TFunDecl,
      typeArgs: List[Type],
      specSym:  Symbol,
  ): TFunDecl =
    val paramNames = collectTypeParamNames(generic)
    val substMap   = paramNames.zip(typeArgs).toMap

    // Re-mint each param with a fresh id, concrete type.
    val symMap     = mutable.Map.empty[Int, Symbol]
    val newParams  = generic.params.map { p =>
      val newT  = substituteKindVars(p.tpe, substMap)
      val fresh = symbols.mint(p.name, newT, SymKind.Param)
      symMap(p.id) = fresh
      fresh
    }

    val newBody = specializeExpr(generic.body, substMap, symMap)
    val newRet  = substituteKindVars(generic.returnType, substMap)

    TFunDecl(
      sym        = specSym,
      params     = newParams,
      returnType = newRet,
      body       = newBody,
      isPrivate  = generic.isPrivate,
      attributes = generic.attributes,
      pos        = generic.pos,
    )

  /** Walk an expression and produce a specialized clone. Two
    * transformations happen here:
    *
    *  1. Any `tpe` field containing a `TyKindVar` is rewritten by
    *     `substituteKindVars(_, substMap)`. The result is concrete
    *     (assuming `β.1` validated the call site).
    *  2. Any local symbol referenced via `TVarRef` whose id appears in
    *     `symMap` is rewritten to use the cloned symbol.
    *  3. Nested call sites to a generic function recursively register
    *     a specialization and have their callee rewritten too.
    */
  private def specializeExpr(
      e:        TExpr,
      substMap: Map[String, Type],
      symMap:   mutable.Map[Int, Symbol],
  ): TExpr =
    def goT(t: Type): Type = substituteKindVars(t, substMap)
    def go(x: TExpr): TExpr = specializeExpr(x, substMap, symMap)

    e match
      // -- Literals ----------------------------------------------------
      case lit: TIntLit    => lit
      case lit: TRealLit   => lit
      case lit: TBoolLit   => lit
      case lit: TStringLit => lit
      case lit: TUnitLit   => lit
      case TInterpStringLit(parts, p, t) =>
        val newParts = parts.map {
          case TInterpExpr(x) => TInterpExpr(go(x))
          case TInterpRef(s) =>
            symMap.get(s.id) match
              case Some(fresh) => TInterpRef(fresh)
              case None        => TInterpRef(s)
          case other => other
        }
        TInterpStringLit(newParts, p, t)
      case TIntrinsic(opId, refs, p, t) =>
        // A generic intrinsic body carries trailing type-parameter refs
        // (e.g. `@intrinsic("libm.sqrt", T)` → refs=List("T")). At
        // specialization time we look up each ref in the substitution
        // map, append its mangled type to the opId, and clear `refs`
        // — the resulting opId is what backend dispatch tables key on.
        if refs.isEmpty then TIntrinsic(opId, Nil, p, goT(t))
        else
          val suffix = refs.map(name => mangleType(substMap(name))).mkString("$")
          TIntrinsic(s"$opId$$$suffix", Nil, p, goT(t))

      // -- References --------------------------------------------------
      case TVarRef(s, p, _) =>
        symMap.get(s.id) match
          case Some(fresh) => TVarRef(fresh, p, fresh.tpe)
          case None        =>
            // Outer reference. Its `.tpe` might still mention a kind
            // var if it pointed at the generic itself (recursion) —
            // substitute. The .sym stays the same; if it points at a
            // generic, the TCall path below handles the rewrite.
            TVarRef(s, p, goT(s.tpe))

      // -- Operators ---------------------------------------------------
      case TBinOp(op, l, r, p, t)         => TBinOp(op, go(l), go(r), p, goT(t))
      case TUnaryOp(op, x, p, t)          => TUnaryOp(op, go(x), p, goT(t))
      case TJuxtapose(c, b, p, t)         => TJuxtapose(go(c), go(b), p, goT(t))
      case TElementWise(op, l, r, p, t)   => TElementWise(op, go(l), go(r), p, goT(t))
      case TBroadcast(s, a, op, sf, p, t) => TBroadcast(go(s), go(a), op, sf, p, goT(t))
      case TMap(a, f, p, t)               => TMap(go(a), go(f), p, goT(t))
      case TReduce(a, i, f, p, t)         => TReduce(go(a), go(i), go(f), p, goT(t))
      case TMatMul(l, r, p, t)            => TMatMul(go(l), go(r), p, goT(t))
      case TFusedLoop(lv, len, b, cols, p, t) =>
        // Loop var keeps its identity inside this scope; cols/body
        // recurse as usual.
        TFusedLoop(lv, go(len), go(b), cols.map(go), p, goT(t))
      case TFlatIndex(a, i, p, t)         => TFlatIndex(go(a), go(i), p, goT(t))
      case TClone(a, p, t)                => TClone(go(a), p, goT(t))

      // -- Application / projection ------------------------------------
      case TCall(callee, args, p, t) =>
        val newArgs   = args.map(go)
        val newCallee = go(callee)
        // If the callee names a generic template, register its
        // specialization and rewrite the callee.sym in place. We re-
        // derive the substitution from the formal/actual pair so we
        // don't depend on the outer call having stored it.
        newCallee match
          case TVarRef(s, vp, _) if templates.contains(s.id) =>
            val generic = templates(s.id)
            val (_, typeArgs) = deriveSubstitution(generic, newArgs)
            val specSym = getOrMintSpec(s.id, typeArgs)
            val rewrittenCallee = TVarRef(specSym, vp, specSym.tpe)
            TCall(rewrittenCallee, newArgs, p, goT(t))
          case _ =>
            TCall(newCallee, newArgs, p, goT(t))

      case TIndex(a, idx, p, t)       => TIndex(go(a), idx.map(go), p, goT(t))
      case TField(r, n, p, t)         => TField(go(r), n, p, goT(t))
      case TTupleProj(r, i, p, t)     => TTupleProj(go(r), i, p, goT(t))
      case TSlice(a, lo, hi, inc, p, t) =>
        TSlice(go(a), go(lo), go(hi), inc, p, goT(t))
      case TSlice2(a, rAx, cAx, p, t) =>
        def axGo(ax: TAxisSpec): TAxisSpec = ax match
          case TAxisAll              => TAxisAll
          case TAxisIndex(x)         => TAxisIndex(go(x))
          case TAxisRange(lo, hi, i) => TAxisRange(go(lo), go(hi), i)
        TSlice2(go(a), axGo(rAx), axGo(cAx), p, goT(t))
      case _: TAxisAllMark            => e
      case TMethodCall(r, n, args, p, t) =>
        TMethodCall(go(r), n, args.map(go), p, goT(t))

      // -- Lambdas -----------------------------------------------------
      case TLambda(params, body, p, t) =>
        val newParams = params.map { ps =>
          val newT  = goT(ps.tpe)
          val fresh = symbols.mint(ps.name, newT, ps.kind)
          symMap(ps.id) = fresh
          fresh
        }
        val newBody = go(body)
        // Pop param entries so they don't leak to sibling expressions.
        params.foreach(ps => symMap -= ps.id)
        TLambda(newParams, newBody, p, goT(t))

      // -- Aggregates --------------------------------------------------
      case TTuple(es, p, t)    => TTuple(es.map(go), p, goT(t))
      case TArrayLit(es, p, t) => TArrayLit(es.map(go), p, goT(t))

      // -- Control flow ------------------------------------------------
      case TIf(c, th, el, p, t)   => TIf(go(c), go(th), el.map(go), p, goT(t))
      case TFor(vs, it, b, p, t)  =>
        val newVs = vs.map { ps =>
          val fresh = symbols.mint(ps.name, goT(ps.tpe), ps.kind)
          symMap(ps.id) = fresh
          fresh
        }
        val newIt = go(it)
        val newB  = go(b)
        vs.foreach(ps => symMap -= ps.id)
        TFor(newVs, newIt, newB, p, goT(t))
      case TWhile(c, b, p, t)     => TWhile(go(c), go(b), p, goT(t))
      case TReturn(v, p, t)       => TReturn(v.map(go), p, goT(t))
      case TAssign(tgt, v, p, t)  => TAssign(go(tgt), go(v), p, goT(t))

      // -- Block -------------------------------------------------------
      case TBlock(items, r, p, t) =>
        val newItems = items.map {
          case TBlockBinding(s, k, v) =>
            val newV  = go(v)
            val newT2 = goT(s.tpe)
            val fresh = symbols.mint(s.name, newT2, s.kind)
            symMap(s.id) = fresh
            TBlockBinding(fresh, k, newV)
          case TBlockExpr(x) => TBlockExpr(go(x))
        }
        val newR = go(r)
        // Block-scope bindings leak through `result` but not past the
        // enclosing block. Drop them once the block is processed.
        items.foreach {
          case TBlockBinding(s, _, _) => symMap -= s.id
          case _                      => ()
        }
        TBlock(newItems, newR, p, goT(t))

  // ==========================================================================
  // Decl rewriting (non-generic): walks the body looking for generic calls
  // ==========================================================================

  private def rewriteDecl(d: TDecl): TDecl = d match
    case f: TFunDecl =>
      f.copy(body = rewriteExpr(f.body))
    case b: TTopBinding =>
      b.copy(value = rewriteExpr(b.value))
    case other => other

  /** Walk a non-generic body, rewriting TCalls that target generic
    * templates. No type substitution happens here — these decls don't
    * mention `TyKindVar` themselves. We only update callee TVarRefs.
    */
  private def rewriteExpr(e: TExpr): TExpr =
    def go(x: TExpr): TExpr = rewriteExpr(x)
    e match
      case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit
         | _: TUnitLit | _: TVarRef | _: TAxisAllMark | _: TIntrinsic => e
      case TInterpStringLit(parts, p, t) =>
        TInterpStringLit(parts.map {
          case TInterpExpr(x) => TInterpExpr(go(x))
          case other          => other
        }, p, t)
      case TBinOp(op, l, r, p, t)         => TBinOp(op, go(l), go(r), p, t)
      case TUnaryOp(op, x, p, t)          => TUnaryOp(op, go(x), p, t)
      case TJuxtapose(c, b, p, t)         => TJuxtapose(go(c), go(b), p, t)
      case TElementWise(op, l, r, p, t)   => TElementWise(op, go(l), go(r), p, t)
      case TBroadcast(s, a, op, sf, p, t) => TBroadcast(go(s), go(a), op, sf, p, t)
      case TMap(a, f, p, t)               => TMap(go(a), go(f), p, t)
      case TReduce(a, i, f, p, t)         => TReduce(go(a), go(i), go(f), p, t)
      case TMatMul(l, r, p, t)            => TMatMul(go(l), go(r), p, t)
      case TFusedLoop(lv, len, b, cols, p, t) =>
        TFusedLoop(lv, go(len), go(b), cols.map(go), p, t)
      case TFlatIndex(a, i, p, t) => TFlatIndex(go(a), go(i), p, t)
      case TClone(a, p, t)        => TClone(go(a), p, t)
      case TCall(callee, args, p, t) =>
        val newArgs   = args.map(go)
        val newCallee = go(callee)
        newCallee match
          case TVarRef(s, vp, _) if templates.contains(s.id) =>
            val generic       = templates(s.id)
            val (_, typeArgs) = deriveSubstitution(generic, newArgs)
            val specSym       = getOrMintSpec(s.id, typeArgs)
            TCall(TVarRef(specSym, vp, specSym.tpe), newArgs, p, t)
          case _ => TCall(newCallee, newArgs, p, t)
      case TIndex(a, idx, p, t)       => TIndex(go(a), idx.map(go), p, t)
      case TField(r, n, p, t)         => TField(go(r), n, p, t)
      case TTupleProj(r, i, p, t)     => TTupleProj(go(r), i, p, t)
      case TSlice(a, lo, hi, inc, p, t) => TSlice(go(a), go(lo), go(hi), inc, p, t)
      case TSlice2(a, rAx, cAx, p, t) =>
        def axGo(ax: TAxisSpec): TAxisSpec = ax match
          case TAxisAll              => TAxisAll
          case TAxisIndex(x)         => TAxisIndex(go(x))
          case TAxisRange(lo, hi, i) => TAxisRange(go(lo), go(hi), i)
        TSlice2(go(a), axGo(rAx), axGo(cAx), p, t)
      case TMethodCall(r, n, args, p, t) => TMethodCall(go(r), n, args.map(go), p, t)
      case TLambda(ps, body, p, t)      => TLambda(ps, go(body), p, t)
      case TTuple(es, p, t)             => TTuple(es.map(go), p, t)
      case TArrayLit(es, p, t)          => TArrayLit(es.map(go), p, t)
      case TIf(c, th, el, p, t)         => TIf(go(c), go(th), el.map(go), p, t)
      case TFor(vs, it, b, p, t)        => TFor(vs, go(it), go(b), p, t)
      case TWhile(c, b, p, t)           => TWhile(go(c), go(b), p, t)
      case TReturn(v, p, t)             => TReturn(v.map(go), p, t)
      case TAssign(tgt, v, p, t)        => TAssign(go(tgt), go(v), p, t)
      case TBlock(items, r, p, t) =>
        val newItems = items.map {
          case TBlockBinding(s, k, v) => TBlockBinding(s, k, go(v))
          case TBlockExpr(x)          => TBlockExpr(go(x))
        }
        TBlock(newItems, go(r), p, t)
