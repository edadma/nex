package io.github.edadma.nex

import scala.util.parsing.input.Position

/** Stage 2 sub-trait — pattern typing, exhaustiveness checks, and the
  * index / field inference paths. Sibling of [[NexElabInference]];
  * extracted to keep that file focused on the expression dispatcher and
  * the binop / arith / call paths. Methods called from across the
  * elaborator are routed through [[NexElabState]]'s abstract decls.
  */
protected trait NexElabInferPatterns extends NexElabState:

  // ==========================================================================
  // Match-pattern typing
  // ==========================================================================

  /** Pin field-binding symbols inside a match pattern to the field types
    * declared by the scrutinee's enum. Wildcard patterns and variant-
    * specific patterns are validated against `enumTy`; bare-name
    * wildcards (`case x =>`) bind the scrutinee's whole type. If the
    * scrutinee type is missing (e.g. the user wrote `match x` where `x`
    * has an inference error), pattern symbols stay [[TyUnknown]] and
    * exhaustiveness checking is skipped.
    */
  protected def typePattern(p: TPattern, enumTy: Option[TyEnum]): TPattern = p match
    case TWildcardPat(_) => p
    case TVarPat(sym, pp) =>
      val ty = enumTy.getOrElse(TyUnknown)
      TVarPat(setSymType(sym, ty), pp)
    case TVariantPat(vs, args, pp) =>
      val vsFields: List[(String, Type)] =
        enumTy match
          case Some(te) =>
            te.variants.find(_._1 == vs.name).map(_._2) match
              case Some(fs) => fs
              case None =>
                err(s"variant `${vs.name}` is not part of enum `${te.name}`", pp)
                Nil
          case None =>
            // Fallback to the variant's own type signature.
            symbols.get(vs.id).map(_.tpe) match
              case Some(TyFunc(ps, _)) => ps.map { case (t, _) => ("", t) }
              case _                   => Nil
      if vsFields.size != args.size then
        // Mismatch was already reported by the elaborator's arity check;
        // type sub-patterns against a padded TyUnknown list to keep
        // recursion safe.
        val padded = vsFields.padTo(args.size, ("", TyUnknown))
        val subs = args.zip(padded).map { case (sp, (_, ft)) => bindPattern(sp, ft) }
        TVariantPat(refreshSym(vs), subs, pp)
      else
        val subs = args.zip(vsFields).map { case (sp, (_, ft)) => bindPattern(sp, ft) }
        TVariantPat(refreshSym(vs), subs, pp)

  /** Bind sub-pattern symbols to the slot type at this position. */
  private def bindPattern(p: TPattern, slotTy: Type): TPattern = p match
    case TWildcardPat(_)  => p
    case TVarPat(sym, pp) => TVarPat(setSymType(sym, slotTy), pp)
    case TVariantPat(vs, args, pp) =>
      // Nested variant pattern — recurse using slotTy as the new enum.
      val enumOpt = slotTy match
        case te: TyEnum => Some(te)
        case _          => None
      val tp = typePattern(TVariantPat(vs, args, pp), enumOpt)
      tp

  /** Exhaustiveness check (Stage 2). A `_` wildcard or a bare name
    * pattern with no variant resolution is a catch-all. If no catch-all
    * is present, every declared variant must appear at the top level of
    * some arm. Duplicate variant arms surface as an error: Nex follows
    * Rust here, where redundant arms are a hard failure.
    */
  protected def checkMatchExhaustiveness(
      cases:  List[TMatchCase],
      enumTy: Option[TyEnum],
      pos:    Option[Position],
  ): Unit =
    enumTy match
      case None => () // already reported a primary error; nothing to add
      case Some(te) =>
        val declared = te.variants.map(_._1).toSet
        var sawCatchAll = false
        val covered  = scala.collection.mutable.Set.empty[String]
        for c <- cases do
          c.pat match
            case TVariantPat(vs, _, p) =>
              if !declared.contains(vs.name) then
                err(s"variant `${vs.name}` is not part of enum `${te.name}`", p)
              else if covered.contains(vs.name) then
                err(s"duplicate match arm for variant `${vs.name}`", p)
              else if sawCatchAll then
                err(s"unreachable match arm: catch-all already covers `${vs.name}`", p)
              else
                covered += vs.name
            case _: TWildcardPat | _: TVarPat =>
              if sawCatchAll then
                err("unreachable match arm: catch-all already present", c.pat.pos)
              sawCatchAll = true
        if !sawCatchAll then
          val missing = declared -- covered
          if missing.nonEmpty then
            err(s"non-exhaustive match: missing variants ${missing.toList.sorted.mkString(", ")}", pos)

  // ==========================================================================
  // Index & field inference
  // ==========================================================================

  protected def inferIndex(arr: TExpr, idx: List[TExpr], p: Option[Position]): TExpr =
    // Spec §4.14 slicing detection.
    //
    // Three shapes get rewritten in this dispatch:
    //   - `a[lo..hi]` or `a[lo..=hi]` (single range, rank-1 source) →
    //     `TSlice(arr, lo, hi, inclusive)` rank-1 result.
    //   - 2-element index list on rank-2 source where any element is
    //     a range or the `:` axis-all marker → `TSlice2(arr, rowAx,
    //     colAx)`. Result rank = number of preserved axes (integer
    //     collapses, range / `:` preserves).
    //   - everything else → plain `TIndex` (the existing behaviour).

    /** Classify one position in the index list as an axis spec. */
    def asAxisSpec(e: TExpr): TAxisSpec = e match
      case _: TAxisAllMark => TAxisAll
      case TBinOp(op, lo, hi, _, _) if op == ".." || op == "..=" =>
        if lo.tpe != TyUnknown && lo.tpe != TyInteger then
          err(s"slice lower bound must be integer, got ${lo.tpe}", lo.pos)
        if hi.tpe != TyUnknown && hi.tpe != TyInteger then
          err(s"slice upper bound must be integer, got ${hi.tpe}", hi.pos)
        TAxisRange(Some(lo), Some(hi), inclusive = op == "..=", None)
      case TOpenSliceMark(lo, hi, inclusive, stride, _, _) =>
        lo.foreach { e =>
          if e.tpe != TyUnknown && e.tpe != TyInteger then
            err(s"slice lower bound must be integer, got ${e.tpe}", e.pos)
        }
        hi.foreach { e =>
          if e.tpe != TyUnknown && e.tpe != TyInteger then
            err(s"slice upper bound must be integer, got ${e.tpe}", e.pos)
        }
        stride.foreach { e =>
          if e.tpe != TyUnknown && e.tpe != TyInteger then
            err(s"slice stride must be integer, got ${e.tpe}", e.pos)
        }
        TAxisRange(lo, hi, inclusive, stride)
      case other =>
        if other.tpe != TyUnknown && other.tpe != TyInteger then
          err(s"array index must be integer, got ${other.tpe}", other.pos)
        TAxisIndex(other)

    /** True iff the index element is a range or axis-all (i.e. would
      * trigger a slice rewrite). */
    def isSliceMarker(e: TExpr): Boolean = e match
      case _: TAxisAllMark                                       => true
      case _: TOpenSliceMark                                     => true
      case TBinOp(op, _, _, _, _) if op == ".." || op == "..=" => true
      case _                                                     => false

    idx match
      // -- Rank-1 single-range slice --------------------------------
      case List(TBinOp(op, lo, hi, _, _)) if op == ".." || op == "..=" =>
        if lo.tpe != TyUnknown && lo.tpe != TyInteger then
          err(s"slice lower bound must be integer, got ${lo.tpe}", lo.pos)
        if hi.tpe != TyUnknown && hi.tpe != TyInteger then
          err(s"slice upper bound must be integer, got ${hi.tpe}", hi.pos)
        arr.tpe match
          case TyArray(e, 1) =>
            TSlice(arr, Some(lo), Some(hi), inclusive = op == "..=", None, p, TyArray(e, 1))
          case TyByteArray =>
            TSlice(arr, Some(lo), Some(hi), inclusive = op == "..=", None, p, TyByteArray)
          case TyArray(_, r) =>
            err(s"rank-1 slice requires a rank-1 array, got rank $r", p)
            TSlice(arr, Some(lo), Some(hi), inclusive = op == "..=", None, p, TyUnknown)
          case TyUnknown =>
            TSlice(arr, Some(lo), Some(hi), inclusive = op == "..=", None, p, TyUnknown)
          case other =>
            err(s"cannot slice value of type $other", p)
            TSlice(arr, Some(lo), Some(hi), inclusive = op == "..=", None, p, TyUnknown)

      // -- Rank-1 single open-ended or strided slice ---------------
      // Covers `a[..hi]`, `a[lo..]`, `a[..]`, `a[lo..hi by k]`,
      // `a[..hi by k]`, `a[lo.. by k]`, and bare `a[.. by k]`.
      case List(TOpenSliceMark(lo, hi, inclusive, stride, _, _)) =>
        lo.foreach { e =>
          if e.tpe != TyUnknown && e.tpe != TyInteger then
            err(s"slice lower bound must be integer, got ${e.tpe}", e.pos)
        }
        hi.foreach { e =>
          if e.tpe != TyUnknown && e.tpe != TyInteger then
            err(s"slice upper bound must be integer, got ${e.tpe}", e.pos)
        }
        stride.foreach { e =>
          if e.tpe != TyUnknown && e.tpe != TyInteger then
            err(s"slice stride must be integer, got ${e.tpe}", e.pos)
        }
        arr.tpe match
          case TyArray(e, 1) =>
            TSlice(arr, lo, hi, inclusive, stride, p, TyArray(e, 1))
          case TyByteArray =>
            TSlice(arr, lo, hi, inclusive, stride, p, TyByteArray)
          case TyArray(_, r) =>
            err(s"rank-1 slice requires a rank-1 array, got rank $r", p)
            TSlice(arr, lo, hi, inclusive, stride, p, TyUnknown)
          case TyUnknown =>
            TSlice(arr, lo, hi, inclusive, stride, p, TyUnknown)
          case other =>
            err(s"cannot slice value of type $other", p)
            TSlice(arr, lo, hi, inclusive, stride, p, TyUnknown)

      // -- Orphan `:` in single-position index → error -------------
      case List(_: TAxisAllMark) =>
        err("`:` requires a rank-2 receiver and a paired axis (e.g. `m[:, 1]`)", p)
        TUnitLit(p)

      // -- Rank-2 slicing: 2 positions, at least one is a slice marker
      case List(r, c) if isSliceMarker(r) || isSliceMarker(c) =>
        val rowAx = asAxisSpec(r)
        val colAx = asAxisSpec(c)
        val preservedAxes = List(rowAx, colAx).count {
          case _: TAxisIndex => false
          case _             => true
        }
        arr.tpe match
          case TyArray(e, 2) =>
            val resultT = preservedAxes match
              case 0 => e
              case 1 => TyArray(e, 1)
              case _ => TyArray(e, 2)
            TSlice2(arr, rowAx, colAx, p, resultT)
          case TyArray(_, r0) =>
            err(s"rank-2 slice requires a rank-2 array, got rank $r0", p)
            TSlice2(arr, rowAx, colAx, p, TyUnknown)
          case TyUnknown =>
            TSlice2(arr, rowAx, colAx, p, TyUnknown)
          case other =>
            err(s"cannot slice value of type $other", p)
            TSlice2(arr, rowAx, colAx, p, TyUnknown)

      // -- Plain integer indexing (single or two-index) ------------
      case _ =>
        idx.foreach {
          case _: TAxisAllMark =>
            err("`:` is not legal here — used outside a rank-2 index list", p)
          case _: TOpenSliceMark =>
            err("open-ended slice is not legal here — used outside a rank-1 / rank-2 index list", p)
          case i if i.tpe != TyUnknown && i.tpe != TyInteger =>
            err(s"array index must be integer, got ${i.tpe}", i.pos)
          case _ => ()
        }
        val ty = arr.tpe match
          case TyArray(e, 1) if idx.size == 1 => e
          case TyArray(e, 2) if idx.size == 2 => e
          case TyArray(e, 2) if idx.size == 1 => TyArray(e, 1) // row slice
          case TyByteArray   if idx.size == 1 => TyInteger     // [byte] indexed read widens to integer
          case TyUnknown                      => TyUnknown
          case other =>
            err(s"cannot index value of type $other", p); TyUnknown
        TIndex(arr, idx, p, ty)

  protected def inferField(r: TExpr, name: String, p: Option[Position]): TExpr =
    val ty = r.tpe match
      case TyStruct(_, fs) =>
        fs.find(_._1 == name).map(_._2) match
          case Some(t) => t
          case None    => err(s"no field `$name` on $r.tpe", p); TyUnknown
      case TyComplex if name == "re" || name == "im" => TyReal
      case TyUnknown => TyUnknown
      case other     => err(s"cannot access field `$name` on $other", p); TyUnknown
    TField(r, name, p, ty)
