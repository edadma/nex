package io.github.edadma.nex

import scala.collection.mutable
import scala.util.parsing.input.Position

/** Stage 2 sub-trait — kind-variable unification, overload resolution,
  * and the generic-call / generic-struct construction paths
  * (Stage 3-β.1). Sibling of [[NexElabInference]]. The unification
  * machinery is internal to this trait; the public entry points
  * ([[resolveOverload]], [[inferGenericCall]],
  * [[inferGenericStructConstruct]], [[hasKindVar]]) are declared
  * abstract in [[NexElabState]] so other elaborator traits can reach
  * them through the cross-trait dispatch chain.
  */
protected trait NexElabInferGenerics extends NexElabState:

  /** True iff `t` mentions a `TyKindVar` anywhere in its structure. Used to
    * decide whether a `TyFunc` callee needs the generic call path.
    */
  protected def hasKindVar(t: Type): Boolean = t match
    case _: TyKindVar  => true
    case TyArray(e, _) => hasKindVar(e)
    case TyTuple(es)   => es.exists(hasKindVar)
    case TyStruct(_, fs) => fs.exists { case (_, ft) => hasKindVar(ft) }
    case TyEnum(_, vs)   => vs.exists { case (_, vfs) => vfs.exists { case (_, ft) => hasKindVar(ft) } }
    case TyFunc(ps, r) => ps.exists((pt, _) => hasKindVar(pt)) || hasKindVar(r)
    case _             => false

  /** Substitute `TyKindVar` occurrences in `t` using `subs`. A variable
    * absent from `subs` is left in place (so we can detect under-determined
    * generic calls after unification).
    */
  protected def substituteKindVars(t: Type, subs: Map[String, Type]): Type =
    substituteTyKindVars(t, subs)

  /** Outcome of unifying one formal parameter type against an actual
    * argument type while building a kind-variable substitution.
    */
  protected sealed trait UnifyResult
  protected case object UnifyOk extends UnifyResult
  protected case class UnifyConstraintViolation(name: String, constraint: KindConstraint, actual: Type) extends UnifyResult
  protected case class UnifyInconsistent(name: String, prior: Type, actual: Type) extends UnifyResult
  protected case class UnifyShapeMismatch(formal: Type, actual: Type) extends UnifyResult

  /** Unify `formal` (which may mention `TyKindVar`) against `actual`,
    * extending `subs` with any new bindings. `TyKindVar` binds to the
    * actual type if its constraint admits it; structural shapes
    * (`TyArray`, `TyTuple`, `TyFunc`) recurse pointwise. A previously
    * bound kind variable must re-encounter the same type, modulo numeric
    * promotion within the same constraint (so `f[T: Numeric](x: T, y: T)`
    * called as `f(1, 2.0)` widens T to real).
    */
  protected def unifyKindVars(
      formal: Type,
      actual: Type,
      subs:   mutable.Map[String, Type],
  ): UnifyResult =
    (formal, actual) match
      case (_, TyUnknown) => UnifyOk
      case (TyKindVar(name, c), t) =>
        subs.get(name) match
          case None =>
            // Skip binding `name -> TyKindVar(name)` — re-binding a variable
            // to itself is a no-op and would mask the still-unbound state.
            t match
              case TyKindVar(tname, _) if tname == name => UnifyOk
              case _ if c.admits(t) =>
                subs(name) = t
                UnifyOk
              case _ => UnifyConstraintViolation(name, c, t)
          case Some(prev) =>
            if prev == t then UnifyOk
            else t match
              case TyKindVar(tname, _) if tname == name =>
                // The actual side carries the same variable as the formal —
                // when this enclosing generic specializes, both sides get
                // the same concrete type. The existing `prev` binding stands.
                UnifyOk
              case _ if isNumeric(prev) && isNumeric(t) =>
                promote(prev, t) match
                  case Some(joined) if c.admits(joined) =>
                    subs(name) = joined
                    UnifyOk
                  case _ => UnifyInconsistent(name, prev, t)
              case _ => UnifyInconsistent(name, prev, t)
      case (TyArray(e1, r1), TyArray(e2, r2)) if r1 == r2 =>
        unifyKindVars(e1, e2, subs)
      case (TyTuple(es1), TyTuple(es2)) if es1.size == es2.size =>
        es1.zip(es2).foldLeft[UnifyResult](UnifyOk) {
          case (UnifyOk, (a, b)) => unifyKindVars(a, b, subs)
          case (err, _)          => err
        }
      case (TyFunc(p1, r1), TyFunc(p2, r2)) if p1.size == p2.size =>
        val paramRes = p1.zip(p2).foldLeft[UnifyResult](UnifyOk) {
          case (UnifyOk, ((a, _), (b, _))) => unifyKindVars(a, b, subs)
          case (err, _)                    => err
        }
        paramRes match
          case UnifyOk => unifyKindVars(r1, r2, subs)
          case other   => other
      case (TyStruct(n1, f1), TyStruct(n2, f2)) if n1 == n2 && f1.size == f2.size =>
        f1.zip(f2).foldLeft[UnifyResult](UnifyOk) {
          case (UnifyOk, ((_, a), (_, b))) => unifyKindVars(a, b, subs)
          case (err, _)                    => err
        }
      case (TyEnum(n1, v1), TyEnum(n2, v2)) if n1 == n2 && v1.size == v2.size =>
        v1.zip(v2).foldLeft[UnifyResult](UnifyOk) {
          case (UnifyOk, ((_, fs1), (_, fs2))) if fs1.size == fs2.size =>
            fs1.zip(fs2).foldLeft[UnifyResult](UnifyOk) {
              case (UnifyOk, ((_, a), (_, b))) => unifyKindVars(a, b, subs)
              case (err, _)                    => err
            }
          case (err, _) => err
        }
      case (a, b) if a == b                  => UnifyOk
      case (a, b) if isNumeric(a) && isNumeric(b) && promote(a, b).contains(a) =>
        // Numeric widening — the callee's formal is the wider type and the
        // actual is narrower, so an implicit coercion will run. This isn't
        // a kind-variable issue but it's reached through the same path.
        UnifyOk
      case (a, b) => UnifyShapeMismatch(a, b)

  /** Render a `KindConstraint` for a user-facing diagnostic. */
  protected def constraintLabel(c: KindConstraint): String = c match
    case KindConstraint.Any     => "Any"
    case KindConstraint.Numeric => "Numeric"
    case KindConstraint.Real    => "Real"
    case KindConstraint.Float   => "Float"
    case KindConstraint.Complex => "Complex"
    case KindConstraint.Ord     => "Ord"
    case KindConstraint.Eq      => "Eq"

  /** Pick the best-matching overload from a candidate set, scoring each
    * by the total numeric-promotion distance from the actual arg types to
    * the formal param types. Minimum-cost overload wins; tie at the
    * minimum is an ambiguity error. Returns `None` (after emitting an
    * error) when no candidate accepts the given arg types.
    *
    * Scoring rule: per (formal, actual) pair, 0 for an exact type match,
    * `rank(formal) - rank(actual)` for an admissible numeric promotion
    * (integer → real → complex), `None` otherwise.
    */
  protected def resolveOverload(
      cands: List[Symbol],
      args:  List[TExpr],
      pos:   Option[Position],
  ): Option[Symbol] =
    val (generic, concrete) = cands.partition { c =>
      currentType(c) match
        case TyFunc(params, ret) => params.exists((pt, _) => hasKindVar(pt)) || hasKindVar(ret)
        case _ => false
    }
    val concreteScored = concrete.flatMap { c =>
      currentType(c) match
        case TyFunc(params, _) if params.size == args.size =>
          scoreCall(params.map(_._1), args.map(_.tpe)).map(cost => (c, cost))
        case _ => None
    }
    if concreteScored.nonEmpty then
      val minCost = concreteScored.map(_._2).min
      val best    = concreteScored.filter(_._2 == minCost).map(_._1)
      if best.size > 1 then
        val name = cands.head.name
        err(s"ambiguous call to `$name`: multiple overloads accept these arguments", pos)
        None
      else Some(best.head)
    else
      val genericApplicable = generic.flatMap { c =>
        currentType(c) match
          case TyFunc(params, _) if params.size == args.size =>
            if genericApplies(params.map(_._1), args.map(_.tpe)) then Some(c) else None
          case _ => None
      }
      if genericApplicable.isEmpty then
        val name = cands.headOption.map(_.name).getOrElse("<unknown>")
        val argT = args.map(_.tpe).mkString(", ")
        err(s"no overload of `$name` matches argument types ($argT)", pos)
        None
      else if genericApplicable.size > 1 then
        val name = cands.head.name
        err(s"ambiguous call to `$name`: multiple generic overloads accept these arguments", pos)
        None
      else Some(genericApplicable.head)

  /** True iff a generic candidate's formals can be unified with the actual
    * argument types: each kind-variable formal's constraint must admit the
    * matching actual, repeated occurrences of the same variable must agree,
    * and any concrete formals must equal the corresponding actuals.
    */
  private def genericApplies(formals: List[Type], actuals: List[Type]): Boolean =
    val subs = mutable.Map.empty[String, Type]
    formals.zip(actuals).forall { (f, a) =>
      unifyKindVars(f, a, subs) match
        case UnifyOk => true
        case _       => false
    }

  /** Score an argument list against a formal param list. Returns the
    * total promotion cost, or `None` if any pair isn't assignable.
    * Numeric rank: integer=0, real=1, complex=2. Non-numeric types must
    * match exactly.
    */
  private def scoreCall(formals: List[Type], actuals: List[Type]): Option[Int] =
    var total = 0
    val it = formals.iterator.zip(actuals.iterator)
    while it.hasNext do
      val (f, a) = it.next()
      if f == a then ()
      else if isNumeric(f) && isNumeric(a) && promote(a, f).contains(f) then
        total += numericRank(f) - numericRank(a)
      else return None
    Some(total)

  private def numericRank(t: Type): Int = t match
    case TyInteger => 0
    case TyReal    => 1
    case TyComplex => 2
    case _         => -1

  /** Generic-call path. The callee's `TyFunc` mentions one or more
    * `TyKindVar`s. We unify each formal parameter against the actual
    * argument's type to build a substitution map, validate it,
    * substitute through the formal parameter list and return type, and
    * emit a `TCall` whose `tpe` is the substituted return type — but
    * whose `callee` still references the generic symbol. The
    * monomorphization pass (Stage 3-β.2) rewrites the call site to
    * point at a specialized clone.
    */
  protected def inferGenericCall(
      callee: TExpr,
      params: List[(Type, ParamMode)],
      ret:    Type,
      args:   List[TExpr],
      p:      Option[Position],
  ): TExpr =
    val subs = mutable.Map.empty[String, Type]
    var failed = false
    params.zip(args).foreach { case ((pt, _), a) =>
      if failed then ()
      else
        unifyKindVars(pt, a.tpe, subs) match
          case UnifyOk => ()
          case UnifyConstraintViolation(name, c, actual) =>
            err(
              s"type argument `$name` cannot be `$actual`: constraint `${constraintLabel(c)}` does not admit it",
              a.pos.orElse(p),
            )
            failed = true
          case UnifyInconsistent(name, prior, actual) =>
            err(
              s"type argument `$name` was inferred as `$prior` but the next argument requires `$actual`",
              a.pos.orElse(p),
            )
            failed = true
          case UnifyShapeMismatch(f, b) =>
            err(s"cannot pass `$b` where `$f` is expected", a.pos.orElse(p))
            failed = true
    }
    if failed then TCall(callee, args, p, substituteKindVars(ret, subs.toMap))
    else
      val substMap     = subs.toMap
      val substParams  = params.map { case (pt, m) => (substituteKindVars(pt, substMap), m) }
      val substRet     = substituteKindVars(ret, substMap)
      val coercedArgs  = substParams.zip(args).map { case ((pt, _), a) => coerceTo(a, pt) }
      // `substRet` may still contain a kind variable when a generic
      // function calls another generic with one of its own type
      // parameters threaded through (`def f[T](x: T) = id(x)`).
      // Monomorphization later substitutes the outer T to a concrete
      // type and re-derives the inner call's type arguments — there is
      // nothing to report here.
      TCall(callee, coercedArgs, p, substRet)

  /** Generic-struct construction path. The struct's template carries
    * `TyKindVar`s in its field types; we unify each field against the
    * corresponding actual-argument type, then substitute through the
    * field list to produce a concrete `TyStruct` for the call's `.tpe`.
    * The callee Symbol stays the template — monomorphization mints a
    * fresh specialized type symbol with a mangled name and rewrites the
    * call site to point at it.
    */
  protected def inferGenericStructConstruct(
      callee:   TExpr,
      typeSym:  Symbol,
      template: TyStruct,
      args:     List[TExpr],
      p:        Option[Position],
  ): TExpr =
    val subs   = mutable.Map.empty[String, Type]
    var failed = false
    template.fields.zip(args).foreach { case ((_, ft), a) =>
      if failed then ()
      else
        unifyKindVars(ft, a.tpe, subs) match
          case UnifyOk => ()
          case UnifyConstraintViolation(name, c, actual) =>
            err(
              s"type argument `$name` for struct `${typeSym.name}` cannot be `$actual`: constraint `${constraintLabel(c)}` does not admit it",
              a.pos.orElse(p),
            )
            failed = true
          case UnifyInconsistent(name, prior, actual) =>
            err(
              s"type argument `$name` for struct `${typeSym.name}` was inferred as `$prior` but the next field requires `$actual`",
              a.pos.orElse(p),
            )
            failed = true
          case UnifyShapeMismatch(f, b) =>
            err(s"cannot pass `$b` where `$f` is expected for struct `${typeSym.name}`", a.pos.orElse(p))
            failed = true
    }
    if failed then TCall(callee, args, p, template)
    else
      val substMap   = subs.toMap
      val substFs    = template.fields.map { case (fn, ft) => (fn, substituteKindVars(ft, substMap)) }
      val substTy    = TyStruct(template.name, substFs)
      val coercedArgs = substFs.zip(args).map { case ((_, ft), a) => coerceTo(a, ft) }
      TCall(callee, coercedArgs, p, substTy)
