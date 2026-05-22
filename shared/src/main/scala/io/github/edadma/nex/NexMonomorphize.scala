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
    case _: TyKindVar    => true
    case TyArray(e, _)   => hasKindVar(e)
    case TyTuple(es)     => es.exists(hasKindVar)
    case TyStruct(_, fs) => fs.exists { case (_, ft) => hasKindVar(ft) }
    case TyEnum(_, vs)   => vs.exists { case (_, vfs) => vfs.exists { case (_, ft) => hasKindVar(ft) } }
    case TyFunc(ps, r)   => ps.exists((pt, _) => hasKindVar(pt)) || hasKindVar(r)
    case _               => false

  private def substituteKindVars(t: Type, subs: Map[String, Type]): Type =
    substituteTyKindVars(t, subs)

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
    // Chase indirect bindings to their fixed point. A pair like
    // `T -> integer, U -> TyKindVar(T)` (produced when a lambda arg's
    // body was typed against `T -> T`, matched against formal `T -> U`)
    // collapses to `T -> integer, U -> integer` after one pass; loops
    // are impossible because each pass strictly reduces the number of
    // kind-variable references in the value side.
    var changed = true
    while changed do
      changed = false
      for (k, v) <- subs.toList do
        val substituted = substituteKindVars(v, subs.toMap)
        if substituted != v then
          subs(k) = substituted
          changed = true
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
      case (TyStruct(n1, f1), TyStruct(n2, f2)) if n1 == n2 && f1.size == f2.size =>
        f1.zip(f2).foreach { case ((_, a), (_, b)) => matchType(a, b, subs) }
      case (TyEnum(n1, v1), TyEnum(n2, v2)) if n1 == n2 && v1.size == v2.size =>
        v1.zip(v2).foreach {
          case ((_, fs1), (_, fs2)) if fs1.size == fs2.size =>
            fs1.zip(fs2).foreach { case ((_, a), (_, b)) => matchType(a, b, subs) }
          case _ => ()
        }
      case (TyFunc(p1, r1), TyFunc(p2, r2)) if p1.size == p2.size =>
        p1.zip(p2).foreach { case ((a, _), (b, _)) => matchType(a, b, subs) }
        matchType(r1, r2, subs)
      case _ => ()

  /** Derive the concrete type arguments for a generic struct
    * construction. The template's `typeParams` give the declared
    * ordering; matching each template field against the corresponding
    * actual-argument type fills the substitution map; we then read
    * back the arguments in declared order so the resulting spec key
    * is stable.
    */
  private def deriveStructTypeArgs(template: TStructDecl, args: List[TExpr]): List[Type] =
    val subs = mutable.Map.empty[String, Type]
    template.fields.zip(args).foreach { case ((_, ft), a) =>
      matchType(ft, a.tpe, subs)
    }
    template.typeParams.map { tp =>
      subs.getOrElse(tp.name, TyUnknown)
    }

  /** Derive the concrete type arguments for a generic enum from a
    * concrete `TyEnum` instance (the type pinned by inference at a
    * variant ctor call site or at a bare-variant push-down). The
    * template's stored `TyEnum` carries the same variant order with
    * `TyKindVar`s in the payload positions; matching it pairwise
    * against `concrete` fills the substitution map.
    */
  private def deriveEnumTypeArgs(template: TEnumDecl, concrete: TyEnum): List[Type] =
    val subs = mutable.Map.empty[String, Type]
    template.sym.tpe match
      case te: TyEnum => matchType(te, concrete, subs)
      case _          => ()
    template.typeParams.map { tp =>
      subs.getOrElse(tp.name, TyUnknown)
    }

  /** Derive the concrete type arguments for a variant ctor TCall, by
    * matching the variant's template field types against the actual
    * argument types. Used in addition to `deriveEnumTypeArgs` because
    * a TCall's `args` carry richer information than a bare TyEnum (when
    * the call site is `Pair(1, "hi")` style construction).
    */
  private def deriveVariantTypeArgs(
      enumTemplate: TEnumDecl,
      variantSym:   Symbol,
      args:         List[TExpr],
  ): List[Type] =
    val subs = mutable.Map.empty[String, Type]
    enumTemplate.variants.find(_._1.id == variantSym.id) match
      case Some((_, fs)) =>
        fs.zip(args).foreach { case ((_, ft), a) =>
          matchType(ft, a.tpe, subs)
        }
      case None => ()
    enumTemplate.typeParams.map { tp =>
      subs.getOrElse(tp.name, TyUnknown)
    }

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
    case TyByteArray      => "bytes"
    case TyArray(e, 1)    => s"array_${mangleType(e)}"
    case TyArray(e, r)    => s"array${r}_${mangleType(e)}"
    case TyTuple(es)      => es.map(mangleType).mkString("tup_", "_", "")
    case TyStruct(n, fs)  =>
      // Two specializations of the same generic struct share the base
      // name but differ by field types — include them so a function
      // specialized on `Pair[integer, integer]` doesn't collide with
      // one specialized on `Pair[real, real]`.
      if fs.isEmpty then n
      else fs.map { case (_, ft) => mangleType(ft) }.mkString(s"${n}_", "_", "")
    case TyEnum(n, vs)    =>
      // Same logic as TyStruct — variant payload types distinguish specs.
      if vs.isEmpty then n
      else vs.flatMap { case (_, fs) => fs.map { case (_, ft) => mangleType(ft) } }
        .mkString(s"${n}_", "_", "")
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

  /** Generic struct decl specialization registry, keyed by
    * `(genericTypeSymId, concreteTypeArgs)`. The value is the fresh
    * `Symbol` whose id stands in for the spec everywhere — TVarRef on
    * a constructor call site, `structFields(specSym.id)` in the
    * interpreter's struct-fields map, and the spec's own `TStructDecl`
    * in the rewritten program output.
    */
  private val structSpecs = mutable.Map.empty[(Int, List[Type]), Symbol]

  /** Generic-struct typeSymId → original `TStructDecl`. Populated up-front. */
  private val structTemplates = mutable.Map.empty[Int, TStructDecl]

  /** Pairs queued for struct-spec materialization. Drained at the end
    * of [[rewrite]] alongside the function worklist.
    */
  private val structWorklist = mutable.Queue.empty[(Int, List[Type])]

  /** Newly-emitted specialized struct decls, in mint order. */
  private val structEmitted = mutable.ListBuffer.empty[TStructDecl]

  /** Generic enum decl specialization registry, keyed by
    * `(genericTypeSymId, concreteTypeArgs)`. The value is `(enumSym,
    * variantTemplateId → specVariantSym)` — every variant of the spec
    * gets its own fresh `Symbol`, so backends emit per-spec variant
    * names and so `enumVariantInfo` registers concrete field types.
    */
  private val enumSpecs = mutable.Map.empty[(Int, List[Type]), (Symbol, Map[Int, Symbol])]

  /** Generic-enum typeSymId → original `TEnumDecl`. */
  private val enumTemplates = mutable.Map.empty[Int, TEnumDecl]

  /** Variant symbol id → parent enum symbol id. Populated for every
    * enum decl (generic or monomorphic) at scan time, so a TCall on a
    * variant ctor or a bare TVarRef to a variant can walk up to its
    * declaring enum without scanning.
    */
  private val variantToEnumId = mutable.Map.empty[Int, Int]

  /** Pairs queued for enum-spec materialization. Drained alongside the
    * function and struct worklists.
    */
  private val enumWorklist = mutable.Queue.empty[(Int, List[Type])]

  /** Newly-emitted specialized enum decls, in mint order. */
  private val enumEmitted = mutable.ListBuffer.empty[TEnumDecl]

  /** True iff the function's signature mentions any `TyKindVar`. */
  private def isGenericFunDecl(d: TDecl): Boolean = d match
    case f: TFunDecl =>
      f.params.exists(p => hasKindVar(p.tpe)) || hasKindVar(f.returnType)
    case _ => false

  /** True iff this is a user-declared generic struct (has typeParams). */
  private def isGenericStructDecl(d: TDecl): Boolean = d match
    case s: TStructDecl => s.typeParams.nonEmpty
    case _              => false

  /** True iff this is a user-declared generic enum (has typeParams). */
  private def isGenericEnumDecl(d: TDecl): Boolean = d match
    case e: TEnumDecl => e.typeParams.nonEmpty
    case _            => false

  private def isGenericDecl(d: TDecl): Boolean =
    isGenericFunDecl(d) || isGenericStructDecl(d) || isGenericEnumDecl(d)

  // ==========================================================================
  // Entry point
  // ==========================================================================

  def rewrite(p: TProgram): TProgram =
    specs.clear()
    templates.clear()
    worklist.clear()
    emitted.clear()
    structSpecs.clear()
    structTemplates.clear()
    structWorklist.clear()
    structEmitted.clear()
    enumSpecs.clear()
    enumTemplates.clear()
    variantToEnumId.clear()
    enumWorklist.clear()
    enumEmitted.clear()

    // Populate variantToEnumId for every enum decl (generic or not) so
    // a variant TVarRef can walk up to its declaring enum from a Symbol id
    // alone — the variant ctor and bare-variant rewriting paths need this
    // even for monomorphic enums whose ctor TCalls aren't rewritten.
    for d <- p.decls ::: p.auxDecls do d match
      case e: TEnumDecl =>
        e.variants.foreach { case (vs, _) => variantToEnumId(vs.id) = e.sym.id }
      case _ => ()

    val (allGenerics, _) = (p.decls ::: p.auxDecls).partition(isGenericDecl)
    allGenerics.foreach {
      case f: TFunDecl    => templates(f.sym.id) = f
      case s: TStructDecl => structTemplates(s.sym.id) = s
      case e: TEnumDecl   => enumTemplates(e.sym.id) = e
      case _              => ()
    }

    // Track which generic templates came from auxDecls so each generic's
    // specialized clones land back in the same list. Source-prelude
    // generics (in auxDecls) keep their clones out of user-visible
    // decls — structural test assertions like `tp.decls.size shouldBe 1`
    // stay accurate after monomorph runs over a prelude call.
    val auxGenericFnIds = p.auxDecls.collect {
      case f: TFunDecl if isGenericFunDecl(f) => f.sym.id
    }.toSet
    val auxGenericStructIds = p.auxDecls.collect {
      case s: TStructDecl if isGenericStructDecl(s) => s.sym.id
    }.toSet
    val auxGenericEnumIds = p.auxDecls.collect {
      case e: TEnumDecl if isGenericEnumDecl(e) => e.sym.id
    }.toSet

    // Walk non-generic decls. Each TCall to a generic registers a
    // specialization (lazily minted) and rewrites callee.sym to point
    // at the specialized clone. Generic templates are dropped from
    // both lists — they have no executable form.
    val rewrittenDecls    = p.decls.filterNot(isGenericDecl).map(rewriteDecl)
    val rewrittenAuxDecls = p.auxDecls.filterNot(isGenericDecl).map(rewriteDecl)

    // Drain all three worklists. Function-body specialization may queue
    // more function, struct, or enum specs through nested generic calls.
    // Struct/enum specs materialize body-free (substitution only), so
    // they can't enqueue further work — but a later function-body
    // specialization may still refer to a struct/enum spec already on
    // the worklist.
    val auxEmittedFn   = mutable.ListBuffer.empty[TFunDecl]
    val userEmittedFn  = mutable.ListBuffer.empty[TFunDecl]
    val auxEmittedSt   = mutable.ListBuffer.empty[TStructDecl]
    val userEmittedSt  = mutable.ListBuffer.empty[TStructDecl]
    val auxEmittedEn   = mutable.ListBuffer.empty[TEnumDecl]
    val userEmittedEn  = mutable.ListBuffer.empty[TEnumDecl]
    while worklist.nonEmpty || structWorklist.nonEmpty || enumWorklist.nonEmpty do
      while structWorklist.nonEmpty do
        val (gid, typeArgs) = structWorklist.dequeue()
        val generic  = structTemplates(gid)
        val specSym  = structSpecs((gid, typeArgs))
        val specDecl = specializeStructDecl(generic, typeArgs, specSym)
        structEmitted += specDecl
        if auxGenericStructIds.contains(gid) then auxEmittedSt += specDecl
        else userEmittedSt += specDecl
      while enumWorklist.nonEmpty do
        val (gid, typeArgs) = enumWorklist.dequeue()
        val generic  = enumTemplates(gid)
        val (specSym, variantMap) = enumSpecs((gid, typeArgs))
        val specDecl = specializeEnumDecl(generic, typeArgs, specSym, variantMap)
        enumEmitted += specDecl
        if auxGenericEnumIds.contains(gid) then auxEmittedEn += specDecl
        else userEmittedEn += specDecl
      if worklist.nonEmpty then
        val (gid, typeArgs) = worklist.dequeue()
        val generic = templates(gid)
        val specSym = specs((gid, typeArgs))
        val specDecl = specializeBody(generic, typeArgs, specSym)
        emitted += specDecl
        if auxGenericFnIds.contains(gid) then auxEmittedFn += specDecl
        else userEmittedFn += specDecl

    p.copy(
      decls    = rewrittenDecls ++ userEmittedSt.toList ++ userEmittedEn.toList ++ userEmittedFn.toList,
      auxDecls = rewrittenAuxDecls ++ auxEmittedSt.toList ++ auxEmittedEn.toList ++ auxEmittedFn.toList,
    )

  /** Get-or-mint the specialized struct-type symbol for `(gid, typeArgs)`.
    * The mint enqueues the pair for materialization (no body to walk —
    * just substitution into the field list). The spec keeps the same
    * struct name as the template so the resulting TyStruct compares
    * structurally against the elaborator-inferred constructor-call
    * type; collision avoidance for ARC helpers and function specs
    * happens via `typeMangle` / `mangleType`, both of which include
    * field types.
    */
  private def getOrMintStructSpec(gid: Int, typeArgs: List[Type]): Symbol =
    structSpecs.getOrElseUpdate(
      (gid, typeArgs), {
        val template = structTemplates(gid)
        val paramNames = template.typeParams.map(_.name)
        val substMap   = paramNames.zip(typeArgs).toMap
        val specFs     = template.fields.map { case (fn, ft) =>
          (fn, substituteKindVars(ft, substMap))
        }
        val specTy = TyStruct(template.sym.name, specFs)
        val sym    = symbols.mint(template.sym.name, specTy, SymKind.TypeName)
        structWorklist.enqueue((gid, typeArgs))
        sym
      },
    )

  /** Materialize a `TStructDecl` for one struct specialization. The
    * decl shares its source template's privacy and position; type
    * parameters are dropped (the spec is concrete).
    */
  private def specializeStructDecl(
      template: TStructDecl,
      typeArgs: List[Type],
      specSym:  Symbol,
  ): TStructDecl =
    val paramNames = template.typeParams.map(_.name)
    val substMap   = paramNames.zip(typeArgs).toMap
    val specFs     = template.fields.map { case (fn, ft) =>
      (fn, substituteKindVars(ft, substMap))
    }
    TStructDecl(
      sym        = specSym,
      fields     = specFs,
      isPrivate  = template.isPrivate,
      typeParams = Nil,
      pos        = template.pos,
    )

  /** Get-or-mint the specialized enum-type Symbol plus per-variant
    * Symbols for `(gid, typeArgs)`. The variant map is keyed by the
    * template's variant Symbol ids, so a TVarRef rewriter can look up
    * each template variant by id and replace with the corresponding
    * spec variant. Enum specs share their source template's name (like
    * struct specs); collision avoidance for ARC helpers and function
    * specs is handled by `mangleType` / `typeMangle`, which include
    * variant payload types.
    */
  private def getOrMintEnumSpec(gid: Int, typeArgs: List[Type]): (Symbol, Map[Int, Symbol]) =
    enumSpecs.getOrElseUpdate(
      (gid, typeArgs), {
        val template   = enumTemplates(gid)
        val paramNames = template.typeParams.map(_.name)
        val substMap   = paramNames.zip(typeArgs).toMap
        val specVs = template.variants.map { case (_, fs) =>
          fs.map { case (fn, ft) => (fn, substituteKindVars(ft, substMap)) }
        }
        val variantsByName = template.variants.zip(specVs).map {
          case ((vs, _), fs) => (vs.name, fs)
        }
        val specTy   = TyEnum(template.sym.name, variantsByName)
        val enumSym  = symbols.mint(template.sym.name, specTy, SymKind.TypeName)
        val variantSyms: List[(Int, Symbol)] = template.variants.zip(specVs).map {
          case ((vs, _), fs) =>
            val variantTpe =
              if fs.isEmpty then specTy
              else TyFunc(fs.map { case (_, ft) => (ft, ParamMode.Read) }, specTy)
            val sym = symbols.mint(vs.name, variantTpe, SymKind.EnumVariant)
            // Mirror variantToEnumId for the freshly minted spec variant
            // so subsequent walks (e.g. matched-arm patterns inside
            // generic-function bodies that recursively yield variants of
            // this same spec) can still dispatch via variantToEnumId.
            variantToEnumId(sym.id) = enumSym.id
            (vs.id, sym)
        }
        enumWorklist.enqueue((gid, typeArgs))
        (enumSym, variantSyms.toMap)
      },
    )

  /** Materialize a `TEnumDecl` for one enum specialization. The decl
    * shares its source template's privacy and position; type parameters
    * are dropped, and each variant Symbol is the fresh spec variant
    * registered in `variantMap`.
    */
  private def specializeEnumDecl(
      template:   TEnumDecl,
      typeArgs:   List[Type],
      specSym:    Symbol,
      variantMap: Map[Int, Symbol],
  ): TEnumDecl =
    val paramNames = template.typeParams.map(_.name)
    val substMap   = paramNames.zip(typeArgs).toMap
    val specVariants = template.variants.map { case (vs, fs) =>
      val specFs = fs.map { case (fn, ft) => (fn, substituteKindVars(ft, substMap)) }
      (variantMap(vs.id), specFs)
    }
    TEnumDecl(
      sym        = specSym,
      variants   = specVariants,
      isPrivate  = template.isPrivate,
      typeParams = Nil,
      pos        = template.pos,
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

    def substPattern(p: TPattern): (TPattern, List[Int]) = p match
      case TWildcardPat(_) => (p, Nil)
      case TVarPat(s, pp) =>
        val fresh = symbols.mint(s.name, goT(s.tpe), s.kind)
        symMap(s.id) = fresh
        (TVarPat(fresh, pp), List(s.id))
      case TVariantPat(vs, args, pp) =>
        val (subs, ids) = args.foldRight((List.empty[TPattern], List.empty[Int])) { (ap, acc) =>
          val (np, nids) = substPattern(ap)
          (np :: acc._1, nids ++ acc._2)
        }
        (TVariantPat(vs, subs, pp), ids)

    e match
      // -- Literals ----------------------------------------------------
      case lit: TIntLit    => lit
      case lit: TRealLit   => lit
      case lit: TBoolLit   => lit
      case lit: TStringLit => lit
      case lit: TUnitLit   => lit
      case TInterpStringLit(parts, p, t) =>
        val newParts = parts.map {
          case TInterpExpr(x, sp) => TInterpExpr(go(x), sp)
          case TInterpRef(s, sp) =>
            symMap.get(s.id) match
              case Some(fresh) => TInterpRef(fresh, sp)
              case None        => TInterpRef(s, sp)
          case other => other
        }
        TInterpStringLit(newParts, p, t)
      case TIntrinsic(opId, p, t) =>
        TIntrinsic(opId, p, goT(t))

      // -- References --------------------------------------------------
      case TVarRef(s, p, refT) =>
        symMap.get(s.id) match
          case Some(fresh) => TVarRef(fresh, p, fresh.tpe)
          case None        =>
            // Outer reference. Its `.tpe` might still mention a kind var
            // if it pointed at the generic itself (recursion) — substitute.
            // For a bare variant of a generic enum the substituted .tpe is
            // a concrete `TyEnum`; mint the spec and replace the Symbol
            // here so the TVarRef doesn't go to codegen with a template
            // variant id. Ctor TCalls hit the TCall path below.
            val newT = goT(refT match
              case TyUnknown => s.tpe
              case other     => other,
            )
            variantOfGenericEnum(s) match
              case Some(template) =>
                newT match
                  case te: TyEnum if !hasKindVar(te) =>
                    val typeArgs = deriveEnumTypeArgs(template, te)
                    val (_, variantMap) = getOrMintEnumSpec(template.sym.id, typeArgs)
                    val spec = variantMap.getOrElse(s.id, s)
                    TVarRef(spec, p, spec.tpe)
                  case _ => TVarRef(s, p, newT)
              case None => TVarRef(s, p, newT)

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
            val (localSubs, typeArgs) = deriveSubstitution(generic, newArgs)
            val specSym = getOrMintSpec(s.id, typeArgs)
            val rewrittenCallee = TVarRef(specSym, vp, specSym.tpe)
            // Second pass: re-walk args with the merged substitution so
            // any kind-variable mentioned by an arg's elaborated type
            // (e.g. a lambda whose param types were pushed down from the
            // generic's formal `T -> U`) gets substituted to the concrete
            // type the call site fixes them to. Without this, a lambda
            // arg keeps `TyKindVar(T, _)` and trips llvmType at codegen.
            val mergedSubs = substMap ++ localSubs
            val rewalkedArgs = newArgs.map(a => specializeExpr(a, mergedSubs, symMap))
            TCall(rewrittenCallee, rewalkedArgs, p, substituteKindVars(goT(t), localSubs))
          case TVarRef(s, vp, _) if structTemplates.contains(s.id) =>
            // Generic-struct constructor call inside a generic body. The
            // substMap carrying the outer specialization already resolved
            // every kind-var in `newArgs`; derive the spec by matching
            // the template fields against the resolved arg types.
            val template = structTemplates(s.id)
            val typeArgs = deriveStructTypeArgs(template, newArgs)
            val specSym  = getOrMintStructSpec(s.id, typeArgs)
            TCall(TVarRef(specSym, vp, specSym.tpe), newArgs, p, specSym.tpe)
          case TVarRef(s, vp, _) if variantOfGenericEnum(s).isDefined =>
            // Variant ctor call inside a generic body. Derive typeArgs
            // from the variant's template fields matched against the
            // (already-substituted) arg types, mint the enum spec, and
            // rewrite to the spec variant Symbol with the spec enum tpe.
            val template = variantOfGenericEnum(s).get
            val typeArgs = deriveVariantTypeArgs(template, s, newArgs)
            val (enumSym, variantMap) = getOrMintEnumSpec(template.sym.id, typeArgs)
            val specVariantSym = variantMap.getOrElse(s.id, s)
            TCall(TVarRef(specVariantSym, vp, specVariantSym.tpe), newArgs, p, enumSym.tpe)
          case _ =>
            TCall(newCallee, newArgs, p, goT(t))

      case TIndex(a, idx, p, t)       => TIndex(go(a), idx.map(go), p, goT(t))
      case TField(r, n, p, t)         => TField(go(r), n, p, goT(t))
      case TTupleProj(r, i, p, t)     => TTupleProj(go(r), i, p, goT(t))
      case TSlice(a, lo, hi, inc, st, p, t) =>
        TSlice(go(a), lo.map(go), hi.map(go), inc, st.map(go), p, goT(t))
      case TSlice2(a, rAx, cAx, p, t) =>
        def axGo(ax: TAxisSpec): TAxisSpec = ax match
          case TAxisAll                  => TAxisAll
          case TAxisIndex(x)             => TAxisIndex(go(x))
          case TAxisRange(lo, hi, i, st) => TAxisRange(lo.map(go), hi.map(go), i, st.map(go))
        TSlice2(go(a), axGo(rAx), axGo(cAx), p, goT(t))
      case _: TAxisAllMark            => e
      case _: TOpenSliceMark          => e
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
      case TMatch(s, cases, p, t) =>
        // Match arms introduce per-arm bindings via the patterns; mint
        // fresh symbols so the substitution map stays consistent with the
        // rest of the generic specialization machinery. Variant symbols
        // pointing at a generic enum template are also retargeted to the
        // spec variant matching the (now-concrete) scrutinee type.
        val newS = go(s)
        val newCases = cases.map { c =>
          val pat1 = rewriteVariantPat(c.pat, newS.tpe)
          val (pat2, pendingIds) = substPattern(pat1)
          val body2 = go(c.body)
          pendingIds.foreach(id => symMap -= id)
          TMatchCase(pat2, body2)
        }
        TMatch(newS, newCases, p, goT(t))

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

  /** Variant-of-generic-enum lookup helper. Returns `Some(template)` iff
    * `s` names a variant whose declaring enum is in `enumTemplates`.
    */
  private def variantOfGenericEnum(s: Symbol): Option[TEnumDecl] =
    if s.kind != SymKind.EnumVariant then None
    else variantToEnumId.get(s.id).flatMap(eid => enumTemplates.get(eid))

  /** Rewrite a match pattern so any variant symbol still pointing at a
    * generic-enum template is replaced with the spec'd variant Symbol.
    * The scrutinee's concrete `TyEnum` provides the type arguments;
    * nested variant patterns recurse with their own slot types.
    */
  private def rewriteVariantPat(pat: TPattern, scrutTpe: Type): TPattern = pat match
    case _: TWildcardPat | _: TVarPat => pat
    case TVariantPat(vs, args, pp) =>
      val (newVs, fieldTypes) = variantOfGenericEnum(vs) match
        case Some(template) =>
          scrutTpe match
            case te: TyEnum if !hasKindVar(te) =>
              val typeArgs = deriveEnumTypeArgs(template, te)
              val (_, variantMap) = getOrMintEnumSpec(template.sym.id, typeArgs)
              val spec = variantMap.getOrElse(vs.id, vs)
              val fs = te.variants.find(_._1 == vs.name).map(_._2).getOrElse(Nil)
              (spec, fs.map(_._2))
            case _ => (vs, args.map(_ => TyUnknown))
        case None =>
          // Monomorphic enum or a sibling spec — derive field types
          // straight from the scrutinee's TyEnum if available so nested
          // variant patterns recurse with the right slot tpe.
          val fs = scrutTpe match
            case te: TyEnum => te.variants.find(_._1 == vs.name).map(_._2).getOrElse(Nil)
            case _          => Nil
          (vs, fs.map(_._2))
      val newArgs = args.zip(fieldTypes).map { case (a, ft) => rewriteVariantPat(a, ft) }
      TVariantPat(newVs, newArgs, pp)

  /** Walk a non-generic body, rewriting TCalls that target generic
    * templates. No type substitution happens here — these decls don't
    * mention `TyKindVar` themselves. We only update callee TVarRefs.
    */
  private def rewriteExpr(e: TExpr): TExpr =
    def go(x: TExpr): TExpr = rewriteExpr(x)
    e match
      case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit
         | _: TUnitLit | _: TAxisAllMark | _: TOpenSliceMark | _: TIntrinsic => e
      case v: TVarRef =>
        // Bare variant of a generic enum: the surrounding context has
        // already pinned the concrete `TyEnum` on the `TVarRef.tpe` (via
        // `inferArg` push-down). Derive the spec from that, mint if
        // needed, rewrite the Symbol.
        variantOfGenericEnum(v.sym) match
          case Some(template) =>
            v.tpe match
              case te: TyEnum if !hasKindVar(te) =>
                val typeArgs = deriveEnumTypeArgs(template, te)
                val (_, variantMap) = getOrMintEnumSpec(template.sym.id, typeArgs)
                variantMap.get(v.sym.id) match
                  case Some(spec) => TVarRef(spec, v.pos, spec.tpe)
                  case None       => v
              case _ => v
          case None => v
      case TInterpStringLit(parts, p, t) =>
        TInterpStringLit(parts.map {
          case TInterpExpr(x, sp) => TInterpExpr(go(x), sp)
          case other              => other
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
        val newCallee = go(callee)
        newCallee match
          case TVarRef(s, vp, _) if templates.contains(s.id) =>
            // Generic call site. Derive the substitution from the
            // ORIGINAL args (the inferred types still carry the
            // call-site's kind variables). Specialize each arg with
            // that substitution so kindvars in the args' inferred
            // types (e.g. a lambda whose param/body types were pushed
            // down from the generic's formal `T -> U`) get resolved to
            // concrete types before any nested generic call inside the
            // arg registers its own specialization. Calling `go` on
            // the args first would walk them via this same rewriter —
            // any nested generic call inside the arg would then derive
            // its own substitution against the pre-substitution types
            // and mint a useless `f$T` specialization.
            val generic       = templates(s.id)
            val (localSubs, typeArgs) = deriveSubstitution(generic, args)
            val specSym       = getOrMintSpec(s.id, typeArgs)
            val rewalkedArgs  =
              if localSubs.nonEmpty then
                args.map(a => specializeExpr(a, localSubs, mutable.Map.empty))
              else args.map(go)
            TCall(TVarRef(specSym, vp, specSym.tpe), rewalkedArgs, p, substituteKindVars(t, localSubs))
          case TVarRef(s, vp, _) if structTemplates.contains(s.id) =>
            // Generic-struct constructor call. The elaborator already
            // substituted the field kindvars to concrete types in the
            // call's `.tpe`; derive the spec by matching template fields
            // against the rewritten arg types.
            val rewrittenArgs = args.map(go)
            val template      = structTemplates(s.id)
            val typeArgs      = deriveStructTypeArgs(template, rewrittenArgs)
            val specSym       = getOrMintStructSpec(s.id, typeArgs)
            TCall(TVarRef(specSym, vp, specSym.tpe), rewrittenArgs, p, specSym.tpe)
          case TVarRef(s, vp, _) if variantOfGenericEnum(s).isDefined =>
            // Variant ctor call on a generic enum. Derive typeArgs from
            // the variant's template field types matched against the
            // rewritten argument types; mint the spec; rewrite the
            // callee to the spec variant Symbol and the call's tpe to
            // the spec'd `TyEnum`.
            val rewrittenArgs = args.map(go)
            val template      = variantOfGenericEnum(s).get
            val typeArgs      = deriveVariantTypeArgs(template, s, rewrittenArgs)
            val (enumSym, variantMap) = getOrMintEnumSpec(template.sym.id, typeArgs)
            val specVariantSym = variantMap.getOrElse(s.id, s)
            TCall(TVarRef(specVariantSym, vp, specVariantSym.tpe), rewrittenArgs, p, enumSym.tpe)
          case _ => TCall(newCallee, args.map(go), p, t)
      case TIndex(a, idx, p, t)       => TIndex(go(a), idx.map(go), p, t)
      case TField(r, n, p, t)         => TField(go(r), n, p, t)
      case TTupleProj(r, i, p, t)     => TTupleProj(go(r), i, p, t)
      case TSlice(a, lo, hi, inc, st, p, t) => TSlice(go(a), lo.map(go), hi.map(go), inc, st.map(go), p, t)
      case TSlice2(a, rAx, cAx, p, t) =>
        def axGo(ax: TAxisSpec): TAxisSpec = ax match
          case TAxisAll                  => TAxisAll
          case TAxisIndex(x)             => TAxisIndex(go(x))
          case TAxisRange(lo, hi, i, st) => TAxisRange(lo.map(go), hi.map(go), i, st.map(go))
        TSlice2(go(a), axGo(rAx), axGo(cAx), p, t)
      case TMethodCall(r, n, args, p, t) => TMethodCall(go(r), n, args.map(go), p, t)
      case TLambda(ps, body, p, t)      => TLambda(ps, go(body), p, t)
      case TTuple(es, p, t)             => TTuple(es.map(go), p, t)
      case TArrayLit(es, p, t)          => TArrayLit(es.map(go), p, t)
      case TIf(c, th, el, p, t)         => TIf(go(c), go(th), el.map(go), p, t)
      case TFor(vs, it, b, p, t)        => TFor(vs, go(it), go(b), p, t)
      case TWhile(c, b, p, t)           => TWhile(go(c), go(b), p, t)
      case TMatch(s, cs, p, t)          =>
        val newS = go(s)
        val rewrittenCases = cs.map { c =>
          TMatchCase(rewriteVariantPat(c.pat, newS.tpe), go(c.body))
        }
        TMatch(newS, rewrittenCases, p, t)
      case TReturn(v, p, t)             => TReturn(v.map(go), p, t)
      case TAssign(tgt, v, p, t)        => TAssign(go(tgt), go(v), p, t)
      case TBlock(items, r, p, t) =>
        val newItems = items.map {
          case TBlockBinding(s, k, v) => TBlockBinding(s, k, go(v))
          case TBlockExpr(x)          => TBlockExpr(go(x))
        }
        TBlock(newItems, go(r), p, t)
