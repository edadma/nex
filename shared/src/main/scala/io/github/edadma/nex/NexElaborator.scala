package io.github.edadma.nex

import scala.collection.mutable
import scala.util.parsing.input.Positional

/** The Nex elaborator. Drives the pipeline over a parsed program and
  * returns a fully typed, lowered [[TProgram]] (or a list of elaboration
  * errors). The work is split across four trait files:
  *
  *   - [[NexElabState]]      — shared state, scopes, symbol table,
  *                             prelude registration, abstract cross-trait
  *                             method declarations
  *   - [[NexElabInference]]  — Stage 2 type inference + prelude HOF
  *                             routing + inferIndex / inferField
  *   - [[NexElabLowering]]   — Stage 3 sugar lowering + §6.4 mode
  *                             validation
  *   - this file             — Stage 1 name resolution (entry point,
  *                             declarations, [[elabExpr]], [[typeOf]],
  *                             block bindings, top-binding minting)
  *
  * Pipeline overview:
  *
  *   1. **Name resolution.** Builds nested symbol scopes and produces a
  *      typed AST in which every [[VarRefExpr]] has been resolved to a
  *      [[Symbol]]. Errors: undefined name, duplicate binding in the same
  *      scope (per §5.5), invalid assignment target.
  *
  *   2. **Type inference.** Walks the Stage-1 typed AST and fills in
  *      concrete types via bottom-up inference. Applies the
  *      integer→real→complex promotion lattice (§3.2); rewrites arithmetic
  *      with array operands into [[TElementWise]] / [[TBroadcast]]
  *      (§4.5); rewrites `@` into [[TMatMul]]. Errors: type mismatch,
  *      non-bool condition, comparison on non-comparable types.
  *
  *   3. **Sugar lowering.** Rewrites juxtaposition / method-call dispatch
  *      into their post-sugar forms so downstream stages don't need to
  *      handle them. Validates §6.4 parameter modes.
  *
  *   4. **Lifetime analysis.** Out-of-class — runs [[NexLifetime]] over
  *      the lowered tree to insert [[TClone]] wrappers at move sites
  *      that would otherwise invalidate a still-live `var` array.
  */
class NexElaborator
    extends NexElabState
    with NexElabInference
    with NexElabLowering:

  // ==========================================================================
  // Entry point
  // ==========================================================================

  /** Elaborate a single in-memory program — preserves the original
    * single-file API used by tests and `nex elaborate <file>`. Wraps
    * [[elaborateProject]] with a one-module list whose path is taken from
    * any `module foo.bar` declaration at the top of the file, or `[]` if
    * absent.
    */
  def elaborate(prog: ProgramAST): Either[List[ElabError], TProgram] =
    elaborate(prog, runMonomorph = true)

  /** Variant of [[elaborate]] that lets a caller skip the monomorphization
    * stage. The Stage 3-α elaborator tests use this so they can inspect a
    * generic `def`'s declared `TyKindVar`s before they get substituted out.
    * Production callers always go through the default-argument
    * `elaborate(prog)` overload.
    */
  def elaborate(prog: ProgramAST, runMonomorph: Boolean): Either[List[ElabError], TProgram] =
    val modDecl  = prog.decls.collectFirst { case m: ModuleDeclAST => m }
    val path     = modDecl.map(_.path).getOrElse(Nil)
    val testOnly = modDecl.exists(_.isTestOnly)
    val userMod  = LoadedModule(path, List(FileEntry("<inline>", prog)), testOnly, Nil)
    // Auto-discover and prepend the source prelude so single-program
    // elaboration (used by NexInterpreterTests, NexParityBase, and
    // every other test that calls `elaborate` directly without going
    // through the module loader) sees the same prelude bindings the
    // CLI's `loadAndElaborate` provides. Modules whose path already
    // starts with `prelude` are themselves prelude content and must
    // not self-inject.
    val modules =
      if path.headOption.contains("prelude") then List(userMod)
      else
        NexSysroot.findPreludeRoot(None) match
          case Some(root) =>
            NexModuleLoader.loadPreludeAsModule(root) match
              case Right(p) => List(p, userMod)
              case Left(_)  => List(userMod)
          case None => List(userMod)
    elaborateProject(modules, runMonomorph)

  /** Elaborate a project — multiple modules in topological order, each one
    * containing one-or-more files. The first module is processed in a
    * scope that nests directly under the prelude; subsequent modules also
    * nest under the prelude but get their imports' public symbols injected
    * into the module scope first.
    *
    *  - Per-module isolation: each module gets a fresh scope on top of the
    *    prelude root scope, so `private` decls in module A aren't visible
    *    to module B even by accidental name collision.
    *  - Imports preserve symbol identity: `import a.{x}` binds the same
    *    `Symbol` (same id) that module `a` defined as `x` — so the typed
    *    AST in module B references the exact same symbol as in module A.
    *  - Exports: every non-private top-level decl from a module is added
    *    to its `exports` map, keyed by name; the `imports` lookup honours
    *    this set strictly (a `private` decl is invisible).
    *
    * The final returned `TProgram` is the concatenation of all per-module
    * decls (after Stage 2 / Stage 3) under the project root module path.
    */
  def elaborateProject(modules: List[LoadedModule]): Either[List[ElabError], TProgram] =
    elaborateProject(modules, runMonomorph = true)

  def elaborateProject(modules: List[LoadedModule], runMonomorph: Boolean): Either[List[ElabError], TProgram] =
    registerPrelude()

    // Root scope holds the prelude; modules nest inside it.
    val rootScope = current

    // module path → name → exported Symbol(s). A name with multiple
    // entries is an overloaded `def` set; non-function names always
    // have a singleton list. Populated as each module finishes Pass A
    // so dependent modules' imports can resolve.
    val exports = mutable.Map.empty[List[String], mutable.LinkedHashMap[String, List[Symbol]]]

    val allLowered = mutable.ListBuffer.empty[TDecl]
    // Decls coming from `module prelude` source files. Separated from
    // user decls so structural test assertions on TProgram.decls (Pass
    // 1's "the program contains two top-bindings") are not perturbed
    // by an arbitrary number of source-prelude `@intrinsic` defs
    // — see [[TProgram.auxDecls]].
    val preludeLowered = mutable.ListBuffer.empty[TDecl]

    for module <- modules do
      // Fresh module scope on top of the prelude. We re-create rather than
      // push from `current` (which may carry stale state from a sibling
      // module on failure paths).
      current = new Scope(Some(rootScope))

      // Inject each import's selectors as bindings into the module scope.
      // The bound name is `alias.getOrElse(name)`; the bound Symbol is the
      // exporter's actual symbol so refs resolve cross-module by identity.
      val mergedDecls = module.files.flatMap(_.ast.decls)
      val moduleExports = mutable.LinkedHashMap.empty[String, List[Symbol]]

      def addExport(name: String, sym: Symbol): Unit =
        moduleExports(name) = moduleExports.getOrElse(name, Nil) :+ sym

      // Auto-import the source prelude into every non-prelude module. The
      // synthetic `import prelude.*` is processed by the same path as a
      // user-written wildcard import, so `nex parse` still shows what the
      // user wrote but the elaborator sees the prelude bindings. Skipped
      // for modules whose path starts with `prelude` — those are the
      // source of the symbols and a self-import would either no-op or
      // (with cross-file refs) loop.
      val isPreludeMod = module.path.headOption.contains("prelude")
      val implicitImports: List[ImportDeclAST] =
        if isPreludeMod || !exports.contains(List("prelude")) then Nil
        else List(ImportDeclAST(List("prelude"), Nil, isWildcard = true))

      for d <- (implicitImports ++ mergedDecls) do d match
        case i: ImportDeclAST =>
          exports.get(i.path) match
            case Some(modExports) =>
              // Resolved: bind each selector to the actual exported Symbol
              // so references resolve cross-module by identity. Missing
              // selectors are an error (the source module is loaded; the
              // name they reference simply isn't exported).
              if i.isWildcard then
                // Wildcard: bind every public export under its declared name.
                // Overloaded `def` names (multi-symbol export lists) bind
                // each overload into the importing scope so call-site
                // overload resolution sees the full candidate set. Name
                // clashes (`import a.*` then `import b.*` with overlap)
                // surface as the usual "clashes with an existing binding"
                // error, same as a selective import would.
                for (name, syms) <- modExports do
                  for sym <- syms do
                    if !current.define(name, sym) then
                      err(s"import `$name` from `${i.path.mkString(".")}.*` clashes with an existing binding in this module", i)
              else
                for sel <- i.selectors do
                  modExports.get(sel.name) match
                    case None | Some(Nil) =>
                      err(s"import `${i.path.mkString(".")}` has no public member `${sel.name}`", i)
                    case Some(syms) =>
                      val effective = sel.alias.getOrElse(sel.name)
                      for sym <- syms do
                        if !current.define(effective, sym) then
                          err(s"import `$effective` clashes with an existing binding in this module", i)
            case None =>
              // Unresolved import path: in single-file mode (where the
              // loader didn't discover any module by that path) we fall
              // back to placeholder symbols so the rest of the file still
              // elaborates and tests that exercise import shape continue
              // to work. Names typed against unresolved imports will surface
              // as TyUnknown and either work (if dynamic) or fail at the
              // first concrete-type assertion downstream. Wildcard imports
              // have no named selectors to placehold, so they no-op.
              if !i.isWildcard then
                for sel <- i.selectors do
                  val effective = sel.alias.getOrElse(sel.name)
                  defineNoError(effective, SymKind.Import, TyUnknown)
        case _ => ()

      // Pass A: pre-declare every top-level def / struct / top-binding.
      val topSyms = mutable.LinkedHashMap.empty[DeclAST, List[Symbol]]
      for d <- mergedDecls do d match
        case f: FunDeclAST =>
          val s = define(f.name, SymKind.Function, TyUnknown, f)
          topSyms(d) = List(s)
          if !f.isPrivate then addExport(f.name, s)
        case s: StructDeclAST =>
          val sym = define(s.name, SymKind.TypeName, TyStruct(s.name, Nil), s)
          topSyms(d) = List(sym)
          if !s.isPrivate then addExport(s.name, sym)
        case v: ValDeclAST =>
          val ss = topBindingSyms(v.pat, SymKind.TopLevel, v)
          topSyms(d) = ss
          // `private val` is module-local and never exported; the
          // synthetic `$tuple` temps from destructuring are also excluded
          // by name. Other named bindings are public per spec §9.4.
          if !v.isPrivate then
            ss.foreach(s => if !s.name.startsWith("$") then addExport(s.name, s))
        case v: VarDeclAST =>
          val ss = topBindingSyms(v.pat, SymKind.TopLevel, v)
          topSyms(d) = ss
          if !v.isPrivate then
            ss.foreach(s => if !s.name.startsWith("$") then addExport(s.name, s))
        case c: ConstDeclAST =>
          val ss = topBindingSyms(c.pat, SymKind.TopLevel, c)
          topSyms(d) = ss
          if !c.isPrivate then
            ss.foreach(s => if !s.name.startsWith("$") then addExport(s.name, s))
        case _: ImportDeclAST | _: ModuleDeclAST =>
          ()

      // Pass B: elaborate each declaration's body with all top-level
      // names + imports visible. Module decl must come first per §9.2 —
      // we only let it pass without an error if it's the very first
      // declaration in this file's merged stream.
      val elabDecls = mutable.ListBuffer.empty[TDecl]
      var seenNonModule = false
      for d <- mergedDecls do
        d match
          case f: FunDeclAST     => seenNonModule = true; elabDecls += elabFun(f, topSyms(d).head)
          case s: StructDeclAST  => seenNonModule = true; elabDecls += elabStruct(s, topSyms(d).head)
          case v: ValDeclAST     => seenNonModule = true; elabDecls ++= elabTopBinding(v, BindingKind.Val,   topSyms(d))
          case v: VarDeclAST     => seenNonModule = true; elabDecls ++= elabTopBinding(v, BindingKind.Var,   topSyms(d))
          case c: ConstDeclAST   => seenNonModule = true; elabDecls ++= elabTopBinding(c, BindingKind.Const, topSyms(d))
          case i: ImportDeclAST  => seenNonModule = true; elabDecls += elabImport(i)
          case m: ModuleDeclAST  =>
            if seenNonModule then err("module declaration must come first in file", m)
            elabDecls += elabModuleDecl(m)

      exports(module.path) = moduleExports

      // -- Stage 2: type inference (per-module, inside module scope) --
      // Inference reads `current.lookup(...)` for method-call dispatch
      // and other top-level-name lookups, so it must run while the
      // module's bindings are in scope.
      val perModule         = TProgram(module.path, elabDecls.toList, symbols)
      val perModuleInferred = inferProgram(perModule)

      // Overload sets must have pairwise-distinct signatures (different
      // arity or different param types). A `def f(real)` + `def f(complex)`
      // pair is a valid overload set; two `def f(real)` decls aren't.
      // Runs after Stage 2 so each function symbol has its concrete
      // TyFunc — Pass A only sees TyUnknown placeholders.
      validateOverloadSet(elabDecls.toList)

      // -- Stage 3: sugar lowering + mode validation (per-module) -----
      // Lowering also uses `current.lookup(...)` (lowerMethodCall) for
      // method-to-call dispatch, so it has to stay inside this module's
      // scope too. Mode validation reads paramModes / mutableSymIds —
      // both global — so cross-module forwarding works.
      val perModuleLowered = lowerProgram(perModuleInferred)
      if isPreludeMod then preludeLowered ++= perModuleLowered.decls
      else                 allLowered     ++= perModuleLowered.decls

    // Restore root scope for any post-loop work.
    current = rootScope

    if errors.nonEmpty then return Left(errors.toList)

    // Use the LAST module's path as the program's module path. For a
    // single-module project this is the only module; for multi-module
    // the last is the entry module by topo order.
    val rootPath = modules.lastOption.map(_.path).getOrElse(Nil)

    // -- Stage 4: lifetime analysis (§8.2, §8.3, §4.11) ------------------
    // Wraps move sites whose source is a still-live `var` array binding
    // in [[TClone]] so the original buffer stays usable after the move.
    // Also enforces §4.11: a var-array binding may be captured by at
    // most one closure.
    val pre        = TProgram(rootPath, allLowered.toList, symbols, preludeLowered.toList)
    val lifetime   = new NexLifetime(
      mutableSymIds = mutableSymIds.contains,
      symbolType    = id => symbols.get(id).map(_.tpe).getOrElse(TyUnknown),
      symbolName    = id => symbols.get(id).map(_.name).getOrElse(s"#$id"),
      err           = (msg, pos) => err(msg, pos),
    )
    val rewritten  = lifetime.rewrite(pre)

    if errors.nonEmpty then return Left(errors.toList)

    // -- Stage 5: monomorphization (Stage 3-β.2) -------------------------
    // Specializes every reachable generic call site into a concrete
    // clone of its template, then drops the templates from the
    // program. Backends never see `TyKindVar`. Skipped by Stage 3-α
    // tests that want to inspect generic templates directly.
    val finalProg =
      if runMonomorph then new NexMonomorphize(symbols).rewrite(rewritten)
      else rewritten

    if errors.nonEmpty then return Left(errors.toList)

    Right(finalProg)

  // ==========================================================================
  // Top-binding symbol minting (handles tuple patterns)
  // ==========================================================================

  /** For a top-level val/var/const, mint the symbol(s) the pattern names.
    *
    * For a single name (`val x = ...`), returns a 1-element list containing
    * the bound symbol. For a tuple pattern (`val a, b = ...`), returns
    * `tempSym :: names` where `tempSym` is a synthetic top-level binding
    * that holds the whole tuple value; Pass B emits N+1 TTopBindings, with
    * the named projections reading from `tempSym`.
    *
    * The synthetic temp uses a `$`-prefixed name so it can never collide
    * with a user identifier (the lexer forbids `$` in identifiers).
    */
  private def topBindingSyms(pat: PatternAST, kind: SymKind, where: Positional): List[Symbol] =
    pat match
      case VarPat(name)  => List(define(name, kind, TyUnknown, where))
      case WildcardPat() => List(symbols.mint("_", TyUnknown, kind))
      case t: TuplePat   =>
        val temp  = symbols.mint("$tuple", TyUnknown, kind)
        val names = t.elems.map(p => bindTuplePatternName(p, kind, where))
        temp :: names

  /** Mint a single named symbol for one element of a tuple-destructuring
    * pattern. Nested `TuplePat` cannot reach here because the parser's
    * `patternAtom` has no paren alternative — tuples are paren-less, so
    * there is no syntax for a tuple inside a tuple pattern.
    */
  private def bindTuplePatternName(p: PatternAST, kind: SymKind, where: Positional): Symbol =
    bindTuplePatternName(p, kind, where, TyUnknown)

  private def bindTuplePatternName(
      p: PatternAST,
      kind: SymKind,
      where: Positional,
      declaredTy: Type,
  ): Symbol =
    p match
      case VarPat(name)  => define(name, kind, declaredTy, where)
      case WildcardPat() => symbols.mint("_", declaredTy, kind)
      case _: TuplePat   => sys.error("unreachable: parser rejects nested tuple patterns")

  // ==========================================================================
  // Declarations
  // ==========================================================================

  private def elabModuleDecl(m: ModuleDeclAST): TModuleDecl =
    TModuleDecl(m.path, m.isTestOnly, Some(m.pos))

  private def elabImport(i: ImportDeclAST): TImportDecl =
    // In multi-module mode the imported symbols were already injected
    // into the module scope by [[elaborateProject]]'s pre-elab pass —
    // here we just look them up by their effective name and record them
    // in the TImportDecl so the typed AST stays self-describing.
    //
    // If the import wasn't resolved (e.g., missing module), the inject
    // pass already recorded an error and there's nothing to bind; we
    // return an empty selectors list so the TImportDecl shape is still
    // legal for downstream consumers.
    val sels = i.selectors.flatMap { s =>
      val effective = s.alias.getOrElse(s.name)
      current.lookup(effective).map(sym => (sym, s.alias))
    }
    TImportDecl(i.path, sels, Some(i.pos))

  /** Walk the module's top-level decls and report any overload set whose
    * members have identical signatures. By the time we run, Stage 2 has
    * filled in each function's TyFunc, so we compare param-list shapes
    * directly. An overload "signature" for matching purposes is just the
    * tuple of parameter types — return type does not participate.
    */
  private def validateOverloadSet(decls: List[TDecl]): Unit =
    val byName = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[TFunDecl]]
    for d <- decls do d match
      case f: TFunDecl => byName.getOrElseUpdate(f.sym.name, mutable.ListBuffer.empty) += f
      case _           => ()
    for (name, fns) <- byName if fns.size >= 2 do
      val seen = mutable.LinkedHashMap.empty[List[Type], TFunDecl]
      for f <- fns do
        val key = f.params.map(_.tpe)
        seen.get(key) match
          case Some(_) => err(s"redeclaration of `$name` in the same scope: overload signatures must differ", f.pos)
          case None    => seen(key) = f

  private def elabFun(f: FunDeclAST, sym: Symbol): TFunDecl =
    var paramSyms: List[Symbol] = Nil
    var retTy: Type             = TyUnknown
    val intrinsicAttr = f.attributes.find(_.name == "intrinsic")
    val body = scoped {
      // Mint each type parameter as a TypeName symbol carrying a
      // TyKindVar. The body's `typeOf("T")` look-up then resolves
      // through this scope and yields the kind-variable type directly.
      // Type parameters are scope-local to the def body; the outer
      // module never sees them.
      for tp <- f.typeParams do
        val kc = decodeKindConstraint(tp.constraint, f)
        defineNoError(tp.name, SymKind.TypeName, TyKindVar(tp.name, kc))
      paramSyms = f.params.map { p =>
        val ty = typeOf(p.typ)
        val s  = define(p.name, SymKind.Param, ty, f)
        paramModes(s.id) = p.mode
        if p.mode == ParamMode.Mut then mutableSymIds += s.id
        s
      }
      // Resolve the return type inside the scoped block so a generic
      // `def f[T: Float](x: T): T` finds `T` for the return-type
      // position — by the time the block exits the type-param scope
      // is gone.
      retTy = f.returnType.map(typeOf).getOrElse(TyUnknown)
      (f.body, intrinsicAttr) match
        case (Some(b), None)    => elabExpr(b)
        case (None, Some(attr)) =>
          attr.args match
            case Nil =>
              err("@intrinsic requires at least one argument (the opId)", f)
              TIntrinsic("<error>", Some(f.pos))
            case opId :: rest =>
              // Identifier args (stored with a `@` prefix by the parser)
              // marked an intrinsic as kind-specialized in the Stage 3-γ
              // design. Stage 3-ε retired that path in favor of
              // overload-by-signature in Nex source. The elaborator
              // accepts only a single string opId now.
              if opId.startsWith("@") then
                err("@intrinsic first argument must be the opId string, not a type-parameter reference", f)
                TIntrinsic("<error>", Some(f.pos))
              else if rest.nonEmpty then
                err("@intrinsic takes one argument (the opId string); kind-specialized intrinsics retired in Stage 3-ε — use overloaded source defs instead", f)
                TIntrinsic(opId, Some(f.pos))
              else
                TIntrinsic(opId, Some(f.pos))
        case (Some(b), Some(_)) =>
          err("@intrinsic declarations must not have a body", f)
          elabExpr(b)
        case (None, None) =>
          err("def declaration without body requires @intrinsic(\"opId\")", f)
          TUnitLit(Some(f.pos))
    }
    TFunDecl(sym, paramSyms, retTy, body, f.isPrivate, f.attributes.map(_.name), Some(f.pos))

  private def elabStruct(s: StructDeclAST, sym: Symbol): TStructDecl =
    val fs = s.fields.map(f => (f.name, typeOf(f.typ)))
    // Update the symbol's type now that we know the fields, and reflect
    // the same Symbol on the returned TStructDecl so walkers see the
    // resolved struct type instead of the Pass-A `TyStruct(name, Nil)`
    // placeholder.
    val resolvedSym = sym.copy(tpe = TyStruct(s.name, fs))
    symbols.update(resolvedSym)
    TStructDecl(resolvedSym, fs, s.isPrivate, Some(s.pos))

  private def elabTopBinding(
      d:    DeclAST,
      kind: BindingKind,
      syms: List[Symbol],
  ): List[TTopBinding] =
    val (typAnn, init, p) = d match
      case ValDeclAST(_, t, e, _, _)   => (t, e, d.pos)
      case VarDeclAST(_, t, e, _, _)   => (t, e, d.pos)
      case ConstDeclAST(_, t, e, _, _) => (t, e, d.pos)
      case _                         => sys.error("non-binding passed to elabTopBinding")
    val value = elabExpr(init)
    if kind == BindingKind.Var then syms.foreach(s => mutableSymIds += s.id)
    syms match
      case head :: Nil =>
        // Single-name binding. Type annotation (if any) updates the symbol.
        if typAnn.isDefined then symbols.update(head.copy(tpe = typeOf(typAnn.get)))
        List(TTopBinding(head, kind, value, Some(p)))
      case temp :: names =>
        // Tuple-destructuring binding. The first symbol is a synthetic temp
        // that holds the whole tuple; the rest are user-named projections.
        // Per spec §4.16: `val (a, b): (T, U) = ...` is allowed; the
        // annotation must be a tuple type whose arity matches the pattern.
        typAnn match
          case Some(t) =>
            typeOf(t) match
              case TyTuple(elems) if elems.size == names.size =>
                // Update each projection's declared type to the matching
                // element type. The temp keeps its tuple type so its
                // TVarRef in the projection knows the shape.
                symbols.update(temp.copy(tpe = TyTuple(elems)))
                names.zip(elems).foreach { case (s, et) =>
                  symbols.update(s.copy(tpe = et))
                }
              case TyTuple(elems) =>
                err(s"tuple type annotation has ${elems.size} elements but the pattern binds ${names.size}", d)
              case other =>
                err(s"tuple-destructuring binding requires a tuple type annotation, got $other", d)
          case None => ()
        val tempBinding = TTopBinding(temp, kind, value, Some(p))
        val projections = names.zipWithIndex.map { case (s, i) =>
          TTopBinding(s, kind, TTupleProj(TVarRef(temp, Some(p)), i, Some(p)), Some(p))
        }
        tempBinding :: projections
      case Nil => sys.error("topBindingSyms produced empty list")

  // ==========================================================================
  // Block-level bindings (sequential — each is visible to subsequent items)
  // ==========================================================================

  /** Elaborate one val/var/const inside a block. Returns one item for a
    * single-name binding, N+1 items for a tuple-destructuring binding (a
    * synthetic `$tuple` temp plus N named projections), following the same
    * shape as `elabTopBinding`.
    */
  private def elabBlockBinding(d: DeclAST): List[TBlockItem] =
    val (pat, typAnn, init, kind) = d match
      case ValDeclAST(p, t, e, _, _)   => (p, t, e, BindingKind.Val)
      case VarDeclAST(p, t, e, _, _)   => (p, t, e, BindingKind.Var)
      case ConstDeclAST(p, t, e, _, _) => (p, t, e, BindingKind.Const)
      case _ =>
        err("only val/var/const declarations are allowed inside blocks", d)
        (WildcardPat(), None, UnitLitExpr(), BindingKind.Val)

    // RHS is elaborated BEFORE the pattern names are added to scope, so
    // `val x = x + 1` references the outer `x`. Mirrors Scala/Rust.
    val value      = elabExpr(init)
    val declaredTy = typAnn.map(typeOf).getOrElse(TyUnknown)

    def markMutable(s: Symbol): Symbol =
      if kind == BindingKind.Var then mutableSymIds += s.id
      s

    pat match
      case VarPat(name) =>
        List(TBlockBinding(markMutable(define(name, SymKind.Local, declaredTy, d)), kind, value))
      case WildcardPat() =>
        List(TBlockBinding(markMutable(symbols.mint("_", declaredTy, SymKind.Local)), kind, value))
      case t: TuplePat =>
        // Per spec §4.16: a type annotation on a tuple-destructuring
        // binding must itself be a tuple type whose arity matches.
        val elemTypes: List[Type] = typAnn match
          case Some(annAst) =>
            typeOf(annAst) match
              case TyTuple(elems) if elems.size == t.elems.size => elems
              case TyTuple(elems) =>
                err(s"tuple type annotation has ${elems.size} elements but the pattern binds ${t.elems.size}", d)
                List.fill(t.elems.size)(TyUnknown)
              case other =>
                err(s"tuple-destructuring binding requires a tuple type annotation, got $other", d)
                List.fill(t.elems.size)(TyUnknown)
          case None => List.fill(t.elems.size)(TyUnknown)

        val tempT = typAnn match
          case Some(annAst) => typeOf(annAst)
          case None         => TyUnknown
        val temp = markMutable(symbols.mint("$tuple", tempT, SymKind.Local))
        val tempBinding = TBlockBinding(temp, kind, value)
        val projections = t.elems.zip(elemTypes).zipWithIndex.map {
          case ((subPat, elemT), i) =>
            val s = markMutable(bindTuplePatternName(subPat, SymKind.Local, d, elemT))
            TBlockBinding(s, kind, TTupleProj(TVarRef(temp), i))
        }
        tempBinding :: projections

  // ==========================================================================
  // Type expressions
  // ==========================================================================

  /** Translate a parse-AST type to a typed-AST type. Stage 1 leaves named
    * types that don't match a primitive as `TyStruct(name, Nil)` — the
    * Stage-2 (or a follow-up pass) will resolve them to the actual struct.
    */
  private def typeOf(t: TypeAST): Type =
    t match
      case NamedType("bool")      => TyBool
      case NamedType("integer")   => TyInteger
      // `real` and `real64` denote the same type in v0; once explicit
      // precision types (`real32`, `real128`) land, `real` stays as
      // the canonical alias for `real64` per the prelude roadmap.
      // Same convention for the complex pair.
      case NamedType("real")      => TyReal
      case NamedType("real64")    => TyReal
      case NamedType("complex")   => TyComplex
      case NamedType("complex64") => TyComplex
      case NamedType("unit")      => TyUnit
      case NamedType("string")    => TyString
      case NamedType(other)     =>
        // Read the fresh symbol from the table — the symbol stored in
        // scope.bindings is the snapshot from Pass A and may be stale
        // (e.g. struct fields not yet known). Type parameters of a
        // generic `def` arrive here as TypeName symbols carrying a
        // `TyKindVar`; pass the kind-variable type through verbatim
        // (no struct-wrapping).
        current.lookup(other).flatMap(s => symbols.get(s.id)) match
          case Some(Symbol(_, _, ts: TyStruct, SymKind.TypeName))   => ts
          case Some(Symbol(_, _, kv: TyKindVar, SymKind.TypeName))  => kv
          case Some(Symbol(_, _, _, SymKind.TypeName))              => TyStruct(other, Nil)
          case _                                                    =>
            err(s"unknown type `$other`", t); TyUnknown
      case ArrayType(inner) =>
        // Detect rank-2 by recursive shape (only rank 1 and 2 in v0).
        inner match
          case ArrayType(deep) => TyArray(typeOf(deep), 2)
          case _               => TyArray(typeOf(inner), 1)
      case TupleType(elems) => TyTuple(elems.map(typeOf))
      case FuncType(params, ret) =>
        TyFunc(params.map(p => (typeOf(p), ParamMode.Read)), typeOf(ret))

  /** Decode a source-level kind-constraint name (`Float`, `Numeric`,
    * `Real`, `Any`) into the matching [[KindConstraint]] enum case.
    * `None` (`[T]` with no constraint) defaults to `Any`. An unknown
    * constraint name records an error and returns `Any` so elaboration
    * can continue and surface every other issue with the function.
    */
  private def decodeKindConstraint(name: Option[String], where: Positional): KindConstraint =
    name match
      case None             => KindConstraint.Any
      case Some("Any")      => KindConstraint.Any
      case Some("Numeric")  => KindConstraint.Numeric
      case Some("Real")     => KindConstraint.Real
      case Some("Float")    => KindConstraint.Float
      case Some("Complex")  => KindConstraint.Complex
      case Some(other)      =>
        err(s"unknown kind constraint `$other` — supported: Any, Numeric, Real, Float, Complex", where)
        KindConstraint.Any

  // ==========================================================================
  // Expressions
  // ==========================================================================

  protected def elabExpr(e: ExprAST): TExpr =
    val pos = Some(e.pos)
    e match
      case IntLitExpr(v)    => TIntLit(v, pos)
      case RealLitExpr(v)   => TRealLit(v, pos)
      case BoolLitExpr(v)   => TBoolLit(v, pos)
      case StringLitExpr(v) => TStringLit(v, pos)
      case UnitLitExpr()    => TUnitLit(pos)

      case InterpStringLitExpr(parts) =>
        val tparts = parts.map {
          case InterpText(s)         => TInterpText(s)
          case InterpVar(name)       =>
            current.lookup(name) match
              case Some(sym) => TInterpRef(sym)
              case None      =>
                err(s"undefined name `$name`", e); TInterpRaw(name)
          case InterpExprPart(raw)   =>
            // Re-parse the `${...}` body as a single expression and elaborate
            // it in the current lexical scope. Surface-syntax errors inside
            // the interpolation become elaboration errors at the string's
            // position (the lexer only sliced the raw bytes; it never tried
            // to parse them).
            new NexParser().parseExpression(raw) match
              case Right(parsed) => TInterpExpr(elabExpr(parsed))
              case Left(perr)    =>
                err(s"failed to parse interpolated expression: $perr", e)
                TInterpRaw(raw)
        }
        TInterpStringLit(tparts, pos)

      case VarRefExpr(name) =>
        current.lookup(name) match
          case Some(sym) => TVarRef(sym, pos)
          case None      =>
            err(s"undefined name `$name`", e)
            TVarRef(symbols.mint(name, TyUnknown, SymKind.Local), pos)

      case BinOpExpr(op, l, r)        => TBinOp(op, elabExpr(l), elabExpr(r), pos)
      case UnaryOpExpr(op, operand)   => TUnaryOp(op, elabExpr(operand), pos)
      case JuxtaposeExpr(c, b)        => TJuxtapose(elabExpr(c), elabExpr(b), pos)

      case CallExpr(callee, args) =>
        // The parser emits `[e1, e2, ...]` as CallExpr(VarRefExpr("__array"), ...).
        // Intercept here so name resolution doesn't fail on the synthetic marker.
        callee match
          case VarRefExpr("__array") =>
            TArrayLit(args.map(elabExpr), pos)
          case _ =>
            TCall(elabExpr(callee), args.map(elabExpr), pos)
      case IndexExpr(arr, idx) =>
        // `:` (AxisAllExpr) only appears inside an index list — Stage 2
        // detects it and rewrites the surrounding TIndex into a
        // TSlice2 (rank-2 slice). Anywhere else the parser refuses the
        // `:` token; if it somehow slipped through, infExpr handles
        // the orphan case with an error.
        TIndex(elabExpr(arr), idx.map {
          case AxisAllExpr() => TAxisAllMark(pos)
          case e             => elabExpr(e)
        }, pos)

      case AxisAllExpr() =>
        // Defensive: `:` outside an index list is never a value.
        err("`:` is only legal inside an index list (rank-2 slice)", e)
        TUnitLit(pos)
      case FieldExpr(r, name) =>
        TField(elabExpr(r), name, pos)
      case MethodCallExpr(r, n, args) =>
        TMethodCall(elabExpr(r), n, args.map(elabExpr), pos)

      case LambdaExpr(params, body) =>
        scoped {
          val paramSyms = params.map { p =>
            val ty = p.typ.map(typeOf).getOrElse(TyUnknown)
            define(p.name, SymKind.Param, ty, e)
          }
          TLambda(paramSyms, elabExpr(body), pos)
        }

      case TupleExpr(elems) => TTuple(elems.map(elabExpr), pos)

      case IfExpr(c, t, elseO) =>
        TIf(elabExpr(c), elabExpr(t), elseO.map(elabExpr), pos)

      case ForExpr(pat, iter, body) =>
        // Iterator is evaluated in the enclosing scope; loop vars only
        // visible in the body.
        val it = elabExpr(iter)
        scoped {
          val loopVars = collectPatternSyms(pat)
          TFor(loopVars, it, elabExpr(body), pos)
        }

      case WhileExpr(c, b) =>
        TWhile(elabExpr(c), elabExpr(b), pos)

      case ReturnExpr(v) =>
        TReturn(v.map(elabExpr), pos)

      case AssignExpr(tgt, v) =>
        val telab = elabExpr(tgt)
        validateLValue(telab, tgt)
        TAssign(telab, elabExpr(v), pos)

      case BlockExpr(items, result) =>
        scoped {
          val ti = items.flatMap {
            case BlockDecl(d)     => elabBlockBinding(d)
            case BlockExprItem(x) => List(TBlockExpr(elabExpr(x)))
          }
          val tr = elabExpr(result)
          TBlock(ti, tr, pos)
        }

  /** Mint Local symbols for every name in a for-loop pattern. */
  private def collectPatternSyms(pat: PatternAST): List[Symbol] =
    pat match
      case VarPat(name)  => List(define(name, SymKind.Local, TyUnknown, pat))
      case WildcardPat() => List(symbols.mint("_", TyUnknown, SymKind.Local))
      case TuplePat(es)  => es.flatMap(collectPatternSyms)

  /** Assignment targets must be a name, a field access, an index, or a
    * slice form. Anything else is a structural error. Slice targets get
    * Fortran-90-style array-section assignment semantics in the interpreter
    * and codegen.
    */
  private def validateLValue(te: TExpr, src: ExprAST): Unit =
    te match
      case _: TVarRef | _: TField | _: TIndex | _: TSlice | _: TSlice2 => ()
      case _                                                            =>
        err("invalid assignment target", src)
