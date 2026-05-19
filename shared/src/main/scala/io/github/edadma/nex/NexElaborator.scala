package io.github.edadma.nex

import scala.collection.mutable
import scala.util.parsing.input.{Position, Positional}

/** The Nex elaborator. Two passes run inside one [[elaborate]] call:
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
  * Sugar lowering (juxtaposition, method-call, pattern destructuring,
  * interpolated `${...}` re-parsing) is Stage 3 and lives in its own
  * commit.
  */
class NexElaborator:

  // ==========================================================================
  // Errors
  // ==========================================================================

  case class ElabError(message: String, pos: Option[Position]) extends RuntimeException(message):
    override def toString: String =
      pos match
        case Some(p) => s"${p.line}:${p.column}: $message"
        case None    => message

  private val errors = mutable.ListBuffer.empty[ElabError]

  private def err(msg: String, p: Positional): Unit =
    errors += ElabError(msg, Some(p.pos))

  private def err(msg: String, p: Option[Position]): Unit =
    errors += ElabError(msg, p)

  // ==========================================================================
  // Scopes
  // ==========================================================================

  /** A scope is a flat map from name → Symbol. Scopes are stacked via the
    * `outer` pointer; lookup walks the chain. Same-block redeclaration is
    * caught at insertion time by checking the local map.
    */
  private class Scope(val outer: Option[Scope]):
    val bindings: mutable.LinkedHashMap[String, Symbol] = mutable.LinkedHashMap.empty

    def lookup(name: String): Option[Symbol] =
      bindings.get(name).orElse(outer.flatMap(_.lookup(name)))

    def define(name: String, sym: Symbol): Boolean =
      if bindings.contains(name) then false
      else { bindings(name) = sym; true }

  private var current: Scope = new Scope(None)

  private def pushScope(): Unit = current = new Scope(Some(current))
  private def popScope(): Unit =
    current = current.outer.getOrElse(sys.error("elaborator: popped past root scope"))

  private inline def scoped[T](body: => T): T =
    pushScope()
    try body
    finally popScope()

  // ==========================================================================
  // Symbol table
  // ==========================================================================

  private val symbols    = new SymbolTable
  private val paramModes = mutable.Map.empty[Int, ParamMode]

  /** Symbol ids that are legitimate mutation targets — `var` bindings
    * (block-level + top-level) and `mut` parameters. Used by the call-site
    * mode check to verify a `mut`-position argument resolves to something
    * the caller is actually allowed to hand out for mutation.
    */
  private val mutableSymIds = mutable.Set.empty[Int]

  /** Symbol id → TLambda value, for every `val/var = lambda` binding whose
    * lambda has any `TyUnknown` param. Populated by inferTopBinding /
    * inferBlockItem. Consumed by [[inferArg]] when a TVarRef to such a
    * Symbol appears in an arg position with a concrete expected `TyFunc`:
    * the stored lambda gets re-inferred with the pushed-down types and
    * the entry is replaced with the refined version. Stage 3 lowering
    * then swaps the binding's `value` field in place.
    *
    * Monomorphic by construction — the param-set check `currentType(s) ==
    * TyUnknown` inside the TLambda branch of `inferArg` means only the
    * first refinement sticks. Calling a bound-then-called lambda with
    * conflicting expected types at different sites surfaces as a normal
    * type error from `checkAssignable`, which is the right v0 behaviour.
    */
  private val deferredLambdas = mutable.Map.empty[Int, TLambda]

  /** Define a name in the current scope, minting a fresh symbol. Returns
    * either the new symbol or the existing one (and records an error).
    */
  private def define(name: String, kind: SymKind, tpe: Type, where: Positional): Symbol =
    val sym = symbols.mint(name, tpe, kind)
    if !current.define(name, sym) then
      err(s"redeclaration of `$name` in the same scope", where)
    sym

  private def defineNoError(name: String, kind: SymKind, tpe: Type): Symbol =
    val sym = symbols.mint(name, tpe, kind)
    current.define(name, sym)
    sym

  // ==========================================================================
  // Prelude registration (§10)
  // ==========================================================================

  /** Mints prelude symbols into the root scope. Types are placeholders
    * (TyUnknown for functions whose proper signatures will land in Stage 2).
    */
  private def registerPrelude(): Unit =
    // §10.1 Constants
    defineNoError("pi",  SymKind.Prelude, TyReal)
    defineNoError("e",   SymKind.Prelude, TyReal)
    defineNoError("inf", SymKind.Prelude, TyReal)
    defineNoError("nan", SymKind.Prelude, TyReal)
    defineNoError("i",   SymKind.Prelude, TyComplex)

    val preludeFuncs = List(
      // §10.2 scalar math
      "sqrt", "cbrt", "abs", "sign",
      "exp", "log", "log2", "log10",
      "sin", "cos", "tan", "asin", "acos", "atan", "atan2",
      "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
      "floor", "ceil", "round", "trunc",
      "min", "max",
      // §10.3 complex
      "conj", "arg",
      // §10.4 array ops
      "length", "sum", "product", "dot",
      "map", "flatMap", "reduce", "filter", "range", "enumerate", "zip",
      "rows", "cols", "shape",
      "transpose", "matmul", "diag",
      "reshape", "flatten",
      "sum_axis",
      // §10.5 construction
      "zeros", "ones", "fill", "linspace", "identity",
      // §10.6 I/O
      "print", "format",
      // §10.7 type conversions
      "to_real", "to_integer", "to_complex",
      // §10.8 assertions
      "assert", "assert_eq", "assert_approx", "assert_traps",
    )
    for f <- preludeFuncs do defineNoError(f, SymKind.Prelude, TyUnknown)

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
    val modDecl  = prog.decls.collectFirst { case m: ModuleDeclAST => m }
    val path     = modDecl.map(_.path).getOrElse(Nil)
    val testOnly = modDecl.exists(_.isTestOnly)
    elaborateProject(List(LoadedModule(path, List(FileEntry("<inline>", prog)), testOnly, Nil)))

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
    registerPrelude()

    // Root scope holds the prelude; modules nest inside it.
    val rootScope = current

    // module path → name → exported Symbol. Populated as each module
    // finishes Pass A so dependent modules' imports can resolve.
    val exports = mutable.Map.empty[List[String], mutable.LinkedHashMap[String, Symbol]]

    val allLowered = mutable.ListBuffer.empty[TDecl]

    for module <- modules do
      // Fresh module scope on top of the prelude. We re-create rather than
      // push from `current` (which may carry stale state from a sibling
      // module on failure paths).
      current = new Scope(Some(rootScope))

      // Inject each import's selectors as bindings into the module scope.
      // The bound name is `alias.getOrElse(name)`; the bound Symbol is the
      // exporter's actual symbol so refs resolve cross-module by identity.
      val mergedDecls = module.files.flatMap(_.ast.decls)
      val moduleExports = mutable.LinkedHashMap.empty[String, Symbol]

      for d <- mergedDecls do d match
        case i: ImportDeclAST =>
          exports.get(i.path) match
            case Some(modExports) =>
              // Resolved: bind each selector to the actual exported Symbol
              // so references resolve cross-module by identity. Missing
              // selectors are an error (the source module is loaded; the
              // name they reference simply isn't exported).
              for sel <- i.selectors do
                modExports.get(sel.name) match
                  case None =>
                    err(s"import `${i.path.mkString(".")}` has no public member `${sel.name}`", i)
                  case Some(sym) =>
                    val effective = sel.alias.getOrElse(sel.name)
                    if !current.define(effective, sym) then
                      err(s"import `$effective` clashes with an existing binding in this module", i)
            case None =>
              // Unresolved import path: in single-file mode (where the
              // loader didn't discover any module by that path) we fall
              // back to placeholder symbols so the rest of the file still
              // elaborates and tests that exercise import shape continue
              // to work. Names typed against unresolved imports will surface
              // as TyUnknown and either work (if dynamic) or fail at the
              // first concrete-type assertion downstream.
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
          if !f.isPrivate then moduleExports(f.name) = s
        case s: StructDeclAST =>
          val sym = define(s.name, SymKind.TypeName, TyStruct(s.name, Nil), s)
          topSyms(d) = List(sym)
          if !s.isPrivate then moduleExports(s.name) = sym
        case v: ValDeclAST =>
          val ss = topBindingSyms(v.pat, SymKind.TopLevel, v)
          topSyms(d) = ss
          // `private val` is module-local and never exported; the
          // synthetic `$tuple` temps from destructuring are also excluded
          // by name. Other named bindings are public per spec §9.4.
          if !v.isPrivate then
            ss.foreach(s => if !s.name.startsWith("$") then moduleExports(s.name) = s)
        case v: VarDeclAST =>
          val ss = topBindingSyms(v.pat, SymKind.TopLevel, v)
          topSyms(d) = ss
          if !v.isPrivate then
            ss.foreach(s => if !s.name.startsWith("$") then moduleExports(s.name) = s)
        case c: ConstDeclAST =>
          val ss = topBindingSyms(c.pat, SymKind.TopLevel, c)
          topSyms(d) = ss
          if !c.isPrivate then
            ss.foreach(s => if !s.name.startsWith("$") then moduleExports(s.name) = s)
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

      // -- Stage 3: sugar lowering + mode validation (per-module) -----
      // Lowering also uses `current.lookup(...)` (lowerMethodCall) for
      // method-to-call dispatch, so it has to stay inside this module's
      // scope too. Mode validation reads paramModes / mutableSymIds —
      // both global — so cross-module forwarding works.
      val perModuleLowered = lowerProgram(perModuleInferred)
      allLowered ++= perModuleLowered.decls

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
    val pre        = TProgram(rootPath, allLowered.toList, symbols)
    val lifetime   = new NexLifetime(
      mutableSymIds = mutableSymIds.contains,
      symbolType    = id => symbols.get(id).map(_.tpe).getOrElse(TyUnknown),
      symbolName    = id => symbols.get(id).map(_.name).getOrElse(s"#$id"),
      err           = (msg, pos) => err(msg, pos),
    )
    val rewritten  = lifetime.rewrite(pre)

    if errors.nonEmpty then return Left(errors.toList)

    Right(rewritten)

  // -- top-binding symbol minting (handles tuple patterns) -------------------

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

  private def elabFun(f: FunDeclAST, sym: Symbol): TFunDecl =
    var paramSyms: List[Symbol] = Nil
    val body = scoped {
      paramSyms = f.params.map { p =>
        val ty = typeOf(p.typ)
        val s  = define(p.name, SymKind.Param, ty, f)
        paramModes(s.id) = p.mode
        if p.mode == ParamMode.Mut then mutableSymIds += s.id
        s
      }
      elabExpr(f.body)
    }
    val retTy = f.returnType.map(typeOf).getOrElse(TyUnknown)
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
      case NamedType("bool")    => TyBool
      case NamedType("integer") => TyInteger
      case NamedType("real")    => TyReal
      case NamedType("complex") => TyComplex
      case NamedType("unit")    => TyUnit
      case NamedType("string")  => TyString
      case NamedType(other)     =>
        // Read the fresh symbol from the table — the symbol stored in
        // scope.bindings is the snapshot from Pass A and may be stale
        // (e.g. struct fields not yet known).
        current.lookup(other).flatMap(s => symbols.get(s.id)) match
          case Some(Symbol(_, _, ts: TyStruct, SymKind.TypeName)) => ts
          case Some(Symbol(_, _, _, SymKind.TypeName))            => TyStruct(other, Nil)
          case _                                                  =>
            err(s"unknown type `$other`", t); TyUnknown
      case ArrayType(inner) =>
        // Detect rank-2 by recursive shape (only rank 1 and 2 in v0).
        inner match
          case ArrayType(deep) => TyArray(typeOf(deep), 2)
          case _               => TyArray(typeOf(inner), 1)
      case TupleType(elems) => TyTuple(elems.map(typeOf))
      case FuncType(params, ret) =>
        TyFunc(params.map(p => (typeOf(p), ParamMode.Read)), typeOf(ret))

  // ==========================================================================
  // Expressions
  // ==========================================================================

  private def elabExpr(e: ExprAST): TExpr =
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

  /** Assignment targets must be a name, a field access, or an index — per
    * the AssignExpr docstring. Anything else is a structural error.
    */
  private def validateLValue(te: TExpr, src: ExprAST): Unit =
    te match
      case _: TVarRef | _: TField | _: TIndex => ()
      case _                                  =>
        err("invalid assignment target", src)

  // ==========================================================================
  // Stage 2: type inference
  // ==========================================================================

  /** Numeric promotion lattice (§3.2): integer → real → complex. Returns
    * the higher of the two types, or `None` if either is non-numeric.
    */
  private def promote(a: Type, b: Type): Option[Type] =
    def rank(t: Type): Option[Int] = t match
      case TyInteger => Some(0)
      case TyReal    => Some(1)
      case TyComplex => Some(2)
      case _         => None
    for ra <- rank(a); rb <- rank(b) yield
      if ra >= rb then a else b

  /** True iff `t` is one of integer / real / complex. */
  private def isNumeric(t: Type): Boolean = t match
    case TyInteger | TyReal | TyComplex => true
    case _                              => false

  /** Element type of an array, or None if `t` isn't an array type. */
  private def elemOf(t: Type): Option[(Type, Int)] = t match
    case TyArray(e, r) => Some((e, r))
    case _             => None

  /** Pull the current (post-update) type of a Symbol from the table.
    * Stage 2 mutates [[SymbolTable]] when bindings get inferred, so
    * `sym.tpe` on a captured TVarRef may be stale.
    */
  private def currentType(s: Symbol): Type =
    symbols.get(s.id).map(_.tpe).getOrElse(s.tpe)

  /** Update the symbol table to reflect `s`'s newly inferred type. */
  private def setSymType(s: Symbol, t: Type): Symbol =
    val updated = s.copy(tpe = t)
    symbols.update(updated)
    updated

  /** Return the latest Symbol for `s.id` from the symbol table, falling
    * back to `s` if no entry exists. Used to refresh Symbols that were
    * captured in the typed AST during Stage 1 before their type was
    * inferred — without this, ref-style nodes (TVarRef, TInterpRef) carry
    * a stale `.sym.tpe` even after Stage 2 has refined the binding type.
    */
  private def refreshSym(s: Symbol): Symbol =
    symbols.get(s.id).getOrElse(s)

  /** Bidirectional inference for a single argument position.
    *
    * When the caller knows what type is expected at this position (because
    * the callee is a `TyFunc` with concrete param types, or a binding has
    * a declared type), the expected type is pushed down into the argument
    * before its body is inferred. This is what lets `xs.map(x -> x * 2)`
    * — where `x` carries no annotation — infer `x: integer` from the
    * callee's `(integer -> integer)` parameter type.
    *
    * Special-cased: `TLambda` with at least one TyUnknown param. For
    * everything else this just delegates to [[infExpr]].
    */
  private def inferArg(arg: TExpr, expected: Type): TExpr = arg match
    case TLambda(params, body, p, _) =>
      expected match
        case TyFunc(eparams, _) if eparams.size == params.size =>
          params.zip(eparams).foreach { case (s, (pt, _)) =>
            if currentType(s) == TyUnknown && pt != TyUnknown then
              setSymType(s, pt)
          }
          val freshParams = params.map(refreshSym)
          val body2       = infExpr(body)
          val ty          = TyFunc(freshParams.map(s => (s.tpe, ParamMode.Read)), body2.tpe)
          TLambda(freshParams, body2, p, ty)
        case _ => infExpr(arg)

    // Deferred resolve: `val f = lam` left lam's params at TyUnknown.
    // First call site that gives a concrete TyFunc refines lam in place
    // — the lambda is re-inferred with pushed-down param types, the
    // deferred map is updated, and the Symbol's tpe is bumped to the
    // refined function type. Stage 3 swaps the refined value back into
    // the binding's `value` field. The refreshed TVarRef is returned so
    // the call site sees the now-concrete function type.
    case TVarRef(s, p, _) if deferredLambdas.contains(s.id) =>
      expected match
        case TyFunc(_, _) =>
          val lam      = deferredLambdas(s.id)
          val refined  = inferArg(lam, expected).asInstanceOf[TLambda]
          deferredLambdas(s.id) = refined
          val freshSym = setSymType(s, refined.tpe)
          TVarRef(freshSym, p, refined.tpe)
        case _ => infExpr(arg)

    // Empty array literal with an expected array type: take the expected
    // element/rank rather than running through `infExpr` (which would
    // error per spec §4.13's "annotation required" rule). This is the
    // happy path for `val x: [real] = []` / `f([])` where f wants `[T]`.
    case TArrayLit(Nil, p, _) =>
      expected match
        case TyArray(_, _) => TArrayLit(Nil, p, expected)
        case _             => infExpr(arg)

    // Non-empty array literal with an expected element type: push the
    // expected element down into each element via [[inferArg]] (which
    // recurses, so nested literals and sub-aggregates compose) and then
    // run each element through [[coerceTo]] so narrower numeric
    // literals fold up to the expected element type. This makes
    //   val xs: [complex] = [1.0, 0.0, 1.0, 0.0]
    // work uniformly without manual `1.0 + 0i` cosmetics.
    case TArrayLit(elems, p, _) =>
      expected match
        case TyArray(elemT, 1) =>
          val coerced = elems.map { e =>
            val inferred = inferArg(e, elemT)
            coerceTo(inferred, elemT)
          }
          TArrayLit(coerced, p, TyArray(elemT, 1))
        case TyArray(elemT, 2) =>
          // Rank-2 literal — each row is itself a `[elemT]` array, so
          // recurse with the rank-1 element type.
          val rowT = TyArray(elemT, 1)
          val coerced = elems.map(row => inferArg(row, rowT))
          TArrayLit(coerced, p, TyArray(elemT, 2))
        case _ => infExpr(arg)

    case _ => infExpr(arg)

  /** A lambda is "partially inferred" iff at least one param's type in
    * the symbol table is still TyUnknown. Used to decide whether a
    * `val f = lam` binding goes into [[deferredLambdas]].
    */
  private def isPartiallyInferredLambda(lam: TLambda): Boolean =
    lam.params.exists(s => currentType(s) == TyUnknown)

  // -- program -------------------------------------------------------------

  private def inferProgram(p: TProgram): TProgram =
    // Two-pass: first register every function's declared TyFunc shape
    // (params + declared return type, or TyUnknown if not declared),
    // then infer bodies. Without this, mutual recursion broke: when
    // inferring `def isEven(...)` whose body calls `isOdd` (defined
    // later), isOdd's symbol still carried its registration-time type
    // — usually TyUnknown — and the call site captured that stale
    // type. Codegen then emitted `call i64 @isOdd(...)` while the
    // body's other branch had the declared return type, producing
    // phi-node type mismatches.
    p.decls.foreach {
      case f: TFunDecl =>
        val params = f.params.map(pp =>
          (currentType(pp), paramModes.getOrElse(pp.id, ParamMode.Read)),
        )
        setSymType(f.sym, TyFunc(params, f.returnType))
      case _ => ()
    }
    p.copy(decls = p.decls.map(inferDecl))

  private def inferDecl(d: TDecl): TDecl = d match
    case t: TFunDecl     => inferFun(t)
    case t: TStructDecl  => t  // fields already typed
    case t: TTopBinding  => inferTopBinding(t)
    case t: TModuleDecl  => t
    case t: TImportDecl  => t

  private def inferFun(f: TFunDecl): TFunDecl =
    // Pre-register the function's TyFunc shape BEFORE inferring its
    // body so any recursive `TVarRef` to `f.sym` inside the body
    // resolves to the right return type. The declared return type is
    // used directly if present; otherwise we register TyUnknown for
    // the return and the second `setSymType` below refines it after
    // the body is inferred.
    val params = f.params.map(p => (currentType(p), paramModes.getOrElse(p.id, ParamMode.Read)))
    setSymType(f.sym, TyFunc(params, f.returnType))

    val body2 = infExpr(f.body)
    val ret =
      if f.returnType != TyUnknown then f.returnType
      else body2.tpe
    val sym2 = setSymType(f.sym, TyFunc(params, ret))
    f.copy(sym = sym2, body = body2, returnType = ret)

  private def inferTopBinding(b: TTopBinding): TTopBinding =
    val declared = currentType(b.sym)
    val v2 =
      if declared != TyUnknown then inferArg(b.value, declared)
      else infExpr(b.value)
    val (vCoerced, t) = declared match
      case TyUnknown => (v2, v2.tpe)
      case _         => (coerceTo(v2, declared), declared)
    val sym2 = setSymType(b.sym, t)
    registerDeferredLambda(sym2, vCoerced)
    // Spec §5.3: `const` bindings require a constant expression on the
    // RHS. Track every const Symbol id so later const refs to this one
    // can be recognised, and validate the value tree.
    if b.kind == BindingKind.Const then
      constSymIds += sym2.id
      validateConstExpr(vCoerced)
    b.copy(sym = sym2, value = vCoerced)

  /** Symbol ids of top-level `const` bindings — used by
    * [[validateConstExpr]] to allow references between consts.
    */
  private val constSymIds = mutable.Set.empty[Int]

  /** Spec §5.3: enforce that a `const` RHS is a constant expression.
    *
    * Allowed:
    *   - Numeric / boolean literals (incl. the parsed-out negative-juxt
    *     forms — these are integer/real lits in Stage 1).
    *   - Arithmetic, comparison, and logical binops: `+ - * / div % ^
    *     and or == != < <= > >=` (juxt is parsed as `*`).
    *   - Unary `-`, `not`.
    *   - References to other `const` bindings.
    *   - References to prelude constants (`pi`, `e`, `inf`, `nan`, `i`).
    *
    * Rejected: function calls (deferred to v1+), references to `val` /
    * `var`, lambdas, array/tuple/struct construction, control flow,
    * string literals, interpolated strings, every other expression
    * form. Each rejection emits an elaboration error rooted at the
    * offending node's position.
    */
  private def validateConstExpr(e: TExpr): Unit =
    val allowedBinOps = Set(
      "+", "-", "*", "/", "div", "%", "^",
      "and", "or",
      "==", "!=", "<", "<=", ">", ">=",
    )
    e match
      case _: TIntLit | _: TRealLit | _: TBoolLit | _: TUnitLit => ()
      case TBinOp(op, l, r, p, _) =>
        if !allowedBinOps.contains(op) then
          err(s"const expression cannot use operator `$op`", p)
        else
          validateConstExpr(l); validateConstExpr(r)
      case TUnaryOp(op, x, p, _) =>
        if op != "-" && op != "not" then
          err(s"const expression cannot use unary `$op`", p)
        else validateConstExpr(x)
      case TJuxtapose(c, b, _, _) =>
        validateConstExpr(c); validateConstExpr(b)
      case TVarRef(s, p, _) =>
        s.kind match
          case SymKind.Prelude =>
            // Prelude constants are values (TyReal / TyComplex / TyBool /
            // TyInteger); prelude functions are TyFunc — those are
            // calls, not allowed in const expressions.
            s.tpe match
              case TyFunc(_, _) =>
                err(s"const expression cannot reference function `${s.name}`", p)
              case _ => ()
          case _ =>
            if !constSymIds.contains(s.id) then
              err(s"const expression cannot reference `${s.name}` — only other `const` bindings and prelude constants are allowed", p)
      case TCall(_, _, p, _) =>
        err("const expression cannot contain a function call (deferred to v1+)", p)
      case TLambda(_, _, p, _) =>
        err("const expression cannot be a lambda", p)
      case TArrayLit(_, p, _) =>
        err("const expression cannot construct an array", p)
      case TTuple(_, p, _) =>
        err("const expression cannot construct a tuple", p)
      case TIf(_, _, _, p, _) =>
        err("const expression cannot use `if`", p)
      case TStringLit(_, p, _) =>
        err("const expression cannot be a string literal in v0", p)
      case other =>
        err(s"const expression contains a non-constant form (${other.getClass.getSimpleName})", other.pos)

  private def registerDeferredLambda(sym: Symbol, v: TExpr): Unit = v match
    case lam: TLambda if isPartiallyInferredLambda(lam) =>
      deferredLambdas(sym.id) = lam
    case _ => ()

  /** Assignment-compatibility check used at val/var/const sites and at
    * `assignment-expression` sites. If `expected` is numeric and the
    * supplied value's type is also numeric and assignable via promotion,
    * we accept. Otherwise we require structural equality.
    */
  private def checkAssignable(v: TExpr, expected: Type): Unit =
    val actual = v.tpe
    if actual == TyUnknown || expected == TyUnknown then return
    if actual == expected then return
    (actual, expected) match
      case (a, b) if isNumeric(a) && isNumeric(b) =>
        promote(a, b) match
          case Some(t) if t == b => () // a promotes up to b
          case _                  => err(s"cannot assign value of type $actual to $expected", v.pos)
      case _ =>
        err(s"cannot assign value of type $actual to $expected", v.pos)

  /** Like [[checkAssignable]] but returns the (possibly-coerced) value
    * instead of just validating. When `v` is narrower than `expected`
    * and they're both numeric, wrap `v` in the appropriate prelude
    * conversion (`to_real` / `to_complex`) so downstream codegen sees a
    * value of the expected type. When no coercion is needed, returns
    * `v` unchanged; when the assignment is invalid, emits the same
    * diagnostic as [[checkAssignable]] and returns `v` so elaboration
    * can continue.
    *
    * Used at val/top-binding, assign-target, and function-arg sites so
    * that mixed-type assignments like `val z: complex = 1.0` or
    * `f(1)` where `f: (real -> ...)` don't surface to the codegen as
    * type-mismatched stores.
    */
  private def coerceTo(v: TExpr, expected: Type): TExpr =
    val actual = v.tpe
    if actual == TyUnknown || expected == TyUnknown then v
    else if actual == expected then v
    else (actual, expected) match
      case (a, b) if isNumeric(a) && isNumeric(b) =>
        promote(a, b) match
          case Some(t) if t == b =>
            // Wrap in `to_<expected>(v)`. The prelude registers these
            // under [[SymKind.Prelude]] at startup, so they're always
            // in scope at the root level.
            synthCoerce(v, b)
          case _ =>
            err(s"cannot assign value of type $actual to $expected", v.pos)
            v
      case _ =>
        err(s"cannot assign value of type $actual to $expected", v.pos)
        v

  /** Wrap `v` in a synthetic call to the appropriate prelude conversion
    * function. Returns `v` unchanged if no conversion exists for the
    * target type or if the prelude name can't be resolved (defensive —
    * registerPrelude always installs these). Uses [[current]] to walk
    * the scope chain so prelude names resolve from anywhere.
    *
    * Constant folding: when `v` is a literal whose promotion result is
    * also expressible as a literal, fold to the literal directly so
    * neither the interpreter nor the AOT codegen has to round-trip
    * through the runtime conversion. Most common case is `val x: real
    * = 1` → `val x = 1.0` instead of `val x = to_real(1)`.
    *  - `TIntLit(n) → TyReal`     ⇒ `TRealLit(n.toDouble)`
    *  - `TBoolLit(b) → TyReal`    is not a valid promotion (rejected by
    *    [[promote]]), so no fold needed.
    *  - Promotions targeting `TyComplex` keep the wrapping `TCall` —
    *    Nex's AST has no complex-literal node, and clang's `-O1`
    *    constant folder collapses the resulting insertvalue chain into
    *    an aggregate constant anyway.
    */
  private def synthCoerce(v: TExpr, target: Type): TExpr =
    // Compile-time literal fold for the most common case.
    (v, target) match
      case (TIntLit(n, p, _), TyReal) => return TRealLit(n.toDouble, p)
      case _ => ()

    val fnName = target match
      case TyReal    => "to_real"
      case TyComplex => "to_complex"
      case _         => return v
    current.lookup(fnName) match
      case Some(sym) => TCall(TVarRef(sym, v.pos, sym.tpe), List(v), v.pos, target)
      case None      => v

  // -- expressions ---------------------------------------------------------

  private def infExpr(e: TExpr): TExpr = e match
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit | _: TUnitLit => e

    case TInterpStringLit(parts, p, _) =>
      // `${...}` re-parse happens in Stage 1, so the parts list may
      // contain `TInterpExpr(x)` holding a freshly elaborated subtree.
      // Those subtrees need the same Stage-2 inference treatment any
      // other expression gets, or their types stay TyUnknown and the
      // mut-call-site check skips them. `TInterpRef` also needs a
      // refresh — its captured Symbol is a Stage-1 snapshot.
      val inferredParts = parts.map {
        case TInterpExpr(x) => TInterpExpr(infExpr(x))
        case TInterpRef(s)  => TInterpRef(refreshSym(s))
        case other          => other
      }
      TInterpStringLit(inferredParts, p, TyString)

    case TVarRef(s, p, _) =>
      // Refresh both `.sym` and `.tpe`. Stage 1 captured the symbol
      // before its type was inferred; if we only refresh `.tpe` then
      // `ref.sym.tpe` is stale and downstream walkers see the wrong
      // type. Same bug class as the for-loop loopVars staleness.
      val freshSym = refreshSym(s)
      TVarRef(freshSym, p, freshSym.tpe)

    case TBinOp(op, l, r, p, _) =>
      val ll = infExpr(l); val rr = infExpr(r)
      inferBinOp(op, ll, rr, p)

    case TUnaryOp(op, x, p, _) =>
      val xx = infExpr(x)
      inferUnaryOp(op, xx, p)

    case TJuxtapose(c, b, p, _) =>
      // Treat as multiplication for inference; Stage 3 will lower the node.
      val cc = infExpr(c); val bb = infExpr(b)
      inferBinOp("*", cc, bb, p) match
        case TBinOp("*", l, r, pp, t) => TJuxtapose(l, r, pp, t)
        case other                    => other // already an element-wise/broadcast form

    case TCall(callee, args, p, _) =>
      val cc = infExpr(callee)
      // Prelude HOFs (map/reduce/filter) have TyUnknown signatures in v0
      // because the type system has no type variables. But their shapes
      // are fixed and well-known, so we can hand-roll bidirectional
      // inference for them — push the array's element type into the
      // lambda arg and compute a sensible result type.
      cc match
        case TVarRef(s, _, _) if isPreludeHOF(s) =>
          inferPreludeHOFCall(s.name, cc, args, p)
        case TVarRef(s, _, _) if isPreludeRank1Only(s) =>
          inferPreludeRank1Call(s.name, cc, args, p)
        case _ =>
          val aa = cc.tpe match
            case TyFunc(params, _) if params.size == args.size =>
              args.zip(params).map { case (a, (pt, _)) => inferArg(a, pt) }
            case _ =>
              args.map(infExpr)
          inferCall(cc, aa, p)

    case TIndex(arr, idx, p, _) =>
      val aa = infExpr(arr); val ii = idx.map(infExpr)
      inferIndex(aa, ii, p)

    case TField(r, name, p, _) =>
      val rr = infExpr(r)
      inferField(rr, name, p)

    case TTupleProj(r, idx, p, _) =>
      val rr = infExpr(r)
      val ty = rr.tpe match
        case TyTuple(elems) if idx < elems.size => elems(idx)
        case TyTuple(elems) =>
          err(s"tuple destructuring binds ${idx + 1} names but the value has only ${elems.size} elements", p)
          TyUnknown
        case TyUnknown => TyUnknown
        case other     =>
          err(s"tuple destructuring requires a tuple value, got $other", p)
          TyUnknown
      TTupleProj(rr, idx, p, ty)

    case TMethodCall(r, n, args, p, _) =>
      // Stage 3 will lower this into either a field access or a function
      // call `n(r, args...)`. Meanwhile, push expected param types into
      // unannotated lambda args so `xs.foo(x -> x + 1)` works. Three
      // shapes are handled here:
      //   1. `n` resolves to a top-level user function with a concrete
      //      TyFunc signature — push from `params.tail` into `args`.
      //   2. `n` is one of the prelude HOFs (map/reduce/filter) — use
      //      [[inferPreludeHOFCall]] with the receiver as the first arg.
      //   3. Otherwise infer args generically.
      val rr = infExpr(r)
      current.lookup(n) match
        case Some(sym) if isPreludeHOF(sym) =>
          // Reuse the prelude HOF path. The lowered call form is
          // `n(receiver, args...)`, so we synthesize that shape here and
          // let inferPreludeHOFCall do the push-down. We re-wrap as a
          // TMethodCall so Stage 3 can still recognize and dispatch it.
          val syntheticCallee = TVarRef(sym, p, currentType(sym))
          val tcall = inferPreludeHOFCall(n, syntheticCallee, r :: args, p).asInstanceOf[TCall]
          TMethodCall(rr, n, tcall.args.tail, p, tcall.tpe)
        case Some(sym) if isPreludeRank1Only(sym) =>
          // Same routing for the rank-1-only prelude helpers — `xs.dot(ys)`,
          // `xs.enumerate()`, `xs.zip(ys)` all need the rank guard.
          val syntheticCallee = TVarRef(sym, p, currentType(sym))
          val tcall = inferPreludeRank1Call(n, syntheticCallee, r :: args, p).asInstanceOf[TCall]
          TMethodCall(rr, n, tcall.args.tail, p, tcall.tpe)
        case Some(sym) =>
          val aa = currentType(sym) match
            case TyFunc(params, _) if params.size == args.size + 1 =>
              args.zip(params.tail).map { case (a, (pt, _)) => inferArg(a, pt) }
            case _ =>
              args.map(infExpr)
          TMethodCall(rr, n, aa, p, TyUnknown)
        case None =>
          TMethodCall(rr, n, args.map(infExpr), p, TyUnknown)

    case TLambda(params, body, p, _) =>
      // No expected type at this position — params keep whatever type they
      // had at Stage 1 (annotated or TyUnknown). Refresh the param Symbols
      // so downstream consumers don't see a stale `.tpe`; for bound-then-
      // called lambdas with annotations, the annotation already set the
      // type at Stage 1 mint time.
      val body2       = infExpr(body)
      val freshParams = params.map(refreshSym)
      val ty          = TyFunc(freshParams.map(s => (s.tpe, ParamMode.Read)), body2.tpe)
      TLambda(freshParams, body2, p, ty)

    case TTuple(elems, p, _) =>
      val es = elems.map(infExpr)
      TTuple(es, p, TyTuple(es.map(_.tpe)))

    case TArrayLit(Nil, p, _) =>
      // Spec §4.13: empty rank-1 literals require a type annotation. If
      // we reached `infExpr` for an empty literal that means no expected
      // type was pushed in — emit an error so the user is told to annotate.
      // [[inferArg]] handles the `val x: [real] = []` happy path before
      // this case ever fires.
      err("empty array literal requires a type annotation (e.g. `val x: [real] = []`)", p)
      TArrayLit(Nil, p, TyArray(TyUnknown, 1))

    case TArrayLit(elems, p, _) =>
      val es = elems.map(infExpr)
      val et = lubTypes(es.map(_.tpe))
      val (elemT, rank) = et match
        case TyArray(inner, 1) => (inner, 2) // rank-2: array of rank-1
        case other             => (other, 1)
      TArrayLit(es, p, TyArray(elemT, rank))

    case TIf(c, t, eO, p, _) =>
      val cc = infExpr(c)
      requireBool(cc, "if condition")
      val tt = infExpr(t)
      eO match
        case None =>
          // unit branch — both must be unit
          if tt.tpe != TyUnknown && tt.tpe != TyUnit then
            err(s"if-without-else branch must be unit, got ${tt.tpe}", tt.pos)
          TIf(cc, tt, None, p, TyUnit)
        case Some(eb) =>
          val ee = infExpr(eb)
          val ty = lubTypes(List(tt.tpe, ee.tpe))
          TIf(cc, tt, Some(ee), p, ty)

    case TFor(loopVars, iter, body, p, _) =>
      val it = infExpr(iter)
      // Pin the freshly-typed Symbols back into `loopVars` — `setSymType`
      // returns the new immutable Symbol with the updated `tpe`, and the
      // TFor field must hold those updated copies so anything walking the
      // typed AST (codegen, analysis) sees the right loop-var types.
      val typedLoopVars: List[Symbol] = it.tpe match
        case TyArray(elem, 1) if loopVars.size == 1 =>
          List(setSymType(loopVars.head, elem))
        case TyArray(TyTuple(elems), 1) if loopVars.size == elems.size =>
          // `for i, x in [(1, 1.5), (2, 2.5)] do ...` — destructure
          // tuple-typed elements across the loop vars one-to-one.
          loopVars.zip(elems).map { case (s, t) => setSymType(s, t) }
        case TyArray(TyTuple(elems), 1) =>
          err(s"for-loop tuple destructuring binds ${loopVars.size} names but each element has ${elems.size}", it.pos)
          loopVars
        case TyArray(elem, 1) =>
          // Single-array-of-non-tuple but multiple loop vars — user
          // wrote e.g. `for a, b in xs` over a non-tuple array.
          err(s"for-loop binds ${loopVars.size} names but each element is $elem, not a tuple", it.pos)
          loopVars
        case TyArray(_, _) => loopVars // rank-2 iteration not yet defined
        case TyUnknown     => loopVars
        case other         =>
          err(s"for-loop iterable must be an array, got $other", it.pos)
          loopVars
      val bb = infExpr(body)
      TFor(typedLoopVars, it, bb, p, TyUnit)

    case TWhile(c, b, p, _) =>
      val cc = infExpr(c); requireBool(cc, "while condition")
      TWhile(cc, infExpr(b), p, TyUnit)

    case TReturn(v, p, _) =>
      TReturn(v.map(infExpr), p, TyUnit)

    case TAssign(t, v, p, _) =>
      val tt = infExpr(t); val vv = infExpr(v)
      val vvCoerced = if tt.tpe != TyUnknown then coerceTo(vv, tt.tpe) else vv
      TAssign(tt, vvCoerced, p, TyUnit)

    case TBlock(items, result, p, _) =>
      val its = items.map(inferBlockItem)
      val rr  = infExpr(result)
      TBlock(its, rr, p, rr.tpe)

    // Element-wise & matmul nodes don't appear in Stage 1 output; they
    // get introduced here. If we see them in a second-pass scenario,
    // pass through. TFusedLoop is similar — introduced by NexFusion
    // (Stage 4, post-lowering), never present during Stage 2 today,
    // but pass it through defensively in case the pipeline is rerun.
    case _: TElementWise | _: TBroadcast | _: TMap | _: TReduce | _: TMatMul | _: TFusedLoop | _: TFlatIndex | _: TSlice | _: TSlice2 | _: TAxisAllMark | _: TClone => e

  private def inferBlockItem(i: TBlockItem): TBlockItem = i match
    case TBlockBinding(s, kind, v) =>
      val declared = currentType(s)
      val vv =
        if declared != TyUnknown then inferArg(v, declared)
        else infExpr(v)
      val (vvCoerced, t) = declared match
        case TyUnknown => (vv, vv.tpe)
        case _         => (coerceTo(vv, declared), declared)
      val s2 = setSymType(s, t)
      registerDeferredLambda(s2, vvCoerced)
      TBlockBinding(s2, kind, vvCoerced)
    case TBlockExpr(x) => TBlockExpr(infExpr(x))

  // -- helpers --------------------------------------------------------------

  /** Least-upper-bound of a list of types under the promotion lattice
    * and structural equality. For numeric mixes, returns the highest type.
    * For mixed non-numeric / non-equal: returns the first non-unknown type
    * and emits no error (Stage 2 is permissive about array literal types
    * unless explicitly mismatched).
    */
  private def lubTypes(ts: List[Type]): Type =
    val concrete = ts.filterNot(_ == TyUnknown)
    if concrete.isEmpty then TyUnknown
    else
      concrete.tail.foldLeft(concrete.head) { (acc, t) =>
        if acc == t then acc
        else (promote(acc, t)) match
          case Some(p) => p
          case None    => acc
      }

  private def requireBool(e: TExpr, ctx: String): Unit =
    if e.tpe != TyUnknown && e.tpe != TyBool then
      err(s"$ctx must be bool, got ${e.tpe}", e.pos)

  private def inferBinOp(op: String, l: TExpr, r: TExpr, p: Option[Position]): TExpr =
    val lt = l.tpe; val rt = r.tpe

    // matrix multiplication
    if op == "@" then
      return TMatMul(l, r, p, matMulType(lt, rt, p))

    // logical
    if op == "and" || op == "or" then
      requireBool(l, s"left operand of `$op`")
      requireBool(r, s"right operand of `$op`")
      return TBinOp(op, l, r, p, TyBool)

    // comparison
    if Set("==", "!=", "<", "<=", ">", ">=").contains(op) then
      return inferCompare(op, l, r, p)

    // range
    if op == ".." || op == "..=" then
      if lt != TyUnknown && lt != TyInteger then err("range bound must be integer", l.pos)
      if rt != TyUnknown && rt != TyInteger then err("range bound must be integer", r.pos)
      return TBinOp(op, l, r, p, TyArray(TyInteger, 1))

    // arithmetic with arrays → element-wise / broadcast
    if isArrayArith(op) then
      (elemOf(lt), elemOf(rt)) match
        case (Some((le, lr)), Some((re, rr))) if lr == rr =>
          val resultElem = promote(le, re).getOrElse(le)
          return TElementWise(op, l, r, p, TyArray(resultElem, lr))
        case (Some((_, _)), None) if isNumeric(rt) || rt == TyUnknown =>
          // `arr op scalar` — scalar is on the right.
          return TBroadcast(r, l, op, scalarFirst = false, p, lt)
        case (None, Some((_, _))) if isNumeric(lt) || lt == TyUnknown =>
          // `scalar op arr` — scalar is on the left.
          return TBroadcast(l, r, op, scalarFirst = true, p, rt)
        case _ => ()

    // scalar arithmetic
    val t = inferArith(op, lt, rt, p)
    TBinOp(op, l, r, p, t)

  private def isArrayArith(op: String): Boolean =
    Set("+", "-", "*", "/", "%", "div", "^").contains(op)

  private def inferArith(op: String, lt: Type, rt: Type, p: Option[Position]): Type =
    op match
      case "div" | "%" =>
        if lt != TyUnknown && lt != TyInteger then
          err(s"`$op` requires integer operands, got $lt", p)
        if rt != TyUnknown && rt != TyInteger then
          err(s"`$op` requires integer operands, got $rt", p)
        TyInteger
      case "/" =>
        // real division — promote both to at least real
        if !(isNumeric(lt) || lt == TyUnknown) then
          err(s"`/` requires numeric operands, got $lt", p)
        if !(isNumeric(rt) || rt == TyUnknown) then
          err(s"`/` requires numeric operands, got $rt", p)
        promote(lt, rt) match
          case Some(TyInteger) => TyReal
          case Some(t)         => t
          case None            => TyUnknown
      case _ =>
        if !(isNumeric(lt) || lt == TyUnknown) then
          err(s"`$op` requires numeric operands, got $lt", p)
        if !(isNumeric(rt) || rt == TyUnknown) then
          err(s"`$op` requires numeric operands, got $rt", p)
        promote(lt, rt).getOrElse(TyUnknown)

  private def inferCompare(op: String, l: TExpr, r: TExpr, p: Option[Position]): TExpr =
    val lt = l.tpe; val rt = r.tpe
    // Array element-wise comparison
    (elemOf(lt), elemOf(rt)) match
      case (Some((_, lr)), Some((_, rr))) if lr == rr =>
        return TElementWise(op, l, r, p, TyArray(TyBool, lr))
      case (Some((_, lr)), None) if isNumeric(rt) || rt == TyUnknown =>
        return TBroadcast(r, l, op, scalarFirst = false, p, TyArray(TyBool, lr))
      case (None, Some((_, rr))) if isNumeric(lt) || lt == TyUnknown =>
        return TBroadcast(l, r, op, scalarFirst = true, p, TyArray(TyBool, rr))
      case _ => ()
    op match
      case "==" | "!=" =>
        // permissive: any equal pair, plus numeric pairs that promote
        if lt == TyUnknown || rt == TyUnknown || lt == rt then ()
        else if isNumeric(lt) && isNumeric(rt) then ()
        else err(s"cannot compare $lt and $rt", p)
        TBinOp(op, l, r, p, TyBool)
      case _ =>
        // ordered comparison — only on ordered numerics (no complex)
        if lt == TyUnknown || rt == TyUnknown then ()
        else if (lt == TyInteger || lt == TyReal) && (rt == TyInteger || rt == TyReal) then ()
        else err(s"`$op` requires ordered numeric operands, got $lt and $rt", p)
        TBinOp(op, l, r, p, TyBool)

  private def matMulType(lt: Type, rt: Type, p: Option[Position]): Type =
    (lt, rt) match
      case (TyArray(le, 2), TyArray(re, 2)) => TyArray(promote(le, re).getOrElse(le), 2)
      case (TyArray(le, 2), TyArray(re, 1)) => TyArray(promote(le, re).getOrElse(le), 1)
      case (TyArray(le, 1), TyArray(re, 2)) => TyArray(promote(le, re).getOrElse(le), 1)
      case (TyArray(le, 1), TyArray(re, 1)) => promote(le, re).getOrElse(le)
      case (TyUnknown, _) | (_, TyUnknown)  => TyUnknown
      case _ =>
        err(s"`@` requires array operands, got $lt and $rt", p); TyUnknown

  private def inferUnaryOp(op: String, x: TExpr, p: Option[Position]): TExpr =
    op match
      case "-" =>
        if !(isNumeric(x.tpe) || x.tpe == TyUnknown || (elemOf(x.tpe).exists(e => isNumeric(e._1)))) then
          err(s"unary `-` requires numeric operand, got ${x.tpe}", x.pos)
        TUnaryOp(op, x, p, x.tpe)
      case "not" =>
        requireBool(x, "operand of `not`")
        TUnaryOp(op, x, p, TyBool)
      case _ =>
        TUnaryOp(op, x, p, TyUnknown)

  /** Prelude higher-order functions whose shape is fixed enough for
    * bidirectional inference even though their registered signature is
    * TyUnknown. Limited to those that actually take a lambda — adding more
    * is cheap but needs a per-name entry in [[inferPreludeHOFCall]].
    */
  private val preludeHOFNames: Set[String] = Set("map", "flatMap", "reduce", "filter")

  private def isPreludeHOF(s: Symbol): Boolean =
    s.kind == SymKind.Prelude && preludeHOFNames.contains(s.name)

  /** Prelude functions whose runtime only accepts rank-1 input. Calls with
    * rank-2 (or higher) sources should error at elaborate time rather than
    * trap with an unhelpful runtime message. Each entry needs a matching
    * branch in [[inferPreludeRank1Call]] that supplies the result type.
    */
  private val preludeRank1OnlyNames: Set[String] = Set("dot", "enumerate", "zip")

  private def isPreludeRank1Only(s: Symbol): Boolean =
    s.kind == SymKind.Prelude && preludeRank1OnlyNames.contains(s.name)

  /** Reject rank-2+ array arguments and supply a concrete result type for
    * rank-1-only prelude functions. Without this, calls fall through to
    * the generic `inferCall` which returns `TyUnknown` and lets the
    * runtime trap with "expects rank-1" — fine for users who never made
    * the mistake, hostile to users who did.
    */
  private def inferPreludeRank1Call(
    name: String,
    callee: TExpr,
    args: List[TExpr],
    p: Option[Position],
  ): TExpr =
    val aa = args.map(infExpr)

    def requireRank1(arg: TExpr, ctx: String): Unit = arg.tpe match
      case TyArray(_, r) if r > 1 =>
        err(s"$name $ctx requires a rank-1 array, got rank $r", p)
      case _ =>

    name match
      case "dot" if aa.size == 2 =>
        requireRank1(aa(0), "first argument")
        requireRank1(aa(1), "second argument")
        val elemT = elemOf(aa(0).tpe).map(_._1).getOrElse(TyUnknown)
        TCall(callee, aa, p, elemT)

      case "enumerate" if aa.size == 1 =>
        requireRank1(aa(0), "argument")
        val elemT = elemOf(aa(0).tpe).map(_._1).getOrElse(TyUnknown)
        TCall(callee, aa, p, TyArray(TyTuple(List(TyInteger, elemT)), 1))

      case "zip" if aa.size == 2 =>
        requireRank1(aa(0), "first argument")
        requireRank1(aa(1), "second argument")
        val aT = elemOf(aa(0).tpe).map(_._1).getOrElse(TyUnknown)
        val bT = elemOf(aa(1).tpe).map(_._1).getOrElse(TyUnknown)
        TCall(callee, aa, p, TyArray(TyTuple(List(aT, bT)), 1))

      case _ =>
        TCall(callee, aa, p, TyUnknown)

  /** Hand-rolled bidirectional inference for the prelude HOFs:
    *   - `map(arr, f)`       — f: (elem -> U); result: [U]
    *   - `reduce(arr, init, f)` — f: (U, elem) -> U; result: U
    *   - `filter(arr, f)`    — f: (elem -> bool); result: same array type
    *
    * Each branch infers non-lambda args via [[infExpr]], extracts the
    * array's element type, builds the expected `TyFunc` for the lambda
    * arg, and pushes it down via [[inferArg]]. The result type of the
    * call is computed when enough is known; otherwise stays TyUnknown.
    */
  private def inferPreludeHOFCall(
    name: String,
    callee: TExpr,
    args: List[TExpr],
    p: Option[Position],
  ): TExpr = name match
    case "map" if args.size == 2 =>
      val arr           = infExpr(args.head)
      val (elemT, srcR) = elemOf(arr.tpe).getOrElse((TyUnknown, 1))
      val f             = inferArg(args(1), TyFunc(List((elemT, ParamMode.Read)), TyUnknown))
      // `mapArray` preserves the source's rank at runtime — VArray2 in
      // gives VArray2 out — so the result type must do the same. Hard-
      // coding rank-1 here would break shape-preservation downstream
      // (notably the fusion pass, which keys off this `tpe`).
      val outT          = f.tpe match
        case TyFunc(_, ret) if ret != TyUnknown => TyArray(ret, srcR)
        case _                                  => TyUnknown
      TCall(callee, List(arr, f), p, outT)

    // `flatMap(arr, f)` where `f: T -> [U]`. Result is `[U]` —
    // always rank-1 since the inner arrays are concatenated. Reject
    // rank-2 sources at elaborate time (semantics on a matrix would
    // be ambiguous — flat-iterate the elements? concat the rows?).
    case "flatMap" if args.size == 2 =>
      val arr   = infExpr(args.head)
      arr.tpe match
        case TyArray(_, r) if r > 1 =>
          err(s"flatMap requires a rank-1 array, got rank $r", p)
        case _ =>
      val elemT = elemOf(arr.tpe).map(_._1).getOrElse(TyUnknown)
      val f     = inferArg(
        args(1),
        TyFunc(List((elemT, ParamMode.Read)), TyArray(TyUnknown, 1)),
      )
      val outT = f.tpe match
        case TyFunc(_, TyArray(u, _)) if u != TyUnknown => TyArray(u, 1)
        case _                                          => TyUnknown
      TCall(callee, List(arr, f), p, outT)

    case "reduce" if args.size == 3 =>
      val arr   = infExpr(args.head)
      val init  = infExpr(args(1))
      val elemT = elemOf(arr.tpe).map(_._1).getOrElse(TyUnknown)
      val accT  = init.tpe
      val f     = inferArg(
        args(2),
        TyFunc(List((accT, ParamMode.Read), (elemT, ParamMode.Read)), TyUnknown),
      )
      TCall(callee, List(arr, init, f), p, accT)

    case "filter" if args.size == 2 =>
      val arr = infExpr(args.head)
      // `filter` semantics on rank-2 are ambiguous (filter rows? filter
      // elements and flatten?); the runtime only handles `VArray1`. Reject
      // rank-2+ at elaborate time rather than letting it trap later.
      arr.tpe match
        case TyArray(_, r) if r > 1 =>
          err(s"filter requires a rank-1 array, got rank $r", p)
        case _ =>
      val elemT = elemOf(arr.tpe).map(_._1).getOrElse(TyUnknown)
      val f     = inferArg(args(1), TyFunc(List((elemT, ParamMode.Read)), TyBool))
      TCall(callee, List(arr, f), p, TyArray(elemT, 1))

    case _ =>
      // Wrong arity for a known HOF — fall back to default inference and
      // let the runtime / future arity-check report it.
      val aa = args.map(infExpr)
      TCall(callee, aa, p, TyUnknown)

  private def inferCall(callee: TExpr, args: List[TExpr], p: Option[Position]): TExpr =
    callee match
      case TVarRef(s, _, _) if s.kind == SymKind.TypeName =>
        // Struct construction.
        currentType(s) match
          case TyStruct(_, fs) =>
            if fs.size != args.size then
              err(s"struct `${s.name}` expects ${fs.size} args, got ${args.size}", p)
              TCall(callee, args, p, currentType(s))
            else
              val coercedArgs = fs.zip(args).map { case ((_, ft), a) => coerceTo(a, ft) }
              TCall(callee, coercedArgs, p, currentType(s))
          case _ => TCall(callee, args, p, TyUnknown)

      case _ =>
        callee.tpe match
          case TyFunc(params, ret) =>
            if params.size != args.size then
              err(s"function call expects ${params.size} args, got ${args.size}", p)
              TCall(callee, args, p, ret)
            else
              val coercedArgs = params.zip(args).map { case ((pt, _), a) => coerceTo(a, pt) }
              TCall(callee, coercedArgs, p, ret)
          case _ =>
            // Fallback for prelude functions whose signatures aren't in
            // [[TyFunc]] form yet. We don't refine the param types here
            // (they'd need overload resolution), but we DO supply a
            // return type where we know one — so a containing function's
            // inferred return type isn't poisoned by TyUnknown bubbling
            // up from `print` / `assert` / etc.
            val ret = callee match
              case TVarRef(s, _, _) if s.kind == SymKind.Prelude =>
                preludeReturnTypeFor(s.name, args)
              case _ => TyUnknown
            TCall(callee, args, p, ret)

  /** Lookup table for prelude functions whose return type is known
    * statically and doesn't depend on argument types. HOFs (map/reduce/
    * filter), Rank-1 calls (sum/dot/enumerate), construction (zeros/
    * ones), and scalar math (sqrt/sin) have their own inference paths
    * elsewhere — this table only covers the unit-returning side-effect
    * functions and conversions.
    */
  private val preludeReturnType: Map[String, Type] = Map(
    "print"          -> TyUnit,
    "assert"         -> TyUnit,
    "assert_eq"      -> TyUnit,
    "assert_approx"  -> TyUnit,
    "assert_traps"   -> TyUnit,
    "format"         -> TyString,
    "to_integer"     -> TyInteger,
    "to_real"        -> TyReal,
    "to_complex"     -> TyComplex,
    "length"         -> TyInteger,
    "rows"           -> TyInteger,
    "cols"           -> TyInteger,
    // §10.2 scalar math — all return real regardless of arg type.
    "sqrt" -> TyReal, "cbrt" -> TyReal,
    "exp"  -> TyReal, "log"  -> TyReal, "log2" -> TyReal, "log10" -> TyReal,
    "sin"  -> TyReal, "cos"  -> TyReal, "tan"  -> TyReal,
    "asin" -> TyReal, "acos" -> TyReal, "atan" -> TyReal, "atan2" -> TyReal,
    "sinh" -> TyReal, "cosh" -> TyReal, "tanh" -> TyReal,
    "asinh"-> TyReal, "acosh"-> TyReal, "atanh"-> TyReal,
    "floor"-> TyReal, "ceil" -> TyReal, "round"-> TyReal, "trunc" -> TyReal,
    // §10.3 complex
    "arg"  -> TyReal,
  ).withDefaultValue(TyUnknown)

  /** Pick a return type for a prelude call. Most names are in
    * [[preludeReturnType]] directly; [[abs]] / [[sign]] / [[min]] /
    * [[max]] / [[conj]] depend on argument types, so they branch here.
    */
  private def preludeReturnTypeFor(name: String, args: List[TExpr]): Type =
    name match
      case "abs" =>
        args.headOption.map(_.tpe) match
          case Some(TyInteger) => TyInteger
          case Some(TyReal)    => TyReal
          case Some(TyComplex) => TyReal // |z| is the modulus, a real
          case _               => TyUnknown
      case "sign" =>
        args.headOption.map(_.tpe) match
          case Some(TyInteger) => TyInteger
          case Some(TyReal)    => TyReal
          case _               => TyUnknown
      case "min" | "max" =>
        (args.headOption.map(_.tpe), args.lift(1).map(_.tpe)) match
          case (Some(TyInteger), Some(TyInteger)) => TyInteger
          case (Some(TyReal), _) | (_, Some(TyReal)) => TyReal
          case _ => TyUnknown
      case "conj" =>
        args.headOption.map(_.tpe) match
          case Some(TyComplex) => TyComplex
          case Some(t)         => t
          case None            => TyUnknown
      // Spec §10.2 line 32: sin, cos, exp, log, sqrt apply to real
      // AND complex. The complex case returns complex; everything
      // else returns real (with sqrt(negative real) handled by the
      // interpreter at runtime).
      case "sin" | "cos" | "tan" | "exp" | "log" | "log2" | "log10" if args.size == 1 =>
        args.head.tpe match
          case TyComplex => TyComplex
          case _         => TyReal
      case "sqrt" | "cbrt" if args.size == 1 =>
        args.head.tpe match
          case TyComplex => TyComplex
          case _         => TyReal
      // §10.5 construction. `fill(n, v)` shape depends on n's type:
      //   - `n: integer`            → `[T]`  where T = v.tpe
      //   - `n: (integer, integer)` → `[[T]]`
      // Without an arg-type-aware path, the default `TyUnknown` would
      // make `print(fill(8, 0+0i))` silently drop the value.
      case "fill" if args.size == 2 =>
        val elemT = args(1).tpe
        args.head.tpe match
          case TyInteger                              => TyArray(elemT, 1)
          case TyTuple(List(TyInteger, TyInteger))    => TyArray(elemT, 2)
          case _                                       => TyUnknown
      // `zeros` / `ones` always produce integer arrays in the
      // interpreter; the rank is determined by the arg shape.
      case "zeros" | "ones" if args.size == 1 =>
        args.head.tpe match
          case TyInteger                              => TyArray(TyInteger, 1)
          case TyTuple(List(TyInteger, TyInteger))    => TyArray(TyInteger, 2)
          case _                                       => TyUnknown
      // §10.4 rank-1 reductions — return the element type of the
      // (first) array argument. Without an arg-aware path, the static
      // default would be TyUnknown and downstream `print(sum(xs))`
      // would silently drop the value.
      case "sum" | "product" if args.size == 1 =>
        args.head.tpe match
          case TyArray(e, 1) => e
          case _             => TyUnknown
      case "dot" if args.size == 2 =>
        args.head.tpe match
          case TyArray(e, 1) => e
          case _             => TyUnknown
      // §10.4 rank-1 builders — materialize a fresh array. range and
      // linspace are total-shape-known; enumerate/zip are handled in
      // inferPreludeRank1Call already.
      case "range" if args.size == 2 =>
        TyArray(TyInteger, 1)
      case "linspace" if args.size == 3 =>
        TyArray(TyReal, 1)
      case _ => preludeReturnType(name)

  private def inferIndex(arr: TExpr, idx: List[TExpr], p: Option[Position]): TExpr =
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
        TAxisRange(lo, hi, inclusive = op == "..=")
      case other =>
        if other.tpe != TyUnknown && other.tpe != TyInteger then
          err(s"array index must be integer, got ${other.tpe}", other.pos)
        TAxisIndex(other)

    /** True iff the index element is a range or axis-all (i.e. would
      * trigger a slice rewrite). */
    def isSliceMarker(e: TExpr): Boolean = e match
      case _: TAxisAllMark                                       => true
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
            TSlice(arr, lo, hi, inclusive = op == "..=", p, TyArray(e, 1))
          case TyArray(_, r) =>
            err(s"rank-1 slice requires a rank-1 array, got rank $r", p)
            TSlice(arr, lo, hi, inclusive = op == "..=", p, TyUnknown)
          case TyUnknown =>
            TSlice(arr, lo, hi, inclusive = op == "..=", p, TyUnknown)
          case other =>
            err(s"cannot slice value of type $other", p)
            TSlice(arr, lo, hi, inclusive = op == "..=", p, TyUnknown)

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
          case i if i.tpe != TyUnknown && i.tpe != TyInteger =>
            err(s"array index must be integer, got ${i.tpe}", i.pos)
          case _ => ()
        }
        val ty = arr.tpe match
          case TyArray(e, 1) if idx.size == 1 => e
          case TyArray(e, 2) if idx.size == 2 => e
          case TyArray(e, 2) if idx.size == 1 => TyArray(e, 1) // row slice
          case TyUnknown                      => TyUnknown
          case other =>
            err(s"cannot index value of type $other", p); TyUnknown
        TIndex(arr, idx, p, ty)

  private def inferField(r: TExpr, name: String, p: Option[Position]): TExpr =
    val ty = r.tpe match
      case TyStruct(_, fs) =>
        fs.find(_._1 == name).map(_._2) match
          case Some(t) => t
          case None    => err(s"no field `$name` on $r.tpe", p); TyUnknown
      case TyComplex if name == "re" || name == "im" => TyReal
      case TyUnknown => TyUnknown
      case other     => err(s"cannot access field `$name` on $other", p); TyUnknown
    TField(r, name, p, ty)

  // ==========================================================================
  // Stage 3: sugar lowering + mode validation
  // ==========================================================================

  /** Rewrite the typed AST to remove sugar that the interpreter would
    * otherwise need to handle:
    *
    *   - [[TJuxtapose]]  → [[TBinOp]] (`*`), or [[TBroadcast]] when one
    *     side is an array (§4.4 + §4.5 interaction)
    *   - [[TMethodCall]] → [[TField]] (when `name` is a struct field) or
    *     [[TCall]] (`name(receiver, args...)`) per §4.9
    *
    * Also validates mode rules on function parameters: a read-mode
    * parameter must not appear as an assignment target inside the body
    * (§6.4). Mutating sub-fields/sub-indices of a read-mode parameter is
    * also a violation. Mode rules for *passing* a value to a `mut`
    * parameter are a future-work item (Stage 3.5).
    *
    * Sugar items deferred to Stage 3+:
    *   - Tuple destructuring (`val a, b = pair`) — needs new typed-AST
    *     projection node
    *   - Interpolated `${...}` raw text re-parsing
    */
  private def lowerProgram(p: TProgram): TProgram =
    p.copy(decls = p.decls.map(lowerDecl))

  private def lowerDecl(d: TDecl): TDecl = d match
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

  private def lowerExpr(e: TExpr): TExpr = e match
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
  private def lowerMethodCall(
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
  private def validateModes(f: TFunDecl, body: TExpr): Unit =
    val readParams: Set[Int] =
      f.params.iterator
        .filter(s => paramModes.get(s.id).contains(ParamMode.Read))
        .map(_.id).toSet
    walkForMutations(body, readParams, f.params.map(s => s.id -> s.name).toMap)

  private def walkForMutations(e: TExpr, reads: Set[Int], names: Map[Int, String]): Unit =
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
  private def checkCallSiteModes(c: TCall): Unit =
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
  private def rootSym(e: TExpr): Option[Symbol] = e match
    case TVarRef(s, _, _)         => Some(s)
    case TField(r, _, _, _)       => rootSym(r)
    case TIndex(r, _, _, _)       => rootSym(r)
    case TTupleProj(r, _, _, _)   => rootSym(r)
    case _                        => None

  private def walkChildren(e: TExpr, reads: Set[Int], names: Map[Int, String]): Unit =
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
