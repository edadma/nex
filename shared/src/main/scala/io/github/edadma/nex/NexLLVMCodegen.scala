package io.github.edadma.nex

import scala.collection.mutable

/** First vertical slice of the AOT compiler — text-based LLVM IR
  * emission. The codegen takes a fully elaborated [[TProgram]] and
  * returns an LLVM IR module as a `String`. The CLI is responsible
  * for writing the `.ll`, shelling out to `clang` to produce a
  * native binary, and surfacing any tool errors.
  *
  * The implementation is split across several trait files in this
  * package; the public surface is just the [[compile]] method.
  *
  *   - [[NexLLVMState]]    — shared state, helpers, types, ARC tracking
  *   - [[NexLLVMPreamble]] — extern declarations + `__nex_init_globals`
  *   - [[NexLLVMLambdas]]  — closure subsystem
  *   - [[NexLLVMArrays]]   — array literal, index, ew, broadcast, slice, fused
  *   - [[NexLLVMPrint]]    — print helpers
  *   - [[NexLLVMPrelude]]  — libm dispatch + assertions
  *
  * What lives in this file: the top-level program driver, `emitExpr`
  * (the dispatch over typed AST nodes), function emission, control
  * flow (`if` / `while` / `for` / `return` / short-circuit `and` / `or`),
  * assignment, local bindings, user-function calls, tuples, and
  * structs — i.e. the small, self-contained nodes that don't pull
  * enough weight to deserve their own trait.
  *
  * Design notes:
  *   - One pass over the typed AST per top-level function. Each function
  *     gets its own register counter and a `locals` map (Symbol id →
  *     LLVM alloca pointer) reset on entry.
  *   - Bindings are always allocated on the entry block via `alloca`,
  *     then `store`d. References load each time — no SSA promotion;
  *     LLVM's `mem2reg` pass at `clang -O1+` does that for free.
  *   - String/bool literals use the obvious `i1` / `i8*` mapping; we
  *     don't yet have a string type to worry about.
  */
class NexLLVMCodegen
    extends NexLLVMState
    with NexLLVMPreamble
    with NexLLVMLambdas
    with NexLLVMArrays
    with NexLLVMPrint
    with NexLLVMAsserts
    with NexLLVMArrayPrelude
    with NexLLVMMatrixPrelude
    with NexLLVMControl
    with NexLLVMValueToString
    with NexLLVMEnums
    with NexLLVMPrelude:

  /** Compile a program. Returns the full LLVM IR module text. */
  def compile(tp: TProgram): String =
    emitPreamble()

    // Pass 0 (lambda pre-pass): collect every `var` binding's symbol id
    // (so lambda capture analysis can pick the right CaptureMode), then
    // walk every function body to assign each TLambda a synthetic
    // top-level name and compute its capture layout. Stored in
    // [[lambdaTable]] for use by the construction site and synthetic
    // function body emission below.
    collectVarBindings(tp)
    collectLambdas(tp)

    // Walk every enum declaration so variant-Symbol-id → (parent enum
    // type, tag index, fields) lookups in the value emission paths
    // resolve without needing the TEnumDecl on hand. Mirrors the
    // interpreter's `enumVariantInfo` map. Populates [[variantInfo]].
    for d <- tp.allDecls do d match
      case e: TEnumDecl =>
        // [[TyEnum]] stores variants by their declared *name*; the
        // [[TEnumDecl]] keeps the Symbol so the analyzer / interpreter
        // can resolve references by id. Build the type from the name
        // projection so it matches the type produced by the elaborator
        // for [[TVarRef]] sites.
        val variantsByName = e.variants.map { case (vs, fs) => (vs.name, fs) }
        val te = TyEnum(e.sym.name, variantsByName)
        e.variants.zipWithIndex.foreach { case ((vs, fs), idx) =>
          variantInfo(vs.id) = (te, idx, fs)
        }
      case _ => ()

    // Pass 1: emit `@<name> = global <T> zeroinitializer` for every
    // top-level binding. Records them in [[globalBindings]] so later
    // [[TVarRef]] / [[TAssign]] sites can route through load/store on
    // the global pointer. Initial values are filled in by the runtime
    // init function below — this matches the interpreter, which runs
    // every binding initializer at program start.
    // Top-level bindings live in either `decls` (user code) or
    // `auxDecls` (source prelude — e.g. the `const pi` / `const e`
    // declarations in `prelude/scalar.nex`). Both need global slots +
    // init.
    val topBindings = tp.allDecls.collect { case b: TTopBinding => b }
    for b <- topBindings do
      globalBindings += b.sym.id
      val ty = llvmType(b.sym.tpe)
      out.append(s"@${b.sym.name} = global $ty ${zeroInitFor(ty)}\n")
    if topBindings.nonEmpty then out.append("\n")

    // Pass 2: emit the init function (if there are any bindings), every
    // synthetic lambda function, and every user function. main has an
    // implicit call to the init function inserted before its body — see
    // [[emitFunction]].
    if topBindings.nonEmpty then emitInitFunction(topBindings)

    // Synthetic lambda bodies emit before user functions so they're
    // visible by name from the construction sites — but LLVM IR allows
    // forward references, so this ordering is for human readability only.
    emitLambdaFunctions()

    // Pre-scan: record every `@intrinsic` function so the call-site path
    // can dispatch inline. These functions get no LLVM wrapper emitted —
    // a wrapper would either collide with the libm symbol of the same
    // name (when the source `def` reuses the libm name verbatim) or
    // recurse into itself, so the call site emits the libm call directly.
    // Walk `allDecls` (user + source-prelude) so prelude-bundled
    // intrinsics get registered alongside user-defined ones.
    for d <- tp.allDecls do d match
      case f: TFunDecl =>
        f.body match
          case TIntrinsic(opId, _, _) => intrinsicFunctionOpIds(f.sym.id) = opId
          case _                       => ()
      case _ => ()

    // Overload-set name mangling. LLVM's symbol namespace is flat —
    // when two source `def`s share a name (function overloads), only
    // one can use the bare name in the emitted module. The libm-bridge
    // overload doesn't emit a body (skipped below); other overloads
    // get `<name>$<param-type-mangle>` so they coexist with the libm
    // declare and with each other. Singleton bindings keep their bare
    // names unchanged. See `mangleParamTypes` for the suffix shape.
    val nameCounts = scala.collection.mutable.Map.empty[String, Int]
    for d <- tp.allDecls do d match
      case f: TFunDecl => nameCounts(f.sym.name) = nameCounts.getOrElse(f.sym.name, 0) + 1
      case _           => ()
    for d <- tp.allDecls do d match
      case f: TFunDecl if nameCounts.getOrElse(f.sym.name, 0) > 1 =>
        // Intrinsic overloads in an overloaded set don't emit a body
        // (line 99 skip), so they don't need a mangled name — they
        // own the bare name via libm's `declare`. Non-intrinsic
        // members must mangle to avoid colliding.
        if !intrinsicFunctionOpIds.contains(f.sym.id) then
          val suffix = mangleParamTypes(f.params.map(_.tpe))
          llvmFuncNames(f.sym.id) = s"${f.sym.name}$$$suffix"
      case _ => ()

    for d <- tp.allDecls do d match
      case f: TFunDecl =>
        if !intrinsicFunctionOpIds.contains(f.sym.id) then emitFunction(f)
      case _: TTopBinding | _: TStructDecl | _: TEnumDecl | _: TModuleDecl | _: TImportDecl =>
        () // top-bindings already emitted as globals; type / module decls are metadata

    // Emit any per-element-type deep-dec helpers that were registered
    // by [[arrDecFor]] during function-body emission. The flush loop
    // is fixed-point — a deep dec for `[[String]]` registers an
    // inner helper for `[String]` while emitting.
    flushDeepDecs()

    // Emit any per-aggregate-type inc / drop helpers that were
    // registered during function-body / deep-dec emission. Same
    // fixed-point pattern: nested aggregates register their inner
    // helpers as their outer helper is emitted.
    flushAggHelpers()

    // A deep-dec body emitted by flushAggHelpers may have requested a
    // new array deep-dec (e.g., a struct holds `[String]` — the drop
    // helper calls `__nex_arr1_dec_str`). Drain once more so those
    // late-registered helpers land in the module too.
    flushDeepDecs()

    // Enum ARC helpers (inc / drop) for any TyEnum carrying refcounted
    // fields. Requested by [[emitArrInc]] / [[emitArrDec]] in
    // [[NexLLVMState]] and by the aggregate-field / deep-dec paths in
    // [[NexLLVMPreamble]]. Drained after the aggregate helpers so any
    // nested enum reachable through a tuple / struct / array field is
    // covered.
    flushEnumHelpers()

    // Enum print + value-to-string helpers — every TyEnum reached by
    // `print(...)` or by an interpolated `${enum}` site needs these.
    // Tracked separately from inc / drop because pure-value enums
    // still need formatting support.
    flushEnumPrintHelpers()

    // Flush the string-literal pool LAST. LLVM IR allows forward
    // references to module-level identifiers, so a function body that
    // uses `@.str.N` works even though the constant is defined below —
    // *but* the constants must actually be emitted somewhere in the
    // module, and any helper-flush above can intern fresh literals
    // (variant names, enum print fragments, etc.) that the flush
    // therefore has to cover. Emitting after every other flush makes
    // the cumulative pool the final list of `@.str.*` definitions.
    flushStringPool()

    // Flush per-top-level-def closure-shape thunks. A `def` used as a
    // function value (passed to a HOF, stored in a binding, etc.)
    // requested a wrapper that prepends an env arg the bare def
    // doesn't declare; emit the bodies now that every user function
    // is in the module.
    flushDefThunks()

    out.toString

  // ---------------------------------------------------------------------------
  // Function emission. `main` is special-cased: Nex's `def main() : unit`
  // maps to the LLVM `i32 @main()` entry, which always returns 0. Every
  // other user function emits `@<name>(<params>)` with the user-declared
  // return type.
  //
  // Param-passing strategy: each param is received in SSA form (`%arg0`,
  // `%arg1`, ...) and immediately spilled to an `alloca` so that
  // subsequent `TVarRef` loads work uniformly with `val` / `var`. The
  // alloca register is recorded in `locals` keyed by Symbol id; LLVM's
  // `mem2reg` at `-O1+` collapses the indirection back to SSA.
  // ---------------------------------------------------------------------------

  /** Read the parameter-mode list out of a function's declared TyFunc.
    * Defaults to Read for every position if the sym's type isn't a
    * TyFunc (shouldn't happen post-elaboration, but defensive).
    */
  private def paramModesOf(f: TFunDecl): List[ParamMode] =
    f.sym.tpe match
      case TyFunc(ps, _) if ps.size == f.params.size => ps.map(_._2)
      case _                                          => List.fill(f.params.size)(ParamMode.Read)

  private def emitFunction(f: TFunDecl): Unit =
    regCounter   = 0
    labelCounter = 0
    locals.clear()
    arrayLocalSlots.clear()
    blockArrayScopes = Nil
    boxedFunctionSlots.clear()
    blockBoxedScopes = Nil

    val isMain  = f.sym.name == "main" && f.params.isEmpty
    val retLLT  = if isMain then "i32" else llvmType(f.returnType)
    val funcId  = llvmFuncNameOf(f.sym)

    currentReturnType = f.returnType
    currentIsMain     = isMain

    // Mut params take a `ptr` to the caller's slot so callee writes
    // flow back to the caller's binding. Array-typed read params get
    // `noalias` because Nex's uniqueness type system (§8) guarantees
    // `[T]` references don't alias each other; telling LLVM unlocks
    // more aggressive load/store reordering. Mut params can't carry
    // noalias because the by-ref ptr can alias other arguments
    // through pointer chasing.
    val modes = paramModesOf(f)
    val paramSig =
      f.params.zipWithIndex
        .map { case (p, i) =>
          modes(i) match
            case ParamMode.Mut  => s"ptr %arg$i"
            case ParamMode.Read =>
              val attr = if isArrayType(p.tpe) then " noalias" else ""
              s"${llvmType(p.tpe)}$attr %arg$i"
        }
        .mkString(", ")

    out.append(s"define $retLLT @$funcId($paramSig) {\n")
    currentBlock = Some("entry")
    out.append("entry:\n")

    // main initializes all top-level bindings before running its body
    // — matches the interpreter's two-stage init (functions first via
    // module emission order, then top-binding initializers).
    if isMain && globalBindings.nonEmpty then
      emitLine("  call void @__nex_init_globals()\n")

    // Bind each param to a slot pointer for uniform TVarRef load/store.
    //  - Read mode: spill the SSA value into a fresh alloca; the slot
    //    takes ownership of any refcounted share the caller inc'd.
    //  - Mut mode: %arg is already the caller's slot ptr — use it
    //    directly, no alloca, no copy. Refcount lifecycles on mut
    //    params stay with the caller, so they DON'T get registered
    //    into arrayLocalSlots (the function-exit dec would over-
    //    release).
    for ((p, i) <- f.params.zipWithIndex) do
      modes(i) match
        case ParamMode.Mut =>
          locals(p.id) = s"%arg$i"
        case ParamMode.Read =>
          val ty   = llvmType(p.tpe)
          val slot = newReg()
          emitLine(s"  $slot = alloca $ty\n")
          emitLine(s"  store $ty %arg$i, ptr $slot\n")
          locals(p.id) = slot
          if isRefCountedType(p.tpe) then arrayLocalSlots(p.id) = (slot, p.tpe)

    f.body match
      case TIntrinsic(opId, _, _) =>
        emitIntrinsicBody(opId, f.params, f.returnType)
      case _ =>
        val result = emitExpr(f.body)

        // Emit the final ret only if no terminator has fired yet. Inside the
        // body, an early `return` (or both branches of an if that return)
        // will already have closed the block (with their own dec-locals).
        if currentBlock.isDefined then
          // If the function body produced an owning array value but we are
          // about to discard it (main returns i32, or fn returns void), dec
          // the result first so it doesn't leak.
          val bodyIsRefCounted = isRefCountedType(f.body.tpe)
          val willDiscardResult = isMain || f.returnType == TyUnit || result == "void"
          if bodyIsRefCounted && willDiscardResult && result != "0" && result != "void" then
            emitArrDec(result, f.body.tpe)

          decAllLocalArrays()

          if isMain then
            emitTerminator("  ret i32 0\n")
          else if f.returnType == TyUnit || result == "void" then
            emitTerminator("  ret void\n")
          else
            emitTerminator(s"  ret ${llvmType(f.returnType)} $result\n")

    out.append("}\n\n")

  /** Emit the function body for an `@intrinsic("opId")` declaration. The
    * params are already spilled to alloca slots in [[locals]] but we read
    * them from the raw `%arg<i>` SSA registers — there's no value in the
    * detour through memory for intrinsic shapes. Adds nothing to
    * `arrayLocalSlots` (intrinsics own no captured arrays).
    */
  private def emitIntrinsicBody(
      opId: String,
      params: List[Symbol],
      returnType: Type,
  ): Unit =
    NexIntrinsics.require(opId)
    opId match
      case "test.identity" =>
        if params.size != 1 then
          throw new RuntimeException(s"test.identity expects 1 param, got ${params.size}")
        emitTerminator(s"  ret ${llvmType(returnType)} %arg0\n")

      case "libm.cbrt" =>
        if params.size != 1 then
          throw new RuntimeException(s"libm.cbrt expects 1 param, got ${params.size}")
        val reg = newReg()
        emitLine(s"  $reg = call double @cbrt(double %arg0)\n")
        emitTerminator(s"  ret double $reg\n")

      case other =>
        throw new RuntimeException(
          s"intrinsic `$other` has no LLVM implementation — register one in NexLLVMCodegen.emitIntrinsicBody",
        )

  /** Call-site dispatch for an `@intrinsic` function. The user wrote
    * `f(args)` where `f` is registered in [[intrinsicFunctionOpIds]];
    * we emit the intrinsic body inline at the call site so no wrapper
    * function (which would collide with the libm symbol of the same
    * name) ever appears in the module.
    *
    * `liftToReal` is reused from the legacy prelude path so integer
    * arguments to `cbrt(8)`-style calls promote to `double` before the
    * libm call, matching the behaviour the SymKind.Prelude branch
    * provided when the entries lived in [[NexLLVMPrelude]].
    */
  protected def emitIntrinsicCall(opId: String, args: List[TExpr], resultT: Type): String =
    NexIntrinsics.require(opId)
    opId match
      case "test.identity" =>
        if args.size != 1 then
          throw new RuntimeException(s"test.identity expects 1 arg, got ${args.size}")
        emitExpr(args.head)

      case _ if libmUnaryName.isDefinedAt(opId) =>
        val xv  = liftToRealForIntrinsic(args.head)
        val reg = newReg()
        emitLine(s"  $reg = call double @${libmUnaryName(opId)}(double $xv)\n")
        reg

      case "libm.atan2" =>
        val yv = liftToRealForIntrinsic(args.head)
        val xv = liftToRealForIntrinsic(args(1))
        val reg = newReg()
        emitLine(s"  $reg = call double @atan2(double $yv, double $xv)\n")
        reg

      case "libm.hypot" =>
        val xv = liftToRealForIntrinsic(args.head)
        val yv = liftToRealForIntrinsic(args(1))
        val reg = newReg()
        emitLine(s"  $reg = call double @hypot(double $xv, double $yv)\n")
        reg

      case "libm.asinh" =>
        val xv = liftToRealForIntrinsic(args.head)
        val reg = newReg()
        emitLine(s"  $reg = call double @__nex_asinh(double $xv)\n")
        reg
      case "libm.acosh" =>
        val xv = liftToRealForIntrinsic(args.head)
        val reg = newReg()
        emitLine(s"  $reg = call double @__nex_acosh(double $xv)\n")
        reg
      case "libm.atanh" =>
        val xv = liftToRealForIntrinsic(args.head)
        val reg = newReg()
        emitLine(s"  $reg = call double @__nex_atanh(double $xv)\n")
        reg

      case other =>
        throw new RuntimeException(
          s"intrinsic `$other` has no LLVM implementation — register one in NexLLVMCodegen.emitIntrinsicCall",
        )

  /** Mapping from intrinsic opId to the libm function name for unary
    * real → real intrinsics that bridge to a direct libm call. asinh /
    * acosh / atanh route through the `__nex_*` wrappers (which compose
    * the analytic form because libm declarations are absent on some
    * targets) so they live in their own match arms above.
    */
  private val libmUnaryName: PartialFunction[String, String] =
    case "libm.cbrt"  => "cbrt"
    case "libm.sqrt"  => "sqrt"
    case "libm.exp"   => "exp"
    case "libm.log"   => "log"
    case "libm.log2"  => "log2"
    case "libm.log10" => "log10"
    case "libm.sin"   => "sin"
    case "libm.cos"   => "cos"
    case "libm.tan"   => "tan"
    case "libm.floor" => "floor"
    case "libm.ceil"  => "ceil"
    case "libm.round" => "round"
    case "libm.trunc" => "trunc"
    case "libm.asin"  => "asin"
    case "libm.acos"  => "acos"
    case "libm.atan"  => "atan"
    case "libm.sinh"  => "sinh"
    case "libm.cosh"  => "cosh"
    case "libm.tanh"  => "tanh"

  /** Same shape as NexLLVMPrelude.liftToReal but visible from
    * [[emitIntrinsicCall]]. The sibling helper is `private`; rather than
    * loosen its access, copy the four-line conversion locally — it has
    * no per-call-site state so the duplication is harmless.
    */
  private def liftToRealForIntrinsic(e: TExpr): String =
    val v = emitExpr(e)
    e.tpe match
      case TyInteger =>
        val r = newReg()
        emitLine(s"  $r = sitofp i64 $v to double\n")
        r
      case _ => v

  // ---------------------------------------------------------------------------
  // Expression emission. Returns the LLVM operand text for the value
  // (either an immediate like `42`, an SSA register like `%t3`, or
  // `void` for unit-typed expressions).
  // ---------------------------------------------------------------------------

  protected def emitExpr(e: TExpr): String = e match
    case TIntLit(v, _, _)  => v.toString
    case TRealLit(v, _, _) => formatReal(v)
    case TBoolLit(v, _, _) => if v then "1" else "0"
    case TUnitLit(_)       => "void"

    case TIntrinsic(opId, _, _) =>
      // TIntrinsic only appears as a function body and is consumed by
      // emitIntrinsicBody directly; if we reach this case in value
      // position the elaborator placed it somewhere illegal.
      notYet(s"intrinsic `$opId` in value position")
      "0"

    case TStringLit(s, _, _) =>
      // String literals lower to a private %nex_str descriptor with the
      // immortal-sentinel refcount; the SSA value is a pointer to that
      // descriptor. Identical literals share one descriptor (and one
      // byte-array global underneath it).
      internStringDescriptor(s)

    case TInterpStringLit(parts, _, _) =>
      // Build a fresh %nex_str by concat-chaining each part. Text parts
      // route through the literal descriptor pool (immortal); $ref and
      // ${expr} parts go through emitValueToString which produces a
      // heap-allocated descriptor for non-string types. Final result
      // is heap-allocated with refcount=1.
      emitInterpStringValue(parts)

    case TVarRef(s, _, t) if s.kind == SymKind.Prelude =>
      s.name match
        case "inf" => "0x7FF0000000000000"
        case "nan" => "0x7FF8000000000000"
        case "i"   =>
          // Complex unit: build `{ 0.0, 1.0 }` via insertvalue. Subsequent
          // arithmetic with reals/ints folds through the complex binop path.
          val c0 = newReg()
          emitLine(s"  $c0 = insertvalue { double, double } undef, double 0.0, 0\n")
          val c1 = newReg()
          emitLine(s"  $c1 = insertvalue { double, double } $c0, double 1.0, 1\n")
          c1
        case other => notYet(s"prelude reference `$other`"); "0"

    case TVarRef(s, _, _) if s.kind == SymKind.Function =>
      // Top-level def used as a value (passed as argument, stored in a
      // binding, returned, etc). Build the closure literal `{ thunk,
      // null }` so the indirect-call path through `emitClosureCall`
      // works uniformly — the thunk takes an env_ptr it ignores. The
      // thunk itself is emitted by [[flushDefThunks]] after the main
      // function loop completes.
      val thunkName = requestDefThunk(s)
      val c0 = newReg()
      emitLine(s"  $c0 = insertvalue { ptr, ptr } undef, ptr @$thunkName, 0\n")
      val c1 = newReg()
      emitLine(s"  $c1 = insertvalue { ptr, ptr } $c0, ptr null, 1\n")
      c1

    case TVarRef(s, _, _) if s.kind == SymKind.EnumVariant =>
      // Bare variant value (no fields). Reach this when source writes
      // `Red`, `Diverged`, etc. — including when used as a top-level
      // binding RHS, an arm result, or the scrutinee of a match.
      // Fielded variants applied through `Foo(a, b)` are reached via
      // the TCall case below; using a fielded variant as a first-class
      // value (HOF arg, etc.) is not yet supported.
      variantInfo.get(s.id) match
        case Some((te, idx, fs)) if fs.isEmpty =>
          emitVariantConstruct(te, idx, Nil)
        case Some((_, _, fs)) =>
          notYet(s"reference to fielded variant `${s.name}` as a first-class value (apply with parens: `${s.name}(...)`)")
          "zeroinitializer"
        case None =>
          notYet(s"unknown variant `${s.name}` — codegen variantInfo missed it")
          "zeroinitializer"

    case TVarRef(s, _, t) =>
      // Lambda body: a free reference to an outer binding is captured.
      // Look up the env field index, GEP through the env pointer, and
      // load. For ByRef captures the env stores a pointer to the
      // parent's alloca; we load through that for the actual value.
      lambdaCaptures.get(s.id) match
        case Some((idx, capT, mode)) =>
          mode match
            case CaptureMode.ByVal =>
              val slot = newReg()
              emitLine(s"  $slot = getelementptr inbounds $lambdaEnvTy, ptr %env, i32 0, i32 $idx\n")
              val reg = newReg()
              emitLine(s"  $reg = load ${llvmType(capT)}, ptr $slot\n")
              emitArrInc(reg, capT)
              reg
            case CaptureMode.ByRef =>
              val pslot = newReg()
              emitLine(s"  $pslot = getelementptr inbounds $lambdaEnvTy, ptr %env, i32 0, i32 $idx\n")
              val pp    = newReg()
              emitLine(s"  $pp = load ptr, ptr $pslot\n")
              val reg   = newReg()
              emitLine(s"  $reg = load ${llvmType(capT)}, ptr $pp\n")
              emitArrInc(reg, capT)
              reg
        case None =>
          // For array-typed references, the loaded value gets an inc so the
          // caller has its own owning share (per the spec §8.5 ARC model
          // documented above [[arrayLocalSlots]]). Scalars need no inc.
          locals.get(s.id) match
            case Some(slot) if boxedVarTypes.contains(s.id) =>
              // Escape-captured var lives in a heap box. Double-load:
              // alloca → box-ptr, then box-ptr → value.
              val bp = newReg()
              emitLine(s"  $bp = load ptr, ptr $slot\n")
              val reg = newReg()
              emitLine(s"  $reg = load ${llvmType(t)}, ptr $bp\n")
              emitArrInc(reg, t)
              reg
            case Some(slot) =>
              val reg = newReg()
              emitLine(s"  $reg = load ${llvmType(t)}, ptr $slot\n")
              emitArrInc(reg, t)
              reg
            case None if globalBindings.contains(s.id) =>
              val reg = newReg()
              emitLine(s"  $reg = load ${llvmType(t)}, ptr @${s.name}\n")
              emitArrInc(reg, t)
              reg
            case None =>
              notYet(s"reference to non-local `${s.name}`"); "0"

    case TBinOp("and", l, r, _, _) => emitShortCircuit(l, r, isAnd = true)
    case TBinOp("or",  l, r, _, _) => emitShortCircuit(l, r, isAnd = false)

    case TBinOp("..", lo, hi, _, _)  => emitRangeValue(lo, hi, inclusive = false)
    case TBinOp("..=", lo, hi, _, _) => emitRangeValue(lo, hi, inclusive = true)

    case TBinOp("^", l, r, _, TyInteger) =>
      // Integer power: exponentiation by squaring via the runtime
      // helper. Matches the interpreter's `intPow` for exp >= 0.
      val lv = emitExpr(l)
      val rv = emitExpr(r)
      val reg = newReg()
      emitLine(s"  $reg = call i64 @__nex_ipow(i64 $lv, i64 $rv)\n")
      reg

    case TBinOp("^", l, r, _, TyReal) =>
      // Real power: route through libm `pow`. Integer operands get
      // sitofp-lifted to double first, matching how the interpreter
      // promotes via `asReal`.
      val lv = emitExpr(l)
      val rv = emitExpr(r)
      val ld = if l.tpe == TyInteger then
        val r2 = newReg(); emitLine(s"  $r2 = sitofp i64 $lv to double\n"); r2
      else lv
      val rd = if r.tpe == TyInteger then
        val r2 = newReg(); emitLine(s"  $r2 = sitofp i64 $rv to double\n"); r2
      else rv
      val reg = newReg()
      emitLine(s"  $reg = call double @pow(double $ld, double $rd)\n")
      reg

    case TBinOp("+", l, r, _, TyString) =>
      // String concat. Both operands are owning %nex_str descriptor
      // pointers (TVarRef inc'd a share at load, or they're already-
      // owning concat / value-to-string results). After concat the
      // result has refcount=1 and we release the operand shares —
      // immortal-literal shares are silently no-op'd by __nex_str_dec.
      val lv = emitExpr(l)
      val rv = emitExpr(r)
      val res = newReg()
      emitLine(s"  $res = call ptr @__nex_str_concat(ptr $lv, ptr $rv)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $lv)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $rv)\n")
      res

    case TBinOp(op, l, r, _, TyComplex) =>
      // Complex arithmetic: promote any int/real operand to a complex
      // pair `{ x, 0.0 }`, then run the per-component formula. The
      // result type is `{ double, double }`.
      val lv = emitExpr(l)
      val rv = emitExpr(r)
      val (lre, lim) = toComplex(lv, l.tpe)
      val (rre, rim) = toComplex(rv, r.tpe)
      emitComplexArith(op, lre, lim, rre, rim)

    case TBinOp(op @ ("==" | "!="), l, r, _, TyBool) if l.tpe == TyByteArray && r.tpe == TyByteArray =>
      // Structural equality on byte buffers: length-then-memcmp via
      // `__nex_bytes_eq`. Mirrors the interpreter's [byte] equality
      // (NexInterpEval.scala:949). Both source shares are released.
      val lv = emitExpr(l)
      val rv = emitExpr(r)
      val eq = newReg()
      emitLine(s"  $eq = call i1 @__nex_bytes_eq(ptr $lv, ptr $rv)\n")
      emitArrDec(lv, l.tpe)
      emitArrDec(rv, r.tpe)
      if op == "==" then eq
      else
        val neg = newReg()
        emitLine(s"  $neg = xor i1 $eq, 1\n")
        neg

    case TBinOp(op @ ("==" | "!="), l, r, _, TyBool) if l.tpe == TyString && r.tpe == TyString =>
      // String equality: descriptor-level memcmp via the runtime
      // helper. Mirrors the interpreter's value-equality on strings.
      // Both source shares are released — emitExpr handed them to us
      // with refcount inc'd and the comparison is read-only.
      val lv  = emitExpr(l)
      val rv  = emitExpr(r)
      val eq  = newReg()
      emitLine(s"  $eq = call i1 @__nex_str_eq(ptr $lv, ptr $rv)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $lv)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $rv)\n")
      if op == "==" then eq
      else
        val neg = newReg()
        emitLine(s"  $neg = xor i1 $eq, 1\n")
        neg

    case TBinOp("==", l, r, _, TyBool) if l.tpe == TyComplex || r.tpe == TyComplex =>
      // Complex equality: both real and imaginary parts must match.
      val lv = emitExpr(l)
      val rv = emitExpr(r)
      val (lre, lim) = toComplex(lv, l.tpe)
      val (rre, rim) = toComplex(rv, r.tpe)
      val eR = newReg()
      emitLine(s"  $eR = fcmp oeq double $lre, $rre\n")
      val eI = newReg()
      emitLine(s"  $eI = fcmp oeq double $lim, $rim\n")
      val and = newReg()
      emitLine(s"  $and = and i1 $eR, $eI\n")
      and

    case TBinOp("!=", l, r, _, TyBool) if l.tpe == TyComplex || r.tpe == TyComplex =>
      val lv = emitExpr(l)
      val rv = emitExpr(r)
      val (lre, lim) = toComplex(lv, l.tpe)
      val (rre, rim) = toComplex(rv, r.tpe)
      val eR = newReg()
      emitLine(s"  $eR = fcmp oeq double $lre, $rre\n")
      val eI = newReg()
      emitLine(s"  $eI = fcmp oeq double $lim, $rim\n")
      val and = newReg()
      emitLine(s"  $and = and i1 $eR, $eI\n")
      val neg = newReg()
      emitLine(s"  $neg = xor i1 $and, 1\n")
      neg

    case TBinOp(op, l, r, _, t) =>
      val lv0 = emitExpr(l)
      val rv0 = emitExpr(r)
      // Real-typed result with integer operand(s) needs a sitofp lift
      // before the actual op runs — covers `int / int → real` (always
      // a real-divide in Nex) and `int + real` / `real + int` mixes.
      // For bool-typed result (comparisons), same lift on integer ops
      // when comparing against a real.
      val needsRealLift = t == TyReal && (l.tpe == TyInteger || r.tpe == TyInteger)
      val needsCmpReal  =
        t == TyBool && (l.tpe == TyReal || r.tpe == TyReal) &&
        (l.tpe == TyInteger || r.tpe == TyInteger)
      val (lv, rv, opT) =
        if needsRealLift || needsCmpReal then
          val ll = if l.tpe == TyInteger then
            val r2 = newReg()
            emitLine(s"  $r2 = sitofp i64 $lv0 to double\n")
            r2
          else lv0
          val rr = if r.tpe == TyInteger then
            val r2 = newReg()
            emitLine(s"  $r2 = sitofp i64 $rv0 to double\n")
            r2
          else rv0
          (ll, rr, TyReal)
        else
          (lv0, rv0, l.tpe)

      emitDivZeroCheck(op, opT, rv)
      val (instr, _) = binOpInst(op, opT)
      val reg     = newReg()
      emitLine(s"  $reg = $instr ${llvmType(opT)} $lv, $rv\n")
      reg

    case TUnaryOp("-", x, _, t) =>
      val xv  = emitExpr(x)
      val reg = newReg()
      t match
        case TyInteger => emitLine(s"  $reg = sub i64 0, $xv\n")
        case TyReal    => emitLine(s"  $reg = fneg double $xv\n")
        case TyComplex =>
          val re0 = newReg()
          emitLine(s"  $re0 = extractvalue { double, double } $xv, 0\n")
          val im0 = newReg()
          emitLine(s"  $im0 = extractvalue { double, double } $xv, 1\n")
          val nre = newReg()
          emitLine(s"  $nre = fneg double $re0\n")
          val nim = newReg()
          emitLine(s"  $nim = fneg double $im0\n")
          val c0 = newReg()
          emitLine(s"  $c0 = insertvalue { double, double } undef, double $nre, 0\n")
          emitLine(s"  $reg = insertvalue { double, double } $c0, double $nim, 1\n")
        case other     => notYet(s"unary - on $other")
      reg

    case TUnaryOp("not", x, _, _) =>
      val xv  = emitExpr(x)
      val reg = newReg()
      emitLine(s"  $reg = xor i1 $xv, 1\n")
      reg

    case TCall(callee, args, _, _) =>
      callee match
        case TVarRef(s, _, _) if s.name == "print" && args.size == 1 =>
          emitPrintCall(args.head); "void"
        case TVarRef(s, _, _) if s.kind == SymKind.Prelude =>
          emitPreludeCall(s.name, args, e.tpe)
        case TVarRef(s, _, _) if s.kind == SymKind.TypeName =>
          // Struct constructor: `Point(x, y)` lowers like a tuple
          // literal — insertvalue chain into the struct's `{ ... }` type.
          emitStructConstruct(s, args, e.tpe)
        case TVarRef(s, _, _) if s.kind == SymKind.EnumVariant =>
          // Fielded variant construction: `Converged(3.14)` →
          // `{ i32 tag, [N x i64] payload }` with each arg packed into
          // its own i64 slot. The variant's tag index and field types
          // come from [[variantInfo]].
          variantInfo.get(s.id) match
            case Some((te, idx, _)) =>
              emitVariantConstruct(te, idx, args)
            case None =>
              notYet(s"unknown variant `${s.name}` — codegen variantInfo missed it")
              "zeroinitializer"
        case TVarRef(s, _, _) if intrinsicFunctionOpIds.contains(s.id) =>
          // Source-prelude `@intrinsic` function — no wrapper exists,
          // emit the libm body inline at the call site.
          emitIntrinsicCall(intrinsicFunctionOpIds(s.id), args, e.tpe)
        case TVarRef(s, _, calleeT) if s.kind == SymKind.Function =>
          emitUserCall(s, calleeT, args)
        case _ =>
          // Closure-typed callee: extract fn_ptr + env_ptr from the
          // 16-byte `{ ptr, ptr }` value and call indirectly. Works
          // for both lambda values and TyFunc-typed parameters /
          // bindings.
          callee.tpe match
            case TyFunc(_, retT) =>
              emitClosureCall(callee, args, retT)
            case _ =>
              notYet(s"call to ${callee.getClass.getSimpleName}"); "0"

    case lam: TLambda =>
      emitLambdaConstruct(lam)

    case TField(receiver, name, _, t) =>
      emitFieldAccess(receiver, name, t)

    case TArrayLit(elems, _, t) =>
      emitArrayLit(elems, t)

    case TIndex(arr, indices, _, t) =>
      emitIndex(arr, indices, t)

    case TElementWise(op, l, r, _, t) =>
      emitElementWise(op, l, r, t)

    case TBroadcast(scalar, arr, op, scalarFirst, _, t) =>
      emitBroadcast(scalar, arr, op, scalarFirst, t)

    case TSlice(arr, lo, hi, inclusive, stride, _, t) =>
      emitSlice(arr, lo, hi, inclusive, stride, t)

    case TSlice2(arr, rowAx, colAx, _, t) =>
      emitSlice2(arr, rowAx, colAx, t)

    case TClone(arr, _, t) =>
      emitClone(arr, t)

    case TFlatIndex(arr, idx, _, t) =>
      emitFlatIndex(arr, idx, t)

    case TFusedLoop(loopVar, length, body, cols, _, t) =>
      emitFusedLoop(loopVar, length, body, cols, t)

    case TTuple(elems, _, t) =>
      emitTuple(elems, t)

    case TTupleProj(receiver, idx, _, t) =>
      emitTupleProj(receiver, idx, t)

    case TIf(cond, thenB, elseOpt, _, t) =>
      emitIf(cond, thenB, elseOpt, t)

    case TMatch(scrutinee, cases, _, t) =>
      emitMatch(scrutinee, cases, t)

    case TWhile(cond, body, _, _) =>
      emitWhile(cond, body); "void"

    case TFor(loopVars, iter, body, _, _) =>
      emitFor(loopVars, iter, body); "void"

    case TReturn(v, _, _) =>
      emitReturn(v); "void"

    case TBlock(items, result, _, _) =>
      // Each `TBlock` opens its own ARC scope: any `val` / `var` whose
      // RHS is array-typed is registered into the top scope. At block
      // exit (after the result is computed) we dec every slot in the
      // popped scope so the bindings release their owning share —
      // critical for blocks inside loops (each iteration owns and frees
      // its own arrays).
      //
      // TBlockExpr whose discarded value is array-typed needs its own
      // dec (the value never reached a slot).
      pushBlockScope()
      items.foreach {
        case TBlockBinding(sym, _, value) => emitLocalBinding(sym, value)
        case TBlockExpr(x) =>
          val v = emitExpr(x)
          if isRefCountedType(x.tpe) then emitArrDec(v, x.tpe)
      }
      val rv    = emitExpr(result)
      val scope = popBlockScope()
      decBlockScope(scope)
      rv

    case TAssign(target, value, _, _) =>
      emitAssign(target, value); "void"

    case TMatMul(l, r, _, t) =>
      emitMatMulShared(l, r, t)

    case other =>
      notYet(s"expression ${other.getClass.getSimpleName}"); "0"


  // ---------------------------------------------------------------------------
  // Complex (TyComplex) arithmetic helpers.
  //
  // The value layout is `{ double real, double imag }`. Integer and real
  // operands promote to `{ x, 0.0 }` before per-component arithmetic.
  // Division follows the canonical (a + bi) / (c + di) = ((ac + bd) +
  // (bc - ad)i) / (c² + d²) formula and traps when the denominator
  // c² + d² is zero, matching the interpreter's `complex division by
  // zero` trap.
  // ---------------------------------------------------------------------------

  /** Decompose a value of type `t` into its real and imaginary
    * components. For TyComplex this extracts the two fields; for
    * TyInteger / TyReal this promotes to `(value, 0.0)`. Mixed-type
    * binary ops use this to align operand shapes before applying the
    * per-component formulas.
    */
  protected def toComplex(v: String, t: Type): (String, String) = t match
    case TyComplex =>
      val re = newReg()
      emitLine(s"  $re = extractvalue { double, double } $v, 0\n")
      val im = newReg()
      emitLine(s"  $im = extractvalue { double, double } $v, 1\n")
      (re, im)
    case TyReal =>
      (v, "0.0")
    case TyInteger =>
      val re = newReg()
      emitLine(s"  $re = sitofp i64 $v to double\n")
      (re, "0.0")
    case other =>
      notYet(s"complex promotion from $other"); (v, "0.0")

  /** Pack a (re, im) pair into a `{ double, double }` aggregate value. */
  protected def packComplex(re: String, im: String): String =
    val c0 = newReg()
    emitLine(s"  $c0 = insertvalue { double, double } undef, double $re, 0\n")
    val c1 = newReg()
    emitLine(s"  $c1 = insertvalue { double, double } $c0, double $im, 1\n")
    c1

  /** Per-component complex arithmetic. Caller has already split both
    * operands into (re, im) pairs via [[toComplex]].
    */
  protected def emitComplexArith(op: String, lre: String, lim: String, rre: String, rim: String): String =
    op match
      case "+" =>
        val re = newReg(); emitLine(s"  $re = fadd double $lre, $rre\n")
        val im = newReg(); emitLine(s"  $im = fadd double $lim, $rim\n")
        packComplex(re, im)
      case "-" =>
        val re = newReg(); emitLine(s"  $re = fsub double $lre, $rre\n")
        val im = newReg(); emitLine(s"  $im = fsub double $lim, $rim\n")
        packComplex(re, im)
      case "*" =>
        // (a + bi)(c + di) = (ac - bd) + (ad + bc)i
        val ac = newReg(); emitLine(s"  $ac = fmul double $lre, $rre\n")
        val bd = newReg(); emitLine(s"  $bd = fmul double $lim, $rim\n")
        val ad = newReg(); emitLine(s"  $ad = fmul double $lre, $rim\n")
        val bc = newReg(); emitLine(s"  $bc = fmul double $lim, $rre\n")
        val re = newReg(); emitLine(s"  $re = fsub double $ac, $bd\n")
        val im = newReg(); emitLine(s"  $im = fadd double $ad, $bc\n")
        packComplex(re, im)
      case "/" =>
        // (a + bi) / (c + di) = ((ac + bd) + (bc - ad)i) / (c² + d²)
        val cc   = newReg(); emitLine(s"  $cc = fmul double $rre, $rre\n")
        val dd   = newReg(); emitLine(s"  $dd = fmul double $rim, $rim\n")
        val den  = newReg(); emitLine(s"  $den = fadd double $cc, $dd\n")
        // Trap on zero denominator to match the interpreter, which
        // throws NexTrap("complex division by zero"). Without this
        // the divide silently produces NaNs / Infs.
        val isZ  = newReg(); emitLine(s"  $isZ = fcmp oeq double $den, 0.0\n")
        val okL  = freshLabel("cdz.ok")
        val zL   = freshLabel("cdz.fail")
        emitTerminator(s"  br i1 $isZ, label %$zL, label %$okL\n")
        startBlock(zL)
        emitLine(s"  call void @__nex_trap_with(ptr @.cdiv_zero_msg)\n")
        emitTerminator(s"  unreachable\n")
        startBlock(okL)
        val ac   = newReg(); emitLine(s"  $ac = fmul double $lre, $rre\n")
        val bd   = newReg(); emitLine(s"  $bd = fmul double $lim, $rim\n")
        val bc   = newReg(); emitLine(s"  $bc = fmul double $lim, $rre\n")
        val ad   = newReg(); emitLine(s"  $ad = fmul double $lre, $rim\n")
        val rnum = newReg(); emitLine(s"  $rnum = fadd double $ac, $bd\n")
        val inum = newReg(); emitLine(s"  $inum = fsub double $bc, $ad\n")
        val re   = newReg(); emitLine(s"  $re = fdiv double $rnum, $den\n")
        val im   = newReg(); emitLine(s"  $im = fdiv double $inum, $den\n")
        packComplex(re, im)
      case other =>
        notYet(s"complex `$other` op"); packComplex("0.0", "0.0")

  // ---------------------------------------------------------------------------
  // User-function call — emits `call <retT> @<name>(<argT> <arg>, ...)`.
  // A unit-returning callee produces a `call void @name(...)` with no
  // result register; the emitExpr return value is `"void"` so any
  // downstream consumer (statement position) discards it.
  // ---------------------------------------------------------------------------

  private def emitUserCall(callee: Symbol, calleeT: Type, args: List[TExpr]): String =
    // Pull modes off whichever type carried the TyFunc signature —
    // call-site's `calleeT` (post-elaboration push-down) wins, with
    // callee.tpe as the fallback. Default to all-Read so we don't
    // crash if the elaborator left the signature blank.
    val modes: List[ParamMode] = calleeT match
      case TyFunc(ps, _) if ps.size == args.size => ps.map(_._2)
      case _ => callee.tpe match
        case TyFunc(ps, _) if ps.size == args.size => ps.map(_._2)
        case _                                      => List.fill(args.size)(ParamMode.Read)

    val argList = args.zip(modes).map { case (a, m) =>
      m match
        case ParamMode.Mut =>
          // Pass the address of the caller's slot so callee writes
          // flow back. Elaborator's checkCallSiteModes (NexElabLowering)
          // guarantees mut args are var-rooted lvalues; we currently
          // resolve direct TVarRef bindings only (struct-field /
          // array-element mut args fall back to by-value, matching the
          // interpreter's same-shape limitation).
          a match
            case TVarRef(s, _, _) =>
              // Three resolution paths, in priority order:
              //  1. ByRef-captured var inside a lambda body — the env
              //     stores a ptr to the parent's alloca; load it and
              //     pass directly so callee writes reach the parent.
              //  2. Local alloca in the surrounding function — the
              //     alloca's SSA reg IS the slot ptr.
              //  3. Module-level global — pass the global symbol's
              //     ptr directly.
              lambdaCaptures.get(s.id) match
                case Some((idx, _, CaptureMode.ByRef)) =>
                  val pslot = newReg()
                  emitLine(s"  $pslot = getelementptr inbounds $lambdaEnvTy, ptr %env, i32 0, i32 $idx\n")
                  val pp = newReg()
                  emitLine(s"  $pp = load ptr, ptr $pslot\n")
                  s"ptr $pp"
                case Some(_) =>
                  notImpl(s"mut-arg captured ByVal `${s.name}` — captured copy can't propagate writes back")
                case None =>
                  locals.get(s.id) match
                    case Some(slot) => s"ptr $slot"
                    case None if globalBindings.contains(s.id) => s"ptr @${s.name}"
                    case None =>
                      notImpl(s"mut-arg passes unbound `${s.name}`")
            case _ =>
              // Compute the value, spill to a temporary alloca, pass
              // its address. Mutations land in the temp and are lost —
              // matches the interpreter's by-value fallback for non-
              // TVarRef mut args.
              val v    = emitExpr(a)
              val tmp  = newReg()
              val ty   = llvmType(a.tpe)
              emitLine(s"  $tmp = alloca $ty\n")
              emitLine(s"  store $ty $v, ptr $tmp\n")
              s"ptr $tmp"
        case ParamMode.Read =>
          val v = emitExpr(a)
          s"${llvmType(a.tpe)} $v"
    }.mkString(", ")

    val retT = calleeT match
      case TyFunc(_, r) => r
      case _            => callee.tpe match
        case TyFunc(_, r) => r
        case _            => TyUnknown

    val fnName = llvmFuncNameOf(callee)
    retT match
      case TyUnit =>
        emitLine(s"  call void @$fnName($argList)\n")
        "void"
      case other =>
        val reg = newReg()
        emitLine(s"  $reg = call ${llvmType(other)} @$fnName($argList)\n")
        reg

  // ---------------------------------------------------------------------------
  // Tuples. We use anonymous LLVM struct types — `{ T0, T1, ... }` literals
  // everywhere — so there's no preamble registration. Tuples are kept
  // stack-allocated (no heap, no ARC); when stored into a slot we copy the
  // struct value, and when projected we use `extractvalue`.
  //
  // For nested tuples (`(1, (2, 3))`) the inner tuple lives inside the outer
  // struct's payload — extractvalue/insertvalue work transparently on the
  // nested layout.
  // ---------------------------------------------------------------------------

  /** Lower `(e0, e1, ...)`. Builds an undef struct of the tuple's LLVM
    * type, then folds in each element via `insertvalue`. Returns the
    * final aggregate as an SSA value.
    */
  private def emitTuple(elems: List[TExpr], t: Type): String =
    val tupTy = llvmType(t)
    if elems.isEmpty then
      // The empty tuple `()` is just unit at this point; the elaborator
      // should never produce a zero-elem TTuple, but treat it gracefully.
      notYet("empty tuple literal"); "0"
    else
      val vs = elems.map(emitExpr)
      var acc: String = "undef"
      for i <- elems.indices do
        val elemTy = llvmType(elems(i).tpe)
        val next   = newReg()
        emitLine(s"  $next = insertvalue $tupTy $acc, $elemTy ${vs(i)}, $i\n")
        acc = next
      acc

  /** Lower `t.<idx>` — extract field `idx` from a tuple value. When the
    * receiver tuple carries refcounted fields, the projection inc's
    * the extracted share and dec's the receiver as a whole (the drop
    * helper walks every field, including the extracted one, but the
    * pre-inc keeps the projected share alive). Mirrors `emitIndex` for
    * array element loads.
    */
  private def emitTupleProj(receiver: TExpr, idx: Int, resultT: Type): String =
    val recv = emitExpr(receiver)
    val recvTy = llvmType(receiver.tpe)
    val reg = newReg()
    emitLine(s"  $reg = extractvalue $recvTy $recv, $idx\n")
    if isRefCountedType(resultT) then emitArrInc(reg, resultT)
    if isRefCountedType(receiver.tpe) then emitArrDec(recv, receiver.tpe)
    reg

  // ---------------------------------------------------------------------------
  // Structs. The struct type is materialized as an anonymous LLVM struct
  // (same shape as tuples — `{ T0, T1, ... }`), since the field order is
  // declared by the `struct Foo { x, y, ... }` form and is already part of
  // the TyStruct type. Construction lowers to insertvalue chain; field
  // access lowers to extractvalue by position.
  // ---------------------------------------------------------------------------

  /** Lower `StructName(arg0, arg1, ...)`. */
  private def emitStructConstruct(sym: Symbol, args: List[TExpr], resultT: Type): String =
    val structTy = llvmType(resultT)
    val vs = args.map(emitExpr)
    var acc: String = "undef"
    for i <- args.indices do
      val ft = llvmType(args(i).tpe)
      val next = newReg()
      emitLine(s"  $next = insertvalue $structTy $acc, $ft ${vs(i)}, $i\n")
      acc = next
    acc

  /** Lower `recv.field` — pick out the field's index from the receiver's
    * TyStruct then emit `extractvalue`. When the struct carries
    * refcounted fields, inc the extracted share and dec the whole
    * receiver — same scheme as [[emitTupleProj]]. Complex receivers
    * get `.re` (field 0) and `.im` (field 1).
    */
  private def emitFieldAccess(receiver: TExpr, fieldName: String, resultT: Type): String =
    receiver.tpe match
      case TyStruct(_, fields) =>
        val idx = fields.indexWhere(_._1 == fieldName)
        if idx < 0 then
          notYet(s"field `$fieldName` not found on struct"); "0"
        else
          val rv = emitExpr(receiver)
          val ty = llvmType(receiver.tpe)
          val reg = newReg()
          emitLine(s"  $reg = extractvalue $ty $rv, $idx\n")
          if isRefCountedType(resultT) then emitArrInc(reg, resultT)
          if isRefCountedType(receiver.tpe) then emitArrDec(rv, receiver.tpe)
          reg
      case TyComplex =>
        val idx = fieldName match
          case "re" => 0
          case "im" => 1
          case _    => notYet(s"complex field `$fieldName`"); return "0.0"
        val rv = emitExpr(receiver)
        val reg = newReg()
        emitLine(s"  $reg = extractvalue { double, double } $rv, $idx\n")
        reg
      case other =>
        notYet(s"field access on non-struct type $other"); "0"

