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
      case _: TTopBinding | _: TStructDecl | _: TModuleDecl | _: TImportDecl =>
        () // top-bindings already emitted as globals; struct/module are metadata

    // Flush the string-literal pool at the END. LLVM IR allows forward
    // references to module-level identifiers, so a function body that
    // uses `@.str.N` works even though the constant is defined below.
    flushStringPool()

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

    case TSlice(arr, lo, hi, inclusive, _, t) =>
      emitSlice(arr, lo, hi, inclusive, t)

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
  // Control flow — TIf / TWhile / TReturn / short-circuit and/or.
  //
  // Every block emitted here uses an explicit label generated by
  // [[freshLabel]]. The basic rule of LLVM IR is "each basic block ends
  // with exactly one terminator instruction (br, ret, unreachable);"
  // [[currentBlock]] tracks whether we're currently in an open block so
  // that emits don't leak into a closed block (e.g., after an early
  // return inside a branch).
  // ---------------------------------------------------------------------------

  /** Insert a divide-by-zero check before a `/`, `div`, or `%` operation.
    *
    * The interpreter explicitly traps on these — without an explicit
    * AOT check, integer divide-by-zero is UB (likely SIGFPE on x86,
    * silent on aarch64) and real divide-by-zero produces inf. Both
    * diverge from the interpreter's trap. Inserting an icmp/fcmp +
    * branch keeps the message wired through `__nex_trap_with` so an
    * enclosing `assert_traps` catches and (with 2-arg form) finds the
    * matching substring.
    */
  private def emitDivZeroCheck(op: String, opT: Type, rv: String): Unit =
    val (needsCheck, isZeroIR, msgSym, prefix) = (op, opT) match
      case ("/",   TyReal)    =>
        val z = newReg()
        emitLine(s"  $z = fcmp oeq double $rv, 0.0\n")
        (true, z, "@.div_zero_msg", "rdz")
      case ("div", TyInteger) =>
        val z = newReg()
        emitLine(s"  $z = icmp eq i64 $rv, 0\n")
        (true, z, "@.idiv_zero_msg", "idz")
      case ("%",   TyInteger) =>
        val z = newReg()
        emitLine(s"  $z = icmp eq i64 $rv, 0\n")
        (true, z, "@.mod_zero_msg", "mdz")
      case _ => (false, "", "", "")
    if needsCheck then
      val okL   = freshLabel(s"$prefix.ok")
      val failL = freshLabel(s"$prefix.fail")
      emitTerminator(s"  br i1 $isZeroIR, label %$failL, label %$okL\n")
      startBlock(failL)
      emitLine(s"  call void @__nex_trap_with(ptr $msgSym)\n")
      emitTerminator(s"  unreachable\n")
      startBlock(okL)

  private def emitIf(cond: TExpr, thenB: TExpr, elseOpt: Option[TExpr], resultT: Type): String =
    val condV  = emitExpr(cond)
    val thenL  = freshLabel("if.then")
    val mergeL = freshLabel("if.end")
    val elseL  = elseOpt.map(_ => freshLabel("if.else")).getOrElse(mergeL)

    emitTerminator(s"  br i1 $condV, label %$thenL, label %$elseL\n")

    // Then branch
    startBlock(thenL)
    val thenV    = emitExpr(thenB)
    val thenExit = currentBlock
    if currentBlock.isDefined then emitTerminator(s"  br label %$mergeL\n")

    // Else branch (if present)
    val (elseV, elseExit) =
      elseOpt match
        case Some(elseB) =>
          startBlock(elseL)
          val v = emitExpr(elseB)
          val x = currentBlock
          if currentBlock.isDefined then emitTerminator(s"  br label %$mergeL\n")
          (v, x)
        case None =>
          // No-else `if` has no else value; merge edge from the
          // condition's predecessor block (the one we branched FROM).
          ("0", Some("__predec_unused__"))

    // Merge — only emit if at least one branch reaches it.
    if thenExit.isDefined || elseExit.isDefined then
      startBlock(mergeL)
      resultT match
        case TyUnit | TyUnknown =>
          "void"
        case t =>
          // One-or-both branches may have terminated. Skip phi if only one
          // predecessor survived; the surviving value flows directly.
          val parts = List(
            thenExit.map(b => s"[ $thenV, %$b ]"),
            elseExit.map(b => s"[ $elseV, %$b ]"),
          ).flatten
          if parts.size <= 1 then
            // Only one branch contributes the result — but we still need
            // a value at the merge. Emit a phi with the single edge.
            val sv     = if thenExit.isDefined then thenV else elseV
            val origin = thenExit.orElse(elseExit).get
            val reg    = newReg()
            emitLine(s"  $reg = phi ${llvmType(t)} [ $sv, %$origin ]\n")
            reg
          else
            val reg = newReg()
            emitLine(s"  $reg = phi ${llvmType(t)} ${parts.mkString(", ")}\n")
            reg
    else
      // Both branches terminated. Merge is unreachable; the if
      // expression's value is irrelevant because nothing follows.
      "void"

  private def emitWhile(cond: TExpr, body: TExpr): Unit =
    val condL = freshLabel("while.cond")
    val bodyL = freshLabel("while.body")
    val exitL = freshLabel("while.exit")

    emitTerminator(s"  br label %$condL\n")
    startBlock(condL)
    val condV = emitExpr(cond)
    emitTerminator(s"  br i1 $condV, label %$bodyL, label %$exitL\n")
    startBlock(bodyL)
    emitExpr(body)
    if currentBlock.isDefined then emitTerminator(s"  br label %$condL\n")
    startBlock(exitL)

  /** Lower `for i in lo..hi do body` (or `lo..=hi`) to a counting while:
    *
    * {{{
    *   alloca i64 %i ; store %lo
    *   br label %for.cond
    *  for.cond:
    *   %cur = load %i
    *   %ok  = icmp s(le|lt) %cur, %hi
    *   br i1 %ok, label %for.body, label %for.exit
    *  for.body:
    *   ... body (loopVar = %cur load) ...
    *   %next = add %cur, 1
    *   store %next, %i
    *   br label %for.cond
    *  for.exit:
    * }}}
    *
    * Iterating over arrays / general iterables is deferred to a later
    * chunk that introduces array codegen.
    */
  private def emitFor(loopVars: List[Symbol], iter: TExpr, body: TExpr): Unit =
    iter match
      case TBinOp("..",  lo, hi, _, _) => emitForRange(loopVars, lo, hi, body, inclusive = false)
      case TBinOp("..=", lo, hi, _, _) => emitForRange(loopVars, lo, hi, body, inclusive = true)
      case _ if iter.tpe match { case TyArray(_, _) => true; case _ => false } =>
        emitForArray(loopVars, iter, body)
      case _ =>
        notYet(s"for over iterable of type ${iter.tpe}")

  /** Lower `for x in arr do body` (rank-1) to a flat counting loop. The
    * iterator expression is evaluated once into a fresh slot; each
    * iteration loads `data[i]` and binds it to `x`.
    *
    * Rank-2 iteration semantics (row-major, element-by-element) is the
    * same shape but indexes `__nex_arr2_flat_slot`; we'll fold that in
    * when rank-2 lands.
    */
  private def emitForArray(loopVars: List[Symbol], iter: TExpr, body: TExpr): Unit =
    val rank = arrayRank(iter.tpe)
    val elem = arrayElem(iter.tpe)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val langT = llvmType(elem)

    // Tuple destructuring: when the source array's element type is a
    // tuple and the user wrote `for (a, b, ...) in arr do`, bind each
    // loop var to one tuple field. The number of loop vars must match
    // the tuple's arity.
    val tupleFieldTypes: Option[List[Type]] = elem match
      case TyTuple(fs) if loopVars.size == fs.size => Some(fs)
      case _                                       => None

    if loopVars.size != 1 && tupleFieldTypes.isEmpty then
      notYet(s"for-over-array with ${loopVars.size}-way destructuring on element type $elem")
      return

    // Compute the array ptr once, stash in a slot so the cond-block can
    // reload it (we don't have phi over array ptrs yet). The length is
    // also stashed so we don't re-call the helper each iteration.
    //
    // Open a fresh block scope around the entire loop and register the
    // iter alloca in it — this way an early `return` from inside the
    // body participates in `decAllLocalArrays`, and the normal exit
    // dec's the iter via `popBlockScope`. Without this the iter share
    // taken by `emitExpr(iter)` would leak on early return.
    pushBlockScope()
    val arrV  = emitExpr(iter)
    val arrSlot = newReg()
    emitLine(s"  $arrSlot = alloca ptr\n")
    emitLine(s"  store ptr $arrV, ptr $arrSlot\n")
    val iterScopeId = -regCounter // synthetic id; can't collide with user symbols
    registerArraySlot(iterScopeId, arrSlot, iter.tpe)

    val lenReg = newReg()
    rank match
      case 1 => emitLine(s"  $lenReg = call i64 @__nex_arr1_len(ptr $arrV)\n")
      case 2 => emitLine(s"  $lenReg = call i64 @__nex_arr2_len(ptr $arrV)\n")
      case other => notYet(s"for over rank-$other array"); return

    val iSlot = newReg()
    emitLine(s"  $iSlot = alloca i64\n")
    emitLine(s"  store i64 0, ptr $iSlot\n")

    // Per-loop-var slots. For the non-destructured case there's a
    // single `langT` slot; for tuple destructuring there's one slot
    // per tuple field, each typed by the field's LLVM type.
    val perVarSlots: List[(String, String)] = tupleFieldTypes match
      case Some(fs) =>
        fs.zip(loopVars).map { case (fieldT, sym) =>
          val s = newReg()
          val ft = llvmType(fieldT)
          emitLine(s"  $s = alloca $ft\n")
          locals(sym.id) = s
          (s, ft)
        }
      case None =>
        val loopVar = loopVars.head
        val xSlot   = newReg()
        emitLine(s"  $xSlot = alloca $langT\n")
        locals(loopVar.id) = xSlot
        List((xSlot, langT))

    val condL = freshLabel("forarr.cond")
    val bodyL = freshLabel("forarr.body")
    val exitL = freshLabel("forarr.exit")

    emitTerminator(s"  br label %$condL\n")
    startBlock(condL)
    val cur = newReg()
    emitLine(s"  $cur = load i64, ptr $iSlot\n")
    val ok  = newReg()
    emitLine(s"  $ok = icmp slt i64 $cur, $lenReg\n")
    emitTerminator(s"  br i1 $ok, label %$bodyL, label %$exitL\n")

    startBlock(bodyL)
    val arrCur = newReg()
    emitLine(s"  $arrCur = load ptr, ptr $arrSlot\n")
    val slotPtr = newReg()
    rank match
      case 1 => emitLine(s"  $slotPtr = call ptr @__nex_arr1_slot(ptr $arrCur, i64 $cur, i64 $esz)\n")
      case 2 => emitLine(s"  $slotPtr = call ptr @__nex_arr2_flat_slot(ptr $arrCur, i64 $cur, i64 $esz)\n")
      case _ => ()
    val v = loadElem(stT, slotPtr, langT)
    tupleFieldTypes match
      case Some(fs) =>
        // Extract each field from the tuple value and store into its
        // corresponding loop-var slot.
        for ((fieldT, idx) <- fs.zipWithIndex) do
          val (slot, ft) = perVarSlots(idx)
          val fv = newReg()
          emitLine(s"  $fv = extractvalue $langT $v, $idx\n")
          emitLine(s"  store $ft $fv, ptr $slot\n")
      case None =>
        val (xSlot, _) = perVarSlots.head
        emitLine(s"  store $langT $v, ptr $xSlot\n")
    emitExpr(body)
    if currentBlock.isDefined then
      val cur2 = newReg()
      emitLine(s"  $cur2 = load i64, ptr $iSlot\n")
      val next = newReg()
      emitLine(s"  $next = add i64 $cur2, 1\n")
      emitLine(s"  store i64 $next, ptr $iSlot\n")
      emitTerminator(s"  br label %$condL\n")

    startBlock(exitL)
    // Pop the for-loop's synthetic scope. Decs the registered iter slot,
    // releasing the share we took at loop entry. Early returns inside
    // the body already drained the scope via `decAllLocalArrays`.
    val iterScope = popBlockScope()
    decBlockScope(iterScope)

  private def emitForRange(
      loopVars: List[Symbol],
      lo:       TExpr,
      hi:       TExpr,
      body:     TExpr,
      inclusive: Boolean,
  ): Unit =
    if loopVars.size != 1 then
      notYet("for-over-range with tuple destructuring")
      return

    val loopVar = loopVars.head
    val loV     = emitExpr(lo)
    val hiV     = emitExpr(hi)

    val slot = newReg()
    emitLine(s"  $slot = alloca i64\n")
    emitLine(s"  store i64 $loV, ptr $slot\n")
    locals(loopVar.id) = slot

    val condL = freshLabel("for.cond")
    val bodyL = freshLabel("for.body")
    val exitL = freshLabel("for.exit")
    val cmp   = if inclusive then "sle" else "slt"

    emitTerminator(s"  br label %$condL\n")
    startBlock(condL)
    val cur = newReg()
    emitLine(s"  $cur = load i64, ptr $slot\n")
    val ok = newReg()
    emitLine(s"  $ok = icmp $cmp i64 $cur, $hiV\n")
    emitTerminator(s"  br i1 $ok, label %$bodyL, label %$exitL\n")

    startBlock(bodyL)
    emitExpr(body)
    if currentBlock.isDefined then
      val cur2 = newReg()
      emitLine(s"  $cur2 = load i64, ptr $slot\n")
      val next = newReg()
      emitLine(s"  $next = add i64 $cur2, 1\n")
      emitLine(s"  store i64 $next, ptr $slot\n")
      emitTerminator(s"  br label %$condL\n")

    startBlock(exitL)

  /** Lower a range expression at value position (`lo..hi` / `lo..=hi`)
    * to a freshly-allocated rank-1 integer array containing the
    * sequence. The interpreter materialises ranges this way; the for-
    * loop path consumes them lazily via [[emitForRange]], so this
    * helper only fires when the range escapes a loop header. Empty
    * ranges (`lo > hi`, or `lo == hi` for exclusive) produce a zero-
    * length array — matching the interpreter. */
  private def emitRangeValue(lo: TExpr, hi: TExpr, inclusive: Boolean): String =
    val loV = emitExpr(lo)
    val hiV = emitExpr(hi)

    // length = max(0, hi - lo + (inclusive ? 1 : 0))
    val diff = newReg()
    emitLine(s"  $diff = sub i64 $hiV, $loV\n")
    val rawLen = if inclusive then
      val r = newReg(); emitLine(s"  $r = add i64 $diff, 1\n"); r
    else diff
    val isNeg = newReg()
    emitLine(s"  $isNeg = icmp slt i64 $rawLen, 0\n")
    val length = newReg()
    emitLine(s"  $length = select i1 $isNeg, i64 0, i64 $rawLen\n")

    val desc = newReg()
    emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $length, i64 8)\n")
    val buf = bufPtr(desc, TyArray(TyInteger, 1))

    emitCountingLoop(length, "range") { i =>
      val v = newReg()
      emitLine(s"  $v = add i64 $loV, $i\n")
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds i64, ptr $buf, i64 $i\n")
      emitLine(s"  store i64 $v, ptr $slot\n")
    }
    desc

  private def emitReturn(v: Option[TExpr]): Unit =
    v match
      case None =>
        decAllLocalArrays()
        if currentIsMain then emitTerminator("  ret i32 0\n")
        else emitTerminator("  ret void\n")
      case Some(expr) =>
        val rv = emitExpr(expr)
        // If the function is about to discard the value (main / unit
        // return), dec the SSA value before we tear down the rest of
        // the local arrays.
        val willDiscard = currentIsMain || currentReturnType == TyUnit
        if willDiscard && isRefCountedType(expr.tpe) && rv != "0" && rv != "void" then
          emitArrDec(rv, expr.tpe)

        decAllLocalArrays()

        if currentIsMain then
          emitTerminator("  ret i32 0\n")
        else currentReturnType match
          case TyUnit =>
            emitTerminator("  ret void\n")
          case TyUnknown =>
            expr.tpe match
              case TyUnit => emitTerminator("  ret void\n")
              case t      => emitTerminator(s"  ret ${llvmType(t)} $rv\n")
          case t =>
            emitTerminator(s"  ret ${llvmType(t)} $rv\n")

  /** Short-circuit `and` / `or` evaluated as branches + phi. For `and`,
    * the LHS-false case skips evaluating the RHS and yields `false`;
    * for `or`, the LHS-true case skips the RHS and yields `true`.
    */
  private def emitShortCircuit(l: TExpr, r: TExpr, isAnd: Boolean): String =
    val lv      = emitExpr(l)
    if currentBlock.isEmpty then return "0"  // LHS terminated; nothing to merge
    val afterL  = currentBlock.get
    val rhsL    = freshLabel(if isAnd then "and.rhs" else "or.rhs")
    val endL    = freshLabel(if isAnd then "and.end" else "or.end")

    if isAnd then
      emitTerminator(s"  br i1 $lv, label %$rhsL, label %$endL\n")
    else
      emitTerminator(s"  br i1 $lv, label %$endL, label %$rhsL\n")

    startBlock(rhsL)
    val rv     = emitExpr(r)
    val afterR = currentBlock
    if afterR.isDefined then emitTerminator(s"  br label %$endL\n")

    startBlock(endL)
    val reg = newReg()
    val shortValue = if isAnd then "0" else "1"
    if afterR.isDefined then
      emitLine(s"  $reg = phi i1 [ $shortValue, %$afterL ], [ $rv, %${afterR.get} ]\n")
    else
      emitLine(s"  $reg = phi i1 [ $shortValue, %$afterL ]\n")
    reg

  private def emitAssign(target: TExpr, value: TExpr): Unit =
    target match
      case TVarRef(s, _, t) =>
        // Captured-by-ref var inside a lambda body: env stores a box
        // pointer; load it and store through. Checked before `locals`
        // since a captured var is not in the lambda's locals.
        lambdaCaptures.get(s.id) match
          case Some((idx, capT, CaptureMode.ByRef)) =>
            val rv = emitExpr(value)
            val pslot = newReg()
            emitLine(s"  $pslot = getelementptr inbounds $lambdaEnvTy, ptr %env, i32 0, i32 $idx\n")
            val bp = newReg()
            emitLine(s"  $bp = load ptr, ptr $pslot\n")
            if isRefCountedType(capT) then
              val old = newReg()
              emitLine(s"  $old = load ${llvmType(capT)}, ptr $bp\n")
              emitArrDec(old, capT)
            emitLine(s"  store ${llvmType(capT)} $rv, ptr $bp\n")
          case Some(_) =>
            notYet(s"assign to ByVal-captured `${s.name}`")
          case None =>
            val rv = emitExpr(value)
            // For array slots, the previous occupant owns a share — dec it
            // before storing the new value so the old buffer can be freed if
            // this was its last reference. Scalars need no such cleanup.
            locals.get(s.id) match
              case Some(slot) if boxedVarTypes.contains(s.id) =>
                // Boxed var: store goes through the box. If the var holds a
                // refcounted value, dec the previous occupant via the box.
                val bp = newReg()
                emitLine(s"  $bp = load ptr, ptr $slot\n")
                if isRefCountedType(t) then
                  val old = newReg()
                  emitLine(s"  $old = load ${llvmType(t)}, ptr $bp\n")
                  emitArrDec(old, t)
                emitLine(s"  store ${llvmType(t)} $rv, ptr $bp\n")
              case Some(slot) =>
                if isRefCountedType(t) then
                  val old = newReg()
                  emitLine(s"  $old = load ${llvmType(t)}, ptr $slot\n")
                  emitArrDec(old, t)
                emitLine(s"  store ${llvmType(t)} $rv, ptr $slot\n")
              case None if globalBindings.contains(s.id) =>
                if isRefCountedType(t) then
                  val old = newReg()
                  emitLine(s"  $old = load ${llvmType(t)}, ptr @${s.name}\n")
                  emitArrDec(old, t)
                emitLine(s"  store ${llvmType(t)} $rv, ptr @${s.name}\n")
              case None       => notYet(s"assign to non-local `${s.name}`")

      case TIndex(arr, indices, _, _) =>
        // arr[i] = v / arr[i, j] = v.  Compute slot ptr via the runtime
        // helper, then store the value into that slot.
        val rank = arrayRank(arr.tpe)
        val elem = arrayElem(arr.tpe)
        val esz  = elemSize(elem)
        val stT  = storageType(elem)
        val av   = emitExpr(arr)
        val slot = newReg()
        (rank, indices) match
          case (1, List(i)) =>
            val iv = emitExpr(i)
            emitLine(s"  $slot = call ptr @__nex_arr1_slot(ptr $av, i64 $iv, i64 $esz)\n")
          case (2, List(i, j)) =>
            val iv = emitExpr(i)
            val jv = emitExpr(j)
            emitLine(s"  $slot = call ptr @__nex_arr2_slot(ptr $av, i64 $iv, i64 $jv, i64 $esz)\n")
          case (r, ixs) =>
            notYet(s"assign to index rank $r / ${ixs.size}")
            return
        val rv = emitExpr(value)
        storeElem(stT, rv, slot)
        // The receiver array `av` was loaded as an owning share — release it.
        emitArrDec(av, arr.tpe)

      case TSlice(arr, lo, hi, inclusive, _, _) =>
        emitSliceAssign(arr, lo, hi, inclusive, value)

      case TSlice2(arr, rowAx, colAx, _, _) =>
        emitSlice2Assign(arr, rowAx, colAx, value)

      case f: TField =>
        emitFieldAssign(f, value)

      case other =>
        notYet(s"assign to ${other.getClass.getSimpleName}")

  /** Anchor for an addressable lvalue receiver: a slot pointer plus the
    * static LLVM struct type at that slot. `TVarRef` resolves to its
    * alloca / global; `TIndex` resolves through the runtime array-slot
    * helper and counts the array descriptor as owing a balancing dec.
    */
  private case class FieldAnchor(slotPtr: String, slotTy: String, decAfter: () => Unit)

  /** Walk a `TField` chain to its addressable receiver, collecting
    * (struct-type, field-index) pairs from outermost to innermost.
    * Returns `None` when the chain bottoms out on something not yet
    * supported (captures, complex receivers).
    */
  private def resolveFieldChain(f: TField): Option[(FieldAnchor, List[(Type, Int)])] =
    @annotation.tailrec
    def loop(e: TExpr, acc: List[(Type, Int)]): Option[(FieldAnchor, List[(Type, Int)])] =
      e match
        case TField(recv, name, _, _) =>
          recv.tpe match
            case TyStruct(_, fields) =>
              val idx = fields.indexWhere(_._1 == name)
              if idx < 0 then None
              else loop(recv, (recv.tpe, idx) :: acc)
            case _ => None
        case TVarRef(s, _, _) =>
          locals.get(s.id) match
            case Some(slot) =>
              Some((FieldAnchor(slot, llvmType(s.tpe), () => ()), acc))
            case None if globalBindings.contains(s.id) =>
              Some((FieldAnchor(s"@${s.name}", llvmType(s.tpe), () => ()), acc))
            case None => None
        case TIndex(arr, indices, _, _) =>
          val rank = arrayRank(arr.tpe)
          val elem = arrayElem(arr.tpe)
          val esz  = elemSize(elem)
          val av   = emitExpr(arr)
          val slot = newReg()
          (rank, indices) match
            case (1, List(i)) =>
              val iv = emitExpr(i)
              emitLine(s"  $slot = call ptr @__nex_arr1_slot(ptr $av, i64 $iv, i64 $esz)\n")
            case (2, List(i, j)) =>
              val iv = emitExpr(i)
              val jv = emitExpr(j)
              emitLine(s"  $slot = call ptr @__nex_arr2_slot(ptr $av, i64 $iv, i64 $jv, i64 $esz)\n")
            case _ => return None
          Some((FieldAnchor(slot, llvmType(elem), () => emitArrDec(av, arr.tpe)), acc))
        case _ => None
    loop(f, Nil)

  /** Emit a struct-field assignment. Walks the field chain to its
    * addressable anchor, emits a single GEP to reach the innermost
    * field's address, and writes through with ARC-aware release-old /
    * store-new when the field type is refcounted. Falls back to
    * `notYet` for receiver shapes that aren't anchored yet (lambda
    * captures, computed receivers other than indexed arrays).
    */
  private def emitFieldAssign(target: TField, value: TExpr): Unit =
    val fieldT = target.tpe
    val fieldLLT = llvmType(fieldT)
    resolveFieldChain(target) match
      case None =>
        notYet(s"assign to TField with non-addressable receiver")
      case Some((anchor, path)) =>
        // Build the GEP index list: a leading 0 (deref the slot ptr) plus
        // one i32 per field hop. LLVM resolves anonymous struct types
        // structurally, so the same GEP works for any nested layout.
        val gepIdx = path.map { case (_, i) => s"i32 $i" }.mkString(", ")
        val slot = newReg()
        emitLine(s"  $slot = getelementptr inbounds ${anchor.slotTy}, ptr ${anchor.slotPtr}, i32 0, $gepIdx\n")

        if isRefCountedType(fieldT) then
          val old = newReg()
          emitLine(s"  $old = load $fieldLLT, ptr $slot\n")
          emitArrDec(old, fieldT)

        val rv = emitExpr(value)
        emitLine(s"  store $fieldLLT $rv, ptr $slot\n")
        anchor.decAfter()

  private def emitLocalBinding(sym: Symbol, value: TExpr): Unit =
    val rv = emitExpr(value)
    val ty = llvmType(sym.tpe)
    if boxedVarTypes.contains(sym.id) then
      // Var is captured by a closure that may outlive this stack frame.
      // Allocate a heap-backed single-cell box so the captured cell
      // survives. The slot stores a pointer to the box; the box stores
      // the value. Reads and writes go through one extra load.
      val box  = newReg()
      val size = boxSizeBytes(sym.tpe)
      emitLine(s"  $box = call ptr @__nex_env_alloc(i64 $size, ptr null)\n")
      if ty != "void" then emitLine(s"  store $ty $rv, ptr $box\n")
      val slot = newReg()
      emitLine(s"  $slot = alloca ptr\n")
      emitLine(s"  store ptr $box, ptr $slot\n")
      locals(sym.id) = slot
      registerBoxedSlot(sym.id, slot, sym.tpe)
    else
      val slot = newReg()
      emitLine(s"  $slot = alloca $ty\n")
      if ty != "void" then emitLine(s"  store $ty $rv, ptr $slot\n")
      locals(sym.id) = slot
      if isRefCountedType(sym.tpe) then
        registerArraySlot(sym.id, slot, sym.tpe)

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
  private def toComplex(v: String, t: Type): (String, String) = t match
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
  private def packComplex(re: String, im: String): String =
    val c0 = newReg()
    emitLine(s"  $c0 = insertvalue { double, double } undef, double $re, 0\n")
    val c1 = newReg()
    emitLine(s"  $c1 = insertvalue { double, double } $c0, double $im, 1\n")
    c1

  /** Per-component complex arithmetic. Caller has already split both
    * operands into (re, im) pairs via [[toComplex]].
    */
  private def emitComplexArith(op: String, lre: String, lim: String, rre: String, rim: String): String =
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

  // ---------------------------------------------------------------------------
  // Interpolated string at value position.
  //
  // Builds a fresh %nex_str descriptor by concat-chaining each part. Text
  // parts route through the literal-descriptor pool (immortal); $ref and
  // ${expr} parts go through emitValueToString — which returns either the
  // already-a-descriptor SSA value (for TyString refs) or a fresh heap
  // descriptor (snprintf-built for numerics and bool). Each concat
  // allocates a fresh result, so a 3-part interp string allocates 2
  // intermediate descriptors plus the final one. ARC follow-up will
  // address the intermediate leaks.
  // ---------------------------------------------------------------------------

  /** Emit code that produces a %nex_str descriptor pointer for the value
    * of `e`. For TyString this is just `emitExpr(e)`; for scalar types
    * it routes through a `__nex_str_from_<T>` runtime helper. Aggregate
    * types (complex / tuple / array / struct) build descriptors by
    * concat-chaining the formatted parts — mirrors `formatValue` in the
    * interpreter byte-for-byte.
    */
  protected def emitValueToString(e: TExpr): String =
    e.tpe match
      case TyString  => emitExpr(e)
      case TyInteger => emitValueToStringInt(emitExpr(e))
      case TyReal    => emitValueToStringReal(emitExpr(e))
      case TyBool    => emitValueToStringBool(emitExpr(e))
      case TyComplex => emitValueToStringComplex(emitExpr(e))
      case TyTuple(ts) =>
        val v = emitExpr(e)
        emitValueToStringTuple(v, e.tpe, ts)
      case TyStruct(name, fields) =>
        val v = emitExpr(e)
        emitValueToStringStruct(v, e.tpe, name, fields)
      case TyArray(_, _) =>
        emitValueToStringArray(e)
      case TyUnit =>
        internStringDescriptor("()")
      case other =>
        notYet(s"value-to-string for $other in interpolated `s\"...\"` at value position")
        internStringDescriptor("")

  /** Emit i64 → %nex_str descriptor. */
  protected def emitValueToStringInt(v: String): String =
    val r = newReg()
    emitLine(s"  $r = call ptr @__nex_str_from_i64(i64 $v)\n")
    r

  /** Emit double → %nex_str descriptor (`<lld>.0` for whole reals,
    * `%g` otherwise — matches the interpreter's [[formatValue]]).
    */
  protected def emitValueToStringReal(v: String): String =
    val r = newReg()
    emitLine(s"  $r = call ptr @__nex_str_from_double(double $v)\n")
    r

  /** Emit i1 → "true" / "false" descriptor via runtime select. */
  protected def emitValueToStringBool(v: String): String =
    val tPtr = internStringDescriptor("true")
    val fPtr = internStringDescriptor("false")
    val sel  = newReg()
    emitLine(s"  $sel = select i1 $v, ptr $tPtr, ptr $fPtr\n")
    sel

  /** Emit `{ double, double }` → `<re><sign><|im|>i` descriptor. */
  protected def emitValueToStringComplex(v: String): String =
    val re = newReg()
    emitLine(s"  $re = extractvalue { double, double } $v, 0\n")
    val im = newReg()
    emitLine(s"  $im = extractvalue { double, double } $v, 1\n")
    val isNeg = newReg()
    emitLine(s"  $isNeg = fcmp olt double $im, 0.0\n")
    val plus  = internStringDescriptor("+")
    val minus = internStringDescriptor("-")
    val sign  = newReg()
    emitLine(s"  $sign = select i1 $isNeg, ptr $minus, ptr $plus\n")
    val absIm = newReg()
    emitLine(s"  $absIm = call double @fabs(double $im)\n")
    val reStr = emitValueToStringReal(re)
    val imStr = emitValueToStringReal(absIm)
    val iSfx  = internStringDescriptor("i")
    concatChain(List(reStr, sign, imStr, iSfx))

  /** Emit a tuple value → `(a, b, c)` descriptor. */
  protected def emitValueToStringTuple(v: String, tupT: Type, elemTs: List[Type]): String =
    if elemTs.isEmpty then return internStringDescriptor("()")
    val tupTy = llvmType(tupT)
    val open  = internStringDescriptor("(")
    val close = internStringDescriptor(")")
    val sep   = internStringDescriptor(", ")
    val parts = scala.collection.mutable.ListBuffer[String](open)
    for i <- elemTs.indices do
      if i > 0 then parts += sep
      val fieldT = elemTs(i)
      val fv = newReg()
      emitLine(s"  $fv = extractvalue $tupTy $v, $i\n")
      parts += emitTypedValueToString(fv, fieldT)
    parts += close
    concatChain(parts.toList)

  /** Emit a struct value → `Name { k=v, ... }` descriptor. */
  protected def emitValueToStringStruct(
    v: String,
    structT: Type,
    name: String,
    fields: List[(String, Type)],
  ): String =
    val ty = llvmType(structT)
    val header = internStringDescriptor(s"$name { ")
    val closer = internStringDescriptor(" }")
    val sep    = internStringDescriptor(", ")
    val eq     = internStringDescriptor("=")
    val parts  = scala.collection.mutable.ListBuffer[String](header)
    for i <- fields.indices do
      val (fname, ftype) = fields(i)
      if i > 0 then parts += sep
      parts += internStringDescriptor(fname)
      parts += eq
      val fv = newReg()
      emitLine(s"  $fv = extractvalue $ty $v, $i\n")
      parts += emitTypedValueToString(fv, ftype)
    parts += closer
    concatChain(parts.toList)

  /** Emit an array value → `[a, b, c]` (rank-1) or
    * `[[a, b], [c, d]]` (rank-2). Loops at runtime since the length
    * isn't statically known; each iteration concats the per-element
    * descriptor + separator into a single growing accumulator.
    */
  protected def emitValueToStringArray(arr: TExpr): String =
    val rank   = arrayRank(arr.tpe)
    val elem   = arrayElem(arr.tpe)
    val arrV   = emitExpr(arr)
    val result = rank match
      case 1 => emitArrayToStringR1(arrV, arr.tpe, elem)
      case 2 => emitArrayToStringR2(arrV, arr.tpe, elem)
      case r =>
        notYet(s"value-to-string for rank-$r array")
        internStringDescriptor("")
    emitArrDec(arrV, arr.tpe)
    result

  /** Build a `[a, b, c]` descriptor for a rank-1 array. The accumulator
    * starts as `[`, each iteration appends the formatted element and
    * (except for the first) a leading `, `; finally `]` is appended.
    */
  protected def emitArrayToStringR1(arrV: String, arrT: Type, elem: Type): String =
    val open  = internStringDescriptor("[")
    val close = internStringDescriptor("]")
    val sep   = internStringDescriptor(", ")
    val accSlot = newReg()
    emitLine(s"  $accSlot = alloca ptr\n")
    emitLine(s"  store ptr $open, ptr $accSlot\n")
    val len = newReg()
    emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $arrV)\n")
    val buf = bufPtr(arrV, arrT)
    val stT = storageType(elem)
    val llT = llvmType(elem)
    emitCountingLoop(len, "v2s.r1") { i =>
      val isPos = newReg()
      emitLine(s"  $isPos = icmp sgt i64 $i, 0\n")
      val ifFirst = freshLabel("v2s.first")
      val ifSep   = freshLabel("v2s.sep")
      val afterS  = freshLabel("v2s.afterSep")
      emitTerminator(s"  br i1 $isPos, label %$ifSep, label %$ifFirst\n")
      startBlock(ifSep)
      val curS = newReg()
      emitLine(s"  $curS = load ptr, ptr $accSlot\n")
      val withSep = newReg()
      emitLine(s"  $withSep = call ptr @__nex_str_concat(ptr $curS, ptr $sep)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $curS)\n")
      emitLine(s"  store ptr $withSep, ptr $accSlot\n")
      emitTerminator(s"  br label %$afterS\n")
      startBlock(ifFirst)
      emitTerminator(s"  br label %$afterS\n")
      startBlock(afterS)
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $i\n")
      val elemV = loadElem(stT, slot, llT)
      val elemS = emitTypedValueToString(elemV, elem)
      val cur2  = newReg()
      emitLine(s"  $cur2 = load ptr, ptr $accSlot\n")
      val with2 = newReg()
      emitLine(s"  $with2 = call ptr @__nex_str_concat(ptr $cur2, ptr $elemS)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $cur2)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $elemS)\n")
      emitLine(s"  store ptr $with2, ptr $accSlot\n")
    }
    val finalAcc = newReg()
    emitLine(s"  $finalAcc = load ptr, ptr $accSlot\n")
    val withClose = newReg()
    emitLine(s"  $withClose = call ptr @__nex_str_concat(ptr $finalAcc, ptr $close)\n")
    emitLine(s"  call void @__nex_str_dec(ptr $finalAcc)\n")
    withClose

  /** Build a `[[a, b], [c, d]]` descriptor for a rank-2 array. Outer
    * loop walks rows; inner builds each row's `[..]` form. Reuses
    * the rank-1 layout per row by concat-chaining manually rather
    * than re-entering the runtime helper (interpreter inlines the
    * loop too).
    */
  protected def emitArrayToStringR2(arrV: String, arrT: Type, elem: Type): String =
    val open  = internStringDescriptor("[")
    val close = internStringDescriptor("]")
    val sep   = internStringDescriptor(", ")
    val accSlot = newReg()
    emitLine(s"  $accSlot = alloca ptr\n")
    emitLine(s"  store ptr $open, ptr $accSlot\n")
    val rows = newReg()
    emitLine(s"  $rows = call i64 @__nex_arr2_rows(ptr $arrV)\n")
    val cols = newReg()
    emitLine(s"  $cols = call i64 @__nex_arr2_cols(ptr $arrV)\n")
    val buf = bufPtr(arrV, arrT)
    val stT = storageType(elem)
    val llT = llvmType(elem)
    emitCountingLoop(rows, "v2s.r2.row") { i =>
      val isPos = newReg()
      emitLine(s"  $isPos = icmp sgt i64 $i, 0\n")
      val ifSep  = freshLabel("v2s.r2.sep")
      val ifFst  = freshLabel("v2s.r2.first")
      val afterS = freshLabel("v2s.r2.afterSep")
      emitTerminator(s"  br i1 $isPos, label %$ifSep, label %$ifFst\n")
      startBlock(ifSep)
      val curS = newReg()
      emitLine(s"  $curS = load ptr, ptr $accSlot\n")
      val withSep = newReg()
      emitLine(s"  $withSep = call ptr @__nex_str_concat(ptr $curS, ptr $sep)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $curS)\n")
      emitLine(s"  store ptr $withSep, ptr $accSlot\n")
      emitTerminator(s"  br label %$afterS\n")
      startBlock(ifFst)
      emitTerminator(s"  br label %$afterS\n")
      startBlock(afterS)
      val curOpen = newReg()
      emitLine(s"  $curOpen = load ptr, ptr $accSlot\n")
      val withOpen = newReg()
      emitLine(s"  $withOpen = call ptr @__nex_str_concat(ptr $curOpen, ptr $open)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $curOpen)\n")
      emitLine(s"  store ptr $withOpen, ptr $accSlot\n")
      val rowOff = newReg()
      emitLine(s"  $rowOff = mul i64 $i, $cols\n")
      emitCountingLoop(cols, "v2s.r2.col") { j =>
        val isPosJ = newReg()
        emitLine(s"  $isPosJ = icmp sgt i64 $j, 0\n")
        val jSep   = freshLabel("v2s.r2.jsep")
        val jFst   = freshLabel("v2s.r2.jfirst")
        val jAfter = freshLabel("v2s.r2.jafter")
        emitTerminator(s"  br i1 $isPosJ, label %$jSep, label %$jFst\n")
        startBlock(jSep)
        val curSj = newReg()
        emitLine(s"  $curSj = load ptr, ptr $accSlot\n")
        val withSepJ = newReg()
        emitLine(s"  $withSepJ = call ptr @__nex_str_concat(ptr $curSj, ptr $sep)\n")
        emitLine(s"  call void @__nex_str_dec(ptr $curSj)\n")
        emitLine(s"  store ptr $withSepJ, ptr $accSlot\n")
        emitTerminator(s"  br label %$jAfter\n")
        startBlock(jFst)
        emitTerminator(s"  br label %$jAfter\n")
        startBlock(jAfter)
        val k = newReg()
        emitLine(s"  $k = add i64 $rowOff, $j\n")
        val slot = newReg()
        emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $k\n")
        val elemV = loadElem(stT, slot, llT)
        val elemS = emitTypedValueToString(elemV, elem)
        val cur2 = newReg()
        emitLine(s"  $cur2 = load ptr, ptr $accSlot\n")
        val with2 = newReg()
        emitLine(s"  $with2 = call ptr @__nex_str_concat(ptr $cur2, ptr $elemS)\n")
        emitLine(s"  call void @__nex_str_dec(ptr $cur2)\n")
        emitLine(s"  call void @__nex_str_dec(ptr $elemS)\n")
        emitLine(s"  store ptr $with2, ptr $accSlot\n")
      }
      val curClose = newReg()
      emitLine(s"  $curClose = load ptr, ptr $accSlot\n")
      val withClose = newReg()
      emitLine(s"  $withClose = call ptr @__nex_str_concat(ptr $curClose, ptr $close)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $curClose)\n")
      emitLine(s"  store ptr $withClose, ptr $accSlot\n")
    }
    val finalAcc = newReg()
    emitLine(s"  $finalAcc = load ptr, ptr $accSlot\n")
    val withClose = newReg()
    emitLine(s"  $withClose = call ptr @__nex_str_concat(ptr $finalAcc, ptr $close)\n")
    emitLine(s"  call void @__nex_str_dec(ptr $finalAcc)\n")
    withClose

  /** Like [[emitValueToString]] but takes an already-emitted SSA value
    * (from extractvalue / loadElem) plus a Type. Used by the aggregate
    * formatters to recurse into their already-loaded components.
    */
  protected def emitTypedValueToString(v: String, t: Type): String =
    t match
      case TyString  =>
        // `v` was loaded out of an aggregate (extractvalue / loadElem)
        // and carries no ownership — inc here so the caller (concat
        // chain or print site) can dec uniformly with every other
        // typed-formatter result.
        emitLine(s"  call void @__nex_str_inc(ptr $v)\n")
        v
      case TyInteger => emitValueToStringInt(v)
      case TyReal    => emitValueToStringReal(v)
      case TyBool    => emitValueToStringBool(v)
      case TyComplex => emitValueToStringComplex(v)
      case TyTuple(ts) => emitValueToStringTuple(v, t, ts)
      case TyStruct(n, fs) => emitValueToStringStruct(v, t, n, fs)
      case TyUnit    => internStringDescriptor("()")
      case other =>
        notYet(s"value-to-string for nested $other"); internStringDescriptor("")

  /** Fold a list of owning descriptor pointers down to one via repeated
    * __nex_str_concat, releasing each operand share as it's consumed.
    * Empty list → empty-string literal descriptor (immortal). Single-
    * element list → the input untouched (caller owns it). Immortal
    * literals are silently skipped by __nex_str_dec, so mixing
    * computed and literal parts is safe.
    */
  protected def concatChain(parts: List[String]): String =
    parts match
      case Nil      => internStringDescriptor("")
      case List(d)  => d
      case d :: ds  =>
        ds.foldLeft(d) { (acc, next) =>
          val r = newReg()
          emitLine(s"  $r = call ptr @__nex_str_concat(ptr $acc, ptr $next)\n")
          emitLine(s"  call void @__nex_str_dec(ptr $acc)\n")
          emitLine(s"  call void @__nex_str_dec(ptr $next)\n")
          r
        }

  /** Emit a concat chain that builds the full interpolated-string value.
    * Empty parts list collapses to the empty-string literal descriptor.
    */
  protected def emitInterpStringValue(parts: List[TInterpPart]): String =
    val partDescs: List[String] = parts.map {
      case TInterpText(text) => internStringDescriptor(text)
      case TInterpRef(sym)   => emitValueToString(TVarRef(sym, None, sym.tpe))
      case TInterpExpr(x)    => emitValueToString(x)
      case _: TInterpRaw     =>
        notYet("interpolated `${...}` raw fragment (should have been re-parsed in Stage 1)")
        internStringDescriptor("")
    }
    concatChain(partDescs)
