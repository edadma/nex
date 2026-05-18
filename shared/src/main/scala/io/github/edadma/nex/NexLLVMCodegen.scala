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
    val topBindings = tp.decls.collect { case b: TTopBinding => b }
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

    for d <- tp.decls do d match
      case f: TFunDecl =>
        emitFunction(f)
      case _: TTopBinding | _: TStructDecl | _: TModuleDecl | _: TImportDecl =>
        () // top-bindings already emitted as globals; struct/module are metadata

    // Flush the string-literal pool at the END. LLVM IR allows forward
    // references to module-level identifiers, so a function body that
    // uses `@.str.N` works even though the constant is defined below.
    flushStringPool()

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

  private def emitFunction(f: TFunDecl): Unit =
    regCounter   = 0
    labelCounter = 0
    locals.clear()
    arrayLocalSlots.clear()
    blockArrayScopes = Nil

    val isMain  = f.sym.name == "main" && f.params.isEmpty
    val retLLT  = if isMain then "i32" else llvmType(f.returnType)
    val funcId  = f.sym.name

    currentReturnType = f.returnType
    currentIsMain     = isMain

    val paramSig =
      f.params.zipWithIndex
        .map { case (p, i) => s"${llvmType(p.tpe)} %arg$i" }
        .mkString(", ")

    out.append(s"define $retLLT @$funcId($paramSig) {\n")
    currentBlock = Some("entry")
    out.append("entry:\n")

    // main initializes all top-level bindings before running its body
    // — matches the interpreter's two-stage init (functions first via
    // module emission order, then top-binding initializers).
    if isMain && globalBindings.nonEmpty then
      emitLine("  call void @__nex_init_globals()\n")

    // Spill each param to an alloca so TVarRef loads work uniformly.
    // Array-typed params arrive already inc'd by the caller (per the
    // owned-everywhere convention); the slot takes ownership and the
    // function-exit dec releases it.
    for ((p, i) <- f.params.zipWithIndex) do
      val ty   = llvmType(p.tpe)
      val slot = newReg()
      emitLine(s"  $slot = alloca $ty\n")
      emitLine(s"  store $ty %arg$i, ptr $slot\n")
      locals(p.id) = slot
      if isArrayType(p.tpe) then arrayLocalSlots(p.id) = (slot, p.tpe)

    val result = emitExpr(f.body)

    // Emit the final ret only if no terminator has fired yet. Inside the
    // body, an early `return` (or both branches of an if that return)
    // will already have closed the block (with their own dec-locals).
    if currentBlock.isDefined then
      // If the function body produced an owning array value but we are
      // about to discard it (main returns i32, or fn returns void), dec
      // the result first so it doesn't leak.
      val bodyIsArray = isArrayType(f.body.tpe)
      val willDiscardResult = isMain || f.returnType == TyUnit || result == "void"
      if bodyIsArray && willDiscardResult && result != "0" && result != "void" then
        emitArrDec(result, f.body.tpe)

      decAllLocalArrays()

      if isMain then
        emitTerminator("  ret i32 0\n")
      else if f.returnType == TyUnit || result == "void" then
        emitTerminator("  ret void\n")
      else
        emitTerminator(s"  ret ${llvmType(f.returnType)} $result\n")

    out.append("}\n\n")

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

    case TStringLit(s, _, _) =>
      // String literals lower to a private global; the SSA value is the
      // global's pointer. Identical literals share a single global.
      internStringLiteral(s)

    case TInterpStringLit(parts, _, _) =>
      // For chunk-4 v0, only the print-statement form is fully
      // supported (see emitPrintCall). At value position, surface a
      // diagnostic — building the interpolated string into a heap
      // buffer needs sprintf + malloc.
      notYet("interpolated string at value position (use print)"); "null"

    case TVarRef(s, _, t) if s.kind == SymKind.Prelude =>
      s.name match
        case "pi"  => "0x400921FB54442D18"  // double bit-rep of math.Pi
        case "e"   => "0x4005BF0A8B145769"  // math.E
        case "inf" => "0x7FF0000000000000"
        case "nan" => "0x7FF8000000000000"
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
          if isArrayType(x.tpe) then emitArrDec(v, x.tpe)
      }
      val rv    = emitExpr(result)
      val scope = popBlockScope()
      decBlockScope(scope)
      rv

    case TAssign(target, value, _, _) =>
      emitAssign(target, value); "void"

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
    if loopVars.size != 1 then
      notYet("for-over-array with tuple destructuring")
      return

    val rank = arrayRank(iter.tpe)
    val elem = arrayElem(iter.tpe)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val langT = llvmType(elem)

    // Compute the array ptr once, stash in a slot so the cond-block can
    // reload it (we don't have phi over array ptrs yet). The length is
    // also stashed so we don't re-call the helper each iteration.
    val arrV  = emitExpr(iter)
    val arrSlot = newReg()
    emitLine(s"  $arrSlot = alloca ptr\n")
    emitLine(s"  store ptr $arrV, ptr $arrSlot\n")

    val lenReg = newReg()
    rank match
      case 1 => emitLine(s"  $lenReg = call i64 @__nex_arr1_len(ptr $arrV)\n")
      case 2 => emitLine(s"  $lenReg = call i64 @__nex_arr2_len(ptr $arrV)\n")
      case other => notYet(s"for over rank-$other array"); return

    val iSlot = newReg()
    emitLine(s"  $iSlot = alloca i64\n")
    emitLine(s"  store i64 0, ptr $iSlot\n")

    val loopVar = loopVars.head
    val xSlot   = newReg()
    emitLine(s"  $xSlot = alloca $langT\n")
    locals(loopVar.id) = xSlot

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
    // Release the owning share we took on the iter expression. (An early
    // `return` inside the body would skip this — a known leak for chunk
    // 6; the larger fix is to register the synthetic iter alloca with
    // `arrayLocalSlots` so it participates in decAllLocalArrays.)
    val arrFinal = newReg()
    emitLine(s"  $arrFinal = load ptr, ptr $arrSlot\n")
    emitArrDec(arrFinal, iter.tpe)

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
        if willDiscard && isArrayType(expr.tpe) && rv != "0" && rv != "void" then
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
        val rv = emitExpr(value)
        // For array slots, the previous occupant owns a share — dec it
        // before storing the new value so the old buffer can be freed if
        // this was its last reference. Scalars need no such cleanup.
        locals.get(s.id) match
          case Some(slot) =>
            if isArrayType(t) then
              val old = newReg()
              emitLine(s"  $old = load ${llvmType(t)}, ptr $slot\n")
              emitArrDec(old, t)
            emitLine(s"  store ${llvmType(t)} $rv, ptr $slot\n")
          case None if globalBindings.contains(s.id) =>
            if isArrayType(t) then
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

      case other =>
        notYet(s"assign to ${other.getClass.getSimpleName}")

  private def emitLocalBinding(sym: Symbol, value: TExpr): Unit =
    val rv   = emitExpr(value)
    val ty   = llvmType(sym.tpe)
    val slot = newReg()
    emitLine(s"  $slot = alloca $ty\n")
    if ty != "void" then emitLine(s"  store $ty $rv, ptr $slot\n")
    locals(sym.id) = slot
    // Register array-typed bindings into the innermost block scope (or
    // function-level if not inside one) so they're dec'd at scope exit.
    // The slot takes ownership of the stored ref; no extra inc needed —
    // [[emitExpr]] already returned an owning value.
    if isArrayType(sym.tpe) then
      registerArraySlot(sym.id, slot, sym.tpe)

  // ---------------------------------------------------------------------------
  // User-function call — emits `call <retT> @<name>(<argT> <arg>, ...)`.
  // A unit-returning callee produces a `call void @name(...)` with no
  // result register; the emitExpr return value is `"void"` so any
  // downstream consumer (statement position) discards it.
  // ---------------------------------------------------------------------------

  private def emitUserCall(callee: Symbol, calleeT: Type, args: List[TExpr]): String =
    val argList = args.map { a =>
      val v = emitExpr(a)
      s"${llvmType(a.tpe)} $v"
    }.mkString(", ")

    val retT = calleeT match
      case TyFunc(_, r) => r
      case _            => callee.tpe match
        case TyFunc(_, r) => r
        case _            => TyUnknown

    retT match
      case TyUnit =>
        emitLine(s"  call void @${callee.name}($argList)\n")
        "void"
      case other =>
        val reg = newReg()
        emitLine(s"  $reg = call ${llvmType(other)} @${callee.name}($argList)\n")
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

  /** Lower `t.<idx>` — extract field `idx` from a tuple value. */
  private def emitTupleProj(receiver: TExpr, idx: Int, resultT: Type): String =
    val recv = emitExpr(receiver)
    val recvTy = llvmType(receiver.tpe)
    val reg = newReg()
    emitLine(s"  $reg = extractvalue $recvTy $recv, $idx\n")
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
    * TyStruct then emit `extractvalue`.
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
          reg
      case other =>
        notYet(s"field access on non-struct type $other"); "0"
