package io.github.edadma.nex

import scala.collection.mutable

/** Closure subsystem: lambda pre-pass (collecting var bindings + lambdas
  * with their capture layouts), synthetic top-level function emission,
  * closure construction at the use site, and indirect closure dispatch.
  */
protected trait NexLLVMLambdas extends NexLLVMState:

  // ---------------------------------------------------------------------------
  // Lambda pre-pass — walks every function body once, collects each TLambda
  // (in deterministic traversal order), records its capture set, and stores
  // the result in [[lambdaTable]] for later use by the construction site
  // and the synthetic function emitter.
  // ---------------------------------------------------------------------------

  protected def collectVarBindings(tp: TProgram): Unit =
    def walkExpr(e: TExpr): Unit = e match
      case TBlock(items, r, _, _) =>
        items.foreach {
          case TBlockBinding(s, BindingKind.Var, v) => varBindings += s.id; walkExpr(v)
          case TBlockBinding(_, _, v)               => walkExpr(v)
          case TBlockExpr(x)                        => walkExpr(x)
        }
        walkExpr(r)
      case _ => walkChildren(e, walkExpr)
    for d <- tp.decls do d match
      case b: TTopBinding if b.kind == BindingKind.Var => varBindings += b.sym.id; walkExpr(b.value)
      case b: TTopBinding                              => walkExpr(b.value)
      case f: TFunDecl                                 => walkExpr(f.body)
      case _                                           => ()

  protected def collectLambdas(tp: TProgram): Unit =
    def walkExpr(e: TExpr, bound: Set[Int]): Unit = e match
      case lam @ TLambda(params, body, _, _) =>
        val paramIds = params.map(_.id).toSet
        val newBound = bound ++ paramIds
        // For free-var analysis we treat the outer scope as UNBOUND so
        // every outer reference becomes a capture. The lambda's own
        // params are the only true bindings during analysis.
        val frees    = scala.collection.mutable.LinkedHashMap.empty[Int, (Symbol, Type)]
        collectFrees(body, paramIds, frees)
        val captures = frees.values.toList.map { case (s, t) =>
          val mode = if varBindings.contains(s.id) then CaptureMode.ByRef else CaptureMode.ByVal
          (s, t, mode)
        }
        val envFieldTypes = captures.map {
          case (_, _, CaptureMode.ByRef) => "ptr"
          case (_, t, CaptureMode.ByVal) => llvmType(t)
        }
        val envTy =
          if envFieldTypes.isEmpty then ""
          else envFieldTypes.mkString("{ ", ", ", " }")
        val envSize = captures.map {
          case (_, _, CaptureMode.ByRef) => 8
          case (_, t, CaptureMode.ByVal) => captureSlotSize(t)
        }.sum
        val id       = lambdaTable.size
        val llvmName = s"__nex_lambda_$id"
        val retType  = lam.tpe match
          case TyFunc(_, r) => r
          case _            => body.tpe
        lambdaTable.put(lam, LambdaInfo(id, llvmName, params, body, retType, captures, envTy, envSize))
        // Recurse into the body using newBound — nested lambdas inside
        // see the params as bound (so they don't capture them again).
        walkExpr(body, newBound)
      case TBlock(items, r, _, _) =>
        var sub = bound
        items.foreach {
          case TBlockBinding(s, _, v) => walkExpr(v, sub); sub = sub + s.id
          case TBlockExpr(x)          => walkExpr(x, sub)
        }
        walkExpr(r, sub)
      case TFor(loopVars, iter, body, _, _) =>
        walkExpr(iter, bound)
        val sub = bound ++ loopVars.map(_.id)
        walkExpr(body, sub)
      case _ =>
        walkChildren(e, walkExpr(_, bound))

    for d <- tp.decls do d match
      case f: TFunDecl =>
        val paramSet = f.params.map(_.id).toSet
        walkExpr(f.body, paramSet)
      case b: TTopBinding =>
        walkExpr(b.value, Set.empty)
      case _ => ()

  /** Walk an expression's children invoking `f` on each. Local mirror of
    * [[NexLifetime.walkChildren]] so codegen doesn't depend on the
    * lifetime pass.
    */
  private def walkChildren(e: TExpr, f: TExpr => Unit): Unit = e match
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit
       | _: TUnitLit | _: TVarRef | _: TAxisAllMark => ()
    case TInterpStringLit(parts, _, _) =>
      parts.foreach {
        case TInterpExpr(x) => f(x)
        case _              => ()
      }
    case TBinOp(_, l, r, _, _)            => f(l); f(r)
    case TUnaryOp(_, x, _, _)             => f(x)
    case TJuxtapose(c, b, _, _)           => f(c); f(b)
    case TElementWise(_, l, r, _, _)      => f(l); f(r)
    case TBroadcast(s, a, _, _, _, _)     => f(s); f(a)
    case TMap(a, fn, _, _)                => f(a); f(fn)
    case TReduce(a, i, fn, _, _)          => f(a); f(i); f(fn)
    case TMatMul(l, r, _, _)              => f(l); f(r)
    case TFusedLoop(_, len, b, cols, _, _) =>
      f(len); f(b); cols.foreach(f)
    case TFlatIndex(a, i, _, _)           => f(a); f(i)
    case TClone(a, _, _)                  => f(a)
    case TCall(c, args, _, _)             => f(c); args.foreach(f)
    case TIndex(a, idx, _, _)             => f(a); idx.foreach(f)
    case TSlice(a, lo, hi, _, _, _)       => f(a); f(lo); f(hi)
    case TSlice2(a, rAx, cAx, _, _)       =>
      f(a)
      def goAx(ax: TAxisSpec): Unit = ax match
        case TAxisAll              => ()
        case TAxisIndex(e2)        => f(e2)
        case TAxisRange(lo, hi, _) => f(lo); f(hi)
      goAx(rAx); goAx(cAx)
    case TField(r, _, _, _)               => f(r)
    case TTupleProj(r, _, _, _)           => f(r)
    case TMethodCall(r, _, args, _, _)    => f(r); args.foreach(f)
    case TLambda(_, body, _, _)           => f(body)
    case TTuple(es, _, _)                 => es.foreach(f)
    case TArrayLit(es, _, _)              => es.foreach(f)
    case TIf(c, t, e2, _, _)              => f(c); f(t); e2.foreach(f)
    case TFor(_, it, b, _, _)             => f(it); f(b)
    case TWhile(c, b, _, _)               => f(c); f(b)
    case TReturn(v, _, _)                 => v.foreach(f)
    case TAssign(t, v, _, _)              => f(t); f(v)
    case TBlock(items, r, _, _) =>
      items.foreach {
        case TBlockBinding(_, _, v) => f(v)
        case TBlockExpr(x)          => f(x)
      }
      f(r)

  /** Collect free (un-bound) variable references in `e`. A reference is
    * "free" when its Symbol id is not in the current `bound` set.
    * Captures Local / Param / TopLevel only — Function / TypeName /
    * Prelude / Module / Import / Field resolve to fixed top-level
    * definitions, not closure state.
    */
  private def collectFrees(
      e: TExpr,
      bound: Set[Int],
      acc: scala.collection.mutable.LinkedHashMap[Int, (Symbol, Type)],
  ): Unit = e match
    case TVarRef(s, _, t) if !bound.contains(s.id) && capturable(s) =>
      if !acc.contains(s.id) then acc(s.id) = (s, t)
    case TLambda(ps, body, _, _) =>
      collectFrees(body, bound ++ ps.map(_.id), acc)
    case TBlock(items, r, _, _) =>
      var sub = bound
      items.foreach {
        case TBlockBinding(s, _, v) => collectFrees(v, sub, acc); sub = sub + s.id
        case TBlockExpr(x)          => collectFrees(x, sub, acc)
      }
      collectFrees(r, sub, acc)
    case TFor(vs, it, body, _, _) =>
      collectFrees(it, bound, acc)
      collectFrees(body, bound ++ vs.map(_.id), acc)
    case _ =>
      walkChildren(e, x => collectFrees(x, bound, acc))

  /** Which Symbol kinds participate in closure capture. */
  private def capturable(s: Symbol): Boolean = s.kind match
    case SymKind.Local | SymKind.Param | SymKind.TopLevel => true
    case _                                                 => false

  /** Size of a captured value's slot in the env struct when stored
    * by-value (ByRef slots are always 8 bytes — `ptr`). Aggregates
    * use [[elemSize]] which already sums field sizes. Closure values
    * are `{ ptr, ptr }` — 16 bytes by value.
    */
  private def captureSlotSize(t: Type): Int = t match
    case TyBool       => 1
    case TyInteger    => 8
    case TyReal       => 8
    case TyString     => 8
    case TyFunc(_, _) => 16
    case _            => elemSize(t)

  // ---------------------------------------------------------------------------
  // Synthetic lambda functions.
  //
  // For each TLambda found in the pre-pass, emit a top-level function
  // `define <retT> @__nex_lambda_<N>(ptr %env, T0 %arg0, ...)`. The body
  // starts by loading every capture out of `env` into locals so the rest
  // of `emitExpr` is unchanged. Captures with mode ByVal are stored
  // directly in the env struct (so the load yields a value); ByRef
  // captures store a pointer to the parent's alloca, so the lambda
  // body's read goes through one extra load.
  //
  // To make ByRef captures transparent to `emitExpr`, we don't enter
  // the captured Symbol's slot into [[locals]]. Instead we install
  // [[lambdaCaptures]] for the duration of the lambda body; the
  // [[TVarRef]] case in `emitExpr` consults it first and emits the
  // env-GEP+load sequence inline.
  // ---------------------------------------------------------------------------

  protected def emitLambdaFunctions(): Unit =
    val it = lambdaTable.values().iterator()
    val sorted = scala.collection.mutable.ArrayBuffer.empty[LambdaInfo]
    while it.hasNext do sorted += it.next()
    sorted.sortInPlaceBy(_.id)
    for info <- sorted do
      emitOneLambda(info)
      if needsEnvDtor(info) then emitEnvDtor(info)

  /** A lambda needs a dedicated env-dtor function when its env owns
    * any refcounted shares — i.e., at least one ByVal capture of a
    * refcounted type. ByRef captures store parent-owned pointers and
    * must NOT be dec'd here.
    */
  private def needsEnvDtor(info: LambdaInfo): Boolean =
    info.captures.exists {
      case (_, t, CaptureMode.ByVal) => isRefCountedType(t)
      case _                         => false
    }

  /** Operand for the `dtor` argument to `__nex_env_alloc`. Returns
    * either `@__nex_lambda_<id>_env_dtor` (when the env owns
    * refcounted shares) or the literal `null` (plain free is correct
    * when there's nothing to dec).
    */
  private def envDtorOperand(info: LambdaInfo): String =
    if needsEnvDtor(info) then s"@${info.llvmName}_env_dtor" else "null"

  /** Emit `void @__nex_lambda_<id>_env_dtor(ptr %env)` — walks each
    * refcounted ByVal capture and dec's it, then frees the header.
    * ByRef captures are skipped (they store parent-owned pointers).
    * Closure-valued captures extract the env from the `{ fn, env }`
    * value before routing through the generic env_dec.
    */
  private def emitEnvDtor(info: LambdaInfo): Unit =
    val name = s"${info.llvmName}_env_dtor"
    val sb = new StringBuilder
    sb.append(s"define void @$name(ptr %env) {\n")
    sb.append("entry:\n")
    var localCtr = 0
    def freshLocal(): String =
      localCtr += 1
      s"%v$localCtr"
    for (((s, t, mode), idx) <- info.captures.zipWithIndex) do
      mode match
        case CaptureMode.ByVal if isRefCountedType(t) =>
          val slot = freshLocal()
          sb.append(s"  $slot = getelementptr inbounds ${info.envTy}, ptr %env, i32 0, i32 $idx\n")
          val v = freshLocal()
          sb.append(s"  $v = load ${llvmType(t)}, ptr $slot\n")
          t match
            case TyString =>
              sb.append(s"  call void @__nex_str_dec(ptr $v)\n")
            case TyArray(_, _) =>
              sb.append(s"  call void ${arrDecFor(t)}(ptr $v)\n")
            case TyFunc(_, _) =>
              val env = freshLocal()
              sb.append(s"  $env = extractvalue { ptr, ptr } $v, 1\n")
              sb.append(s"  call void @__nex_env_dec(ptr $env)\n")
            case _ => ()
        case _ => ()
    val hdr = freshLocal()
    sb.append(s"  $hdr = getelementptr inbounds i8, ptr %env, i64 -16\n")
    sb.append(s"  call void @free(ptr $hdr)\n")
    sb.append("  ret void\n")
    sb.append("}\n\n")
    out.append(sb.toString)

  private def emitOneLambda(info: LambdaInfo): Unit =
    regCounter   = 0
    labelCounter = 0
    locals.clear()
    arrayLocalSlots.clear()
    blockArrayScopes = Nil
    currentReturnType = info.retType
    currentIsMain     = false

    val retLLT = llvmType(info.retType)
    // Same `noalias` treatment as ordinary user functions — see the
    // matching comment in NexLLVMCodegen.emitFunction. The env pointer
    // is not marked noalias because nested closures can share the
    // outer env (the lambda captures route through it).
    val paramSig =
      ("ptr %env" :: info.params.zipWithIndex.map { case (p, i) =>
        val attr = if isArrayType(p.tpe) then " noalias" else ""
        s"${llvmType(p.tpe)}$attr %arg$i"
      }).mkString(", ")

    out.append(s"define $retLLT @${info.llvmName}($paramSig) {\n")
    currentBlock = Some("entry")
    out.append("entry:\n")

    // Spill each lambda param to an alloca so TVarRef loads work uniformly.
    for ((p, i) <- info.params.zipWithIndex) do
      val ty   = llvmType(p.tpe)
      val slot = newReg()
      emitLine(s"  $slot = alloca $ty\n")
      emitLine(s"  store $ty %arg$i, ptr $slot\n")
      locals(p.id) = slot
      if isRefCountedType(p.tpe) then arrayLocalSlots(p.id) = (slot, p.tpe)

    // Install per-lambda capture map; TVarRef in emitExpr will consult
    // this and emit the env-GEP+load sequence inline.
    lambdaCaptures = info.captures.zipWithIndex.map { case ((s, t, m), i) =>
      s.id -> (i, t, m)
    }.toMap
    lambdaEnvTy = info.envTy

    val result = emitExpr(info.body)

    if currentBlock.isDefined then
      val bodyIsRefCounted = isRefCountedType(info.body.tpe)
      val willDiscardResult = info.retType == TyUnit || result == "void"
      if bodyIsRefCounted && willDiscardResult && result != "0" && result != "void" then
        emitArrDec(result, info.body.tpe)

      decAllLocalArrays()

      if info.retType == TyUnit || result == "void" then
        emitTerminator("  ret void\n")
      else
        emitTerminator(s"  ret $retLLT $result\n")

    // Clear lambda capture map so subsequent emissions don't see stale state.
    lambdaCaptures = Map.empty
    lambdaEnvTy = ""

    out.append("}\n\n")

  // ---------------------------------------------------------------------------
  // Closure construction + dispatch (chunk 9).
  // ---------------------------------------------------------------------------

  /** Construct a closure value `{ fn_ptr, env_ptr }` from a TLambda. The
    * synthetic function was emitted in the pre-pass — look it up in
    * [[lambdaTable]], allocate + populate the env (or set env to null if
    * the lambda captures nothing), then `insertvalue` both fields into
    * the closure literal.
    */
  protected def emitLambdaConstruct(lam: TLambda): String =
    val info = lambdaTable.get(lam)
    if info == null then
      notYet("lambda not registered in pre-pass — codegen bug"); "0"
    else
      val envPtr =
        if info.captures.isEmpty then "null"
        else
          val ep = newReg()
          val dtor = envDtorOperand(info)
          emitLine(s"  $ep = call ptr @__nex_env_alloc(i64 ${info.envSize}, ptr $dtor)\n")
          // Store each capture into its env slot. ByVal captures emit
          // a TVarRef-style load of the source value; ByRef captures
          // store a pointer to the source binding's alloca (or @global)
          // so mutations remain visible to the lambda body.
          for (((s, t, mode), idx) <- info.captures.zipWithIndex) do
            val slot = newReg()
            emitLine(s"  $slot = getelementptr inbounds ${info.envTy}, ptr $ep, i32 0, i32 $idx\n")
            mode match
              case CaptureMode.ByVal =>
                val v = readCapturedValue(s, t)
                emitLine(s"  store ${llvmType(t)} $v, ptr $slot\n")
              case CaptureMode.ByRef =>
                val p = lvalueOfBinding(s)
                emitLine(s"  store ptr $p, ptr $slot\n")
          ep
      val c0 = newReg()
      emitLine(s"  $c0 = insertvalue { ptr, ptr } undef, ptr @${info.llvmName}, 0\n")
      val c1 = newReg()
      emitLine(s"  $c1 = insertvalue { ptr, ptr } $c0, ptr $envPtr, 1\n")
      c1

  /** Read the current value of a binding referenced from a closure-
    * construction site (the parent scope), inc'ing any refcounted
    * value so the env owns its own share. The matching dec lives in
    * the per-lambda env-dtor (see [[emitEnvDtor]]) — without the inc
    * here, the dtor would dec a share that was never produced and the
    * source binding would be over-freed when its own scope closes.
    * ByRef captures store a pointer to the parent alloca and must NOT
    * inc (the parent's slot is the sole owner).
    */
  private def readCapturedValue(s: Symbol, t: Type): String =
    lambdaCaptures.get(s.id) match
      case Some((idx, capT, mode)) =>
        // Nested capture: the outer-lambda body is the parent here, so
        // the binding lives in the OUTER env. Recurse: read through env.
        mode match
          case CaptureMode.ByVal =>
            val slot = newReg()
            emitLine(s"  $slot = getelementptr inbounds $lambdaEnvTy, ptr %env, i32 0, i32 $idx\n")
            val reg = newReg()
            emitLine(s"  $reg = load ${llvmType(capT)}, ptr $slot\n")
            if isRefCountedType(capT) then emitArrInc(reg, capT)
            reg
          case CaptureMode.ByRef =>
            val pslot = newReg()
            emitLine(s"  $pslot = getelementptr inbounds $lambdaEnvTy, ptr %env, i32 0, i32 $idx\n")
            val pp    = newReg()
            emitLine(s"  $pp = load ptr, ptr $pslot\n")
            val reg   = newReg()
            emitLine(s"  $reg = load ${llvmType(capT)}, ptr $pp\n")
            reg
      case None =>
        locals.get(s.id) match
          case Some(slot) =>
            val reg = newReg()
            emitLine(s"  $reg = load ${llvmType(t)}, ptr $slot\n")
            if isRefCountedType(t) then emitArrInc(reg, t)
            reg
          case None if globalBindings.contains(s.id) =>
            val reg = newReg()
            emitLine(s"  $reg = load ${llvmType(t)}, ptr @${s.name}\n")
            if isRefCountedType(t) then emitArrInc(reg, t)
            reg
          case None =>
            notYet(s"capture of unbound `${s.name}`"); "0"

  /** Resolve a binding to a pointer for ByRef capture. Returns the
    * alloca / global pointer where the binding's value lives, so the
    * env can stash it.
    */
  private def lvalueOfBinding(s: Symbol): String =
    lambdaCaptures.get(s.id) match
      case Some((idx, _, CaptureMode.ByRef)) =>
        // Re-capturing an already-ByRef'd var from a nested lambda:
        // pull the pointer straight out of the outer env.
        val pslot = newReg()
        emitLine(s"  $pslot = getelementptr inbounds $lambdaEnvTy, ptr %env, i32 0, i32 $idx\n")
        val pp = newReg()
        emitLine(s"  $pp = load ptr, ptr $pslot\n")
        pp
      case Some(_) =>
        notYet(s"ByRef recapture of ByVal `${s.name}`"); "null"
      case None =>
        locals.get(s.id) match
          case Some(slot) => slot
          case None if globalBindings.contains(s.id) => s"@${s.name}"
          case None       => notYet(s"ByRef capture of unbound `${s.name}`"); "null"

  /** Emit a TyFunc-typed call: extract `{fn_ptr, env_ptr}` and call
    * indirectly with `env` prepended to the arg list.
    */
  protected def emitClosureCall(callee: TExpr, args: List[TExpr], retT: Type): String =
    val cl = emitExpr(callee)
    val fnPtr = newReg()
    emitLine(s"  $fnPtr = extractvalue { ptr, ptr } $cl, 0\n")
    val envPtr = newReg()
    emitLine(s"  $envPtr = extractvalue { ptr, ptr } $cl, 1\n")
    val argList =
      (s"ptr $envPtr" :: args.map { a =>
        val v = emitExpr(a)
        s"${llvmType(a.tpe)} $v"
      }).mkString(", ")

    // For the indirect call, LLVM requires the function type. Build
    // it from retT + the prepended env ptr + each arg type. Bool args
    // pass as i1 (matches the lambda's declared signature).
    val argSig = ("ptr" :: args.map(a => llvmType(a.tpe))).mkString(", ")
    val retLLT = llvmType(retT)
    val sig    = s"$retLLT ($argSig)"

    // `emitExpr(callee)` returned a closure value with an owning share
    // of env (TVarRef inc's on load; TLambda literals come fresh from
    // __nex_env_alloc with rc=1). The call doesn't consume it, so we
    // dec env once we're done dispatching through it.
    retT match
      case TyUnit =>
        emitLine(s"  call $sig $fnPtr($argList)\n")
        emitLine(s"  call void @__nex_env_dec(ptr $envPtr)\n")
        "void"
      case _ =>
        val reg = newReg()
        emitLine(s"  $reg = call $sig $fnPtr($argList)\n")
        emitLine(s"  call void @__nex_env_dec(ptr $envPtr)\n")
        reg
