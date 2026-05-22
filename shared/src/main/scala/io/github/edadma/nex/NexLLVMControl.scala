package io.github.edadma.nex

/** Control-flow and L-value lowering for the LLVM backend.
  *
  * Bundles the emitters routed through `emitExpr` for the control-flow
  * AST shapes (`TIf` / `TWhile` / `TFor` / `TReturn` / `TAssign`),
  * the short-circuit `and` / `or` lowering, and the divide-by-zero
  * trap insertion shared by `/` / `div` / `%` on integer and real
  * operands. Also owns the L-value walker pieces:
  *
  *   - [[emitAssign]] dispatches on the target's AST shape.
  *   - [[resolveFieldChain]] flattens nested `TField` receivers to an
  *     addressable anchor + index list, used by [[emitFieldAssign]] to
  *     emit a single GEP through the struct hierarchy with ARC-aware
  *     release-old / store-new for refcounted fields.
  *   - [[emitLocalBinding]] is the `var x = ...` site, which decides
  *     between a stack `alloca` and a heap-backed box when a closure
  *     captures the var.
  *
  * `for`-loops over integer ranges and rank-1 / rank-2 arrays both
  * lower to counted loops here ([[emitForRange]] / [[emitForArray]]);
  * range expressions at value position fall through to
  * [[emitRangeValue]], which materialises a fresh rank-1 integer
  * array (matching the interpreter's eager `range` semantics).
  */
protected trait NexLLVMControl extends NexLLVMState:

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
  protected def emitDivZeroCheck(op: String, opT: Type, rv: String): Unit =
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

  protected def emitIf(cond: TExpr, thenB: TExpr, elseOpt: Option[TExpr], resultT: Type): String =
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

  protected def emitWhile(cond: TExpr, body: TExpr): Unit =
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
  protected def emitFor(loopVars: List[Symbol], iter: TExpr, body: TExpr): Unit =
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
  protected def emitRangeValue(lo: TExpr, hi: TExpr, inclusive: Boolean): String =
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

  protected def emitReturn(v: Option[TExpr]): Unit =
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
  protected def emitShortCircuit(l: TExpr, r: TExpr, isAnd: Boolean): String =
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

  protected def emitAssign(target: TExpr, value: TExpr): Unit =
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

      case TSlice(arr, lo, hi, inclusive, stride, _, _) =>
        emitSliceAssign(arr, lo, hi, inclusive, stride, value)

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

  protected def emitLocalBinding(sym: Symbol, value: TExpr): Unit =
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
