package io.github.edadma.nex

import scala.collection.mutable

/** Sum-type codegen for the LLVM backend: variant construction, match
  * dispatch, and the per-enum-type ARC / print / value-to-string
  * helper functions.
  *
  * Representation: `{ i32 tag, [N x i64] }` where N is the maximum
  * field count across the enum's variants. Each variant field
  * occupies one i64 payload slot — scalars are bit-cast / zext'd into
  * the slot at construction and inverted at pattern dispatch:
  *
  *   - integer → i64 (identity)
  *   - real    → bitcast to/from i64
  *   - bool    → zext i1 → i64 / trunc i64 → i1
  *   - string  → ptrtoint / inttoptr (descriptor ptr ↔ i64)
  *
  * Aggregate field types (complex / tuple / struct / array / nested
  * enum) are rejected up front by [[enumFieldSlotType]].
  *
  * Per-enum-type helpers are emitted on demand: each call site that
  * needs an inc / dec / print / to-string helper for a given
  * `TyEnum` adds it to the `enumHelperPending` / `enumPrintPending`
  * sets (declared in [[NexLLVMState]]). The flush loops
  * [[flushEnumHelpers]] / [[flushEnumPrintHelpers]] drain those sets
  * after all user functions have been emitted, stamping out one
  * `define void @__nex_enum_inc_<Name>` per pending enum (and similar
  * for drop / print / str).
  *
  * Each helper function uses [[withFreshFunction]] to save and restore
  * the function-local emit state so per-helper register / label
  * counters and the locals map don't bleed into the user-function
  * scope that triggered the request.
  */
protected trait NexLLVMEnums extends NexLLVMState:

  /** Convert a natural-typed SSA value `v: t` into the i64 payload slot
    * encoding used by tagged-union construction. Returns the new SSA
    * register holding the i64 form.
    */
  private def fieldToSlotI64(v: String, t: Type): String =
    val r = newReg()
    t match
      case TyInteger => emitLine(s"  $r = add i64 $v, 0\n")
      case TyReal    => emitLine(s"  $r = bitcast double $v to i64\n")
      case TyBool    => emitLine(s"  $r = zext i1 $v to i64\n")
      case TyString  => emitLine(s"  $r = ptrtoint ptr $v to i64\n")
      case other     =>
        notImpl(s"variant field of type $other cannot fit a one-slot payload")
    r

  /** Recover a natural-typed value from a loaded i64 payload slot. */
  private def slotI64ToField(v: String, t: Type): String =
    val r = newReg()
    t match
      case TyInteger => emitLine(s"  $r = add i64 $v, 0\n")
      case TyReal    => emitLine(s"  $r = bitcast i64 $v to double\n")
      case TyBool    => emitLine(s"  $r = trunc i64 $v to i1\n")
      case TyString  => emitLine(s"  $r = inttoptr i64 $v to ptr\n")
      case other     =>
        notImpl(s"variant field of type $other cannot fit a one-slot payload")
    r

  /** Build an enum value for variant `idx` of `te` with `args` filling
    * the variant's declared fields. Bare variants pass `args = Nil` and
    * leave the payload `undef`. The returned SSA register carries the
    * `llvmType(te)` value `{ i32, [N x i64] }`.
    */
  protected def emitVariantConstruct(te: TyEnum, idx: Int, args: List[TExpr]): String =
    val enumTy = llvmType(te)
    val (_, _, fields) = te.variants(idx) match
      case (_, fs) => (te, idx, fs)
    require(args.size == fields.size,
      s"variant construct arity mismatch: ${fields.size} fields vs ${args.size} args")

    val withTag = newReg()
    emitLine(s"  $withTag = insertvalue $enumTy undef, i32 $idx, 0\n")
    var acc = withTag
    for ((arg, i) <- args.zipWithIndex) do
      val v       = emitExpr(arg)
      val slotI64 = fieldToSlotI64(v, fields(i)._2)
      val next    = newReg()
      emitLine(s"  $next = insertvalue $enumTy $acc, i64 $slotI64, 1, $i\n")
      acc = next
    acc

  /** Source-order arm selection encoded as a tag → arm-index map plus an
    * optional default arm. Used to lay out the `switch i32 %tag, ...`
    * dispatch table without re-walking the case list. The elaborator
    * has already guaranteed exhaustiveness and uniqueness, so each tag
    * appears at most once.
    */
  private case class ArmTable(
      byTag:        Map[Int, Int],
      defaultArm:   Option[Int],
  )

  /** Build the per-tag arm lookup for the scrutinee's enum. A
    * top-level [[TVarPat]] or [[TWildcardPat]] becomes the default
    * arm; any tag without an explicit arm falls through to the default.
    */
  private def buildArmTable(te: TyEnum, cases: List[TMatchCase]): ArmTable =
    val byTag        = mutable.Map.empty[Int, Int]
    var defaultArm: Option[Int] = None
    for ((c, armIdx) <- cases.zipWithIndex) do
      c.pat match
        case TVariantPat(vs, _, _) =>
          variantInfo.get(vs.id) match
            case Some((_, idx, _)) =>
              if !byTag.contains(idx) then byTag(idx) = armIdx
            case None =>
              notYet(s"unknown variant Symbol id ${vs.id} in match — codegen variantInfo missed it")
        case _: TVarPat | _: TWildcardPat =>
          if defaultArm.isEmpty then defaultArm = Some(armIdx)
    ArmTable(byTag.toMap, defaultArm)

  /** Emit a `match` expression as a `switch` over the runtime tag with
    * per-arm basic blocks that bind pattern fields and evaluate the
    * arm body, joining at a single merge block.
    *
    * Discipline:
    *   - The scrutinee is evaluated once. Its share is consumed inside
    *     each arm right after pattern extraction completes.
    *   - Each pattern-bound refcounted field is inc'd before the arm
    *     body runs (so the binding owns its own share).
    *   - Pattern bindings live in a fresh block scope (push/pop) so
    *     refcounted slots are dec'd at arm exit. Non-refcounted slots
    *     are still registered as local allocas keyed by Symbol id —
    *     this is the only place the arm body sees the binding.
    *   - Arms that fall through to merge contribute a phi edge; arms
    *     that diverged (`return`, infinite loop) contribute nothing.
    */
  protected def emitMatch(scrutinee: TExpr, cases: List[TMatchCase], resultT: Type): String =
    val te = scrutinee.tpe match
      case t: TyEnum => t
      case other     =>
        notImpl(s"match scrutinee has non-enum type $other (chunk 2 elaborator must have admitted this in error)")
    val enumTy = llvmType(te)
    val nVariants = te.variants.size
    val scrutV    = emitExpr(scrutinee)
    val tagReg    = newReg()
    emitLine(s"  $tagReg = extractvalue $enumTy $scrutV, 0\n")

    val table  = buildArmTable(te, cases)
    val mergeL = freshLabel("match.end")

    val armLabels: Map[Int, String] = cases.indices.iterator
      .map(i => i -> freshLabel(s"match.arm$i"))
      .toMap

    // Determine destination labels per tag, falling back to default
    // (if any) or to a synthesized unreachable block (elaborator
    // guarantees exhaustiveness, but be defensive).
    val unreachableL = freshLabel("match.unreachable")
    val defaultL = table.defaultArm match
      case Some(armIdx) => armLabels(armIdx)
      case None         => unreachableL

    val switchEntries =
      (0 until nVariants).iterator.map { tag =>
        val dest = table.byTag.get(tag) match
          case Some(armIdx) => armLabels(armIdx)
          case None         => defaultL
        s"    i32 $tag, label %$dest"
      }.mkString("\n")

    emitLine(s"  switch i32 $tagReg, label %$defaultL [\n$switchEntries\n  ]\n")
    currentBlock = None

    if table.defaultArm.isEmpty then
      startBlock(unreachableL)
      emitTerminator("  unreachable\n")

    // Per-arm: bind pattern fields, dec scrutinee share, emit body, jump
    // to merge with the arm's result value.
    val armResults = mutable.ListBuffer.empty[(String, String)]

    for ((c, armIdx) <- cases.zipWithIndex) do
      startBlock(armLabels(armIdx))
      pushBlockScope()
      bindPattern(c.pat, scrutV, te)
      // Release the scrutinee's share — any refcounted bindings were
      // already inc'd by [[bindPattern]] so they outlive the dec.
      if isRefCountedType(te) then emitArrDec(scrutV, te)
      val armV    = emitExpr(c.body)
      // Drop pattern bindings registered in this scope.
      val popped  = popBlockScope()
      decBlockScope(popped)
      val exitBB  = currentBlock
      if exitBB.isDefined then
        emitTerminator(s"  br label %$mergeL\n")
        armResults += ((armV, exitBB.get))

    if armResults.isEmpty then
      // Every arm diverged — the merge is unreachable; nothing
      // downstream will observe the result. Emit a dead merge label
      // so subsequent emit calls don't crash, terminated immediately.
      startBlock(mergeL)
      emitTerminator("  unreachable\n")
      "void"
    else
      startBlock(mergeL)
      resultT match
        case TyUnit | TyUnknown => "void"
        case t =>
          val edges = armResults.map { case (v, bb) => s"[ $v, %$bb ]" }.mkString(", ")
          val reg = newReg()
          emitLine(s"  $reg = phi ${llvmType(t)} $edges\n")
          reg

  /** Pull payload slots out of `scrutV` according to the pattern,
    * binding any sub-pattern variables into the current scope. The
    * scrutinee is *not* dec'd here — the caller handles that after the
    * binding extraction completes (so any refcounted field share that
    * the binding inherits is alive when we release the parent).
    */
  private def bindPattern(pat: TPattern, scrutV: String, te: TyEnum): Unit =
    pat match
      case TWildcardPat(_) => ()
      case TVarPat(sym, _) =>
        bindWholeScrutinee(sym, scrutV, te)
      case TVariantPat(_, args, _) if args.isEmpty => ()
      case TVariantPat(vs, args, _) =>
        val (_, _, fields) = variantInfo(vs.id)
        val enumTy = llvmType(te)
        for ((sub, i) <- args.zipWithIndex) do
          val (_, fT) = fields(i)
          val slotI64 = newReg()
          emitLine(s"  $slotI64 = extractvalue $enumTy $scrutV, 1, $i\n")
          val nat = slotI64ToField(slotI64, fT)
          bindSubPattern(sub, nat, fT)

  /** Bind a sub-pattern of a variant against an already-extracted,
    * already-converted field value `v: fT`. Wildcard / variant sub-
    * patterns are not yet wired (nested matching on aggregate fields
    * is rejected at codegen, so sub-patterns of bare scalar fields
    * are TWildcardPat or TVarPat).
    */
  private def bindSubPattern(sub: TPattern, v: String, fT: Type): Unit =
    sub match
      case TWildcardPat(_) => ()
      case TVarPat(sym, _) =>
        val ty   = llvmType(fT)
        val slot = newReg()
        emitLine(s"  $slot = alloca $ty\n")
        if isRefCountedType(fT) then emitArrInc(v, fT)
        emitLine(s"  store $ty $v, ptr $slot\n")
        locals(sym.id) = slot
        if isRefCountedType(fT) then registerArraySlot(sym.id, slot, fT)
      case TVariantPat(_, _, _) =>
        notImpl("nested variant pattern on a scalar variant field — variant fields are restricted to scalar types")

  /** Bind the whole scrutinee to `sym` (top-level TVarPat). Mirrors
    * [[bindSubPattern]]'s alloca-and-register shape but uses the enum
    * type for the slot.
    */
  private def bindWholeScrutinee(sym: Symbol, v: String, te: TyEnum): Unit =
    val ty   = llvmType(te)
    val slot = newReg()
    emitLine(s"  $slot = alloca $ty\n")
    if isRefCountedType(te) then emitArrInc(v, te)
    emitLine(s"  store $ty $v, ptr $slot\n")
    locals(sym.id) = slot
    if isRefCountedType(te) then registerArraySlot(sym.id, slot, te)

  // ---------------------------------------------------------------------------
  // Enum helper flush loops. Drain the per-enum-type pending sets
  // populated by [[emitArrInc]] / [[emitArrDec]] and by the print and
  // value-to-string paths. Self-contained — each helper emits a
  // multi-block function with its own register / label counters, so
  // the surrounding emit state (currentBlock, regCounter, locals) is
  // untouched.
  // ---------------------------------------------------------------------------

  protected def flushEnumHelpers(): Unit =
    while enumHelperPending.nonEmpty do
      val te = enumHelperPending.head
      enumHelperPending -= te
      if !enumHelperEmitted.contains(te) then
        enumHelperEmitted += te
        emitEnumIncHelper(te)
        emitEnumDropHelper(te)

  protected def flushEnumPrintHelpers(): Unit =
    while enumPrintPending.nonEmpty do
      val te = enumPrintPending.head
      enumPrintPending -= te
      if !enumPrintEmitted.contains(te) then
        enumPrintEmitted += te
        emitEnumPrintHelper(te)
        emitEnumStrHelper(te)

  /** Save / restore the function-local emit state around `body`. The
    * shared `currentBlock` / `regCounter` / `labelCounter` / `locals` /
    * `arrayLocalSlots` / scope stacks all roll back to whatever the
    * caller had set — critical because helper emission happens after
    * user-function emission but reuses the same machinery, and we don't
    * want a leftover entry to corrupt a subsequent helper's state.
    */
  private def withFreshFunction[A](body: => A): A =
    val savedReg    = regCounter
    val savedLbl    = labelCounter
    val savedBlk    = currentBlock
    val savedLocals = locals.toMap
    val savedArrSl  = arrayLocalSlots.toMap
    val savedBlkSc  = blockArrayScopes
    val savedBoxFn  = boxedFunctionSlots.toMap
    val savedBoxSc  = blockBoxedScopes
    regCounter      = 0
    labelCounter    = 0
    currentBlock    = Some("entry")
    locals.clear()
    arrayLocalSlots.clear()
    blockArrayScopes = Nil
    boxedFunctionSlots.clear()
    blockBoxedScopes = Nil
    try body
    finally
      regCounter   = savedReg
      labelCounter = savedLbl
      currentBlock = savedBlk
      locals.clear(); locals ++= savedLocals
      arrayLocalSlots.clear(); arrayLocalSlots ++= savedArrSl
      blockArrayScopes = savedBlkSc
      boxedFunctionSlots.clear(); boxedFunctionSlots ++= savedBoxFn
      blockBoxedScopes = savedBoxSc

  /** Emit `define void @__nex_enum_inc_<Name>(<enumTy> %v)` — switch on
    * the tag and inc each refcounted payload slot for the active
    * variant. Variants with no refcounted fields produce an empty
    * body (just `ret void`).
    */
  private def emitEnumIncHelper(te: TyEnum): Unit =
    val enumTy = llvmType(te)
    val name   = enumIncHelperName(te).drop(1)
    out.append(s"define void @$name($enumTy %v) {\n")
    withFreshFunction {
      out.append("entry:\n")
      emitVariantSwitch(te, "%v", perVariant = (idx, fields) => emitEnumIncBody(te, "%v", fields))
    }
    out.append("}\n\n")

  private def emitEnumDropHelper(te: TyEnum): Unit =
    val enumTy = llvmType(te)
    val name   = enumDropHelperName(te).drop(1)
    out.append(s"define void @$name($enumTy %v) {\n")
    withFreshFunction {
      out.append("entry:\n")
      emitVariantSwitch(te, "%v", perVariant = (idx, fields) => emitEnumDropBody(te, "%v", fields))
    }
    out.append("}\n\n")

  /** Per-variant body for the inc helper: extract every refcounted
    * slot and inc it. Bare variants and pure-scalar variants emit
    * only the trailing `ret void`.
    */
  private def emitEnumIncBody(te: TyEnum, scrut: String, fields: List[(String, Type)]): Unit =
    val enumTy = llvmType(te)
    for ((_, fT), i) <- fields.zipWithIndex do
      if isRefCountedType(fT) then
        val slotI64 = newReg()
        emitLine(s"  $slotI64 = extractvalue $enumTy $scrut, 1, $i\n")
        val nat = slotI64ToField(slotI64, fT)
        emitArrInc(nat, fT)

  /** Per-variant body for the drop helper — symmetric to the inc body. */
  private def emitEnumDropBody(te: TyEnum, scrut: String, fields: List[(String, Type)]): Unit =
    val enumTy = llvmType(te)
    for ((_, fT), i) <- fields.zipWithIndex do
      if isRefCountedType(fT) then
        val slotI64 = newReg()
        emitLine(s"  $slotI64 = extractvalue $enumTy $scrut, 1, $i\n")
        val nat = slotI64ToField(slotI64, fT)
        emitArrDec(nat, fT)

  /** Emit the `switch i32 %tag, label %unreachable [ i32 0, %case0 ... ]`
    * skeleton plus per-variant basic blocks that call `perVariant(idx, fs)`
    * before returning. Used by inc / drop / print helpers — they all
    * share the dispatch shape and only differ in the per-case body.
    */
  private def emitVariantSwitch(
      te: TyEnum,
      scrut: String,
      perVariant: (Int, List[(String, Type)]) => Unit,
  ): Unit =
    val enumTy = llvmType(te)
    val tagR   = newReg()
    emitLine(s"  $tagR = extractvalue $enumTy $scrut, 0\n")
    val caseLabels = te.variants.indices.iterator.map(i => i -> freshLabel(s"v$i")).toMap
    val unreachL   = freshLabel("vbad")
    val entries    = te.variants.indices.iterator.map { i =>
      s"    i32 $i, label %${caseLabels(i)}"
    }.mkString("\n")
    emitLine(s"  switch i32 $tagR, label %$unreachL [\n$entries\n  ]\n")
    currentBlock = None
    startBlock(unreachL)
    emitTerminator("  unreachable\n")
    for (i, _) <- te.variants.zipWithIndex.map { case ((s, fs), idx) => (idx, (s, fs)) } do
      val (_, fs) = te.variants(i)
      startBlock(caseLabels(i))
      perVariant(i, fs)
      emitTerminator("  ret void\n")

  /** Emit `define void @__nex_print_enum_<Name>(<enumTy> %v)` —
    * per-variant prints `Name` or `Name(field0, field1, ...)` via the
    * existing per-type print helpers. Releases nothing; the caller of
    * `print(...)` decides whether the scrutinee's share is dec'd.
    */
  private def emitEnumPrintHelper(te: TyEnum): Unit =
    val enumTy = llvmType(te)
    val name   = enumPrintHelperName(te).drop(1)
    out.append(s"define void @$name($enumTy %v) {\n")
    withFreshFunction {
      out.append("entry:\n")
      emitVariantSwitch(te, "%v", perVariant = (idx, fields) => emitEnumPrintBody(te, "%v", idx, fields))
    }
    out.append("}\n\n")

  /** Per-variant print body: emit `Name` followed by `(f0, f1, ...)` if
    * the variant carries fields. Uses the same comma-separated form as
    * the interpreter's [[formatValue]] for VEnum.
    */
  private def emitEnumPrintBody(
      te: TyEnum,
      scrut: String,
      idx: Int,
      fields: List[(String, Type)],
  ): Unit =
    val (vName, _) = te.variants(idx)
    val namePtr = internStringLiteral(vName)
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $namePtr)\n")
    if fields.nonEmpty then
      val openPtr  = internStringLiteral("(")
      val closePtr = internStringLiteral(")")
      emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $openPtr)\n")
      val enumTy = llvmType(te)
      for ((_, fT), i) <- fields.zipWithIndex do
        if i > 0 then emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_sep)\n")
        val slotI64 = newReg()
        emitLine(s"  $slotI64 = extractvalue $enumTy $scrut, 1, $i\n")
        val nat = slotI64ToField(slotI64, fT)
        emitPrintArrayElem(fT, nat)
      emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $closePtr)\n")

  /** Emit `define ptr @__nex_enum_str_<Name>(<enumTy> %v)` — returns a
    * fresh %nex_str descriptor formatted as `Name` or
    * `Name(f0, f1, ...)`. Mirrors [[emitValueToStringStruct]]'s
    * concat-chain approach but with the variant header in place of the
    * struct's `Name { ` opener.
    */
  private def emitEnumStrHelper(te: TyEnum): Unit =
    val enumTy = llvmType(te)
    val name   = enumStrHelperName(te).drop(1)
    out.append(s"define ptr @$name($enumTy %v) {\n")
    withFreshFunction {
      out.append("entry:\n")
      val tagR = newReg()
      emitLine(s"  $tagR = extractvalue $enumTy %v, 0\n")
      val caseLabels = te.variants.indices.iterator.map(i => i -> freshLabel(s"sv$i")).toMap
      val unreachL   = freshLabel("svbad")
      val entries = te.variants.indices.iterator.map { i =>
        s"    i32 $i, label %${caseLabels(i)}"
      }.mkString("\n")
      emitLine(s"  switch i32 $tagR, label %$unreachL [\n$entries\n  ]\n")
      currentBlock = None
      startBlock(unreachL)
      emitTerminator("  unreachable\n")
      for i <- te.variants.indices do
        val (vName, fs) = te.variants(i)
        startBlock(caseLabels(i))
        val result = emitEnumStrBody(te, "%v", vName, fs)
        emitTerminator(s"  ret ptr $result\n")
    }
    out.append("}\n\n")

  /** Per-variant body for the value-to-string helper: build a
    * descriptor for `Name(f0, f1, ...)` (or just `Name` for bare
    * variants) via concat-chain, returning the final descriptor ptr.
    */
  private def emitEnumStrBody(
      te: TyEnum,
      scrut: String,
      variantName: String,
      fields: List[(String, Type)],
  ): String =
    if fields.isEmpty then internStringDescriptor(variantName)
    else
      val enumTy = llvmType(te)
      val parts  = mutable.ListBuffer[String](
        internStringDescriptor(variantName),
        internStringDescriptor("("),
      )
      for ((_, fT), i) <- fields.zipWithIndex do
        if i > 0 then parts += internStringDescriptor(", ")
        val slotI64 = newReg()
        emitLine(s"  $slotI64 = extractvalue $enumTy $scrut, 1, $i\n")
        val nat = slotI64ToField(slotI64, fT)
        parts += emitTypedValueToString(nat, fT)
      parts += internStringDescriptor(")")
      concatChain(parts.toList)
