package io.github.edadma.nex

import scala.collection.mutable

/** Sum-type codegen for the MLIR backend. Mirrors the LLVM backend's
  * tagged-union representation: `!llvm.struct<(i32, !llvm.array<NxI64>)>`
  * where N is the maximum field count across all variants. Each variant
  * field occupies one i64 payload slot — scalars are bit-encoded into
  * the slot at construction and inverted at pattern dispatch:
  *
  *   - integer → i64                  (identity)
  *   - real    → bitcast f64 ↔ i64    (llvm.bitcast)
  *   - bool    → zext i1 → i64       (llvm.zext) / trunc i64 → i1
  *   - string  → ptrtoint ptr ↔ i64  (opaque-ptr/i64 already, identity)
  *
  * Aggregate field types (complex, tuple, struct, array, nested enum)
  * are rejected up front — out of scope for the first cut.
  */
trait NexMLIREnums:
  self: NexMLIRCodegen =>

  /** Variant Symbol id → (parent enum, declared tag index, fields). */
  protected val variantInfo =
    mutable.Map.empty[Int, (TyEnum, Int, List[(String, Type)])]

  /** Cache MEnum by enum name so two queries for the same TyEnum return
    * the same MlirType instance.
    */
  protected val enumTypeCache = mutable.Map.empty[String, MEnum]

  /** Per-enum value-to-string helpers requested from print and s-interp
    * sites. Drained after `@main` is emitted.
    */
  protected val enumStrPending = mutable.LinkedHashSet.empty[TyEnum]
  protected val enumStrEmitted = mutable.Set.empty[TyEnum]

  protected def enumStrHelperName(te: TyEnum): String =
    s"@nex_enum_str_${te.name}"

  protected def requestEnumStrHelper(te: TyEnum): Unit =
    if !enumStrEmitted.contains(te) then enumStrPending += te

  protected def enumPayloadSlots(te: TyEnum): Int =
    if te.variants.isEmpty then 0
    else te.variants.iterator.map(_._2.size).max

  /** Build an [[MEnum]] for a Nex [[TyEnum]]. Cached by enum name. */
  protected def menumOf(te: TyEnum): MEnum =
    enumTypeCache.getOrElseUpdate(
      te.name,
      MEnum(te.name, te.variants, enumPayloadSlots(te)),
    )

  /** Scan every TEnumDecl in the program and register its variants in
    * [[variantInfo]] so [[TVarRef]] and [[TCall]] sites resolving an
    * `EnumVariant` symbol can find the construction info.
    */
  protected def collectEnumVariants(tp: TProgram): Unit =
    tp.allDecls.foreach {
      case e: TEnumDecl =>
        val variantsByName = e.variants.map { case (vs, fs) => (vs.name, fs) }
        val te = TyEnum(e.sym.name, variantsByName)
        e.variants.zipWithIndex.foreach { case ((vs, fs), idx) =>
          variantInfo(vs.id) = (te, idx, fs)
        }
      case _ => ()
    }

  /** Convert a Nex-typed payload value (already in MLIR SSA form) into
    * the i64 slot encoding used by tagged-union construction. Returns
    * the new SSA register holding the i64 form.
    */
  protected def fieldToSlotI64(v: MlirVal, t: Type): String =
    t match
      case TyInteger => v.reg
      case TyReal    =>
        val r = fresh("eslot")
        out.append(s"  $r = llvm.bitcast ${v.reg} : f64 to i64\n")
        r
      case TyBool    =>
        val r = fresh("eslot")
        out.append(s"  $r = llvm.zext ${v.reg} : i1 to i64\n")
        r
      case TyString  => v.reg  // MString is already i64
      case other     =>
        notYet(s"variant field of type $other cannot fit a one-slot payload")

  /** Recover a Nex-typed value from a loaded i64 payload slot. */
  protected def slotI64ToField(reg: String, t: Type): MlirVal =
    t match
      case TyInteger => MlirVal(reg, MScalar(TyInteger))
      case TyReal    =>
        val r = fresh("efld")
        out.append(s"  $r = llvm.bitcast $reg : i64 to f64\n")
        MlirVal(r, MScalar(TyReal))
      case TyBool    =>
        val r = fresh("efld")
        out.append(s"  $r = llvm.trunc $reg : i64 to i1\n")
        MlirVal(r, MScalar(TyBool))
      case TyString  => MlirVal(reg, MString)
      case other     =>
        notYet(s"variant field of type $other cannot fit a one-slot payload")

  /** Build an enum value for variant `idx` of `te` with `args` filling
    * the variant's declared fields. Bare variants pass `args = Nil` and
    * leave the payload slots `undef` — none are read because the match
    * dispatcher only extracts slots for the active variant.
    */
  protected def emitVariantConstruct(te: TyEnum, idx: Int, args: List[TExpr]): MlirVal =
    val me     = menumOf(te)
    val enumTx = me.text
    val undef  = fresh(s"e_${te.name}_undef")
    out.append(s"  $undef = llvm.mlir.undef : $enumTx\n")
    val tagR = fresh(s"e_${te.name}_tag")
    out.append(s"  $tagR = llvm.mlir.constant($idx : i32) : i32\n")
    val withTag = fresh(s"e_${te.name}")
    out.append(s"  $withTag = llvm.insertvalue $tagR, $undef[0] : $enumTx\n")

    val fields = te.variants(idx)._2
    if args.size != fields.size then
      notYet(s"variant construct arity mismatch: ${fields.size} fields vs ${args.size} args for ${te.name}.${te.variants(idx)._1}")

    var acc = withTag
    for ((arg, i) <- args.zipWithIndex) do
      val v       = emitExpr(arg)
      val slotI64 = fieldToSlotI64(v, fields(i)._2)
      val next    = fresh(s"e_${te.name}")
      out.append(s"  $next = llvm.insertvalue $slotI64, $acc[1, $i] : $enumTx\n")
      acc = next
    MlirVal(acc, me)

  /** Lower an `s match { ... }` expression. Builds a nested
    * `scf.if -> (T)` chain over the runtime tag.
    *
    * For each source-order arm:
    *   - A [[TVariantPat]] becomes `scf.if (tag == idx)` with the arm
    *     body in the then-region and the remaining arms in the
    *     else-region.
    *   - A [[TVarPat]] / [[TWildcardPat]] is the default — emit the
    *     arm body directly into the current else-region (no further
    *     test). Any subsequent arms are unreachable and ignored.
    *
    * If no default arm is present and not every tag has an arm, the
    * tail else yields a zero placeholder — the elaborator guarantees
    * exhaustiveness so this branch is dynamically dead.
    */
  protected def emitMatch(scrutinee: TExpr, cases: List[TMatchCase], resultT: Type): MlirVal =
    val sv = emitExpr(scrutinee)
    val me = sv.ty match
      case m: MEnum => m
      case other    => notYet(s"match scrutinee has non-enum MLIR type $other")
    val te = scrutinee.tpe match
      case t: TyEnum => t
      case other     => notYet(s"match scrutinee has non-enum Nex type $other")

    val tagR = fresh("etag")
    out.append(s"  $tagR = llvm.extractvalue ${sv.reg}[0] : ${me.text}\n")

    val outTy: MlirType = resultT match
      case TyUnit | TyUnknown => MScalar(TyInteger) // placeholder; never read
      case other =>
        mlirTypeOf(other).getOrElse(notYet(s"match result type $other"))

    val isUnit = resultT == TyUnit || resultT == TyUnknown

    emitMatchChain(sv, me, te, tagR, cases, outTy, isUnit)

  /** Walk the arms in source order, building a chain of `scf.if -> (T)`
    * (or no-result `scf.if` for unit-typed match). Each variant pattern
    * tests its tag and yields the body's result; a wildcard / var pattern
    * terminates the chain by emitting the body directly into the
    * current branch.
    */
  private def emitMatchChain(
      sv:     MlirVal,
      me:     MEnum,
      te:     TyEnum,
      tagR:   String,
      cases:  List[TMatchCase],
      outTy:  MlirType,
      isUnit: Boolean,
  ): MlirVal =
    cases match
      case Nil =>
        // No arms left and no default — elaborator guarantees this is
        // unreachable. Yield a zero placeholder so the scf.if region
        // verifies.
        if isUnit then MlirVal("%unreach", outTy)
        else
          val z = fresh("ematch_zero")
          out.append(s"  $z = ${zeroOf(outTy)}\n")
          MlirVal(z, outTy)

      case head :: rest =>
        head.pat match
          case _: TVarPat | _: TWildcardPat =>
            // Default arm — emit body directly with the binding for
            // TVarPat. Remaining arms are unreachable and ignored.
            head.pat match
              case TVarPat(sym, _) =>
                env(sym.id) = sv
              case _ => ()
            emitArmBody(head.body, outTy, isUnit)

          case TVariantPat(vs, args, _) =>
            val (parentEnum, idx, fields) = variantInfo.getOrElse(
              vs.id,
              notYet(s"unknown variant Symbol id ${vs.id} in match — codegen variantInfo missed it"),
            )
            // Test tag equality
            val tcR = fresh("ec")
            val cR  = fresh("ecmp")
            out.append(s"  $tcR = llvm.mlir.constant($idx : i32) : i32\n")
            out.append(s"  $cR = arith.cmpi eq, $tagR, $tcR : i32\n")

            if isUnit then
              out.append(s"  scf.if $cR {\n")
              bindPatternArgs(sv, me, fields, args)
              emitArmStatement(head.body)
              out.append("  } else {\n")
              val _ = emitMatchChain(sv, me, te, tagR, rest, outTy, isUnit)
              out.append("  }\n")
              MlirVal("%unit", outTy)
            else
              val r = fresh("ematch")
              out.append(s"  $r = scf.if $cR -> (${outTy.text}) {\n")
              bindPatternArgs(sv, me, fields, args)
              val bv = emitExpr(head.body)
              val bReg = coerceToType(bv, outTy)
              out.append(s"    scf.yield $bReg : ${outTy.text}\n")
              out.append("  } else {\n")
              val ev = emitMatchChain(sv, me, te, tagR, rest, outTy, isUnit)
              val eReg = coerceToType(ev, outTy)
              out.append(s"    scf.yield $eReg : ${outTy.text}\n")
              out.append("  }\n")
              MlirVal(r, outTy)

  /** Bind a variant pattern's sub-patterns to env entries. Each
    * sub-pattern is either a TVarPat (extract the slot and bind) or a
    * TWildcardPat (ignored). Nested variant patterns on a scalar field
    * are rejected.
    */
  private def bindPatternArgs(
      sv:     MlirVal,
      me:     MEnum,
      fields: List[(String, Type)],
      args:   List[TPattern],
  ): Unit =
    if args.size != fields.size then
      notYet(s"variant pattern arity mismatch: ${args.size} sub-patterns vs ${fields.size} fields")
    args.zip(fields).zipWithIndex.foreach { case ((sub, (_, fT)), i) =>
      sub match
        case _: TWildcardPat => ()
        case TVarPat(sym, _) =>
          val slotR = fresh("eslot_in")
          out.append(s"  $slotR = llvm.extractvalue ${sv.reg}[1, $i] : ${me.text}\n")
          env(sym.id) = slotI64ToField(slotR, fT)
        case _: TVariantPat =>
          notYet("nested variant pattern on a scalar variant field")
    }

  /** Emit an arm body and yield its result through the active scf.if
    * region. For unit-typed arms the body is emitted as a statement
    * sequence and the region terminates with `scf.yield` (no operand).
    */
  private def emitArmBody(body: TExpr, outTy: MlirType, isUnit: Boolean): MlirVal =
    if isUnit then
      emitArmStatement(body)
      MlirVal("%unit", outTy)
    else
      val bv = emitExpr(body)
      MlirVal(coerceToType(bv, outTy), outTy)

  /** Emit a unit-typed match-arm body as a sequence of side-effecting
    * statements. Currently supports `print(...)` calls and TBlocks
    * thereof; extend as more arm shapes appear in the corpus.
    */
  private def emitArmStatement(body: TExpr): Unit = body match
    case TBlock(items, last, _, _) =>
      items.foreach(emitBlockItem)
      if last.tpe != TyUnit then
        // value-typed tail in a unit-position arm — the elaborator
        // discards the result; emit for its effects.
        val _ = emitExpr(last)
      else
        emitBlockItem(TBlockExpr(last))
    case TCall(TVarRef(p, _, _), List(arg), _, _) if p.name == "print" =>
      emitPrintCall(arg)
    case other if other.tpe == TyUnit =>
      emitBlockItem(TBlockExpr(other))
    case other =>
      // Value-typed body discarded — emit and ignore.
      val _ = emitExpr(other)

  /** Convert an in-flight enum value (already in MLIR SSA form) to a
    * fresh `nex_str` descriptor by calling the per-enum stringifier
    * helper (which is registered for emission and flushed after main).
    */
  protected def emitEnumValToString(reg: String, me: MEnum): String =
    val te = variantInfo.values.collectFirst {
      case (t, _, _) if t.name == me.name => t
    }.getOrElse(notYet(s"emitEnumValToString: no TyEnum for ${me.name}"))
    requestEnumStrHelper(te)
    val helper = enumStrHelperName(te)
    val r = fresh("v2s_e")
    out.append(s"  $r = func.call $helper($reg) : (${me.text}) -> i64\n")
    r

  /** Emit `func.func @nex_enum_str_<Name>(%v: <enumTy>) -> i64` —
    * a per-enum-type stringifier that switches on the tag and returns
    * a fresh `nex_str` descriptor formatted as `Name` for bare variants
    * and `Name(f0, f1, ...)` for fielded variants. Output value type
    * is `i64` because `nex_str` descriptors are opaque pointers.
    *
    * Drained by [[flushEnumStrHelpers]] after `@main` so all enums seen
    * during user-def + main emission land in the module.
    */
  protected def flushEnumStrHelpers(): Unit =
    while enumStrPending.nonEmpty do
      val te = enumStrPending.head
      enumStrPending -= te
      if !enumStrEmitted.contains(te) then
        enumStrEmitted += te
        emitEnumStrHelper(te)

  private def emitEnumStrHelper(te: TyEnum): Unit =
    val me     = menumOf(te)
    val enumTx = me.text
    val helper = enumStrHelperName(te).drop(1)

    // Save outer emit state so the helper's region counters / env don't
    // leak into the post-main caller. Each helper is a self-contained
    // func.func at module scope.
    val savedReg = nextReg
    val savedEnv = env.toMap
    val outer    = out.toString
    out.setLength(0)
    nextReg = 0
    env.clear()

    out.append(s"func.func private @$helper(%arg0: $enumTx) -> i64 {\n")
    val tagR = fresh("etag")
    out.append(s"  $tagR = llvm.extractvalue %arg0[0] : $enumTx\n")
    val resultR = emitEnumStrChain(me, tagR, "%arg0", te.variants.zipWithIndex.toList)
    out.append(s"  func.return $resultR : i64\n")
    out.append("}\n\n")

    val helperText = out.toString
    out.setLength(0)
    out.append(outer)
    out.append(helperText)
    nextReg = savedReg
    env.clear(); env ++= savedEnv

  /** Recursive walk over `(variantName, fields, idx)` entries: each one
    * produces an `scf.if (tag == idx) -> (i64)` whose then-region
    * builds the descriptor for that variant and yields, with the else-
    * region recursing into the remaining variants. The tail recursive
    * call yields a literal-empty placeholder; the elaborator guarantees
    * exhaustiveness so this branch is dynamically dead.
    */
  private def emitEnumStrChain(
      me:     MEnum,
      tagR:   String,
      scrut:  String,
      remain: List[((String, List[(String, Type)]), Int)],
  ): String =
    remain match
      case Nil =>
        // Unreachable. Return a fresh empty-string descriptor so the
        // type-checker is happy.
        emitStringLiteral("?")
      case ((vName, fields), idx) :: rest =>
        val tcR = fresh("ec")
        val cR  = fresh("ecmp")
        out.append(s"  $tcR = llvm.mlir.constant($idx : i32) : i32\n")
        out.append(s"  $cR = arith.cmpi eq, $tagR, $tcR : i32\n")
        val r = fresh("estr")
        out.append(s"  $r = scf.if $cR -> (i64) {\n")
        val d = emitVariantDescriptor(me, scrut, vName, fields)
        out.append(s"    scf.yield $d : i64\n")
        out.append("  } else {\n")
        val tail = emitEnumStrChain(me, tagR, scrut, rest)
        out.append(s"    scf.yield $tail : i64\n")
        out.append("  }\n")
        r

  /** Build a `nex_str` descriptor for `VariantName` (bare) or
    * `VariantName(f0, f1, ...)` (fielded), concat-chaining the parts
    * through `nex_str_concat`. Operands of each concat are dec'd
    * (literals are immortal so the dec is a no-op).
    */
  private def emitVariantDescriptor(
      me:        MEnum,
      scrut:     String,
      variantNm: String,
      fields:    List[(String, Type)],
  ): String =
    val nameLit = emitStringLiteral(variantNm)
    if fields.isEmpty then nameLit
    else
      val openLit = emitStringLiteral("(")
      var acc = concatTwo(nameLit, openLit)
      for ((_, fT), i) <- fields.zipWithIndex do
        if i > 0 then
          val sep = emitStringLiteral(", ")
          acc = concatTwo(acc, sep)
        val slotR = fresh("eslot_in")
        out.append(s"  $slotR = llvm.extractvalue $scrut[1, $i] : ${me.text}\n")
        val fldVal = slotI64ToField(slotR, fT)
        val fldStr = emitMlirValToString(fldVal)
        acc = concatTwo(acc, fldStr)
      val closeLit = emitStringLiteral(")")
      acc = concatTwo(acc, closeLit)
      acc

  /** Inline concat: build a fresh heap descriptor, dec both operands
    * (literals are refcount=-1 so dec is a no-op; intermediate concat
    * results are at refcount=1 and properly released).
    */
  private def concatTwo(a: String, b: String): String =
    val r = fresh("econc")
    out.append(s"  $r = func.call @nex_str_concat($a, $b) : (i64, i64) -> i64\n")
    out.append(s"  func.call @nex_str_dec($a) : (i64) -> ()\n")
    out.append(s"  func.call @nex_str_dec($b) : (i64) -> ()\n")
    r

  /** Render the zero-value initialiser for a given `MlirType`. Used at
    * the unreachable tail of a `match` expression so the scf.if region
    * verifies even though the branch is dynamically dead.
    */
  protected def zeroOf(ty: MlirType): String = ty match
    case MScalar(TyInteger) => s"arith.constant 0 : i64"
    case MScalar(TyReal)    => s"arith.constant 0.0 : f64"
    case MScalar(TyBool)    => s"arith.constant 0 : i1"
    case MString            => s"arith.constant 0 : i64"
    case _: MFunc           => s"arith.constant 0 : i64"
    case other              => notYet(s"zero of $other for match unreachable tail")
