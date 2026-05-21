package io.github.edadma.nex

/** Print helpers: the `print(...)` builtin and its supporting per-shape
  * value printers (scalar / array / tuple / struct).
  */
protected trait NexLLVMPrint extends NexLLVMState:

  // ---------------------------------------------------------------------------
  // `print(...)` — overloaded across integer / real / bool. Threads through
  // a single libc `printf` with the right format-string global.
  // ---------------------------------------------------------------------------

  protected def emitPrintCall(arg: TExpr): Unit =
    arg match
      // Special-case interpolated strings: emit a printf for each part
      // and a trailing newline. Avoids needing an in-memory string
      // builder until we ship real string ops.
      case TInterpStringLit(parts, _, _) =>
        for p <- parts do p match
          case TInterpText(text) =>
            val ptr = internStringLiteral(text)
            emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $ptr)\n")
          case TInterpRef(sym, spec) =>
            // `f"..."` spec routes through emitFormattedDesc which
            // produces a fresh %nex_str descriptor — extract its data
            // ptr before handing to printf, then release the share.
            // Bare `s"..."` interpolation stays on the direct print
            // path which avoids the descriptor allocation entirely.
            if spec.isDefined then
              val desc = emitFormattedDesc(TVarRef(sym, None, sym.tpe), spec.get)
              val data = newReg()
              emitLine(s"  $data = call ptr @__nex_str_data(ptr $desc)\n")
              emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $data)\n")
              emitLine(s"  call void @__nex_str_dec(ptr $desc)\n")
            else
              emitPrintValue(TVarRef(sym, None, sym.tpe))
          case TInterpExpr(e, spec) =>
            if spec.isDefined then
              val desc = emitFormattedDesc(e, spec.get)
              val data = newReg()
              emitLine(s"  $data = call ptr @__nex_str_data(ptr $desc)\n")
              emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $data)\n")
              emitLine(s"  call void @__nex_str_dec(ptr $desc)\n")
            else
              emitPrintValue(e)
          case _: TInterpRaw =>
            notYet("interpolated `${...}` raw fragment (should have been re-parsed in Stage 1)")
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.nl)\n")
      case _ =>
        arg.tpe match
          case TyArray(_, _) =>
            // print an array: emit its formatted form + trailing newline.
            emitPrintArray(arg)
            emitLine(s"  call i32 (ptr, ...) @printf(ptr @.nl)\n")
          case TyTuple(_) =>
            emitPrintTuple(arg)
            emitLine(s"  call i32 (ptr, ...) @printf(ptr @.nl)\n")
          case TyStruct(_, _) =>
            emitPrintStruct(arg)
            emitLine(s"  call i32 (ptr, ...) @printf(ptr @.nl)\n")
          case te: TyEnum =>
            val v = emitExpr(arg)
            requestEnumPrintHelper(te)
            emitLine(s"  call void ${enumPrintHelperName(te)}(${llvmType(te)} $v)\n")
            if isRefCountedType(arg.tpe) then emitArrDec(v, arg.tpe)
            emitLine(s"  call i32 (ptr, ...) @printf(ptr @.nl)\n")
          case TyComplex =>
            emitPrintComplex(emitExpr(arg))
            emitLine(s"  call i32 (ptr, ...) @printf(ptr @.nl)\n")
          case _ =>
            val v = emitExpr(arg)
            arg.tpe match
              case TyInteger =>
                emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_int, i64 $v)\n")
              case TyReal =>
                emitLine(s"  call void @__nex_print_real(double $v)\n")
              case TyBool =>
                val sel = newReg()
                emitLine(s"  $sel = select i1 $v, ptr @.fmt_bool_t, ptr @.fmt_bool_f\n")
                emitLine(s"  call i32 (ptr, ...) @printf(ptr $sel)\n")
              case TyString =>
                val data = newReg()
                emitLine(s"  $data = call ptr @__nex_str_data(ptr $v)\n")
                emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str, ptr $data)\n")
                emitLine(s"  call void @__nex_str_dec(ptr $v)\n")
              case other =>
                notYet(s"print(${other})")

  /** Emits a value-printing printf WITHOUT a trailing newline. Used by
    * the interpolated-string print path so each interpolated part lands
    * inline with the surrounding text.
    */
  protected def emitPrintValue(arg: TExpr): Unit =
    val v = emitExpr(arg)
    arg.tpe match
      case TyInteger =>
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_int_raw, i64 $v)\n")
      case TyReal =>
        emitLine(s"  call void @__nex_print_real_raw(double $v)\n")
      case TyBool =>
        val sel = newReg()
        emitLine(s"  $sel = select i1 $v, ptr @.fmt_str_raw, ptr @.fmt_str_raw\n")
        // Re-use the truth-table format-strings, but without their
        // trailing newlines.
        val (tStr, fStr) = ("true", "false")
        val tPtr = internStringLiteral(tStr)
        val fPtr = internStringLiteral(fStr)
        val sel2 = newReg()
        emitLine(s"  $sel2 = select i1 $v, ptr $tPtr, ptr $fPtr\n")
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $sel2)\n")
      case TyString =>
        val data = newReg()
        emitLine(s"  $data = call ptr @__nex_str_data(ptr $v)\n")
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $data)\n")
        emitLine(s"  call void @__nex_str_dec(ptr $v)\n")
      case TyArray(_, _) =>
        emitPrintArray(arg)
      case TyTuple(_) =>
        emitPrintTuple(arg)
      case TyStruct(_, _) =>
        emitPrintStruct(arg)
      case te: TyEnum =>
        requestEnumPrintHelper(te)
        emitLine(s"  call void ${enumPrintHelperName(te)}(${llvmType(te)} $v)\n")
        if isRefCountedType(arg.tpe) then emitArrDec(v, arg.tpe)
      case TyComplex =>
        emitPrintComplex(v)
      case other =>
        notYet(s"interpolated print($other)")

  /** Print a complex value (without a trailing newline) as `<re>+<im>i` or
    * `<re>-<im>i`, mirroring the interpreter's [[formatValue]]
    * implementation:
    *
    * {{{
    *   val sign = if i >= 0 then "+" else "-"
    *   s"$rs$sign${abs(i)}i"
    * }}}
    *
    * Both components route through the existing real-formatting helper
    * so whole-number parts print as `<n>.0` for parity. The `i` suffix
    * is interned once like any other string literal.
    */
  protected def emitPrintComplex(v: String): Unit =
    val re = newReg()
    emitLine(s"  $re = extractvalue { double, double } $v, 0\n")
    val im = newReg()
    emitLine(s"  $im = extractvalue { double, double } $v, 1\n")
    emitLine(s"  call void @__nex_print_real_raw(double $re)\n")
    val isPos = newReg()
    emitLine(s"  $isPos = fcmp oge double $im, 0.0\n")
    val plusS  = internStringLiteral("+")
    val minusS = internStringLiteral("-")
    val sign   = newReg()
    emitLine(s"  $sign = select i1 $isPos, ptr $plusS, ptr $minusS\n")
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $sign)\n")
    val absIm = newReg()
    emitLine(s"  $absIm = call double @fabs(double $im)\n")
    emitLine(s"  call void @__nex_print_real_raw(double $absIm)\n")
    val iS = internStringLiteral("i")
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $iS)\n")

  /** Print a Nex array as the interpreter does:
    *   rank-1:  [a, b, c]
    *   rank-2:  [[a, b], [c, d]]
    * No trailing newline — callers add one if appropriate.
    *
    * Implementation: emit a `[` then a counting loop over the elements,
    * printing each via the per-element `emitPrintArrayElem` helper with
    * `, ` separators between them, then a `]`. Rank-2 reuses the rank-1
    * helper for each inner row, but the inner-row "array" we walk is the
    * outer's row slice — we generate `i*cols+j` indexing inline.
    *
    * ARC: the `arr` argument is evaluated once into an owning SSA value
    * (TVarRef inc'd, TArrayLit fresh, etc.); after the printing loop
    * finishes, we dec the value so its ownership cycle closes.
    */
  protected def emitPrintArray(arr: TExpr): Unit =
    val rank = arrayRank(arr.tpe)
    val elem = arrayElem(arr.tpe)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val langT = llvmType(elem)
    val arrV  = emitExpr(arr)
    // Marker used after the print body to release the owning share.
    val tType = arr.tpe

    rank match
      case 1 =>
        emitPrintArr1Inline(arrV, elem, esz, stT, langT)
        emitArrDec(arrV, tType)
      case 2 =>
        val rowsR = newReg()
        emitLine(s"  $rowsR = call i64 @__nex_arr2_rows(ptr $arrV)\n")
        val colsR = newReg()
        emitLine(s"  $colsR = call i64 @__nex_arr2_cols(ptr $arrV)\n")

        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_open)\n")
        val iSlot = newReg()
        emitLine(s"  $iSlot = alloca i64\n")
        emitLine(s"  store i64 0, ptr $iSlot\n")
        val condL = freshLabel("parr2.cond")
        val bodyL = freshLabel("parr2.body")
        val exitL = freshLabel("parr2.exit")
        emitTerminator(s"  br label %$condL\n")
        startBlock(condL)
        val i = newReg()
        emitLine(s"  $i = load i64, ptr $iSlot\n")
        val ok = newReg()
        emitLine(s"  $ok = icmp slt i64 $i, $rowsR\n")
        emitTerminator(s"  br i1 $ok, label %$bodyL, label %$exitL\n")
        startBlock(bodyL)
        // print `, ` if i > 0
        val sepL  = freshLabel("parr2.sep")
        val noSep = freshLabel("parr2.nosep")
        val gt0   = newReg()
        emitLine(s"  $gt0 = icmp sgt i64 $i, 0\n")
        emitTerminator(s"  br i1 $gt0, label %$sepL, label %$noSep\n")
        startBlock(sepL)
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_sep)\n")
        emitTerminator(s"  br label %$noSep\n")
        startBlock(noSep)
        // print one row: `[` + per-element loop over j in 0..cols + `]`
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_open)\n")
        val jSlot = newReg()
        emitLine(s"  $jSlot = alloca i64\n")
        emitLine(s"  store i64 0, ptr $jSlot\n")
        val cL = freshLabel("parr2r.cond")
        val bL = freshLabel("parr2r.body")
        val xL = freshLabel("parr2r.exit")
        emitTerminator(s"  br label %$cL\n")
        startBlock(cL)
        val j = newReg()
        emitLine(s"  $j = load i64, ptr $jSlot\n")
        val ok2 = newReg()
        emitLine(s"  $ok2 = icmp slt i64 $j, $colsR\n")
        emitTerminator(s"  br i1 $ok2, label %$bL, label %$xL\n")
        startBlock(bL)
        val sep2L  = freshLabel("parr2r.sep")
        val noSep2 = freshLabel("parr2r.nosep")
        val jgt0   = newReg()
        emitLine(s"  $jgt0 = icmp sgt i64 $j, 0\n")
        emitTerminator(s"  br i1 $jgt0, label %$sep2L, label %$noSep2\n")
        startBlock(sep2L)
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_sep)\n")
        emitTerminator(s"  br label %$noSep2\n")
        startBlock(noSep2)
        val slotPtr = newReg()
        emitLine(s"  $slotPtr = call ptr @__nex_arr2_slot(ptr $arrV, i64 $i, i64 $j, i64 $esz)\n")
        val v = loadElem(stT, slotPtr, langT)
        emitPrintArrayElem(elem, v)
        val nj = newReg()
        emitLine(s"  $nj = add i64 $j, 1\n")
        emitLine(s"  store i64 $nj, ptr $jSlot\n")
        emitTerminator(s"  br label %$cL\n")
        startBlock(xL)
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_close)\n")
        val ni = newReg()
        emitLine(s"  $ni = add i64 $i, 1\n")
        emitLine(s"  store i64 $ni, ptr $iSlot\n")
        emitTerminator(s"  br label %$condL\n")
        startBlock(exitL)
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_close)\n")
        emitArrDec(arrV, tType)
      case _ =>
        notYet(s"print of rank-$rank array")

  /** Print a rank-1 array given its descriptor SSA value. Walks
    * `data[0..len)` calling `emitPrintArrayElem` for each, separated by
    * `, ` and wrapped in `[]`.
    */
  protected def emitPrintArr1Inline(arrV: String, elem: Type, esz: Int, stT: String, langT: String): Unit =
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_open)\n")
    val lenR = newReg()
    emitLine(s"  $lenR = call i64 @__nex_arr1_len(ptr $arrV)\n")
    val iSlot = newReg()
    emitLine(s"  $iSlot = alloca i64\n")
    emitLine(s"  store i64 0, ptr $iSlot\n")
    val condL = freshLabel("parr1.cond")
    val bodyL = freshLabel("parr1.body")
    val exitL = freshLabel("parr1.exit")
    emitTerminator(s"  br label %$condL\n")
    startBlock(condL)
    val i = newReg()
    emitLine(s"  $i = load i64, ptr $iSlot\n")
    val ok = newReg()
    emitLine(s"  $ok = icmp slt i64 $i, $lenR\n")
    emitTerminator(s"  br i1 $ok, label %$bodyL, label %$exitL\n")
    startBlock(bodyL)
    val sepL  = freshLabel("parr1.sep")
    val noSep = freshLabel("parr1.nosep")
    val gt0   = newReg()
    emitLine(s"  $gt0 = icmp sgt i64 $i, 0\n")
    emitTerminator(s"  br i1 $gt0, label %$sepL, label %$noSep\n")
    startBlock(sepL)
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_sep)\n")
    emitTerminator(s"  br label %$noSep\n")
    startBlock(noSep)
    val slotPtr = newReg()
    emitLine(s"  $slotPtr = call ptr @__nex_arr1_slot(ptr $arrV, i64 $i, i64 $esz)\n")
    val v = loadElem(stT, slotPtr, langT)
    emitPrintArrayElem(elem, v)
    val ni = newReg()
    emitLine(s"  $ni = add i64 $i, 1\n")
    emitLine(s"  store i64 $ni, ptr $iSlot\n")
    emitTerminator(s"  br label %$condL\n")
    startBlock(exitL)
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_close)\n")

  /** Print one element of an aggregate (array element or tuple element)
    * without a trailing newline. The SSA value `v` must already have
    * been extracted/loaded by the caller.
    */
  protected def emitPrintArrayElem(elem: Type, v: String): Unit =
    elem match
      case TyInteger =>
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_int_raw, i64 $v)\n")
      case TyReal =>
        emitLine(s"  call void @__nex_print_real_raw(double $v)\n")
      case TyBool =>
        val tPtr = internStringLiteral("true")
        val fPtr = internStringLiteral("false")
        val sel  = newReg()
        emitLine(s"  $sel = select i1 $v, ptr $tPtr, ptr $fPtr\n")
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $sel)\n")
      case TyString =>
        val data = newReg()
        emitLine(s"  $data = call ptr @__nex_str_data(ptr $v)\n")
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $data)\n")
      case t @ TyTuple(es) =>
        // Inline tuple-element print using insertvalue/extractvalue.
        // Reuses emitPrintArrayElem recursively for each field.
        emitPrintTupleValue(v, t, es)
      case t @ TyStruct(name, fields) =>
        emitPrintStructValue(v, t, name, fields)
      case te: TyEnum =>
        requestEnumPrintHelper(te)
        emitLine(s"  call void ${enumPrintHelperName(te)}(${llvmType(te)} $v)\n")
      case TyComplex =>
        // Already-loaded `{ double, double }` value — reuse the
        // top-level complex print path which handles the sign /
        // abs(im) / `i` suffix matching the interpreter.
        emitPrintComplex(v)
      case other =>
        notYet(s"print element of type $other")

  /** Print a tuple SSA value (struct aggregate) as `(a, b, c)` without
    * a trailing newline. Factored from [[emitPrintTuple]] so callers
    * holding an already-loaded struct (e.g., array-of-tuple element
    * printing) can reuse it.
    */
  protected def emitPrintTupleValue(v: String, t: Type, es: List[Type]): Unit =
    val tupTy  = llvmType(t)
    val openP  = internStringLiteral("(")
    val closeP = internStringLiteral(")")
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $openP)\n")
    for (elemT, i) <- es.zipWithIndex do
      if i > 0 then emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_sep)\n")
      val elemV = newReg()
      emitLine(s"  $elemV = extractvalue $tupTy $v, $i\n")
      emitPrintArrayElem(elemT, elemV)
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $closeP)\n")

  /** Print a tuple value as `(a, b, c)` (no trailing newline). Used by
    * both the top-level print path and the interpolated-string element
    * path. The actual loop lives in [[emitPrintTupleValue]] so it can
    * be re-entered for tuple-of-tuple from inside [[emitPrintArrayElem]].
    */
  protected def emitPrintTuple(arg: TExpr): Unit =
    val v   = emitExpr(arg)
    val tup = arg.tpe match
      case TyTuple(es) => es
      case other       =>
        notYet(s"print tuple — expected tuple type, got $other")
        return
    emitPrintTupleValue(v, arg.tpe, tup)

  /** Print a struct value as `Name { field=value, field=value }` (no
    * trailing newline). Used by the print path and tuple/array element
    * recursion.
    */
  protected def emitPrintStructValue(v: String, t: Type, name: String, fields: List[(String, Type)]): Unit =
    val structTy = llvmType(t)
    val header   = internStringLiteral(s"$name { ")
    val closer   = internStringLiteral(" }")
    val eq       = internStringLiteral("=")
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $header)\n")
    for (((fname, ftype), i) <- fields.zipWithIndex) do
      if i > 0 then emitLine(s"  call i32 (ptr, ...) @printf(ptr @.arr_sep)\n")
      val keyPtr = internStringLiteral(fname)
      emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $keyPtr)\n")
      emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $eq)\n")
      val fv = newReg()
      emitLine(s"  $fv = extractvalue $structTy $v, $i\n")
      emitPrintArrayElem(ftype, fv)
    emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $closer)\n")

  protected def emitPrintStruct(arg: TExpr): Unit =
    val v = emitExpr(arg)
    arg.tpe match
      case TyStruct(name, fields) => emitPrintStructValue(v, arg.tpe, name, fields)
      case other                  =>
        notYet(s"print struct — expected struct type, got $other")
