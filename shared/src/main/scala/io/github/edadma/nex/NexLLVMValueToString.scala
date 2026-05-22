package io.github.edadma.nex

/** Value-formatting and `s"..."` / `f"..."` lowering for the LLVM
  * backend.
  *
  * Every numeric / aggregate value-to-string path lives here:
  *
  *   - [[emitValueToString]] is the entry point for an interpolated
  *     value of any type. It dispatches by static type to the
  *     per-shape formatters and routes refcounted args (`TyString`,
  *     `TyEnum` carrying refcounted fields) through the appropriate
  *     ARC `dec` after the descriptor has been built.
  *   - [[emitValueToStringInt]] / [[emitValueToStringReal]] /
  *     [[emitValueToStringBool]] / [[emitValueToStringComplex]] —
  *     scalar paths backed by runtime helpers (`__nex_str_from_i64`
  *     etc.). Bool selects between two interned literal descriptors;
  *     complex builds `<re><sign><|im|>i` via a concat chain.
  *   - [[emitValueToStringTuple]] / [[emitValueToStringStruct]] /
  *     [[emitValueToStringArray]] — aggregate paths. Tuples and
  *     structs format eagerly (every field at codegen time);
  *     arrays loop at runtime and concat per element.
  *   - [[emitTypedValueToString]] is the same dispatch keyed by an
  *     already-loaded SSA + Type; used by the aggregate formatters
  *     to recurse, and also by [[NexLLVMEnums]]' `__nex_enum_str_*`
  *     helpers when building the per-variant `Name(f0, f1, ...)`
  *     descriptor.
  *   - [[concatChain]] folds a list of owned descriptor pointers into
  *     a single descriptor via `__nex_str_concat`, releasing each
  *     operand share as it consumes it.
  *   - [[emitInterpStringValue]] is the `s"..."` driver — text parts
  *     intern as literal descriptors, ref / expr parts route through
  *     [[emitValueToString]] (no spec) or [[emitFormattedDesc]] (when
  *     an `f"..."`-style spec is attached).
  *   - [[emitFormattedDesc]] is the `f"..."` driver: dispatches by
  *     conversion character to `snprintf` (`d e f g s x X o`) or the
  *     inline `%b` binary builder.
  */
protected trait NexLLVMValueToString extends NexLLVMState:

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
      case te: TyEnum =>
        val v = emitExpr(e)
        requestEnumPrintHelper(te)
        val r = newReg()
        emitLine(s"  $r = call ptr ${enumStrHelperName(te)}(${llvmType(te)} $v)\n")
        // The enum value share carried scalar / immortal-string slots
        // only; releasing it requires a per-enum drop walk when it
        // carries refcounted fields. For pure-scalar enums dec is a no-op.
        if isRefCountedType(e.tpe) then emitArrDec(v, e.tpe)
        r
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
      case te: TyEnum =>
        requestEnumPrintHelper(te)
        val r = newReg()
        emitLine(s"  $r = call ptr ${enumStrHelperName(te)}(${llvmType(te)} $v)\n")
        r
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
      case TInterpText(text)        => internStringDescriptor(text)
      case TInterpRef(sym, None)    => emitValueToString(TVarRef(sym, None, sym.tpe))
      case TInterpRef(sym, Some(s)) => emitFormattedDesc(TVarRef(sym, None, sym.tpe), s)
      case TInterpExpr(x, None)     => emitValueToString(x)
      case TInterpExpr(x, Some(s))  => emitFormattedDesc(x, s)
      case _: TInterpRaw            =>
        notYet("interpolated `${...}` raw fragment (should have been re-parsed in Stage 1)")
        internStringDescriptor("")
    }
    concatChain(partDescs)

  /** Format `e` using a printf-style spec (`%5d`, `%.3f`, etc.) from an
    * `f"..."` literal, producing a fresh %nex_str descriptor. Routes
    * through libc `snprintf` for the standard conversions (`d e f g s
    * x X o`) and a small inline loop for `%b` (binary). The conversion
    * char is the spec's last character; the elaborator/lexer already
    * validated the shape.
    */
  protected def emitFormattedDesc(e: TExpr, spec: String): String =
    val conv = spec.last
    conv match
      case 'd' =>
        emitSnprintfDesc(spec, "i64", emitInt(e, spec))
      case 'f' | 'e' | 'g' | 'E' | 'G' =>
        emitSnprintfDesc(spec, "double", emitReal(e, spec))
      case 's' =>
        // String arg: the value already lands as a %nex_str ptr; we need
        // the data ptr for snprintf's `%s`. Route through __nex_str_data
        // and release the descriptor after snprintf reads from it.
        val sv  = emitExpr(e)
        val tt  = e.tpe
        val sp  = newReg()
        emitLine(s"  $sp = call ptr @__nex_str_data(ptr $sv)\n")
        val desc = emitSnprintfDesc(spec, "ptr", sp)
        if tt == TyString then emitArrDec(sv, tt)
        desc
      case 'x' | 'X' | 'o' =>
        emitSnprintfDesc(spec, "i64", emitInt(e, spec))
      case 'b' =>
        emitBinaryDesc(emitInt(e, spec), spec)
      case other =>
        notImpl(s"unknown format conversion `$other` in spec `$spec`")

  /** Coerce a numeric expression to i64 for an integer-typed spec. */
  private def emitInt(e: TExpr, spec: String): String = e.tpe match
    case TyInteger => emitExpr(e)
    case other     =>
      notImpl(s"format spec `$spec` expects integer, got $other")

  /** Coerce a numeric expression to double for a real-typed spec.
    * Integers are sitofp-lifted; reals pass through.
    */
  private def emitReal(e: TExpr, spec: String): String = e.tpe match
    case TyReal    => emitExpr(e)
    case TyInteger =>
      val v = emitExpr(e)
      val r = newReg()
      emitLine(s"  $r = sitofp i64 $v to double\n")
      r
    case other     =>
      notImpl(s"format spec `$spec` expects real, got $other")

  /** Emit a `snprintf` call producing a fresh %nex_str descriptor. The
    * format-string literal lives in the string-literal pool; the
    * sizing pre-pass calls snprintf with a NULL buffer to learn the
    * length, then a real call writes into the descriptor's data buffer.
    */
  private def emitSnprintfDesc(spec: String, argLLT: String, argReg: String): String =
    val fmtPtr = internStringLiteral(spec)
    val lenR   = newReg()
    emitLine(s"  $lenR = call i32 (ptr, i64, ptr, ...) @snprintf(ptr null, i64 0, ptr $fmtPtr, $argLLT $argReg)\n")
    val len64  = newReg()
    emitLine(s"  $len64 = sext i32 $lenR to i64\n")
    val desc   = newReg()
    emitLine(s"  $desc = call ptr @__nex_str_alloc(i64 $len64)\n")
    val dp     = newReg()
    emitLine(s"  $dp = getelementptr inbounds %nex_str, ptr $desc, i32 0, i32 2\n")
    val dat    = newReg()
    emitLine(s"  $dat = load ptr, ptr $dp\n")
    val cap    = newReg()
    emitLine(s"  $cap = add i64 $len64, 1\n")
    emitLine(s"  call i32 (ptr, i64, ptr, ...) @snprintf(ptr $dat, i64 $cap, ptr $fmtPtr, $argLLT $argReg)\n")
    desc

  /** Emit a `%b` (binary) formatted string for an i64 value. Java/C
    * `printf` don't agree on `%b`, so we build it inline: extract the
    * raw binary digits via a runtime helper, then optionally pad to
    * the spec's width with `0`/' ' / left-alignment from the flag set.
    * For chunk 1, only unflagged + width is handled by routing to a
    * runtime `__nex_str_from_bin(i64, ...)` — but to keep the runtime
    * surface small we stamp a small inline LLVM loop here. The
    * width/flag parsing happens at codegen time on the spec literal.
    */
  private def emitBinaryDesc(argReg: String, spec: String): String =
    // Parse width / flags from the spec at compile time (the lexer
    // already accepted the shape). Spec = `%[flags][width]b`.
    val body  = spec.substring(1, spec.length - 1)
    var i     = 0
    var leftAlign = false
    var zeroPad   = false
    while i < body.length && "-+0 ".contains(body.charAt(i)) do
      body.charAt(i) match
        case '-' => leftAlign = true
        case '0' => zeroPad   = true
        case _   => ()
      i += 1
    val widthStr = body.substring(i).takeWhile(_.isDigit)
    val width    = if widthStr.isEmpty then 0 else widthStr.toInt
    // Compute raw binary via the runtime helper, then pad via the
    // shared __nex_str_pad helper if width > 0.
    val raw = newReg()
    emitLine(s"  $raw = call ptr @__nex_str_from_bin(i64 $argReg)\n")
    if width <= 0 then raw
    else
      val padChar = if zeroPad && !leftAlign then 48 else 32  // '0' vs ' '
      val side    = if leftAlign then 1 else 0                // 0 = right-justify, 1 = left-justify
      val padded = newReg()
      emitLine(s"  $padded = call ptr @__nex_str_pad(ptr $raw, i64 $width, i8 $padChar, i32 $side)\n")
      emitLine(s"  call void @__nex_str_dec(ptr $raw)\n")
      padded
