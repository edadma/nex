package io.github.edadma.nex

/** String surface for [[NexMLIRCodegen]]. Holds the module-level
  * `memref.global` byte-array pool and the per-call emitters for
  * string literals, concatenation, equality, interpolation, and
  * f-string format specs. All members are self-typed against the
  * host class so they share its mutable state ([[out]],
  * [[globalDecls]], [[stringLitPool]], …) and its visitor
  * ([[emitExpr]]) for the value-position interpolation parts.
  */
trait NexMLIRStrings:
  self: NexMLIRCodegen =>

  /** Emit a module-level `memref.global` byte array for `text` (or
    * reuse the previously-emitted symbol if `text` was seen before)
    * and return SSA registers carrying the data pointer (as `i64`)
    * and the byte length (as `i64`).
    *
    * Used by both the string-literal pathway ([[emitStringLiteral]])
    * and the f-string format-spec pathway, where the spec text is
    * handed directly to runtime helpers as `(ptr, len)` rather than
    * wrapped in a descriptor.
    */
  protected def emitLiteralBytes(text: String): (String, String) =
    val sym = stringLitPool.getOrElseUpdate(text, {
      val idx = stringLitPool.size
      val name = s"@nex_strlit_$idx"
      val bytes = text.getBytes("UTF-8")
      val byteShape = if bytes.isEmpty then 1 else bytes.length
      val dense =
        if bytes.isEmpty then "dense<0>"
        else s"dense<[${bytes.map(b => (b.toInt & 0xff).toString).mkString(", ")}]>"
      globalDecls.append(s"""memref.global "private" constant $name : memref<${byteShape}xi8> = $dense\n""")
      name
    })
    val bytes = text.getBytes("UTF-8")
    val byteShape = if bytes.isEmpty then 1 else bytes.length
    val gReg = fresh("strg")
    out.append(s"  $gReg = memref.get_global $sym : memref<${byteShape}xi8>\n")
    val idxReg = fresh("stridx")
    out.append(s"  $idxReg = memref.extract_aligned_pointer_as_index $gReg : memref<${byteShape}xi8> -> index\n")
    val ptrReg = fresh("strp")
    out.append(s"  $ptrReg = arith.index_castui $idxReg : index to i64\n")
    val lenReg = fresh("strl")
    out.append(s"  $lenReg = arith.constant ${bytes.length} : i64\n")
    (ptrReg, lenReg)

  /** Intern a string literal and emit code that yields an `i64`
    * descriptor pointer to a `nex_str` whose refcount is the immortal
    * sentinel (-1). The byte array lives in module-level
    * `memref.global` storage (deduplicated across identical literals);
    * each use site builds the descriptor lazily via the C runtime's
    * `nex_str_lit_from_cstr`, which caches by data-pointer identity so
    * repeated calls return the same descriptor address.
    *
    * Returned SSA register has type `i64` (an MString descriptor
    * pointer). Caller should NOT `nex_str_dec` the result — the
    * runtime treats refcount=-1 as a no-op for both inc and dec, but
    * the literal pool retains pointer identity regardless.
    */
  protected def emitStringLiteral(text: String): String =
    val (ptrReg, lenReg) = emitLiteralBytes(text)
    val descReg = fresh("strd")
    out.append(s"  $descReg = func.call @nex_str_lit_from_cstr($ptrReg, $lenReg) : (i64, i64) -> i64\n")
    descReg

  /** Concatenate two strings via `nex_str_concat`. The runtime returns
    * a fresh heap descriptor (refcount=1); both operands are
    * decremented since they are no longer referenced after this
    * expression. Mirrors the LLVM backend's concat-then-dec sequence.
    */
  protected def emitStringConcat(lReg: String, rReg: String): MlirVal =
    val r = fresh("strcat")
    out.append(s"  $r = func.call @nex_str_concat($lReg, $rReg) : (i64, i64) -> i64\n")
    out.append(s"  func.call @nex_str_dec($lReg) : (i64) -> ()\n")
    out.append(s"  func.call @nex_str_dec($rReg) : (i64) -> ()\n")
    MlirVal(r, MString)

  /** Convert a value-producing expression to a fresh `MString`
    * descriptor. Mirrors LLVM's `emitValueToString` dispatch; for now
    * only int / bool / string are supported (real / complex / tuple
    * follow when more corpus cases need them). String values are
    * passed through with an extra `nex_str_inc` so the descriptor's
    * lifetime extends to the concat chain that consumes it.
    */
  protected def emitValueToString(e: TExpr): String =
    val v = emitExpr(e)
    emitMlirValToString(v)

  /** Convert an already-emitted [[MlirVal]] to a fresh string
    * descriptor. Shared between [[emitValueToString]] (which evaluates
    * the source expression first) and the per-slot tuple formatter
    * (which has already extracted the element).
    */
  protected def emitMlirValToString(v: MlirVal): String =
    v.ty match
      case MScalar(TyInteger) =>
        val r = fresh("vstri")
        out.append(s"  $r = func.call @nex_str_from_i64(${v.reg}) : (i64) -> i64\n")
        r
      case MScalar(TyReal) =>
        val r = fresh("vstrr")
        out.append(s"  $r = func.call @nex_str_from_f64(${v.reg}) : (f64) -> i64\n")
        r
      case MScalar(TyBool) =>
        val b = fresh("vstrb")
        out.append(s"  $b = arith.extui ${v.reg} : i1 to i8\n")
        val r = fresh("vstrbs")
        out.append(s"  $r = func.call @nex_str_from_bool($b) : (i8) -> i64\n")
        r
      case MString =>
        out.append(s"  func.call @nex_str_inc(${v.reg}) : (i64) -> ()\n")
        v.reg
      case mt: MTuple =>
        emitTupleToString(v.reg, mt)
      case other =>
        notYet(s"value-to-string for $other")

  /** Format a tuple value as `(e0, e1, ..., eN-1)` into a heap string
    * descriptor. Extracts each slot with `llvm.extractvalue`, converts
    * it via [[emitMlirValToString]] (recurses for nested tuples), and
    * left-folds concat with comma-space separators between slots.
    * Each `nex_str_concat` operand is `dec`'d after the call (literals
    * skip; heap descriptors with `refcount=1` drop to zero and free).
    * The final descriptor's `refcount=1` belongs to the caller.
    */
  protected def emitTupleToString(reg: String, mt: MTuple): String =
    def concatInto(acc: String, next: String): String =
      val r = fresh("tupcc")
      out.append(s"  $r = func.call @nex_str_concat($acc, $next) : (i64, i64) -> i64\n")
      out.append(s"  func.call @nex_str_dec($acc) : (i64) -> ()\n")
      out.append(s"  func.call @nex_str_dec($next) : (i64) -> ()\n")
      r
    var acc = emitStringLiteral("(")
    for ((slotTy, idx) <- mt.elems.zipWithIndex) do
      if idx > 0 then acc = concatInto(acc, emitStringLiteral(", "))
      val slotR = fresh(s"tps_$idx")
      out.append(s"  $slotR = llvm.extractvalue $reg[$idx] : ${mt.text}\n")
      acc = concatInto(acc, emitMlirValToString(MlirVal(slotR, slotTy)))
    concatInto(acc, emitStringLiteral(")"))

  /** Convert a value-producing expression to a fresh `MString`
    * descriptor formatted according to an f-string spec like
    * `%5d`, `%.3f`, `%x`, `%016b`. Dispatch is by the spec's last
    * character (the conversion). Numeric int-typed values reach the
    * `d`/`x`/`X`/`o` arms (runtime translates the spec to a C int64
    * spec). Real values (or int values that need promotion) reach
    * `f`/`e`/`g`/`E`/`G` (runtime passes the spec through to
    * snprintf). Binary `%b` is non-standard in C printf; we parse
    * width and the `0` / `-` flags at codegen time and call a custom
    * runtime that formats by-bit-shift.
    */
  protected def emitFormattedValue(e: TExpr, spec: String): String =
    if spec.isEmpty || spec.head != '%' then
      notYet(s"format spec `$spec` lacks leading `%`")
    val conv = spec.last
    conv match
      case 'd' | 'x' | 'X' | 'o' =>
        val v = emitExpr(e)
        val intReg = v.ty match
          case MScalar(TyInteger) => v.reg
          case other              => notYet(s"format spec `$spec` expects integer, got $other")
        val (ptr, len) = emitLiteralBytes(spec)
        val r = fresh("fmti")
        out.append(s"  $r = func.call @nex_str_format_i64($ptr, $len, $intReg) : (i64, i64, i64) -> i64\n")
        r
      case 'f' | 'e' | 'g' | 'E' | 'G' =>
        val v = emitExpr(e)
        val realReg = v.ty match
          case MScalar(TyReal)    => v.reg
          case MScalar(TyInteger) => promoteIntToReal(v).reg
          case other              => notYet(s"format spec `$spec` expects real, got $other")
        val (ptr, len) = emitLiteralBytes(spec)
        val r = fresh("fmtf")
        out.append(s"  $r = func.call @nex_str_format_f64($ptr, $len, $realReg) : (i64, i64, f64) -> i64\n")
        r
      case 'b' =>
        // Parse spec at codegen time: `%[flags][width]b`.
        val (width, zeroPad, leftAlign) = parseBinSpec(spec)
        val v = emitExpr(e)
        val intReg = v.ty match
          case MScalar(TyInteger) => v.reg
          case other              => notYet(s"binary spec on $other")
        val wReg = fresh("bw")
        out.append(s"  $wReg = arith.constant $width : i64\n")
        val zReg = fresh("bz")
        out.append(s"  $zReg = arith.constant ${if zeroPad then 1 else 0} : i8\n")
        val lReg = fresh("bl")
        out.append(s"  $lReg = arith.constant ${if leftAlign then 1 else 0} : i8\n")
        val r = fresh("fmtb")
        out.append(s"  $r = func.call @nex_str_format_bin($intReg, $wReg, $zReg, $lReg) : (i64, i64, i8, i8) -> i64\n")
        r
      case 's' =>
        // String-typed value with `%s` — for the current corpus this
        // only appears in `def`-returning paths (e.g. `render(label,
        // x)`). Defer until user defs come online.
        notYet(s"format spec `%s` (string conversion not yet routed)")
      case other =>
        notYet(s"format conversion `$other` in spec `$spec`")

  /** Parse a `%[flags][width]b` spec at codegen time. Returns
    * `(width, zeroPad, leftAlign)`. Flags: `0` → zeroPad, `-` →
    * leftAlign. Width is the optional decimal digit run before `b`.
    * Used by [[emitFormattedValue]]'s `b` arm.
    */
  protected def parseBinSpec(spec: String): (Int, Boolean, Boolean) =
    val body = spec.substring(1, spec.length - 1)  // strip `%` and trailing `b`
    var zeroPad = false
    var leftAlign = false
    var i = 0
    while i < body.length && (body.charAt(i) == '0' || body.charAt(i) == '-') do
      if body.charAt(i) == '0' then zeroPad = true
      if body.charAt(i) == '-' then leftAlign = true
      i += 1
    var width = 0
    while i < body.length && body.charAt(i).isDigit do
      width = width * 10 + (body.charAt(i) - '0')
      i += 1
    (width, zeroPad, leftAlign)

  /** Build a fresh heap string descriptor from an interpolation parts
    * list. Each text part becomes an immortal literal; each
    * `${value}` part is emitted and converted via [[emitValueToString]];
    * then the descriptors are left-folded with `nex_str_concat` (each
    * operand dec'd as it is consumed, per the runtime's discipline).
    * Empty parts list yields the empty-literal descriptor.
    */
  protected def emitInterpString(parts: List[TInterpPart]): MlirVal =
    if parts.isEmpty then
      return MlirVal(emitStringLiteral(""), MString)
    val descs = parts.map {
      case TInterpText(t)            => emitStringLiteral(t)
      case TInterpRef(sym, None)     => emitValueToString(TVarRef(sym, None, sym.tpe))
      case TInterpExpr(x, None)      => emitValueToString(x)
      case TInterpRef(sym, Some(s))  => emitFormattedValue(TVarRef(sym, None, sym.tpe), s)
      case TInterpExpr(x, Some(s))   => emitFormattedValue(x, s)
      case _: TInterpRaw             => notYet("raw interp fragment (should have been re-parsed in Stage 1)")
    }
    val folded = descs.reduceLeft { (acc, d) =>
      val r = fresh("istr")
      out.append(s"  $r = func.call @nex_str_concat($acc, $d) : (i64, i64) -> i64\n")
      out.append(s"  func.call @nex_str_dec($acc) : (i64) -> ()\n")
      out.append(s"  func.call @nex_str_dec($d) : (i64) -> ()\n")
      r
    }
    MlirVal(folded, MString)

  /** Byte-wise string equality / inequality via `nex_str_eq`. The
    * runtime returns an `i8` (0 or 1); we truncate to `i1` for boolean
    * use. `!=` xors with 1. Both operands are decremented after the
    * comparison reads from them.
    */
  protected def emitStringCompare(op: String, lReg: String, rReg: String): MlirVal =
    val rawR = fresh("streq")
    out.append(s"  $rawR = func.call @nex_str_eq($lReg, $rReg) : (i64, i64) -> i8\n")
    out.append(s"  func.call @nex_str_dec($lReg) : (i64) -> ()\n")
    out.append(s"  func.call @nex_str_dec($rReg) : (i64) -> ()\n")
    val b1 = fresh("streqb")
    out.append(s"  $b1 = arith.trunci $rawR : i8 to i1\n")
    op match
      case "==" => MlirVal(b1, MScalar(TyBool))
      case "!=" =>
        val one = fresh("one")
        out.append(s"  $one = arith.constant 1 : i1\n")
        val neg = fresh("strne")
        out.append(s"  $neg = arith.xori $b1, $one : i1\n")
        MlirVal(neg, MScalar(TyBool))
      case other => notYet(s"string comparison `$other`")
