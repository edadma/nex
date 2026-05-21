package io.github.edadma.nex

import scala.collection.mutable

/** MLIR backend. Compiles a growing surface of Nex programs to MLIR
  * text, which the parity harness lowers via `mlir-opt` and
  * `mlir-translate` and links with `mlir_runtime.c` (supplying
  * `nex_print_i64` / `nex_print_f64`) through `clang`.
  *
  * Surface as of milestone 3:
  *
  *   - `def main() = print(sum([<num literals>]))`
  *   - `def main() = print(sum(a + b))` where `a + b` are rank-1
  *     numeric array literals, possibly bound via `val` inside a block.
  *
  * The codegen is now structured as an expression visitor —
  * [[emitExpr]] takes a typed expression and returns the SSA register
  * plus its MLIR type. A symbol environment (Symbol id → MlirVal)
  * carries val-bindings through nested blocks. Element-wise array
  * addition lowers to `linalg.map` over `tensor.empty()` outputs;
  * `sum` over a tensor lowers to `linalg.reduce` followed by
  * `tensor.extract`. Both still flow through the same one-shot
  * bufferize → linalg-to-loops pipeline from the milestone-1 PoC.
  *
  * Anything outside the recognised shape throws `notYet`.
  */
class NexMLIRCodegen:

  /** MLIR type of an emitted value. Either an LLVM-like scalar or a
    * static-shape tensor. We don't model rank-2 yet — every tensor in
    * milestone 3 is rank-1 with a known length.
    */
  private sealed trait MlirType:
    def text: String

  private case class MScalar(elem: Type) extends MlirType:
    def text: String = scalarText(elem)

  /** A `tensor<...x<elem>>`. `shape == Nil` denotes a 0-d
    * `tensor<elem>` (the shape of `linalg.reduce`'s output).
    * A `-1` in `shape` denotes a dynamic dimension (prints as `?` in
    * the MLIR type spelling). Each dynamic dim's runtime size is
    * carried as an SSA `index` value by whatever op originally
    * produced the tensor — downstream code recovers it lazily via
    * `tensor.dim`.
    */
  private case class MTensor(elem: Type, shape: List[Int]) extends MlirType:
    def text: String =
      val dims =
        if shape.isEmpty then ""
        else shape.map(d => if d < 0 then "?" else d.toString).mkString("", "x", "x")
      s"tensor<$dims${scalarText(elem)}>"
    def isDynamic: Boolean = shape.exists(_ < 0)

  /** A Nex `string` value at the MLIR level: opaque `i64` carrying the
    * pointer to a C-side `nex_str` descriptor (`{ refcount, length,
    * data }`). All string operations route through the C runtime in
    * `mlir_runtime.c`; this codegen never derefs the descriptor
    * directly, so `i64` is enough.
    */
  private case object MString extends MlirType:
    def text: String = "i64"

  private case class MlirVal(reg: String, ty: MlirType)

  private val out             = new StringBuilder
  /** Module-level declarations (memref.global byte arrays for string
    * literals, etc.) flushed before [[out]] when [[compile]] returns
    * the final module text. Kept in a separate buffer so emitters deep
    * in the visitor can lazily contribute module-scope ops.
    */
  private val globalDecls     = new StringBuilder
  /** Deduplicates string literal globals. Keys are the literal text;
    * values are the `@nex_strlit_N` symbol name. The byte-array global
    * has been emitted into [[globalDecls]] when the entry is first
    * created.
    */
  private val stringLitPool   = mutable.Map.empty[String, String]
  private var nextReg         = 0
  private val env             = mutable.Map.empty[Int, MlirVal]
  /** `var` scalar bindings live in stack memrefs so that
    * load-modify-store patterns and loop-mutated counters compile to
    * the obvious code. Keyed by symbol id; the value carries the
    * memref SSA name and the element type. Registered when a
    * `var x = init` binding is emitted; consulted by both `TVarRef`
    * (which loads) and `TAssign` (which stores).
    */
  private val varSlots        = mutable.Map.empty[Int, (String, MScalar)]
  /** Per-program registry of `@intrinsic("libm.X")` function symbols.
    * Populated at the start of [[compile]] by scanning every
    * [[TFunDecl]] whose body is a [[TIntrinsic]]. At a [[TCall]] site
    * the codegen looks the callee's symbol id up here; on a hit the
    * call lowers to a direct `func.call @X(...)` instead of routing
    * through the generic call path (which still rejects everything
    * else as `notYet`).
    */
  private val libmIntrinsics  = mutable.Map.empty[Int, String]

  /** Per-program registry of user-defined `def`s lowered to
    * `func.func`. Keyed by the declaration symbol id; value carries
    * the mangled MLIR symbol name, the param types, and the return
    * type (`None` for unit-returning defs — not yet supported in this
    * phase). A non-`main` `TFunDecl` is registered here if and only
    * if every param type and the return type are emittable
    * ([[mlirTypeOf]] returns `Some`). Calls whose callee is in this
    * map lower to `func.call @<name>` ; otherwise the existing
    * notYet path stays in place.
    */
  private val userDefs        = mutable.Map.empty[Int, (String, List[MlirType], Option[MlirType])]

  /** Top-level `const` / `val` bindings whose initialiser is a literal
    * scalar (real / int / bool). Stored as the original `TExpr`. At
    * any `TVarRef(sym)` site whose `sym.id` is registered here, the
    * codegen re-emits the literal expression — that gives user-def
    * bodies access to prelude constants like `pi`/`e` (which live in
    * `auxDecls`) and to user top-level constants without needing
    * module-scope globals. Non-literal initialisers (e.g.
    * `val s = square(7)`) are left for env-based lookup inside the
    * main function and notYet inside user-def regions.
    */
  private val topLevelLiteralInits = mutable.Map.empty[Int, TExpr]

  def compile(tp: TProgram): String =
    val mainDecl = tp.decls.collectFirst {
      case f: TFunDecl if f.sym.name == "main" && f.params.isEmpty => f
    }.getOrElse(notYet("program has no `def main()`"))

    libmIntrinsics.clear()
    stringLitPool.clear()
    globalDecls.clear()
    userDefs.clear()
    topLevelLiteralInits.clear()
    tp.allDecls.foreach {
      case TTopBinding(sym, BindingKind.Val | BindingKind.Const, value, _)
          if isLiteralScalarExpr(value) =>
        topLevelLiteralInits(sym.id) = value
      case _ => ()
    }
    tp.allDecls.foreach {
      case f: TFunDecl =>
        f.body match
          case TIntrinsic(opId, _, _) if opId.startsWith("libm.") =>
            libmIntrinsics(f.sym.id) = opId.stripPrefix("libm.")
          case _ => ()
      case _ => ()
    }
    // Registry of user-defined non-main, non-intrinsic `def`s whose
    // signatures the MLIR backend can express. Filtering here means
    // the dispatch site in `emitExpr` can stay narrow and any
    // call to an un-emittable def naturally falls through to notYet.
    tp.allDecls.foreach {
      case f: TFunDecl
          if f.sym.name != "main" && !libmIntrinsics.contains(f.sym.id) =>
        val paramTys = f.params.map(p => mlirTypeOf(p.tpe))
        // Outer Option tracks "supported at all", inner Option tracks
        // "value-returning vs unit-returning". TyUnit registers with
        // an inner None.
        val retSlot: Option[Option[MlirType]] = f.returnType match
          case TyUnit => Some(None)
          case other  => mlirTypeOf(other).map(Some(_))
        if paramTys.forall(_.isDefined) && retSlot.isDefined then
          userDefs(f.sym.id) = (mangleUserDef(f.sym), paramTys.map(_.get), retSlot.get)
      case _ => ()
    }

    out.append("func.func private @nex_print_i64(i64)\n")
    out.append("func.func private @nex_print_f64(f64)\n")
    out.append("func.func private @nex_print_bool(i1)\n")
    out.append("func.func private @nex_print_array_1d_i64(i64, i64)\n")
    out.append("func.func private @nex_print_array_1d_f64(i64, i64)\n")
    out.append("func.func private @nex_print_array_2d_i64(i64, i64, i64)\n")
    out.append("func.func private @nex_print_array_2d_f64(i64, i64, i64)\n")
    out.append("func.func private @nex_print_array_1d_bool(i64, i64)\n")
    out.append("func.func private @nex_print_array_2d_bool(i64, i64, i64)\n")
    out.append("func.func private @nex_ipow(i64, i64) -> i64\n")
    out.append("func.func private @nex_trap_slice_oob()\n")
    out.append("func.func private @nex_trap_axis_oob()\n")
    out.append("func.func private @nex_str_lit_from_cstr(i64, i64) -> i64\n")
    out.append("func.func private @nex_str_inc(i64)\n")
    out.append("func.func private @nex_str_dec(i64)\n")
    out.append("func.func private @nex_print_str(i64)\n")
    out.append("func.func private @nex_str_concat(i64, i64) -> i64\n")
    out.append("func.func private @nex_str_eq(i64, i64) -> i8\n")
    out.append("func.func private @nex_str_from_i64(i64) -> i64\n")
    out.append("func.func private @nex_str_from_bool(i8) -> i64\n")
    out.append("func.func private @nex_str_from_f64(f64) -> i64\n")
    out.append("func.func private @nex_str_format_i64(i64, i64, i64) -> i64\n")
    out.append("func.func private @nex_str_format_f64(i64, i64, f64) -> i64\n")
    out.append("func.func private @nex_str_format_bin(i64, i64, i8, i8) -> i64\n")
    // libm bridges declared by the `@intrinsic` decls discovered above.
    // Two-argument libm fns (atan2, hypot, pow) get a (f64, f64) -> f64
    // signature; everything else is unary. `pow` is also declared
    // unconditionally because the `^` operator routes there.
    out.append("func.func private @pow(f64, f64) -> f64\n")
    libmIntrinsics.values.toSeq.sorted.distinct.foreach {
      case "pow"                  => ()  // already declared above
      case n @ ("atan2" | "hypot") =>
        out.append(s"func.func private @$n(f64, f64) -> f64\n")
      case n =>
        out.append(s"func.func private @$n(f64) -> f64\n")
    }
    out.append("\n")
    // Emit user-defined `def`s as `func.func` ops at module level
    // BEFORE `@main`, so they're visible to call sites inside main
    // (and to each other for mutual recursion). MLIR module ops are
    // order-independent, but emitting in declaration order helps when
    // skimming the lowered text.
    tp.allDecls.foreach {
      case f: TFunDecl if userDefs.contains(f.sym.id) =>
        emitUserDef(f)
      case _ => ()
    }
    out.append("func.func @main() -> i32 {\n")
    nextReg = 0
    env.clear()
    val c0 = fresh("c0")
    out.append(s"  $c0 = arith.constant 0 : i32\n")
    emitTopBindings(tp)
    emitMainBody(mainDecl.body)
    out.append(s"  func.return $c0 : i32\n")
    out.append("}\n")
    // Module-level globals (string literal byte arrays, etc.) come
    // first; MLIR's module is order-independent but the conventional
    // ordering puts globals before functions.
    globalDecls.toString + out.toString

  /** Materialise every top-level `val` and `const` binding into the
    * env, in declaration order, by reusing the expression visitor.
    * Iterates `tp.allDecls` so that source-prelude consts (`pi`,
    * `e`) live in env alongside user-declared top-level vals.
    * Emitted inline at the top of `@main` — top-level bindings
    * semantically execute once at program start, and `@main` is the
    * only function-scope region the backend emits today, so this is
    * the natural place. `var` top-level bindings remain `notYet`.
    */
  private def emitTopBindings(tp: TProgram): Unit =
    tp.allDecls.foreach {
      case TTopBinding(sym, BindingKind.Val | BindingKind.Const, value, _) =>
        env(sym.id) = emitExpr(value)
      case TTopBinding(sym, kind, _, _) =>
        notYet(s"top-level $kind binding for ${sym.name}")
      case _ => ()
    }

  /** Recognise the shapes of `def main()` that the backend supports:
    *
    *   - a bare `print(arg)` expression
    *   - a `TBlock` whose result is a `print(arg)` (val bindings + one
    *     final print)
    *   - a `TBlock` whose result is `TUnitLit` (statement-position
    *     `print(...)` calls interleaved with val bindings)
    *
    * Anything else surfaces as `notYet`.
    */
  private def emitMainBody(body: TExpr): Unit = unwrapEmptyBlock(body) match
    case TCall(TVarRef(p, _, _), List(arg), _, _) if p.name == "print" =>
      emitPrintCall(arg)
    case TBlock(items, TCall(TVarRef(p, _, _), List(arg), _, _), _, _) if p.name == "print" =>
      items.foreach(emitBlockItem)
      emitPrintCall(arg)
    case TBlock(items, TUnitLit(_), _, _) =>
      items.foreach(emitBlockItem)
    case TBlock(items, last, _, _) if last.tpe == TyUnit =>
      items.foreach(emitBlockItem)
      emitBlockItem(TBlockExpr(last))
    case other if other.tpe == TyUnit =>
      emitBlockItem(TBlockExpr(other))
    case other =>
      notYet(s"main body shape: ${other.getClass.getSimpleName}")

  private def unwrapEmptyBlock(e: TExpr): TExpr = e match
    case TBlock(Nil, r, _, _) => unwrapEmptyBlock(r)
    case other                => other

  /** Emit a call into the runtime print surface for a typed value:
    *   - scalar i64/f64 → `nex_print_{i64,f64}`
    *   - rank-1 tensor  → bufferize to memref, extract data pointer as
    *     i64, pass with length to `nex_print_array_1d_{i64,f64}`
    *   - rank-2 tensor  → same plus row + column constants to
    *     `nex_print_array_2d_{i64,f64}`
    *
    * The argument is evaluated through the visitor; its MLIR type
    * chooses the helper. Format matches `NexInterpreter.formatValue`.
    */
  private def emitPrintCall(arg: TExpr): Unit =
    val v = emitExpr(arg)
    v.ty match
      case MScalar(TyInteger) =>
        out.append(s"  func.call @nex_print_i64(${v.reg}) : (i64) -> ()\n")
      case MScalar(TyReal) =>
        out.append(s"  func.call @nex_print_f64(${v.reg}) : (f64) -> ()\n")
      case MScalar(TyBool) =>
        out.append(s"  func.call @nex_print_bool(${v.reg}) : (i1) -> ()\n")
      case MString =>
        out.append(s"  func.call @nex_print_str(${v.reg}) : (i64) -> ()\n")
        out.append(s"  func.call @nex_str_dec(${v.reg}) : (i64) -> ()\n")
      case t @ MTensor(elemT, List(_)) =>
        val lenReg = tensorDimAsI64(v.reg, t, 0)
        val ptrReg = emitTensorPointer(v.reg, t)
        val helper = arrayPrintHelper(elemT, rank = 1)
        out.append(s"  func.call @$helper($ptrReg, $lenReg) : (i64, i64) -> ()\n")
      case t @ MTensor(elemT, List(_, _)) =>
        val rowsReg = tensorDimAsI64(v.reg, t, 0)
        val colsReg = tensorDimAsI64(v.reg, t, 1)
        val ptrReg  = emitTensorPointer(v.reg, t)
        val helper = arrayPrintHelper(elemT, rank = 2)
        out.append(s"  func.call @$helper($ptrReg, $rowsReg, $colsReg) : (i64, i64, i64) -> ()\n")
      case other =>
        notYet(s"print of $other")

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
  private def emitLiteralBytes(text: String): (String, String) =
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
  private def emitStringLiteral(text: String): String =
    val (ptrReg, lenReg) = emitLiteralBytes(text)
    val descReg = fresh("strd")
    out.append(s"  $descReg = func.call @nex_str_lit_from_cstr($ptrReg, $lenReg) : (i64, i64) -> i64\n")
    descReg

  /** Concatenate two strings via `nex_str_concat`. The runtime returns
    * a fresh heap descriptor (refcount=1); both operands are
    * decremented since they are no longer referenced after this
    * expression. Mirrors the LLVM backend's concat-then-dec sequence.
    */
  private def emitStringConcat(lReg: String, rReg: String): MlirVal =
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
  private def emitValueToString(e: TExpr): String =
    val v = emitExpr(e)
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
      case other =>
        notYet(s"value-to-string for $other")

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
  private def emitFormattedValue(e: TExpr, spec: String): String =
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
  private def parseBinSpec(spec: String): (Int, Boolean, Boolean) =
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
  private def emitInterpString(parts: List[TInterpPart]): MlirVal =
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
  private def emitStringCompare(op: String, lReg: String, rReg: String): MlirVal =
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

  /** Bufferize a tensor to a memref and extract its aligned data
    * pointer as a raw `i64`. `bufferization.to_buffer` is the LLVM 22
    * spelling (formerly `bufferization.to_memref`); the rest of the
    * pipeline already lowers memref ops to LLVM dialect, so the
    * resulting `i64` is the actual data address at runtime.
    */
  private def emitTensorPointer(srcReg: String, ty: MTensor): String =
    val mrefT = memrefText(ty)
    val mReg  = fresh("mref")
    out.append(s"  $mReg = bufferization.to_buffer $srcReg : ${ty.text} to $mrefT\n")
    val idxReg = fresh("pidx")
    out.append(s"  $idxReg = memref.extract_aligned_pointer_as_index $mReg : $mrefT -> index\n")
    val ptrReg = fresh("pi")
    out.append(s"  $ptrReg = arith.index_castui $idxReg : index to i64\n")
    ptrReg

  /** Memref text for a tensor: same element + shape, just `memref<...>`
    * instead of `tensor<...>`.
    */
  private def memrefText(ty: MTensor): String =
    val dims =
      if ty.shape.isEmpty then ""
      else ty.shape.map(d => if d < 0 then "?" else d.toString).mkString("", "x", "x")
    s"memref<$dims${scalarText(ty.elem)}>"

  /** Recover the `i64` length of a tensor's `axis`-th dimension. For
    * static dims emits an `arith.constant`; for dynamic dims uses
    * `tensor.dim` and casts the resulting `index` to i64.
    */
  private def tensorDimAsI64(tReg: String, ty: MTensor, axis: Int): String =
    val d = ty.shape(axis)
    if d >= 0 then
      val r = fresh("dimc")
      out.append(s"  $r = arith.constant $d : i64\n")
      r
    else
      val axisR = fresh("daxis")
      out.append(s"  $axisR = arith.constant $axis : index\n")
      val dimR = fresh("dim")
      out.append(s"  $dimR = tensor.dim $tReg, $axisR : ${ty.text}\n")
      val i64R = fresh("dimi")
      out.append(s"  $i64R = arith.index_castui $dimR : index to i64\n")
      i64R

  /** Recover an `index`-typed dimension length for a tensor axis. Used
    * by index-wrap + bounds-trap sites that need an index extent
    * without bouncing through `i64`. Static dims emit a constant;
    * dynamic dims use `tensor.dim`.
    */
  private def tensorDimAsIndex(tReg: String, ty: MTensor, axis: Int): String =
    val d = ty.shape(axis)
    if d >= 0 then
      val r = fresh("dimx")
      out.append(s"  $r = arith.constant $d : index\n")
      r
    else
      val axisR = fresh("daxis")
      out.append(s"  $axisR = arith.constant $axis : index\n")
      val r = fresh("dim")
      out.append(s"  $r = tensor.dim $tReg, $axisR : ${ty.text}\n")
      r

  /** Emit `tensor.empty(...)` for either a static or dynamic shape.
    * `dynSizes` carries the SSA names of `index`-typed values for each
    * `-1` in `ty.shape`, in shape order.
    */
  private def emitTensorEmpty(ty: MTensor, dynSizes: List[String] = Nil): String =
    require(
      ty.shape.count(_ < 0) == dynSizes.size,
      s"tensor.empty: ${dynSizes.size} dyn sizes for shape ${ty.shape}",
    )
    val r = fresh("init")
    val args = if dynSizes.isEmpty then "" else dynSizes.mkString(", ")
    out.append(s"  $r = tensor.empty($args) : ${ty.text}\n")
    r

  /** Emit `tensor.empty(...)` shaped like `srcTy` but with element type
    * `outElem`. For each `-1` axis in the source, recovers the runtime
    * size via `tensor.dim` against `srcReg`. Used by the rank-N HOFs
    * (`map`, `broadcast`, element-wise) where the output shape mirrors
    * the input.
    */
  private def emitTensorEmptyLike(srcReg: String, srcTy: MTensor, outElem: Type): (String, MTensor) =
    val outTy = MTensor(outElem, srcTy.shape)
    val dynSizes = srcTy.shape.zipWithIndex.collect { case (d, axis) if d < 0 =>
      val axisR = fresh("axis")
      out.append(s"  $axisR = arith.constant $axis : index\n")
      val dimR = fresh("dim")
      out.append(s"  $dimR = tensor.dim $srcReg, $axisR : ${srcTy.text}\n")
      dimR
    }
    (emitTensorEmpty(outTy, dynSizes), outTy)

  private def arrayPrintHelper(elemT: Type, rank: Int): String = (elemT, rank) match
    case (TyInteger, 1) => "nex_print_array_1d_i64"
    case (TyReal,    1) => "nex_print_array_1d_f64"
    case (TyBool,    1) => "nex_print_array_1d_bool"
    case (TyInteger, 2) => "nex_print_array_2d_i64"
    case (TyReal,    2) => "nex_print_array_2d_f64"
    case (TyBool,    2) => "nex_print_array_2d_bool"
    case _              => notYet(s"array print helper for $elemT rank-$rank")

  /** The expression visitor. Every node that lowers must produce a
    * single SSA value with a known MLIR type; nodes that don't fit
    * (assignments, side effects, function calls other than the
    * recognised prelude ones) reject via `notYet`.
    */
  private def emitExpr(e: TExpr): MlirVal = e match
    case TIntLit(v, _, _) =>
      val r = fresh("ci")
      out.append(s"  $r = arith.constant $v : i64\n")
      MlirVal(r, MScalar(TyInteger))

    case TRealLit(v, _, _) =>
      val r = fresh("cr")
      out.append(s"  $r = arith.constant ${formatReal(v)} : f64\n")
      MlirVal(r, MScalar(TyReal))

    case TBoolLit(v, _, _) =>
      val r = fresh("cb")
      out.append(s"  $r = arith.constant ${if v then 1 else 0} : i1\n")
      MlirVal(r, MScalar(TyBool))

    case TStringLit(s, _, _) =>
      MlirVal(emitStringLiteral(s), MString)

    case TInterpStringLit(parts, _, _) =>
      emitInterpString(parts)

    case TUnaryOp("-", inner, _, _) =>
      foldLiteralNeg(inner) match
        case Some(lit) => emitExpr(lit)
        case None =>
          val v = emitExpr(inner)
          v.ty match
            case MScalar(TyInteger) =>
              val zero = fresh("z")
              val r    = fresh("neg")
              out.append(s"  $zero = arith.constant 0 : i64\n")
              out.append(s"  $r = arith.subi $zero, ${v.reg} : i64\n")
              MlirVal(r, MScalar(TyInteger))
            case MScalar(TyReal) =>
              val r = fresh("neg")
              out.append(s"  $r = arith.negf ${v.reg} : f64\n")
              MlirVal(r, MScalar(TyReal))
            case other =>
              notYet(s"unary minus on $other")

    case TUnaryOp("not", inner, _, _) =>
      val v = emitExpr(inner)
      v.ty match
        case MScalar(TyBool) =>
          val one = fresh("one")
          val r   = fresh("not")
          out.append(s"  $one = arith.constant 1 : i1\n")
          out.append(s"  $r = arith.xori ${v.reg}, $one : i1\n")
          MlirVal(r, MScalar(TyBool))
        case other =>
          notYet(s"`not` on $other")

    case TArrayLit(elems, _, TyArray(elemT, 1)) if elems.nonEmpty =>
      val vals = elems.map(emitExpr)
      vals.foreach { v =>
        v.ty match
          case MScalar(t) if t == elemT => ()
          case other                    => notYet(s"array element type mismatch: $other vs $elemT")
      }
      val ty = MTensor(elemT, List(elems.size))
      val r  = fresh("arr")
      out.append(s"  $r = tensor.from_elements ${vals.map(_.reg).mkString(", ")} : ${ty.text}\n")
      MlirVal(r, ty)

    case TArrayLit(rows, _, TyArray(elemT, 2)) if rows.nonEmpty =>
      // Each top-level element is itself a rank-1 array literal — the
      // elaborator enforces row uniformity, so the first row's length
      // is the column count. We flatten row-major into one
      // `tensor.from_elements` over `tensor<RxCx<elem>>`.
      val flatRows = rows.map {
        case TArrayLit(rowElems, _, TyArray(t, 1)) if t == elemT => rowElems
        case other => notYet(s"rank-2 row shape: ${other.getClass.getSimpleName}")
      }
      val cols  = flatRows.head.size
      flatRows.foreach { r =>
        if r.size != cols then notYet(s"ragged rank-2 literal — got ${r.size}, expected $cols")
      }
      val flat = flatRows.flatten
      val vals = flat.map(emitExpr)
      val ty   = MTensor(elemT, List(rows.size, cols))
      val r    = fresh("arr2")
      out.append(s"  $r = tensor.from_elements ${vals.map(_.reg).mkString(", ")} : ${ty.text}\n")
      MlirVal(r, ty)

    case TVarRef(s, _, _) if s.kind == SymKind.Prelude && s.name == "nan" =>
      // Compiler-built-in NaN constant. MLIR's `arith.constant` accepts
      // the IEEE 754 bit pattern as a hex literal on an `f64` attribute.
      val r = fresh("nan")
      out.append(s"  $r = arith.constant 0x7FF8000000000000 : f64\n")
      MlirVal(r, MScalar(TyReal))

    case TVarRef(s, _, _) if s.kind == SymKind.Prelude && s.name == "inf" =>
      val r = fresh("inf")
      out.append(s"  $r = arith.constant 0x7FF0000000000000 : f64\n")
      MlirVal(r, MScalar(TyReal))

    case TVarRef(sym, _, _) if varSlots.contains(sym.id) =>
      emitVarLoad(sym.id)

    case TVarRef(sym, _, _) if topLevelLiteralInits.contains(sym.id) =>
      // Re-emit the literal initialiser. Safe because we only
      // register literal-scalar bindings here, so each call produces
      // a fresh `arith.constant` op in the current region — which is
      // exactly what user-def bodies (executing outside `@main`)
      // need for prelude constants like `pi` / `e`.
      emitExpr(topLevelLiteralInits(sym.id))

    case TVarRef(sym, _, _) =>
      env.getOrElse(sym.id, notYet(s"unbound symbol ${sym.name}#${sym.id}"))

    case TBlock(items, result, _, _) =>
      items.foreach(emitBlockItem)
      emitExpr(result)

    case TElementWise(op, lhs, rhs, _, _) if isComparisonOp(op) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty) match
        case (lt: MTensor, rt: MTensor) if lt.shape == rt.shape =>
          emitElementWiseComparison(op, lv, rv, lt, rt)
        case (lt, rt) =>
          notYet(s"element-wise comparison $op on $lt and $rt")

    case TElementWise(op, lhs, rhs, _, _) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty) match
        case (lt: MTensor, rt: MTensor) if lt == rt =>
          emitElementWiseBinop(op, lv, rv, lt)
        case (lt: MTensor, rt: MTensor) if lt.shape == rt.shape =>
          emitElementWiseMixed(op, lv, rv, lt, rt)
        case (lt, rt) =>
          notYet(s"element-wise $op on $lt and $rt")

    case TBroadcast(scalar, arr, op, scalarFirst, _, _) if isComparisonOp(op) =>
      val sv = emitExpr(scalar)
      val av = emitExpr(arr)
      (sv.ty, av.ty) match
        case (MScalar(st), t @ MTensor(et, _)) =>
          emitBroadcastComparison(op, sv, av, t, scalarFirst, st, et)
        case (sty, aty) =>
          notYet(s"broadcast comparison $op on $sty and $aty")

    case TBroadcast(scalar, arr, op, scalarFirst, _, _) =>
      val sv = emitExpr(scalar)
      val av = emitExpr(arr)
      (sv.ty, av.ty) match
        case (MScalar(st), t @ MTensor(et, _)) if st == et =>
          emitBroadcast(op, sv, av, t, scalarFirst)
        case (MScalar(st), t @ MTensor(et, _)) =>
          emitBroadcastMixed(op, sv, av, t, scalarFirst, st, et)
        case (sty, aty) =>
          notYet(s"broadcast $op on $sty and $aty")

    case TBinOp("..", loE, hiE, _, ty) if isRangeArrayType(ty) =>
      emitRangeDispatch(loE, hiE, inclusive = false)

    case TBinOp("..=", loE, hiE, _, ty) if isRangeArrayType(ty) =>
      emitRangeDispatch(loE, hiE, inclusive = true)

    case TBinOp("^", lhs, rhs, _, resultTy) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      emitScalarPower(lv, rv, resultTy)

    case TBinOp("and", lhs, rhs, _, _) => emitShortCircuit(lhs, rhs, isAnd = true)
    case TBinOp("or",  lhs, rhs, _, _) => emitShortCircuit(lhs, rhs, isAnd = false)

    case TBinOp("+", lhs, rhs, _, TyString) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty) match
        case (MString, MString) => emitStringConcat(lv.reg, rv.reg)
        case (lt, rt)           => notYet(s"`+` on string with $lt / $rt")

    case TBinOp(op, lhs, rhs, _, _) if isComparisonOp(op) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty) match
        case (MString, MString) => emitStringCompare(op, lv.reg, rv.reg)
        case _                  => emitComparison(op, lv, rv)

    case TBinOp(op, lhs, rhs, _, resultTy) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty, resultTy) match
        case (MScalar(TyInteger), MScalar(TyInteger), TyReal) =>
          // `int op int` whose elaborated result is real — e.g. `7 / 2`.
          // Lift both operands to f64 before the float op.
          emitScalarBinop(op, promoteIntToReal(lv), promoteIntToReal(rv), MScalar(TyReal))
        case (MScalar(lt), MScalar(rt), _) if lt == rt =>
          emitScalarBinop(op, lv, rv, MScalar(lt))
        case (MScalar(TyInteger), MScalar(TyReal), _) =>
          emitScalarBinop(op, promoteIntToReal(lv), rv, MScalar(TyReal))
        case (MScalar(TyReal), MScalar(TyInteger), _) =>
          emitScalarBinop(op, lv, promoteIntToReal(rv), MScalar(TyReal))
        case (lt, rt, _) =>
          notYet(s"scalar binop $op on $lt and $rt")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "sum" =>
      val av = emitExpr(arr)
      av.ty match
        case t: MTensor => emitSumReduce(av.reg, t)
        case other      => notYet(s"sum over $other")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "product" =>
      val av = emitExpr(arr)
      av.ty match
        case t: MTensor => emitProductReduce(av.reg, t)
        case other      => notYet(s"product over $other")

    case TCall(TVarRef(s, _, _), List(arr, lam: TLambda), _, tpe)
        if s.kind == SymKind.Prelude && s.name == "map" && lam.params.size == 1 =>
      val av = emitExpr(arr)
      val outElem = tpe match
        case TyArray(e, _) => e
        case other         => notYet(s"map returns non-array $other")
      av.ty match
        case t: MTensor if t.shape.nonEmpty => emitMapInlineLambda(av, t, lam, outElem)
        case other                          => notYet(s"map over $other")

    case TCall(TVarRef(s, _, _), List(arr, init, lam: TLambda), _, _)
        if s.kind == SymKind.Prelude && s.name == "reduce" && lam.params.size == 2 =>
      val av = emitExpr(arr)
      val iv = emitExpr(init)
      av.ty match
        case t: MTensor if t.shape.nonEmpty => emitReduceInlineLambda(av, t, iv, lam)
        case other                          => notYet(s"reduce over $other")

    case TCall(TVarRef(s, _, _), List(arr, lam: TLambda), _, _)
        if s.kind == SymKind.Prelude && s.name == "filter" && lam.params.size == 1 =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) => emitFilterInlineLambda(av, t, lam)
        case other                   => notYet(s"filter over $other")

    case TCall(TVarRef(s, _, _), List(arr, lam: TLambda), _, tpe)
        if s.kind == SymKind.Prelude && s.name == "flatMap" && lam.params.size == 1 =>
      val av = emitExpr(arr)
      val outElem = tpe match
        case TyArray(e, _) => e
        case other         => notYet(s"flatMap returns non-array $other")
      av.ty match
        case t @ MTensor(_, List(_)) => emitFlatMapInlineLambda(av, t, lam, outElem)
        case other                   => notYet(s"flatMap over $other")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && (s.name == "min" || s.name == "max") =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) => emitMinMaxReduce(s.name, av.reg, t)
        case other                   => notYet(s"${s.name} over $other")

    case TCall(TVarRef(s, _, _), List(a, b), _, _)
        if s.kind == SymKind.Prelude && (s.name == "min" || s.name == "max") =>
      emitScalarMinMax(s.name, emitExpr(a), emitExpr(b))

    case TCall(TVarRef(s, _, _), List(x), _, _)
        if s.kind == SymKind.Prelude && s.name == "abs" =>
      emitScalarAbs(emitExpr(x))

    case TCall(TVarRef(s, _, _), List(loE, hiE), _, _)
        if s.kind == SymKind.Prelude && s.name == "range" =>
      emitRangeDispatch(loE, hiE, inclusive = false)

    case TCall(TVarRef(s, _, _), List(nE), _, _)
        if s.kind == SymKind.Prelude && (s.name == "zeros" || s.name == "ones") =>
      emitConstFillDispatch(s.name, nE)

    case TCall(TVarRef(s, _, _), List(loE, hiE, nE), _, _)
        if s.kind == SymKind.Prelude && s.name == "linspace" =>
      emitLinspaceDispatch(loE, hiE, nE)

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "transpose" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(rows, cols)) => emitTranspose(av.reg, t, rows, cols)
        case other                            => notYet(s"transpose on $other")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "flatten" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_))           => emitFlatten1D(av)
        case t @ MTensor(_, List(rows, cols))  => emitFlatten2D(av, t, rows, cols)
        case other                             => notYet(s"flatten on $other")

    case TCall(TVarRef(s, _, _), List(arr, TIntLit(rows, _, _), TIntLit(cols, _, _)), _, _)
        if s.kind == SymKind.Prelude && s.name == "reshape" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) => emitReshape(av, t, rows.toInt, cols.toInt)
        case other                   => notYet(s"reshape on $other")

    case TCall(TVarRef(s, _, _), List(arr, TIntLit(axis, _, _)), _, _)
        if s.kind == SymKind.Prelude && s.name == "sum_axis" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_, _)) => emitSumAxis(av, t, axis.toInt)
        case other                      => notYet(s"sum_axis on $other")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "diag" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(n)) => emitDiag(av, t, n)
        case other                   => notYet(s"diag on $other")

    case TCall(TVarRef(s, _, _), List(TIntLit(n, _, _)), _, _)
        if s.kind == SymKind.Prelude && s.name == "identity" =>
      emitIdentity(n.toInt)

    case TCall(TVarRef(s, _, _), args, _, _) if libmIntrinsics.contains(s.id) =>
      emitLibmCall(libmIntrinsics(s.id), args.map(emitExpr))

    case TCall(TVarRef(s, _, _), args, _, _) if userDefs.contains(s.id) =>
      val (name, paramTys, retTyOpt) = userDefs(s.id)
      retTyOpt match
        case Some(retTy) =>
          emitUserDefCall(name, paramTys, retTyOpt, args, s.name)
        case None =>
          // Unit-returning user defs surface only at statement
          // position (see [[emitBlockItem]]); reaching here means
          // someone is trying to use a `def foo() = print(...)` as
          // a value.
          notYet(s"unit-returning user def `${s.name}` reached value position")

    case TMatMul(lhs, rhs, _, _) =>
      emitMatMul(emitExpr(lhs), emitExpr(rhs))

    case TCall(TVarRef(s, _, _), List(lhs, rhs), _, _)
        if s.kind == SymKind.Prelude && s.name == "matmul" =>
      emitMatMul(emitExpr(lhs), emitExpr(rhs))

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "length" =>
      // `arr.length()` desugars to `length(arr)` in elaboration.
      // Result is the size of the outermost dimension — for rank-1
      // that's the element count, for rank-2 the row count. The
      // shape is statically known at codegen, so this lowers to a
      // single integer constant. The receiver is still evaluated
      // for side-effect parity; clang's DCE removes the unused
      // tensor at -O1.
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, shape) if shape.nonEmpty =>
          val r = tensorDimAsI64(av.reg, t, 0)
          MlirVal(r, MScalar(TyInteger))
        case other =>
          notYet(s"length on $other")

    case TIndex(arr, List(idx), _, _) =>
      val av = emitExpr(arr)
      val iv = emitExpr(idx)
      (av.ty, iv.ty) match
        case (t @ MTensor(et, List(_)), MScalar(TyInteger)) =>
          val len = tensorDimAsIndex(av.reg, t, 0)
          val rawR = fresh("idxraw")
          out.append(s"  $rawR = arith.index_cast ${iv.reg} : i64 to index\n")
          val idxR = wrapNegBound(rawR, len)
          val c0Idx = fresh("c0")
          out.append(s"  $c0Idx = arith.constant 0 : index\n")
          emitAxisIndexTrap(idxR, len, c0Idx)
          val r = fresh("elt")
          out.append(s"  $r = tensor.extract ${av.reg}[$idxR] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (t @ MTensor(et, List(_, cols)), MScalar(TyInteger)) =>
          // `m[i]` on a rank-2 receiver: return the i-th row as a fresh
          // rank-1 tensor. The row axis collapses; the column axis is
          // copied wholesale. `tensor.extract_slice`'s rank-reducing form
          // handles the rank drop when a static size-1 axis is present.
          val rows = tensorDimAsIndex(av.reg, t, 0)
          val rawR = fresh("ridxraw")
          out.append(s"  $rawR = arith.index_cast ${iv.reg} : i64 to index\n")
          val idxR = wrapNegBound(rawR, rows)
          val c0Idx = fresh("c0")
          out.append(s"  $c0Idx = arith.constant 0 : index\n")
          emitAxisIndexTrap(idxR, rows, c0Idx)
          val outTy = MTensor(et, List(cols))
          val r     = fresh("row")
          out.append(
            s"  $r = tensor.extract_slice ${av.reg}[$idxR, 0] [1, $cols] [1, 1] : ${t.text} to ${outTy.text}\n",
          )
          MlirVal(r, outTy)
        case (aty, ity) =>
          notYet(s"single-index on $aty with $ity")

    case TIndex(arr, List(rowIdx, colIdx), _, _) =>
      val av = emitExpr(arr)
      val rv = emitExpr(rowIdx)
      val cv = emitExpr(colIdx)
      (av.ty, rv.ty, cv.ty) match
        case (t @ MTensor(et, List(_, _)), MScalar(TyInteger), MScalar(TyInteger)) =>
          val rows = tensorDimAsIndex(av.reg, t, 0)
          val cols = tensorDimAsIndex(av.reg, t, 1)
          val c0Idx = fresh("c0")
          out.append(s"  $c0Idx = arith.constant 0 : index\n")
          val rRaw = fresh("rraw")
          out.append(s"  $rRaw = arith.index_cast ${rv.reg} : i64 to index\n")
          val rR = wrapNegBound(rRaw, rows)
          emitAxisIndexTrap(rR, rows, c0Idx)
          val cRaw = fresh("craw")
          out.append(s"  $cRaw = arith.index_cast ${cv.reg} : i64 to index\n")
          val cR = wrapNegBound(cRaw, cols)
          emitAxisIndexTrap(cR, cols, c0Idx)
          val r = fresh("elt")
          out.append(s"  $r = tensor.extract ${av.reg}[$rR, $cR] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (aty, rty, cty) =>
          notYet(s"rank-2 index on $aty with $rty, $cty")

    case TIf(cond, thenB, Some(elseB), _, tpe) if isMlirScalarType(tpe) =>
      emitIfExpr(cond, thenB, elseB, MScalar(tpe))

    case TFusedLoop(loopVar, length, body, None, _, tpe) =>
      val n = staticLength(length).getOrElse(notYet(s"fused loop with non-static length"))
      val elemT = tpe match
        case TyArray(e, _) => e
        case other         => notYet(s"fused loop returns non-array type $other")
      emitFusedLoop1D(loopVar, n, elemT, body)

    case TFlatIndex(arr, idx, _, _) =>
      val av = emitExpr(arr)
      val iv = emitExpr(idx)
      (av.ty, iv.ty) match
        case (t @ MTensor(et, List(_)), MScalar(TyInteger)) =>
          val idxR = fresh("fidx")
          out.append(s"  $idxR = arith.index_cast ${iv.reg} : i64 to index\n")
          val r = fresh("felt")
          out.append(s"  $r = tensor.extract ${av.reg}[$idxR] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (t @ MTensor(et, List(_, cols)), MScalar(TyInteger)) =>
          val flatI = fresh("fidx")
          out.append(s"  $flatI = arith.index_cast ${iv.reg} : i64 to index\n")
          val colsI = fresh("fcols")
          out.append(s"  $colsI = arith.constant $cols : index\n")
          val iI    = fresh("fi")
          out.append(s"  $iI = arith.divui $flatI, $colsI : index\n")
          val jI    = fresh("fj")
          out.append(s"  $jI = arith.remui $flatI, $colsI : index\n")
          val r = fresh("felt")
          out.append(s"  $r = tensor.extract ${av.reg}[$iI, $jI] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (aty, ity) =>
          notYet(s"flat-index on $aty with $ity")

    case TSlice(arr, Some(TIntLit(lo, _, _)), Some(TIntLit(hi, _, _)), inclusive, None, _, _) =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) =>
          val end      = if inclusive then hi.toInt + 1 else hi.toInt
          val sliceLen = math.max(0, end - lo.toInt)
          emitRank1Slice(av, t, lo.toInt, sliceLen)
        case other =>
          notYet(s"rank-1 slice on $other")

    case TSlice(arr, loE, hiE, inclusive, strideE, _, _) =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) =>
          emitRank1SliceDynamic(av, t, loE, hiE, inclusive, strideE)
        case other =>
          notYet(s"rank-1 slice on $other")

    case TSlice2(arr, rowAx, colAx, _, _) =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(rows, cols)) =>
          if rows >= 0 && cols >= 0 && isStaticAxis(rowAx) && isStaticAxis(colAx) then
            val rowSpec = axisToSlice(rowAx, rows)
            val colSpec = axisToSlice(colAx, cols)
            emitRank2Slice(av, t, rowSpec, colSpec)
          else
            emitRank2SliceDynamic(av, t, rowAx, colAx)
        case other =>
          notYet(s"rank-2 slice on $other")

    case TIntrinsic(opId, _, _) =>
      notYet(s"intrinsic `$opId` (MLIR backend has no Stage-0 intrinsic dispatch yet)")

    case other =>
      notYet(s"expression: ${other.getClass.getSimpleName}")

  private def emitBlockItem(it: TBlockItem): Unit = it match
    case TBlockBinding(sym, BindingKind.Val, value) =>
      env(sym.id) = emitExpr(value)
    case TBlockBinding(sym, BindingKind.Var, value) =>
      val v = emitExpr(value)
      v.ty match
        case s: MScalar => allocVarSlot(sym, v.reg, s)
        case other      => notYet(s"var binding for ${sym.name} of type $other (only scalars supported)")
    case TBlockBinding(sym, kind, _) =>
      notYet(s"$kind binding for ${sym.name}")
    case TBlockExpr(TCall(TVarRef(p, _, _), List(arg), _, _)) if p.name == "print" =>
      emitPrintCall(arg)
    case TBlockExpr(TFor(loopVars, TBinOp("..", lo, hi, _, _), body, _, _)) =>
      emitForRange(loopVars, lo, hi, inclusive = false, body)
    case TBlockExpr(TFor(loopVars, TBinOp("..=", lo, hi, _, _), body, _, _)) =>
      emitForRange(loopVars, lo, hi, inclusive = true, body)
    case TBlockExpr(TFor(loopVars, iter, body, _, _)) =>
      val av = emitExpr(iter)
      av.ty match
        case t @ MTensor(_, List(_)) => emitForArray(loopVars, av, t, body)
        case other                   => notYet(s"for over $other")
    case TBlockExpr(TWhile(cond, body, _, _)) =>
      emitWhile(cond, body)
    case TBlockExpr(TAssign(TVarRef(sym, _, _), value, _, _)) if varSlots.contains(sym.id) =>
      emitVarStore(sym.id, emitExpr(value))
    case TBlockExpr(TCall(TVarRef(s, _, _), args, _, _)) if userDefs.contains(s.id) =>
      val (name, paramTys, retTyOpt) = userDefs(s.id)
      val _ = emitUserDefCall(name, paramTys, retTyOpt, args, s.name)
    case TBlockExpr(TIf(cond, thenB, elseB, _, _)) =>
      emitIfStatement(cond, thenB, elseB)
    case TBlockExpr(other) =>
      notYet(s"statement-position expression: ${other.getClass.getSimpleName}")

  /** Lower a statement-position `if cond then thenB else? elseB` to a
    * no-result `scf.if`. Each branch is emitted via [[emitForBody]]
    * (which already handles `TBlock` + value-discard semantics) and
    * terminated with a bare `scf.yield`. If the else branch is absent,
    * we emit `scf.if %c { ... }` (no else region), which is the
    * dialect's "single-side" form.
    */
  private def emitIfStatement(cond: TExpr, thenB: TExpr, elseB: Option[TExpr]): Unit =
    val cv = emitExpr(cond)
    cv.ty match
      case MScalar(TyBool) =>
      case other          => notYet(s"if-statement condition of type $other")
    elseB match
      case Some(eB) =>
        out.append(s"  scf.if ${cv.reg} {\n")
        emitForBody(thenB)
        out.append("    scf.yield\n")
        out.append("  } else {\n")
        emitForBody(eB)
        out.append("    scf.yield\n")
        out.append("  }\n")
      case None =>
        out.append(s"  scf.if ${cv.reg} {\n")
        emitForBody(thenB)
        out.append("    scf.yield\n")
        out.append("  }\n")

  /** Statement-form `for i in lo..hi do body` over an integer range.
    * Lowers to `scf.for %iv = %lo to %hi step %c1 { … }`. The loop
    * var is provided as MLIR `index` type; we cast to i64 and bind
    * to the symbol id so the body's references resolve as scalars.
    * Inclusive ranges (`..=`) bump the upper bound by one via
    * `arith.addi` (scf.for is exclusive on its upper bound).
    *
    * Bounds are arbitrary `TyInteger` expressions — literals, folded
    * unary minus, var refs, anything that emits to i64. We index-cast
    * each side to `index` after emission.
    *
    * Body emission delegates to [[emitForBody]], which handles a
    * `TBlock` body (multiple statements) the same way it handles
    * `def main()`'s top-level block — items go through
    * `emitBlockItem`, so nested prints / nested for-loops compose
    * naturally.
    */
  private def emitForRange(loopVars: List[Symbol], lo: TExpr, hi: TExpr, inclusive: Boolean, body: TExpr): Unit =
    if loopVars.size != 1 then
      notYet(s"for over range with ${loopVars.size}-way destructuring")
      return
    val loopVar = loopVars.head
    val loV     = emitExpr(lo)
    val hiV     = emitExpr(hi)
    val loC     = fresh("flo")
    out.append(s"  $loC = arith.index_cast ${loV.reg} : i64 to index\n")
    val hiIdx   = fresh("fhi_idx")
    out.append(s"  $hiIdx = arith.index_cast ${hiV.reg} : i64 to index\n")
    val hiC     =
      if inclusive then
        val one  = fresh("fone")
        val bump = fresh("fhi")
        out.append(s"  $one = arith.constant 1 : index\n")
        out.append(s"  $bump = arith.addi $hiIdx, $one : index\n")
        bump
      else hiIdx
    val stepC   = fresh("fst")
    out.append(s"  $stepC = arith.constant 1 : index\n")
    val ivName  = fresh("iv")
    out.append(s"  scf.for $ivName = $loC to $hiC step $stepC {\n")
    val ivI64   = fresh("ivi")
    out.append(s"    $ivI64 = arith.index_cast $ivName : index to i64\n")
    val prev    = env.get(loopVar.id)
    env(loopVar.id) = MlirVal(ivI64, MScalar(TyInteger))
    emitForBody(body)
    prev match
      case Some(v) => env(loopVar.id) = v
      case None    => env.remove(loopVar.id)
    out.append("  }\n")

  /** True when an axis spec can be fully resolved at compile time:
    * `TAxisAll`, a literal `TAxisIndex`, or a `TAxisRange` whose
    * bounds are literals and stride is omitted. Used as the dispatch
    * gate between the static-size [[emitRank2Slice]] fast path and
    * the dynamic-size [[emitRank2SliceDynamic]] path.
    */
  private def isStaticAxis(spec: TAxisSpec): Boolean = spec match
    case TAxisAll                                                                       => true
    case TAxisIndex(TIntLit(_, _, _))                                                   => true
    case TAxisRange(Some(TIntLit(_, _, _)), Some(TIntLit(_, _, _)), _, None)            => true
    case TAxisRange(None, Some(TIntLit(_, _, _)), _, None)                              => true
    case _                                                                              => false

  /** Resolved spec for one axis of a rank-2 slice. `offset` and `size`
    * are the corresponding entries in the `tensor.extract_slice`
    * offsets/sizes lists; `collapsed` is true when this axis was a
    * single integer index (in which case the result tensor drops one
    * rank — MLIR's `extract_slice` handles this via its
    * rank-reducing form when the static size is 1).
    */
  private case class AxisSlice(offset: Int, size: Int, collapsed: Boolean)

  /** Resolve a `TAxisSpec` to its concrete (offset, size, collapsed)
    * triple given the corresponding source dimension extent. Only
    * literal-bound axes are accepted — anything else surfaces as
    * `notYet`.
    */
  private def axisToSlice(spec: TAxisSpec, dim: Int): AxisSlice = spec match
    case TAxisAll =>
      AxisSlice(offset = 0, size = dim, collapsed = false)
    case TAxisIndex(TIntLit(v, _, _)) =>
      AxisSlice(offset = v.toInt, size = 1, collapsed = true)
    case TAxisRange(Some(TIntLit(lo, _, _)), Some(TIntLit(hi, _, _)), inclusive, None) =>
      val end = if inclusive then hi.toInt + 1 else hi.toInt
      AxisSlice(offset = lo.toInt, size = math.max(0, end - lo.toInt), collapsed = false)
    case TAxisRange(None, Some(TIntLit(hi, _, _)), inclusive, None) =>
      val end = if inclusive then hi.toInt + 1 else hi.toInt
      AxisSlice(offset = 0, size = math.max(0, end), collapsed = false)
    case other =>
      notYet(s"rank-2 slice axis spec: ${other.getClass.getSimpleName} with non-literal bound")

  /** Rank-2 `tensor.extract_slice` with literal offset/size on both
    * axes. Output rank is `2 - (number of collapsed axes)`. When
    * both axes are collapsed the result is a scalar — but that
    * shape isn't reachable here because the elaborator emits a
    * `TIndex` (not a `TSlice2`) for the two-integer-index form.
    */
  private def emitRank2Slice(av: MlirVal, srcTy: MTensor, rowSpec: AxisSlice, colSpec: AxisSlice): MlirVal =
    val outShape = List(rowSpec, colSpec).collect {
      case s if !s.collapsed => s.size
    }
    val outTy = MTensor(srcTy.elem, outShape)
    if outShape.contains(0) then
      val r = fresh("emp")
      out.append(s"  $r = tensor.empty() : ${outTy.text}\n")
      return MlirVal(r, outTy)
    val r = fresh("sl2")
    out.append(
      s"  $r = tensor.extract_slice ${av.reg}[${rowSpec.offset}, ${colSpec.offset}] [${rowSpec.size}, ${colSpec.size}] [1, 1] : ${srcTy.text} to ${outTy.text}\n",
    )
    MlirVal(r, outTy)

  /** Resolved axis for the dynamic-rank-2 path. `offset`, `size`, and
    * `stride` are SSA `index` values (or literal strings for the
    * trivial 0 / 1 / size-1 cases). `collapsed` carries the
    * rank-reducing flag for `TAxisIndex`. The result type's
    * corresponding entry is built from these in
    * [[emitRank2SliceDynamic]] — collapsed axes drop entirely; the
    * remaining axes are `?` since their size flows in via the SSA
    * operand.
    */
  private case class AxisSliceD(
      offset:    String,
      size:      String,
      stride:    String,
      collapsed: Boolean,
  )

  /** Resolve a `TAxisSpec` to its concrete operand triple for the
    * rank-2 dynamic-bound slice path. Mirrors
    * [[emitRank1SliceDynamic]] for one axis at a time; the source
    * dimension may itself be dynamic in which case the default `hi`
    * comes from `tensor.dim`.
    */
  private def emitAxisSliceDynamic(
      spec:     TAxisSpec,
      srcTy:    MTensor,
      srcReg:   String,
      axisIdx:  Int,
      c0Idx:    String,
      c1Idx:    String,
  ): AxisSliceD =
    val srcDim = srcTy.shape(axisIdx)
    def srcDimIdx(): String =
      if srcDim >= 0 then
        val r = fresh("sdim")
        out.append(s"  $r = arith.constant $srcDim : index\n")
        r
      else
        val ax = fresh("daxis")
        out.append(s"  $ax = arith.constant $axisIdx : index\n")
        val r = fresh("sdim")
        out.append(s"  $r = tensor.dim $srcReg, $ax : ${srcTy.text}\n")
        r

    spec match
      case TAxisAll =>
        AxisSliceD(c0Idx, srcDimIdx(), c1Idx, collapsed = false)

      case TAxisIndex(e) =>
        val total = srcDimIdx()
        val v = emitExpr(e)
        val rawR = fresh("airaw")
        out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
        val offsetR = wrapNegBound(rawR, total)
        emitAxisIndexTrap(offsetR, total, c0Idx)
        AxisSliceD(offsetR, "1", "1", collapsed = true)

      case TAxisRange(loE, hiE, inclusive, strideE) =>
        val total = srcDimIdx()
        val loIdx = loE match
          case Some(e) =>
            val v = emitExpr(e)
            val rawR = fresh("loraw")
            out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
            wrapNegBound(rawR, total)
          case None => c0Idx
        val hiIdx = hiE match
          case Some(e) =>
            val v = emitExpr(e)
            val rawR = fresh("hiraw")
            out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
            val wrapped = wrapNegBound(rawR, total)
            if inclusive then
              val r = fresh("hix1")
              out.append(s"  $r = arith.addi $wrapped, $c1Idx : index\n")
              r
            else wrapped
          case None =>
            if inclusive then
              val r = fresh("hix1")
              out.append(s"  $r = arith.addi $total, $c1Idx : index\n")
              r
            else total
        val strideIdx = strideE match
          case Some(e) =>
            val v = emitExpr(e)
            val r = fresh("stx")
            out.append(s"  $r = arith.index_cast ${v.reg} : i64 to index\n")
            r
          case None => c1Idx
        emitSliceBoundsTrap(loIdx, hiIdx, total, strideIdx, c0Idx, strideE.isDefined)
        val rawSpan = fresh("span")
        out.append(s"  $rawSpan = arith.subi $hiIdx, $loIdx : index\n")
        val spanC = fresh("spanc")
        out.append(s"  $spanC = arith.maxsi $rawSpan, $c0Idx : index\n")
        val strideM1 = fresh("stm1")
        out.append(s"  $strideM1 = arith.subi $strideIdx, $c1Idx : index\n")
        val numer = fresh("num")
        out.append(s"  $numer = arith.addi $spanC, $strideM1 : index\n")
        val sz = fresh("asz")
        out.append(s"  $sz = arith.divui $numer, $strideIdx : index\n")
        AxisSliceD(loIdx, sz, strideIdx, collapsed = false)

  /** Rank-2 slice with runtime axes — runtime bounds, open-ended
    * forms, and stride all flow through here. Each axis resolves to
    * an SSA `(offset, size, stride)` triple via
    * [[emitAxisSliceDynamic]]; the result type drops collapsed axes
    * and uses `?` for the rest. The static-bound fast path in
    * [[emitRank2Slice]] still handles the all-literal case for
    * tighter IR.
    */
  private def emitRank2SliceDynamic(
      av:    MlirVal,
      srcTy: MTensor,
      rowAx: TAxisSpec,
      colAx: TAxisSpec,
  ): MlirVal =
    val c0Idx = fresh("c0")
    out.append(s"  $c0Idx = arith.constant 0 : index\n")
    val c1Idx = fresh("c1")
    out.append(s"  $c1Idx = arith.constant 1 : index\n")

    val rowAxis = emitAxisSliceDynamic(rowAx, srcTy, av.reg, 0, c0Idx, c1Idx)
    val colAxis = emitAxisSliceDynamic(colAx, srcTy, av.reg, 1, c0Idx, c1Idx)

    val outShape = List(rowAxis, colAxis).filterNot(_.collapsed).map(_ => -1)
    val outTy    = MTensor(srcTy.elem, outShape)

    val r = fresh("dsl2")
    out.append(
      s"  $r = tensor.extract_slice ${av.reg}" +
        s"[${rowAxis.offset}, ${colAxis.offset}] " +
        s"[${rowAxis.size}, ${colAxis.size}] " +
        s"[${rowAxis.stride}, ${colAxis.stride}] : ${srcTy.text} to ${outTy.text}\n",
    )
    MlirVal(r, outTy)

  /** Rank-1 slice `a[lo..hi]` / `a[lo..=hi]` with literal bounds.
    * Lowers to `tensor.extract_slice` with a static offset / size /
    * unit stride, which produces a freshly-allocated tensor of the
    * sliced length. Empty slices (computed length <= 0) collapse to
    * `tensor.empty() : tensor<0xT>` — printing walks zero elements
    * and emits `[]\n`.
    */
  private def emitRank1Slice(av: MlirVal, srcTy: MTensor, offset: Int, len: Int): MlirVal =
    val outTy = MTensor(srcTy.elem, List(len))
    if len == 0 then
      val r = fresh("emp")
      out.append(s"  $r = tensor.empty() : ${outTy.text}\n")
      return MlirVal(r, outTy)
    val r = fresh("sl")
    out.append(s"  $r = tensor.extract_slice ${av.reg}[$offset] [$len] [1] : ${srcTy.text} to ${outTy.text}\n")
    MlirVal(r, outTy)

  /** Rank-1 slice with any combination of open-ended bounds, runtime
    * bounds, and stride. Always produces `tensor<?xT>`:
    *
    *   - Open `lo` (`a[..hi]`) defaults to 0; open `hi` (`a[lo..]`) to
    *     the source length recovered via `tensor.dim`.
    *   - `[..= ]` (inclusive) bumps the upper bound by one.
    *   - Stride defaults to 1; runtime stride uses ceil-divide for the
    *     output length: `(max(0, span) + stride - 1) / stride`.
    *
    * Negative-bound wrap (spec §4.14): `lo`/`hi` < 0 maps to
    * `bound + len` before the bounds check, so `a[-3..len]` selects
    * the last three elements. The check itself trips `lo < 0` after
    * wrap (over-negative input) or `hi > len`, routing through
    * `nex_trap_slice_oob`. Stride <= 0 also traps.
    */
  private def emitRank1SliceDynamic(
      av:        MlirVal,
      srcTy:     MTensor,
      loE:       Option[TExpr],
      hiE:       Option[TExpr],
      inclusive: Boolean,
      strideE:   Option[TExpr],
  ): MlirVal =
    val outTy = MTensor(srcTy.elem, List(-1))

    val c0Idx = fresh("c0")
    out.append(s"  $c0Idx = arith.constant 0 : index\n")
    val c1Idx = fresh("c1")
    out.append(s"  $c1Idx = arith.constant 1 : index\n")

    val srcLenIdx =
      if srcTy.shape.head >= 0 then
        val r = fresh("srclen")
        out.append(s"  $r = arith.constant ${srcTy.shape.head} : index\n")
        r
      else
        val axisR = fresh("axis")
        out.append(s"  $axisR = arith.constant 0 : index\n")
        val r = fresh("srclen")
        out.append(s"  $r = tensor.dim ${av.reg}, $axisR : ${srcTy.text}\n")
        r

    val loIdx = loE match
      case Some(e) =>
        val v = emitExpr(e)
        val rawR = fresh("loraw")
        out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
        wrapNegBound(rawR, srcLenIdx)
      case None => c0Idx

    val hiIdx = hiE match
      case Some(e) =>
        val v = emitExpr(e)
        val rawR = fresh("hiraw")
        out.append(s"  $rawR = arith.index_cast ${v.reg} : i64 to index\n")
        val wrapped = wrapNegBound(rawR, srcLenIdx)
        if inclusive then
          val r = fresh("hix1")
          out.append(s"  $r = arith.addi $wrapped, $c1Idx : index\n")
          r
        else wrapped
      case None =>
        if inclusive then
          val r = fresh("hix1")
          out.append(s"  $r = arith.addi $srcLenIdx, $c1Idx : index\n")
          r
        else srcLenIdx

    val strideIdx = strideE match
      case Some(e) =>
        val v = emitExpr(e)
        val r = fresh("stx")
        out.append(s"  $r = arith.index_cast ${v.reg} : i64 to index\n")
        r
      case None => c1Idx

    emitSliceBoundsTrap(loIdx, hiIdx, srcLenIdx, strideIdx, c0Idx, strideE.isDefined)

    val rawSpan = fresh("span")
    out.append(s"  $rawSpan = arith.subi $hiIdx, $loIdx : index\n")
    val spanClamped = fresh("spanc")
    out.append(s"  $spanClamped = arith.maxsi $rawSpan, $c0Idx : index\n")
    val strideM1 = fresh("stm1")
    out.append(s"  $strideM1 = arith.subi $strideIdx, $c1Idx : index\n")
    val numerator = fresh("num")
    out.append(s"  $numerator = arith.addi $spanClamped, $strideM1 : index\n")
    val sliceLen = fresh("slen")
    out.append(s"  $sliceLen = arith.divui $numerator, $strideIdx : index\n")

    val r = fresh("dsl")
    out.append(
      s"  $r = tensor.extract_slice ${av.reg}[$loIdx] [$sliceLen] [$strideIdx] : ${srcTy.text} to ${outTy.text}\n",
    )
    MlirVal(r, outTy)

  /** Negative-bound wrap (spec §4.14): if `raw < 0` return `raw + extent`,
    * otherwise `raw`. Mirrors the LLVM backend's `wrapNegBound`. The
    * caller's bounds check still runs on the result, so an
    * over-negative input (e.g. `lo = -10` on a length-3 array)
    * still trips the `< 0` clause and traps.
    */
  private def wrapNegBound(raw: String, extent: String): String =
    val c0 = fresh("wc0")
    out.append(s"  $c0 = arith.constant 0 : index\n")
    val isNeg = fresh("wneg")
    out.append(s"  $isNeg = arith.cmpi slt, $raw, $c0 : index\n")
    val wrapped = fresh("wwrap")
    out.append(s"  $wrapped = arith.addi $raw, $extent : index\n")
    val out0 = fresh("wout")
    out.append(s"  $out0 = arith.select $isNeg, $wrapped, $raw : index\n")
    out0

  /** Emit the rank-1 slice bounds-check trap. Conditions match the LLVM
    * backend: post-wrap `lo < 0` (over-negative), `hi < lo`, `hi >
    * srcLen`, plus stride `<= 0` when the slice was user-stride'd.
    * Branch to `nex_trap_slice_oob` on failure; the function exits.
    */
  private def emitSliceBoundsTrap(
      loIdx:    String,
      hiIdx:    String,
      srcLen:   String,
      stride:   String,
      c0Idx:    String,
      hasStride: Boolean,
  ): Unit =
    val negLo = fresh("nlo")
    out.append(s"  $negLo = arith.cmpi slt, $loIdx, $c0Idx : index\n")
    val hiLtLo = fresh("hlt")
    out.append(s"  $hiLtLo = arith.cmpi slt, $hiIdx, $loIdx : index\n")
    val hiBad = fresh("hbad")
    out.append(s"  $hiBad = arith.cmpi sgt, $hiIdx, $srcLen : index\n")
    val any01 = fresh("any1")
    out.append(s"  $any01 = arith.ori $negLo, $hiLtLo : i1\n")
    val any02 = fresh("any2")
    out.append(s"  $any02 = arith.ori $any01, $hiBad : i1\n")
    val any = if hasStride then
      val sBad = fresh("sbad")
      out.append(s"  $sBad = arith.cmpi sle, $stride, $c0Idx : index\n")
      val a = fresh("any")
      out.append(s"  $a = arith.ori $any02, $sBad : i1\n")
      a
    else any02
    out.append(s"  scf.if $any {\n")
    out.append(s"    func.call @nex_trap_slice_oob() : () -> ()\n")
    out.append(s"    scf.yield\n")
    out.append(s"  }\n")

  /** Axis-index trap for the rank-2 `TAxisIndex` case: after wrap,
    * `iv < 0` or `iv >= total` routes through `nex_trap_axis_oob`.
    */
  private def emitAxisIndexTrap(iv: String, total: String, c0Idx: String): Unit =
    val neg = fresh("aneg")
    out.append(s"  $neg = arith.cmpi slt, $iv, $c0Idx : index\n")
    val ge = fresh("age")
    out.append(s"  $ge = arith.cmpi sge, $iv, $total : index\n")
    val bad = fresh("abad")
    out.append(s"  $bad = arith.ori $neg, $ge : i1\n")
    out.append(s"  scf.if $bad {\n")
    out.append(s"    func.call @nex_trap_axis_oob() : () -> ()\n")
    out.append(s"    scf.yield\n")
    out.append(s"  }\n")

  /** Allocate a stack slot for a `var <sym>` scalar binding and store
    * the initial value. The slot lives in `varSlots` keyed by symbol
    * id so later reads/writes can find it. Uses `memref.alloca` for
    * stack-local lifetime — the kernel-stack region is large enough
    * for any plausible number of var counters, and the slot is
    * automatically reclaimed on function exit.
    */
  private def allocVarSlot(sym: Symbol, initReg: String, sty: MScalar): Unit =
    val mrefT = s"memref<${sty.text}>"
    val slot  = fresh(s"var_${sym.name}")
    out.append(s"  $slot = memref.alloca() : $mrefT\n")
    out.append(s"  memref.store $initReg, $slot[] : $mrefT\n")
    varSlots(sym.id) = (slot, sty)

  /** Read a `var` slot. Mirrors the existing `env`-lookup MlirVal
    * shape so the rest of the visitor doesn't have to know that
    * vars are different from vals.
    */
  private def emitVarLoad(symId: Int): MlirVal =
    val (slot, sty) = varSlots(symId)
    val r           = fresh("vr")
    out.append(s"  $r = memref.load $slot[] : memref<${sty.text}>\n")
    MlirVal(r, sty)

  /** Store a new value into a `var` slot. The value's type must match
    * the slot's element type — Nex's type system already guarantees
    * this at TAssign sites, so no promotion is needed here.
    */
  private def emitVarStore(symId: Int, v: MlirVal): Unit =
    val (slot, sty) = varSlots(symId)
    out.append(s"  memref.store ${v.reg}, $slot[] : memref<${sty.text}>\n")

  /** Statement-form `while cond do body` via `scf.while` with no
    * iter_args. The cond region computes the predicate and yields
    * it through `scf.condition`; the body region runs (reading and
    * writing var slots as needed) and ends with a bare `scf.yield`.
    * Mutable counters live in `var` memref slots — the load/store
    * pattern makes the SSA story trivial since the data isn't
    * threaded through region results.
    */
  private def emitWhile(cond: TExpr, body: TExpr): Unit =
    out.append("  scf.while : () -> () {\n")
    val cv = emitExpr(cond)
    out.append(s"    scf.condition(${cv.reg})\n")
    out.append("  } do {\n")
    emitForBody(body)
    out.append("    scf.yield\n")
    out.append("  }\n")

  /** Statement-form `for x in arr do body` over a rank-1 array. The
    * array's length is static (recorded in the `MTensor` shape), so
    * we walk `0..length` via `scf.for` and `tensor.extract` each
    * element into the loop var slot. The same body-emission path as
    * range-based `for` is reused — `emitForBody` handles `TBlock`s
    * and bare statements identically.
    */
  private def emitForArray(loopVars: List[Symbol], av: MlirVal, ty: MTensor, body: TExpr): Unit =
    if loopVars.size != 1 then
      notYet(s"for over array with ${loopVars.size}-way destructuring")
      return
    val loopVar = loopVars.head
    val len     = ty.shape.head
    val loC     = fresh("flo")
    out.append(s"  $loC = arith.constant 0 : index\n")
    val hiC     = fresh("fhi")
    if len >= 0 then
      out.append(s"  $hiC = arith.constant $len : index\n")
    else
      val axisR = fresh("axis")
      out.append(s"  $axisR = arith.constant 0 : index\n")
      out.append(s"  $hiC = tensor.dim ${av.reg}, $axisR : ${ty.text}\n")
    val stepC   = fresh("fst")
    out.append(s"  $stepC = arith.constant 1 : index\n")
    val ivName  = fresh("iv")
    out.append(s"  scf.for $ivName = $loC to $hiC step $stepC {\n")
    val eltR    = fresh("elt")
    out.append(s"    $eltR = tensor.extract ${av.reg}[$ivName] : ${ty.text}\n")
    val prev    = env.get(loopVar.id)
    env(loopVar.id) = MlirVal(eltR, MScalar(ty.elem))
    emitForBody(body)
    prev match
      case Some(v) => env(loopVar.id) = v
      case None    => env.remove(loopVar.id)
    out.append("  }\n")

  /** Body of a loop. Accepts a bare `TBlock` whose result is `Unit`
    * (the common shape `for i do … do print(i)` produces, since the
    * `do` clause introduces a block), a `TBlock` that ends with a
    * value-producing expression (the result is discarded), or a
    * single non-block statement.
    */
  private def emitForBody(body: TExpr): Unit = body match
    case TBlock(items, TUnitLit(_), _, _) =>
      items.foreach(emitBlockItem)
    case TBlock(items, last, _, _) =>
      items.foreach(emitBlockItem)
      emitBlockItem(TBlockExpr(last))
    case other =>
      emitBlockItem(TBlockExpr(other))

  /** Element-wise binop via `linalg.map` over a fresh `tensor.empty()`
    * output. Both operands must already have the same tensor type.
    *
    * LLVM 22 quirk: the `linalg.map` block arity is `inputs + outputs`,
    * not `inputs` as the dialect docs suggest. The out argument is
    * unused — we discard its value and yield only the computed
    * element — but the verifier requires it.
    */
  private def emitElementWiseBinop(op: String, lv: MlirVal, rv: MlirVal, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val opName = scalarBinop(op, elemT)
    val (initR, _) = emitTensorEmptyLike(lv.reg, ty, elemT)
    val outR = fresh("ew")
    out.append(
      s"  $outR = linalg.map ins(${lv.reg}, ${rv.reg} : ${ty.text}, ${ty.text}) outs($initR : ${ty.text})\n",
    )
    out.append(s"    (%a: $scalar, %b: $scalar, %_o: $scalar) {\n")
    out.append(s"      %s = $opName %a, %b : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Scalar `+ - *` on matching int/int or real/real operands. The
    * dispatcher in [[scalarBinop]] is the gate for which operators
    * land; everything else (`/`, `div`, `%`, `^`) bubbles up as
    * `notYet` from there before any IR is emitted.
    */
  private def emitScalarBinop(op: String, lv: MlirVal, rv: MlirVal, ty: MScalar): MlirVal =
    val opName = scalarBinop(op, ty.elem)
    val r      = fresh("sb")
    out.append(s"  $r = $opName ${lv.reg}, ${rv.reg} : ${ty.text}\n")
    MlirVal(r, ty)

  /** Sign-extend-to-float promotion of an `i64` SSA value into `f64`,
    * matching the interpreter's `asReal` promotion at mixed-type
    * binop call sites. Used to handle `int + real`-style expressions
    * where the elaborator leaves operand types mismatched.
    */
  private def promoteIntToReal(v: MlirVal): MlirVal =
    val r = fresh("pr")
    out.append(s"  $r = arith.sitofp ${v.reg} : i64 to f64\n")
    MlirVal(r, MScalar(TyReal))

  private def isComparisonOp(op: String): Boolean =
    op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">="

  /** Scalar comparison `< <= > >= == !=` on matching int/int or
    * real/real operands. Integer ops use `arith.cmpi` with signed
    * predicates. Real ops use `arith.cmpf` with **ordered** predicates
    * (`oeq` / `olt` / etc.) — these return false whenever NaN is on
    * either side, matching the spec §10.2 / NexInterpreter semantics
    * where every relational op against NaN is false. `!=` is the
    * exception: it uses the **unordered** predicate `une`, so
    * `nan != nan` correctly returns true.
    */
  private def emitComparison(op: String, lv: MlirVal, rv: MlirVal): MlirVal =
    val (lp, rp) = (lv.ty, rv.ty) match
      case (MScalar(TyInteger), MScalar(TyReal))    => (promoteIntToReal(lv), rv)
      case (MScalar(TyReal),    MScalar(TyInteger)) => (lv, promoteIntToReal(rv))
      case _                                        => (lv, rv)
    (lp.ty, rp.ty) match
      case (MScalar(TyInteger), MScalar(TyInteger)) =>
        val pred = op match
          case "==" => "eq"
          case "!=" => "ne"
          case "<"  => "slt"
          case "<=" => "sle"
          case ">"  => "sgt"
          case ">=" => "sge"
        val r = fresh("cmp")
        out.append(s"  $r = arith.cmpi $pred, ${lp.reg}, ${rp.reg} : i64\n")
        MlirVal(r, MScalar(TyBool))
      case (MScalar(TyReal), MScalar(TyReal)) =>
        val pred = op match
          case "==" => "oeq"
          case "!=" => "une"
          case "<"  => "olt"
          case "<=" => "ole"
          case ">"  => "ogt"
          case ">=" => "oge"
        val r = fresh("cmp")
        out.append(s"  $r = arith.cmpf $pred, ${lp.reg}, ${rp.reg} : f64\n")
        MlirVal(r, MScalar(TyBool))
      case (MScalar(TyBool), MScalar(TyBool)) =>
        val pred = op match
          case "==" => "eq"
          case "!=" => "ne"
          case _    => notYet(s"ordering $op on bool")
        val r = fresh("cmp")
        out.append(s"  $r = arith.cmpi $pred, ${lp.reg}, ${rp.reg} : i1\n")
        MlirVal(r, MScalar(TyBool))
      case (lt, rt) =>
        notYet(s"comparison $op on $lt and $rt")

  /** Short-circuit `and` / `or` via `scf.if`. The lhs is evaluated
    * eagerly; the rhs is emitted *inside* one branch of the if so it
    * is only executed when the lhs doesn't already pin the result.
    * `--convert-scf-to-cf` (already in the pass pipeline) lowers the
    * `scf.if` to plain branches afterwards.
    *
    * Layout:
    *   `a and b` → `scf.if a then yield b else yield false`
    *   `a or  b` → `scf.if a then yield true else yield b`
    *
    * MLIR text is bracket-delimited, so the rhs's ops textually live
    * inside the region region even though `out` is a single
    * StringBuilder shared with the parent block. MLIR's dominance
    * rules let region bodies reference parent-scope SSA values, so
    * any vals captured by the rhs continue to work.
    */
  /** `map(arr, lambda)` with an inline single-param lambda. The body
    * inlines into a `linalg.map` region: the input operand becomes
    * the lambda's parameter (registered in env), the body is emitted
    * via `emitExpr`, and the result is yielded. Output tensor type
    * derives from the elaborator's result element type — supports
    * `[int] map → [real]` since the body can promote internally.
    * Works for any rank: `linalg.map` walks the shape regardless,
    * and `srcTy.text`/`outTy.text` encode the full shape.
    */
  private def emitMapInlineLambda(av: MlirVal, srcTy: MTensor, lam: TLambda, outElem: Type): MlirVal =
    val inElem        = srcTy.elem
    val inS           = scalarText(inElem)
    val outS          = scalarText(outElem)
    val (initR, outTy) = emitTensorEmptyLike(av.reg, srcTy, outElem)
    val outR   = fresh("hofmap")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${srcTy.text}) outs($initR : ${outTy.text})\n",
    )
    val paramName = fresh("p")
    out.append(s"    ($paramName: $inS, %_o: $outS) {\n")
    val paramSym  = lam.params.head
    val prev      = env.get(paramSym.id)
    env(paramSym.id) = MlirVal(paramName, MScalar(inElem))
    val bv = emitExpr(lam.body)
    out.append(s"      linalg.yield ${bv.reg} : $outS\n")
    out.append("    }\n")
    prev match
      case Some(v) => env(paramSym.id) = v
      case None    => env.remove(paramSym.id)
    MlirVal(outR, outTy)

  /** `filter(arr, x -> pred)` with an inline one-param predicate. Output
    * length depends on how many elements satisfy the predicate, so the
    * result has dynamic shape `tensor<?xT>` and is built in two passes:
    *
    *   1. Count matches into an i64 carried as an `scf.for` iter_arg.
    *   2. Allocate `tensor.empty(%count)` and walk again, inserting each
    *      matched element at the next write position via `tensor.insert`.
    *      Both the output tensor and the write cursor are iter_args.
    *
    * The predicate body is emitted twice — once per pass — using the
    * same lambda parameter symbol bound to a fresh per-pass element
    * extract. Predicates are pure (Nex value-level expressions), so
    * re-emission is semantically safe.
    */
  private def emitFilterInlineLambda(av: MlirVal, srcTy: MTensor, lam: TLambda): MlirVal =
    val elemT = srcTy.elem
    val elemS = scalarText(elemT)
    val outTy = MTensor(elemT, List(-1))

    val c0Idx = fresh("c0")
    out.append(s"  $c0Idx = arith.constant 0 : index\n")
    val c1Idx = fresh("c1")
    out.append(s"  $c1Idx = arith.constant 1 : index\n")
    val c0I64 = fresh("c0i")
    out.append(s"  $c0I64 = arith.constant 0 : i64\n")
    val c1I64 = fresh("c1i")
    out.append(s"  $c1I64 = arith.constant 1 : i64\n")

    val lenIdx =
      if srcTy.shape.head >= 0 then
        val r = fresh("flen")
        out.append(s"  $r = arith.constant ${srcTy.shape.head} : index\n")
        r
      else
        val axisR = fresh("axis")
        out.append(s"  $axisR = arith.constant 0 : index\n")
        val r = fresh("flen")
        out.append(s"  $r = tensor.dim ${av.reg}, $axisR : ${srcTy.text}\n")
        r

    val paramSym = lam.params.head
    val prev     = env.get(paramSym.id)

    val ivPass1   = fresh("fi1")
    val accName   = fresh("acc")
    val countOut  = fresh("count")
    out.append(
      s"  $countOut = scf.for $ivPass1 = $c0Idx to $lenIdx step $c1Idx iter_args($accName = $c0I64) -> (i64) {\n",
    )
    val elt1 = fresh("elt")
    out.append(s"    $elt1 = tensor.extract ${av.reg}[$ivPass1] : ${srcTy.text}\n")
    env(paramSym.id) = MlirVal(elt1, MScalar(elemT))
    val pred1 = emitExpr(lam.body)
    val nextAcc = fresh("nacc")
    out.append(s"    $nextAcc = scf.if ${pred1.reg} -> (i64) {\n")
    val plus1 = fresh("plus1")
    out.append(s"      $plus1 = arith.addi $accName, $c1I64 : i64\n")
    out.append(s"      scf.yield $plus1 : i64\n")
    out.append("    } else {\n")
    out.append(s"      scf.yield $accName : i64\n")
    out.append("    }\n")
    out.append(s"    scf.yield $nextAcc : i64\n")
    out.append("  }\n")

    val countIdx = fresh("countidx")
    out.append(s"  $countIdx = arith.index_cast $countOut : i64 to index\n")
    val outInit  = emitTensorEmpty(outTy, List(countIdx))

    val ivPass2  = fresh("fi2")
    val outIter  = fresh("oit")
    val wposIter = fresh("wp")
    val resPair  = fresh("res")
    out.append(
      s"  $resPair:2 = scf.for $ivPass2 = $c0Idx to $lenIdx step $c1Idx " +
        s"iter_args($outIter = $outInit, $wposIter = $c0Idx) -> (${outTy.text}, index) {\n",
    )
    val elt2 = fresh("elt")
    out.append(s"    $elt2 = tensor.extract ${av.reg}[$ivPass2] : ${srcTy.text}\n")
    env(paramSym.id) = MlirVal(elt2, MScalar(elemT))
    val pred2 = emitExpr(lam.body)
    val nrPair = fresh("nr")
    out.append(s"    $nrPair:2 = scf.if ${pred2.reg} -> (${outTy.text}, index) {\n")
    val inserted = fresh("ins")
    out.append(s"      $inserted = tensor.insert $elt2 into $outIter[$wposIter] : ${outTy.text}\n")
    val nwpos = fresh("nwp")
    out.append(s"      $nwpos = arith.addi $wposIter, $c1Idx : index\n")
    out.append(s"      scf.yield $inserted, $nwpos : ${outTy.text}, index\n")
    out.append("    } else {\n")
    out.append(s"      scf.yield $outIter, $wposIter : ${outTy.text}, index\n")
    out.append("    }\n")
    out.append(s"    scf.yield $nrPair#0, $nrPair#1 : ${outTy.text}, index\n")
    out.append("  }\n")

    prev match
      case Some(v) => env(paramSym.id) = v
      case None    => env.remove(paramSym.id)

    MlirVal(s"$resPair#0", outTy)

  /** `flatMap(arr, x -> [...])` on rank-1. The lambda returns a
    * rank-1 array each call; the result concatenates them. Output
    * length is the sum of inner lengths, so the codegen mirrors
    * [[emitFilterInlineLambda]] with an extra dimension of nesting:
    *
    *   1. Pass 1 walks the input, emits the lambda body once per
    *      iteration, reads the inner tensor's first dim via
    *      `tensor.dim`, and accumulates the i64 sum.
    *   2. Pass 2 allocates `tensor<?xU>` of that size and walks
    *      again. For each iteration it emits the lambda body a
    *      second time, then runs a nested `scf.for` over the inner
    *      tensor that copies element-by-element into the output at
    *      `wpos + j`. The output tensor flows through the nested
    *      loop as an iter_arg; `wpos` advances by the inner length
    *      after each outer iteration.
    *
    * Two re-emissions of the lambda body (vs. one for filter) is
    * accepted: lambda bodies are pure Nex value-level expressions.
    */
  private def emitFlatMapInlineLambda(
      av:      MlirVal,
      srcTy:   MTensor,
      lam:     TLambda,
      outElem: Type,
  ): MlirVal =
    val srcElemT = srcTy.elem
    val outTy    = MTensor(outElem, List(-1))

    val c0Idx = fresh("c0")
    out.append(s"  $c0Idx = arith.constant 0 : index\n")
    val c1Idx = fresh("c1")
    out.append(s"  $c1Idx = arith.constant 1 : index\n")
    val c0I64 = fresh("c0i")
    out.append(s"  $c0I64 = arith.constant 0 : i64\n")

    val lenIdx =
      if srcTy.shape.head >= 0 then
        val r = fresh("flen")
        out.append(s"  $r = arith.constant ${srcTy.shape.head} : index\n")
        r
      else
        val axisR = fresh("axis")
        out.append(s"  $axisR = arith.constant 0 : index\n")
        val r = fresh("flen")
        out.append(s"  $r = tensor.dim ${av.reg}, $axisR : ${srcTy.text}\n")
        r

    val paramSym = lam.params.head
    val prev     = env.get(paramSym.id)

    val ivPass1  = fresh("fi1")
    val accName  = fresh("acc")
    val totalOut = fresh("total")
    out.append(
      s"  $totalOut = scf.for $ivPass1 = $c0Idx to $lenIdx step $c1Idx iter_args($accName = $c0I64) -> (i64) {\n",
    )
    val elt1 = fresh("elt")
    out.append(s"    $elt1 = tensor.extract ${av.reg}[$ivPass1] : ${srcTy.text}\n")
    env(paramSym.id) = MlirVal(elt1, MScalar(srcElemT))
    val inner1 = emitExpr(lam.body)
    val innerTy1 = inner1.ty match
      case t @ MTensor(_, List(_)) => t
      case other =>
        notYet(s"flatMap lambda body did not produce rank-1 tensor: $other")
    val innerLenIdx1 = fresh("ilen")
    if innerTy1.shape.head >= 0 then
      out.append(s"    $innerLenIdx1 = arith.constant ${innerTy1.shape.head} : index\n")
    else
      val iaxis = fresh("iaxis")
      out.append(s"    $iaxis = arith.constant 0 : index\n")
      out.append(s"    $innerLenIdx1 = tensor.dim ${inner1.reg}, $iaxis : ${innerTy1.text}\n")
    val innerLenI64 = fresh("ileni")
    out.append(s"    $innerLenI64 = arith.index_castui $innerLenIdx1 : index to i64\n")
    val newAcc = fresh("nacc")
    out.append(s"    $newAcc = arith.addi $accName, $innerLenI64 : i64\n")
    out.append(s"    scf.yield $newAcc : i64\n")
    out.append("  }\n")

    val totalIdx = fresh("totalidx")
    out.append(s"  $totalIdx = arith.index_cast $totalOut : i64 to index\n")
    val outInit  = emitTensorEmpty(outTy, List(totalIdx))

    val ivPass2  = fresh("fi2")
    val outIter  = fresh("oit")
    val wposIter = fresh("wp")
    val resPair  = fresh("res")
    out.append(
      s"  $resPair:2 = scf.for $ivPass2 = $c0Idx to $lenIdx step $c1Idx " +
        s"iter_args($outIter = $outInit, $wposIter = $c0Idx) -> (${outTy.text}, index) {\n",
    )
    val elt2 = fresh("elt")
    out.append(s"    $elt2 = tensor.extract ${av.reg}[$ivPass2] : ${srcTy.text}\n")
    env(paramSym.id) = MlirVal(elt2, MScalar(srcElemT))
    val inner2 = emitExpr(lam.body)
    val innerTy2 = inner2.ty.asInstanceOf[MTensor]
    val innerLenIdx2 = fresh("ilen2")
    if innerTy2.shape.head >= 0 then
      out.append(s"    $innerLenIdx2 = arith.constant ${innerTy2.shape.head} : index\n")
    else
      val iaxis = fresh("iaxis2")
      out.append(s"    $iaxis = arith.constant 0 : index\n")
      out.append(s"    $innerLenIdx2 = tensor.dim ${inner2.reg}, $iaxis : ${innerTy2.text}\n")

    val ivInner  = fresh("ij")
    val outInner = fresh("oin")
    val outAfter = fresh("oaft")
    out.append(
      s"    $outAfter = scf.for $ivInner = $c0Idx to $innerLenIdx2 step $c1Idx " +
        s"iter_args($outInner = $outIter) -> (${outTy.text}) {\n",
    )
    val v = fresh("v")
    out.append(s"      $v = tensor.extract ${inner2.reg}[$ivInner] : ${innerTy2.text}\n")
    val destIdx = fresh("dst")
    out.append(s"      $destIdx = arith.addi $wposIter, $ivInner : index\n")
    val ins = fresh("ins")
    out.append(s"      $ins = tensor.insert $v into $outInner[$destIdx] : ${outTy.text}\n")
    out.append(s"      scf.yield $ins : ${outTy.text}\n")
    out.append("    }\n")

    val newWpos = fresh("nwp")
    out.append(s"    $newWpos = arith.addi $wposIter, $innerLenIdx2 : index\n")
    out.append(s"    scf.yield $outAfter, $newWpos : ${outTy.text}, index\n")
    out.append("  }\n")

    prev match
      case Some(v) => env(paramSym.id) = v
      case None    => env.remove(paramSym.id)

    MlirVal(s"$resPair#0", outTy)

  /** `reduce(arr, init, lambda)` with an inline two-param lambda.
    * Nex spec §10.4: the lambda is `(acc, x) -> body`. Lowers to
    * `linalg.reduce` over every axis of the input, with `init` seeded
    * into a 0-d output via `tensor.from_elements`. The lambda's `acc`
    * binds to the reduce body's accumulator and `x` to the input
    * element; the body emits and yields. All-dims reduce means the
    * lambda fires once per element regardless of rank — same flat
    * left-to-right traversal the interpreter and LLVM backend use.
    */
  private def emitReduceInlineLambda(av: MlirVal, srcTy: MTensor, iv: MlirVal, lam: TLambda): MlirVal =
    val accSym  = lam.params(0)
    val elemSym = lam.params(1)
    val accTy   = iv.ty match
      case s: MScalar => s
      case other      => notYet(s"reduce with non-scalar init $other")
    val outTy   = MTensor(accTy.elem, Nil)
    val initR   = fresh("init")
    out.append(s"  $initR = tensor.from_elements ${iv.reg} : ${outTy.text}\n")
    val outTR   = fresh("hofred_t")
    val inS     = scalarText(srcTy.elem)
    val accS    = scalarText(accTy.elem)
    val dims    = srcTy.shape.indices.mkString(", ")
    out.append(
      s"  $outTR = linalg.reduce ins(${av.reg} : ${srcTy.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    val inName  = fresh("p")
    val accName = fresh("a")
    out.append(s"    ($inName: $inS, $accName: $accS) {\n")
    val prevAcc  = env.get(accSym.id)
    val prevElem = env.get(elemSym.id)
    env(accSym.id)  = MlirVal(accName, accTy)
    env(elemSym.id) = MlirVal(inName, MScalar(srcTy.elem))
    val bv = emitExpr(lam.body)
    out.append(s"      linalg.yield ${bv.reg} : $accS\n")
    out.append("    }\n")
    prevAcc match { case Some(v) => env(accSym.id) = v; case None => env.remove(accSym.id) }
    prevElem match { case Some(v) => env(elemSym.id) = v; case None => env.remove(elemSym.id) }
    val outR = fresh("hofred")
    out.append(s"  $outR = tensor.extract $outTR[] : ${outTy.text}\n")
    MlirVal(outR, accTy)

  /** Resolve an Nex length expression to a compile-time integer when
    * possible. Recognises:
    *   - `TIntLit(n)` directly.
    *   - `length(arr)` where `arr` evaluates (via the env or a fresh
    *     emit, free of side effects) to a tensor with known static
    *     shape — the outer dim is the length.
    *   - `a * b` where both sides are static integers (used by the
    *     fusion pass for `rows * cols` on rank-2 sources).
    * Returns `None` otherwise so callers can fall back to `notYet`.
    */
  private def staticLength(e: TExpr): Option[Int] = e match
    case TIntLit(n, _, _) => Some(n.toInt)
    case TCall(TVarRef(s, _, _), List(arr), _, _) if s.kind == SymKind.Prelude && s.name == "length" =>
      staticTensorOf(arr).map(_.shape.head)
    case TBinOp("*", l, r, _, _) =>
      for li <- staticLength(l); ri <- staticLength(r) yield li * ri
    case _ => None

  /** Find the static `MTensor` type of an expression that's already
    * sitting in `env` (a `TVarRef` to a let-bound or var-bound array).
    * Used by [[staticLength]] to read the source shape without emitting
    * any side-effecting ops.
    */
  private def staticTensorOf(e: TExpr): Option[MTensor] = e match
    case TVarRef(s, _, _) =>
      env.get(s.id).map(_.ty).collect { case t: MTensor => t }
    case _ => None

  /** Rank-1 fused loop. The fusion pass already inlined the lambda body
    * with the param substituted to a `TFlatIndex(src, loopVar)` shape;
    * here we emit a `linalg.map` over a fresh `tensor<NxT_out>`, bind
    * the loop var symbol to the iteration index (cast to i64), and
    * `emitExpr(body)` inside the region. The body's `TFlatIndex` will
    * tensor.extract from the source tensor as needed.
    */
  private def emitFusedLoop1D(loopVar: Symbol, n: Int, elemT: Type, body: TExpr): MlirVal =
    val outTy = MTensor(elemT, List(n))
    val s     = scalarText(elemT)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("fl")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val ivR   = fresh("ivi")
    out.append(s"      $ivR = arith.index_castui $idxR : index to i64\n")
    val prev  = env.get(loopVar.id)
    env(loopVar.id) = MlirVal(ivR, MScalar(TyInteger))
    val bv    = emitExpr(body)
    out.append(s"      linalg.yield ${bv.reg} : $s\n")
    out.append("    }\n")
    prev match
      case Some(v) => env(loopVar.id) = v
      case None    => env.remove(loopVar.id)
    MlirVal(outR, outTy)

  /** Scalar types the backend can carry through an `scf.if -> (T)`
    * result slot. Tensor-returning `if` would need shape inference
    * (both branches must materialise the same static tensor type)
    * and isn't yet supported.
    */
  private def isMlirScalarType(t: Type): Boolean = t match
    case TyInteger | TyReal | TyBool => true
    case _                           => false

  /** Lower an `if cond then thenB else elseB` expression with a
    * scalar result. Models on [[emitShortCircuit]] — the cond is
    * evaluated eagerly, then both branches live inside an
    * `scf.if -> (T)` whose regions yield through `scf.yield`. The
    * pass pipeline already runs `--convert-scf-to-cf`, so this
    * lowers to plain branches before LLVM IR is emitted. Both
    * branches recurse through [[emitExpr]] so nested blocks /
    * arithmetic / calls / nested ifs are all handled by the same
    * machinery — MLIR is whitespace-insensitive, so the textual
    * indentation of region bodies doesn't have to match.
    */
  private def emitIfExpr(cond: TExpr, thenB: TExpr, elseB: TExpr, outTy: MlirType): MlirVal =
    val cv = emitExpr(cond)
    val r  = fresh("if")
    out.append(s"  $r = scf.if ${cv.reg} -> (${outTy.text}) {\n")
    val tv = emitExpr(thenB)
    out.append(s"    scf.yield ${tv.reg} : ${outTy.text}\n")
    out.append("  } else {\n")
    val ev = emitExpr(elseB)
    out.append(s"    scf.yield ${ev.reg} : ${outTy.text}\n")
    out.append("  }\n")
    MlirVal(r, outTy)

  private def emitShortCircuit(lhs: TExpr, rhs: TExpr, isAnd: Boolean): MlirVal =
    val lv = emitExpr(lhs)
    val r  = fresh(if isAnd then "and" else "or")
    out.append(s"  $r = scf.if ${lv.reg} -> (i1) {\n")
    if isAnd then
      val rv = emitExpr(rhs)
      out.append(s"    scf.yield ${rv.reg} : i1\n")
    else
      val tConst = fresh("scTrue")
      out.append(s"    $tConst = arith.constant 1 : i1\n")
      out.append(s"    scf.yield $tConst : i1\n")
    out.append("  } else {\n")
    if isAnd then
      val fConst = fresh("scFalse")
      out.append(s"    $fConst = arith.constant 0 : i1\n")
      out.append(s"    scf.yield $fConst : i1\n")
    else
      val rv = emitExpr(rhs)
      out.append(s"    scf.yield ${rv.reg} : i1\n")
    out.append("  }\n")
    MlirVal(r, MScalar(TyBool))

  /** Call a libm bridge declared in the prologue. All arguments are
    * promoted to f64 (sign-extending integer operands as needed) and
    * the result is f64. Used by every `@intrinsic("libm.X")` prelude
    * function — `sqrt(9.0)`, `hypot(3, 4)`, etc. Returns NaN for
    * domain errors (libm semantics), which matches NexInterpreter's
    * spec §10.2 behavior — e.g. `sqrt(-4.0)` prints `nan`.
    */
  private def emitLibmCall(name: String, args: List[MlirVal]): MlirVal =
    val argsF = args.map { v =>
      v.ty match
        case MScalar(TyReal)    => v
        case MScalar(TyInteger) => promoteIntToReal(v)
        case other              => notYet(s"libm `$name` arg of type $other")
    }
    val argTypes = argsF.map(_ => "f64").mkString(", ")
    val argRegs  = argsF.map(_.reg).mkString(", ")
    val r        = fresh(name)
    out.append(s"  $r = func.call @$name($argRegs) : ($argTypes) -> f64\n")
    MlirVal(r, MScalar(TyReal))

  /** Scalar `^` (power). `int ^ int` dispatches to the C runtime's
    * `nex_ipow` (exponentiation by squaring), matching the LLVM
    * backend's `@__nex_ipow`. Anything else routes through libm
    * `pow(f64, f64)` after lifting integer operands to f64. The
    * elaborated `resultTy` is the source of truth for which path
    * to take — `int ^ int` only stays integer-typed when the
    * exponent is provably non-negative.
    */
  private def emitScalarPower(lv: MlirVal, rv: MlirVal, resultTy: Type): MlirVal =
    (lv.ty, rv.ty, resultTy) match
      case (MScalar(TyInteger), MScalar(TyInteger), TyInteger) =>
        val r = fresh("ipow")
        out.append(s"  $r = func.call @nex_ipow(${lv.reg}, ${rv.reg}) : (i64, i64) -> i64\n")
        MlirVal(r, MScalar(TyInteger))
      case _ =>
        val lf = lv.ty match
          case MScalar(TyInteger) => promoteIntToReal(lv)
          case MScalar(TyReal)    => lv
          case other              => notYet(s"power lhs $other")
        val rf = rv.ty match
          case MScalar(TyInteger) => promoteIntToReal(rv)
          case MScalar(TyReal)    => rv
          case other              => notYet(s"power rhs $other")
        val r = fresh("rpow")
        out.append(s"  $r = func.call @pow(${lf.reg}, ${rf.reg}) : (f64, f64) -> f64\n")
        MlirVal(r, MScalar(TyReal))

  /** Generalised `@` operator covering the three rank combinations
    * `NexInterpreter` recognises:
    *
    *   - rank-2 × rank-2: `linalg.matmul` → rank-2 result
    *   - rank-2 × rank-1: `linalg.matvec` → rank-1 result
    *   - rank-1 × rank-1: `linalg.dot`    → 0-d result, then extract
    *
    * Element types must match and the contracted dimension must agree.
    * Every variant uses the same zero-fill init pattern since the
    * linalg op accumulates into its output.
    */
  private def emitMatMul(lv: MlirVal, rv: MlirVal): MlirVal =
    (lv.ty, rv.ty) match
      case (MTensor(elemL, List(n, k)), MTensor(elemR, List(k2, m))) if elemL == elemR && k == k2 =>
        val outTy = MTensor(elemL, List(n, m))
        val initR = emitZeroInit(outTy)
        val mmR   = fresh("mm")
        out.append(
          s"  $mmR = linalg.matmul ins(${lv.reg}, ${rv.reg} : ${lv.ty.text}, ${rv.ty.text}) outs($initR : ${outTy.text}) -> ${outTy.text}\n",
        )
        MlirVal(mmR, outTy)
      case (MTensor(elemL, List(n, k)), MTensor(elemR, List(k2))) if elemL == elemR && k == k2 =>
        val outTy = MTensor(elemL, List(n))
        val initR = emitZeroInit(outTy)
        val mvR   = fresh("mv")
        out.append(
          s"  $mvR = linalg.matvec ins(${lv.reg}, ${rv.reg} : ${lv.ty.text}, ${rv.ty.text}) outs($initR : ${outTy.text}) -> ${outTy.text}\n",
        )
        MlirVal(mvR, outTy)
      case (MTensor(elemL, List(k)), MTensor(elemR, List(k2))) if elemL == elemR && k == k2 =>
        val outTy = MTensor(elemL, Nil)
        val initR = emitZeroInit(outTy)
        val dotR  = fresh("dot")
        out.append(
          s"  $dotR = linalg.dot ins(${lv.reg}, ${rv.reg} : ${lv.ty.text}, ${rv.ty.text}) outs($initR : ${outTy.text}) -> ${outTy.text}\n",
        )
        val scalarR = fresh("dot_s")
        out.append(s"  $scalarR = tensor.extract $dotR[] : ${outTy.text}\n")
        MlirVal(scalarR, MScalar(elemL))
      case (lt, rt) =>
        notYet(s"matmul shape: $lt @ $rt")

  /** Zero-filled output tensor for the linalg.{matmul, matvec, dot}
    * family. `linalg.fill` over a `tensor.empty()` is the canonical
    * way to materialise the accumulator they reduce into.
    */
  private def emitZeroInit(ty: MTensor): String =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val zeroR  = fresh("zero")
    out.append(s"  $zeroR = arith.constant ${zeroLit(elemT)} : $scalar\n")
    val emptyR = fresh("empty")
    out.append(s"  $emptyR = tensor.empty() : ${ty.text}\n")
    val initR  = fresh("init")
    out.append(s"  $initR = linalg.fill ins($zeroR : $scalar) outs($emptyR : ${ty.text}) -> ${ty.text}\n")
    initR

  /** Sum-reduce a tensor of any rank to a 0-d tensor, then extract
    * the scalar. Matches `NexInterpreter`'s rule that `sum` walks
    * every element regardless of rank — for rank-N input we reduce
    * along all N dimensions in one `linalg.reduce` and the output
    * is 0-d. Init value is the element-type zero; reducer is the
    * element-type add.
    */
  private def emitSumReduce(srcReg: String, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val outTy  = MTensor(elemT, Nil)
    val initER = fresh("init_e")
    out.append(s"  $initER = arith.constant ${zeroLit(elemT)} : $scalar\n")
    val initR = fresh("init")
    out.append(s"  $initR = tensor.from_elements $initER : ${outTy.text}\n")
    val dims  = ty.shape.indices.mkString(", ")
    val sumTR = fresh("sum_t")
    out.append(
      s"  $sumTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("+", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val sumR = fresh("sum")
    out.append(s"  $sumR = tensor.extract $sumTR[] : ${outTy.text}\n")
    MlirVal(sumR, MScalar(elemT))

  /** Scalar-against-tensor broadcast for arithmetic ops. Emits one
    * `linalg.map` over the tensor; the body closes over the scalar
    * SSA value from the parent scope (MLIR's dominance rules let
    * region bodies reference parent-scope values). `scalarFirst`
    * matters for non-commutative ops: `100 - xs` and `xs - 100`
    * differ.
    */
  private def emitBroadcast(op: String, sv: MlirVal, av: MlirVal, ty: MTensor, scalarFirst: Boolean): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val opName = scalarBinop(op, elemT)
    val (initR, _) = emitTensorEmptyLike(av.reg, ty, elemT)
    val outR = fresh("bc")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${ty.text})\n",
    )
    out.append(s"    (%a: $scalar, %_o: $scalar) {\n")
    if scalarFirst then
      out.append(s"      %s = $opName ${sv.reg}, %a : $scalar\n")
    else
      out.append(s"      %s = $opName %a, ${sv.reg} : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Element-wise arithmetic on tensors of matching shape but mismatched
    * element types (e.g. `[int] + [real]`). The wider numeric type is
    * the result element type; each loaded element is promoted to that
    * type via `arith.sitofp` inside the region body before the binop
    * runs. Mirrors `emitElementWiseComparison` but yields a numeric
    * tensor rather than a bool tensor.
    */
  private def emitElementWiseMixed(op: String, lv: MlirVal, rv: MlirVal, lt: MTensor, rt: MTensor): MlirVal =
    val lElem    = lt.elem
    val rElem    = rt.elem
    val commonT  = if lElem == TyReal || rElem == TyReal then TyReal else TyInteger
    val commonS  = scalarText(commonT)
    val opName   = scalarBinop(op, commonT)
    val (initR, outTy) = emitTensorEmptyLike(lv.reg, lt, commonT)
    val outR = fresh("ewm")
    out.append(
      s"  $outR = linalg.map ins(${lv.reg}, ${rv.reg} : ${lt.text}, ${rt.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(lElem)}, %b: ${scalarText(rElem)}, %_o: $commonS) {\n")
    val aName = promoteInRegion("%a", lElem, commonT)
    val bName = promoteInRegion("%b", rElem, commonT)
    out.append(s"      %s = $opName $aName, $bName : $commonS\n")
    out.append(s"      linalg.yield %s : $commonS\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** Scalar-against-tensor arithmetic broadcast when the scalar's
    * element type doesn't match the tensor's (e.g. `2 + [1.0, 2.0]`).
    * The scalar is promoted once outside the map body if needed; the
    * per-element promotion of the loaded tensor element happens
    * inside the region body. Output element type is the wider common
    * type. `scalarFirst` controls operand order for non-commutative
    * ops.
    */
  private def emitBroadcastMixed(
      op: String,
      sv: MlirVal,
      av: MlirVal,
      ty: MTensor,
      scalarFirst: Boolean,
      scalarElem: Type,
      arrElem: Type,
  ): MlirVal =
    val commonT = if scalarElem == TyReal || arrElem == TyReal then TyReal else TyInteger
    val commonS = scalarText(commonT)
    val opName  = scalarBinop(op, commonT)
    val svPromoted =
      if scalarElem == commonT then sv.reg
      else
        val r = fresh("ps")
        out.append(s"  $r = arith.sitofp ${sv.reg} : ${scalarText(scalarElem)} to $commonS\n")
        r
    val (initR, outTy) = emitTensorEmptyLike(av.reg, ty, commonT)
    val outR = fresh("bcm")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(arrElem)}, %_o: $commonS) {\n")
    val elemName = promoteInRegion("%a", arrElem, commonT)
    val (lhs, rhs) = if scalarFirst then (svPromoted, elemName) else (elemName, svPromoted)
    out.append(s"      %s = $opName $lhs, $rhs : $commonS\n")
    out.append(s"      linalg.yield %s : $commonS\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** Element-wise comparison: `xs < ys`, `xs == ys`, etc. Output element
    * type is always `i1`. Mixed-element-type inputs (`[int] < [real]`)
    * are handled by promoting each loaded element up to the wider numeric
    * type inside the region body, then dispatching to `arith.cmpi` /
    * `arith.cmpf` with the same predicate scheme as scalar comparisons
    * (ordered for everything except `!=`, which uses `une` so NaN-vs-NaN
    * is correctly true).
    */
  private def emitElementWiseComparison(op: String, lv: MlirVal, rv: MlirVal, lt: MTensor, rt: MTensor): MlirVal =
    val lElem    = lt.elem
    val rElem    = rt.elem
    val commonT  = if lElem == TyReal || rElem == TyReal then TyReal else TyInteger
    val commonS  = scalarText(commonT)
    val (initR, outTy) = emitTensorEmptyLike(lv.reg, lt, TyBool)
    val outR = fresh("ewcmp")
    out.append(
      s"  $outR = linalg.map ins(${lv.reg}, ${rv.reg} : ${lt.text}, ${rt.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(lElem)}, %b: ${scalarText(rElem)}, %_o: i1) {\n")
    val aName = promoteInRegion("%a", lElem, commonT)
    val bName = promoteInRegion("%b", rElem, commonT)
    val pred  = comparisonPredicate(op, commonT)
    val cmp   = if commonT == TyInteger then "arith.cmpi" else "arith.cmpf"
    out.append(s"      %s = $cmp $pred, $aName, $bName : $commonS\n")
    out.append(s"      linalg.yield %s : i1\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** Scalar-against-tensor comparison broadcast: `xs < 5`, `2 < xs`,
    * etc. Output is a `tensor<...xi1>` with the tensor's shape. The
    * scalar is promoted once outside the map body if its type doesn't
    * match the common comparison type; the per-element promotion of
    * the loaded tensor element happens inside the region body.
    * `scalarFirst` carries through to the operand order of the cmp op
    * — matters for non-symmetric predicates (`<`, `<=`, `>`, `>=`).
    */
  private def emitBroadcastComparison(
      op: String,
      sv: MlirVal,
      av: MlirVal,
      ty: MTensor,
      scalarFirst: Boolean,
      scalarElem: Type,
      arrElem: Type,
  ): MlirVal =
    val commonT = if scalarElem == TyReal || arrElem == TyReal then TyReal else TyInteger
    val commonS = scalarText(commonT)
    val svPromoted =
      if scalarElem == commonT then sv.reg
      else
        val r = fresh("ps")
        out.append(s"  $r = arith.sitofp ${sv.reg} : ${scalarText(scalarElem)} to $commonS\n")
        r
    val (initR, outTy) = emitTensorEmptyLike(av.reg, ty, TyBool)
    val outR = fresh("bccmp")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(arrElem)}, %_o: i1) {\n")
    val elemName = promoteInRegion("%a", arrElem, commonT)
    val pred     = comparisonPredicate(op, commonT)
    val cmp      = if commonT == TyInteger then "arith.cmpi" else "arith.cmpf"
    val (lhs, rhs) = if scalarFirst then (svPromoted, elemName) else (elemName, svPromoted)
    out.append(s"      %s = $cmp $pred, $lhs, $rhs : $commonS\n")
    out.append(s"      linalg.yield %s : i1\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `product(arr)` (spec §10.4): rank-N multiplicative reduction.
    * Same shape as `emitSumReduce` but seeds the accumulator with 1
    * and uses the type-specific multiply op (`muli` for int, `mulf`
    * for real). Walks every element regardless of rank.
    */
  private def emitProductReduce(srcReg: String, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val outTy  = MTensor(elemT, Nil)
    val oneR   = fresh("init_e")
    val oneLit = elemT match
      case TyInteger => "1"
      case TyReal    => "1.0"
      case other     => notYet(s"one literal for $other")
    out.append(s"  $oneR = arith.constant $oneLit : $scalar\n")
    val initR = fresh("init")
    out.append(s"  $initR = tensor.from_elements $oneR : ${outTy.text}\n")
    val dims  = ty.shape.indices.mkString(", ")
    val prodTR = fresh("prod_t")
    out.append(
      s"  $prodTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("*", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val prodR = fresh("prod")
    out.append(s"  $prodR = tensor.extract $prodTR[] : ${outTy.text}\n")
    MlirVal(prodR, MScalar(elemT))

  /** `diag(arr)` (rank-1 → rank-2): build an n×n matrix with `arr` on
    * the diagonal and zeros elsewhere. Lowers to `linalg.fill` of 0
    * over a fresh n×n tensor, then a `linalg.map` over a rank-1
    * sequence of [0..n) that scatters `arr[i]` to position `(i, i)`
    * via `tensor.insert`. Output is a fresh tensor.
    *
    * Implemented as a `linalg.map` over the n×n output that uses
    * `linalg.index` to recover (i, j) and `arith.cmpi eq` plus
    * `arith.select` to either pick `arr[i]` or zero.
    */
  private def emitDiag(av: MlirVal, srcTy: MTensor, n: Int): MlirVal =
    val elemT = srcTy.elem
    val s     = scalarText(elemT)
    val zero  = zeroLit(elemT)
    val outTy = MTensor(elemT, List(n, n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("diag")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val iR    = fresh("i")
    out.append(s"      $iR = linalg.index 0 : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = linalg.index 1 : index\n")
    val cmpR  = fresh("eq")
    out.append(s"      $cmpR = arith.cmpi eq, $iR, $jR : index\n")
    val zR    = fresh("z")
    out.append(s"      $zR = arith.constant $zero : $s\n")
    val eltR  = fresh("elt")
    out.append(s"      $eltR = tensor.extract ${av.reg}[$iR] : ${srcTy.text}\n")
    val selR  = fresh("sel")
    out.append(s"      $selR = arith.select $cmpR, $eltR, $zR : $s\n")
    out.append(s"      linalg.yield $selR : $s\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `identity(n)`: n×n integer identity matrix. Same shape as `diag`
    * but the diagonal value is the constant 1. Element type is
    * integer (matches the interpreter — the spec says real-typed but
    * v0 has a known divergence).
    */
  private def emitIdentity(n: Int): MlirVal =
    val outTy = MTensor(TyInteger, List(n, n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("id")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: i64) {\n")
    val iR    = fresh("i")
    out.append(s"      $iR = linalg.index 0 : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = linalg.index 1 : index\n")
    val cmpR  = fresh("eq")
    out.append(s"      $cmpR = arith.cmpi eq, $iR, $jR : index\n")
    val oneR  = fresh("one")
    out.append(s"      $oneR = arith.constant 1 : i64\n")
    val zR    = fresh("z")
    out.append(s"      $zR = arith.constant 0 : i64\n")
    val selR  = fresh("sel")
    out.append(s"      $selR = arith.select $cmpR, $oneR, $zR : i64\n")
    out.append(s"      linalg.yield $selR : i64\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `sum_axis(m, axis)` (spec §10.4): rank-2 reduction along one axis,
    * producing a rank-1 result. axis=0 sums down each column (output
    * shape = cols); axis=1 sums across each row (output shape = rows).
    * Lowers to `linalg.reduce ... dimensions = [axis]` with a `+`
    * accumulator body — same kernel as the all-dims `emitSumReduce`,
    * just with a non-empty output shape.
    */
  private def emitSumAxis(av: MlirVal, srcTy: MTensor, axis: Int): MlirVal =
    val elemT     = srcTy.elem
    val scalar    = scalarText(elemT)
    val outShape  = srcTy.shape.zipWithIndex.collect { case (d, i) if i != axis => d }
    val outTy     = MTensor(elemT, outShape)
    val initER    = fresh("init_e")
    out.append(s"  $initER = arith.constant ${zeroLit(elemT)} : $scalar\n")
    val initR     = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val filledR   = fresh("filled")
    out.append(s"  $filledR = linalg.fill ins($initER : $scalar) outs($initR : ${outTy.text}) -> ${outTy.text}\n")
    val outR      = fresh("axis")
    out.append(
      s"  $outR = linalg.reduce ins(${av.reg} : ${srcTy.text}) outs($filledR : ${outTy.text}) dimensions = [$axis]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("+", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `flatten(xs)` on a rank-1 array: identity, but materialise a fresh
    * tensor to match the interpreter's "flatten always copies"
    * semantics. A `linalg.copy`-style identity map does the job.
    */
  private def emitFlatten1D(av: MlirVal): MlirVal =
    val ty    = av.ty.asInstanceOf[MTensor]
    val elemT = ty.elem
    val s     = scalarText(elemT)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val outR  = fresh("flat")
    out.append(s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${ty.text})\n")
    out.append(s"    (%a: $s, %_o: $s) {\n")
    out.append(s"      linalg.yield %a : $s\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** `flatten(m)` on a rank-2 array — Nex spec §10.4 says this is
    * **column-major**: `flat[i + r*j] = m[i, j]` for an r×c matrix.
    * We materialise a fresh rank-1 of length r·c via `linalg.map`
    * whose body uses `linalg.index 0` plus a divmod against `r` to
    * recover the source `(i, j)`, then `tensor.extract %m[i, j]`.
    */
  private def emitFlatten2D(av: MlirVal, srcTy: MTensor, rows: Int, cols: Int): MlirVal =
    val elemT = srcTy.elem
    val s     = scalarText(elemT)
    val total = rows * cols
    val outTy = MTensor(elemT, List(total))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("flat")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val rowsC = fresh("rows")
    out.append(s"      $rowsC = arith.constant $rows : index\n")
    val iR    = fresh("i")
    out.append(s"      $iR = arith.remui $idxR, $rowsC : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = arith.divui $idxR, $rowsC : index\n")
    val eltR  = fresh("elt")
    out.append(s"      $eltR = tensor.extract ${av.reg}[$iR, $jR] : ${srcTy.text}\n")
    out.append(s"      linalg.yield $eltR : $s\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `reshape(flat, r, c)` — column-major inverse of `flatten`. The
    * input is rank-1; the output is rank-2 with `m[i, j] = flat[i + r*j]`.
    * Same shape as `emitFlatten2D` but iterates the (rows, cols) output
    * grid via two `linalg.index` calls and computes the flat source
    * index by hand. The elaborator rejects shape mismatches before
    * we get here, so `length(flat)` is guaranteed to equal `r*c`.
    */
  private def emitReshape(av: MlirVal, srcTy: MTensor, rows: Int, cols: Int): MlirVal =
    val elemT = srcTy.elem
    val s     = scalarText(elemT)
    val outTy = MTensor(elemT, List(rows, cols))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("rs")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val iR    = fresh("i")
    out.append(s"      $iR = linalg.index 0 : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = linalg.index 1 : index\n")
    val rowsC = fresh("rows")
    out.append(s"      $rowsC = arith.constant $rows : index\n")
    val mul   = fresh("mul")
    out.append(s"      $mul = arith.muli $rowsC, $jR : index\n")
    val flat  = fresh("flat")
    out.append(s"      $flat = arith.addi $iR, $mul : index\n")
    val eltR  = fresh("elt")
    out.append(s"      $eltR = tensor.extract ${av.reg}[$flat] : ${srcTy.text}\n")
    out.append(s"      linalg.yield $eltR : $s\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** Rank-2 `.transpose()` / `transpose(m)`. Output shape swaps the
    * row and column dimensions; element type is unchanged. Lowers to
    * a single `linalg.transpose` with permutation `[1, 0]`, which
    * `--convert-linalg-to-loops` reduces to a nested counting loop
    * that does `out[j, i] = in[i, j]`.
    */
  private def emitTranspose(srcReg: String, ty: MTensor, rows: Int, cols: Int): MlirVal =
    val outTy = MTensor(ty.elem, List(cols, rows))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("tr")
    out.append(
      s"  $outR = linalg.transpose ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) permutation = [1, 0]\n",
    )
    MlirVal(outR, outTy)

  /** Literal `range(lo, hi)`: integer half-open range with statically
    * known length. Length zero (when `hi <= lo`) lowers to an empty
    * `tensor<0xi64>`. Otherwise we walk the iteration domain via
    * `linalg.index` and add the loaded position to the literal `lo`.
    */
  private def emitRangeCall(lo: Long, hi: Long): MlirVal =
    val len = math.max(0L, hi - lo).toInt
    val ty  = MTensor(TyInteger, List(len))
    if len == 0 then
      val r = fresh("rng")
      out.append(s"  $r = tensor.empty() : ${ty.text}\n")
      return MlirVal(r, ty)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val loConst = fresh("lo")
    out.append(s"  $loConst = arith.constant $lo : i64\n")
    val outR = fresh("rng")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: i64) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI  = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val sumR  = fresh("sum")
    out.append(s"      $sumR = arith.addi $loConst, $idxI : i64\n")
    out.append(s"      linalg.yield $sumR : i64\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Literal `zeros(n)` / `ones(n)`. Always integer-typed per the
    * elaborator (matches the interpreter; the spec's real-typed
    * signature is a v0 divergence shared across backends). Emits a
    * `linalg.fill` over a fresh empty tensor.
    */
  private def emitConstFill(name: String, n: Int): MlirVal =
    val ty    = MTensor(TyInteger, List(n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val v     = if name == "zeros" then 0 else 1
    val cR    = fresh("c")
    out.append(s"  $cR = arith.constant $v : i64\n")
    val outR  = fresh(name)
    out.append(s"  $outR = linalg.fill ins($cR : i64) outs($initR : ${ty.text}) -> ${ty.text}\n")
    MlirVal(outR, ty)

  /** Literal `linspace(lo, hi, n)`: real array of length n with values
    * `lo + i * (hi - lo) / (n - 1)`. We compute the step at codegen
    * time and emit a `linalg.map` that yields `lo + step * i`. When
    * `n == 1`, every element collapses to `lo` (`step` is a 0/0 NaN
    * otherwise); special-case to avoid emitting NaN in the IR.
    */
  private def emitLinspaceCall(lo: Double, hi: Double, n: Int): MlirVal =
    val ty    = MTensor(TyReal, List(n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val loC   = fresh("lo")
    out.append(s"  $loC = arith.constant ${formatReal(lo)} : f64\n")
    val step  = if n <= 1 then 0.0 else (hi - lo) / (n - 1)
    val stepC = fresh("step")
    out.append(s"  $stepC = arith.constant ${formatReal(step)} : f64\n")
    val outR  = fresh("ls")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: f64) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI  = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val idxF  = fresh("idxf")
    out.append(s"      $idxF = arith.sitofp $idxI : i64 to f64\n")
    val mulR  = fresh("mul")
    out.append(s"      $mulR = arith.mulf $stepC, $idxF : f64\n")
    val addR  = fresh("add")
    out.append(s"      $addR = arith.addf $loC, $mulR : f64\n")
    out.append(s"      linalg.yield $addR : f64\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Pull a `Double` out of an `TIntLit` or `TRealLit`. Used by the
    * `linspace` dispatch where the elaborator may leave `0` (int) or
    * `0.0` (real) untouched on the lo/hi arguments — both meanings
    * are valid sources.
    */
  private def realLitValue(e: TExpr): Double = e match
    case TIntLit(v, _, _)  => v.toDouble
    case TRealLit(v, _, _) => v
    case other             => notYet(s"non-literal linspace bound: ${other.getClass.getSimpleName}")

  /** True when the given Nex type is a rank-1 integer array — the
    * surface form a value-position range expression takes. Used to
    * gate the `..` / `..=` value-position handler so scalar binops
    * (which can't reach `..` anyway, but as a defensive guard) still
    * fall through to the generic dispatch.
    */
  private def isRangeArrayType(tpe: Type): Boolean = tpe match
    case TyArray(TyInteger, 1) => true
    case _                     => false

  /** Lower `range(lo, hi)` / `lo..hi` / `lo..=hi` to a rank-1 integer
    * tensor. Literal bounds get the static-shape `tensor<NxI64>` path;
    * any other operand shape falls through to the dynamic path that
    * computes the length at runtime and emits `tensor<?xI64>`.
    */
  private def emitRangeDispatch(loE: TExpr, hiE: TExpr, inclusive: Boolean): MlirVal =
    (loE, hiE) match
      case (TIntLit(lo, _, _), TIntLit(hi, _, _)) =>
        val end = if inclusive then hi + 1 else hi
        emitRangeCall(lo, end)
      case _ =>
        val loV   = emitExpr(loE)
        val hiRaw = emitExpr(hiE)
        val hiV   =
          if inclusive then
            val one = fresh("one")
            out.append(s"  $one = arith.constant 1 : i64\n")
            val bumped = fresh("hi1")
            out.append(s"  $bumped = arith.addi ${hiRaw.reg}, $one : i64\n")
            MlirVal(bumped, MScalar(TyInteger))
          else hiRaw
        emitRangeDynamic(loV, hiV)

  /** Lower `zeros(n)` / `ones(n)` to a rank-1 integer tensor. Literal
    * length keeps the static `tensor.empty()` + `linalg.fill` shape;
    * runtime length emits `tensor<?xI64>` with a dynamic size operand.
    */
  private def emitConstFillDispatch(name: String, nE: TExpr): MlirVal = nE match
    case TIntLit(n, _, _) => emitConstFill(name, n.toInt)
    case _                => emitConstFillDynamic(name, emitExpr(nE))

  /** Lower `linspace(lo, hi, n)` to a rank-1 real tensor. Literal `n`
    * folds the step constant at codegen time; runtime `n` keeps the
    * same shape with a runtime-computed step and a select that
    * collapses to `lo` when `n <= 1` (matches the literal special-case).
    */
  private def emitLinspaceDispatch(loE: TExpr, hiE: TExpr, nE: TExpr): MlirVal =
    nE match
      case TIntLit(n, _, _) =>
        val lo = realLitValue(loE)
        val hi = realLitValue(hiE)
        emitLinspaceCall(lo, hi, n.toInt)
      case _ =>
        val loV = emitExpr(loE)
        val hiV = emitExpr(hiE)
        val nV  = emitExpr(nE)
        emitLinspaceDynamic(loV, hiV, nV)

  /** Dynamic `range(lo, hi)` (half-open) producing `tensor<?xi64>`.
    * Length is `max(0, hi - lo)` computed at runtime; `tensor.empty`
    * takes the size as an index operand. The body of the `linalg.map`
    * adds the `linalg.index 0` to the captured `lo` so the result is
    * the iota sequence offset by `lo`.
    */
  private def emitRangeDynamic(loV: MlirVal, hiV: MlirVal): MlirVal =
    val ty       = MTensor(TyInteger, List(-1))
    val rawR     = fresh("rdiff")
    out.append(s"  $rawR = arith.subi ${hiV.reg}, ${loV.reg} : i64\n")
    val zeroR    = fresh("z64")
    out.append(s"  $zeroR = arith.constant 0 : i64\n")
    val sizeI    = fresh("size")
    out.append(s"  $sizeI = arith.maxsi $rawR, $zeroR : i64\n")
    val sizeIdx  = fresh("sidx")
    out.append(s"  $sizeIdx = arith.index_cast $sizeI : i64 to index\n")
    val initR    = emitTensorEmpty(ty, List(sizeIdx))
    val outR     = fresh("rng")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: i64) {\n")
    val idxR     = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI     = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val sumR     = fresh("sum")
    out.append(s"      $sumR = arith.addi ${loV.reg}, $idxI : i64\n")
    out.append(s"      linalg.yield $sumR : i64\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Dynamic `zeros(n)` / `ones(n)` producing `tensor<?xi64>`.
    */
  private def emitConstFillDynamic(name: String, nV: MlirVal): MlirVal =
    val ty      = MTensor(TyInteger, List(-1))
    val sizeIdx = fresh("sidx")
    out.append(s"  $sizeIdx = arith.index_cast ${nV.reg} : i64 to index\n")
    val initR   = emitTensorEmpty(ty, List(sizeIdx))
    val fillV   = if name == "zeros" then 0 else 1
    val cR      = fresh("c")
    out.append(s"  $cR = arith.constant $fillV : i64\n")
    val outR    = fresh(name)
    out.append(s"  $outR = linalg.fill ins($cR : i64) outs($initR : ${ty.text}) -> ${ty.text}\n")
    MlirVal(outR, ty)

  /** Dynamic `linspace(lo, hi, n)` producing `tensor<?xf64>`. The
    * step is `(hi - lo) / (n - 1)` when `n > 1`, else 0.0 — chosen
    * via `arith.select` to keep the `arith.divf` out of the
    * divide-by-zero path's user-visible result (NaN/Inf still gets
    * computed but is immediately discarded). Both `lo` and `hi` are
    * promoted from i64 to f64 if needed before entering the body.
    */
  private def emitLinspaceDynamic(loV: MlirVal, hiV: MlirVal, nV: MlirVal): MlirVal =
    val ty      = MTensor(TyReal, List(-1))
    val loF     = if loV.ty == MScalar(TyInteger) then promoteIntToReal(loV) else loV
    val hiF     = if hiV.ty == MScalar(TyInteger) then promoteIntToReal(hiV) else hiV
    val sizeIdx = fresh("sidx")
    out.append(s"  $sizeIdx = arith.index_cast ${nV.reg} : i64 to index\n")
    val initR   = emitTensorEmpty(ty, List(sizeIdx))
    val one64   = fresh("one64")
    out.append(s"  $one64 = arith.constant 1 : i64\n")
    val gt1     = fresh("gt1")
    out.append(s"  $gt1 = arith.cmpi sgt, ${nV.reg}, $one64 : i64\n")
    val nm1     = fresh("nm1")
    out.append(s"  $nm1 = arith.subi ${nV.reg}, $one64 : i64\n")
    val nm1f    = fresh("nm1f")
    out.append(s"  $nm1f = arith.sitofp $nm1 : i64 to f64\n")
    val diff    = fresh("diff")
    out.append(s"  $diff = arith.subf ${hiF.reg}, ${loF.reg} : f64\n")
    val rawStep = fresh("rstep")
    out.append(s"  $rawStep = arith.divf $diff, $nm1f : f64\n")
    val zeroF   = fresh("zerof")
    out.append(s"  $zeroF = arith.constant 0.000000e+00 : f64\n")
    val stepR   = fresh("step")
    out.append(s"  $stepR = arith.select $gt1, $rawStep, $zeroF : f64\n")
    val outR    = fresh("ls")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: f64) {\n")
    val idxR    = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI    = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val idxF    = fresh("idxf")
    out.append(s"      $idxF = arith.sitofp $idxI : i64 to f64\n")
    val mulR    = fresh("mul")
    out.append(s"      $mulR = arith.mulf $stepR, $idxF : f64\n")
    val addR    = fresh("add")
    out.append(s"      $addR = arith.addf ${loF.reg}, $mulR : f64\n")
    out.append(s"      linalg.yield $addR : f64\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Predicate string for `arith.cmpi` / `arith.cmpf`. Integers use
    * signed predicates; reals use ordered (`oeq`, `olt`, …) for every
    * op except `!=`, where `une` makes `nan != nan` true — matching
    * IEEE 754 and the spec §10.2.
    */
  private def comparisonPredicate(op: String, t: Type): String = (op, t) match
    case ("==", TyInteger) => "eq"
    case ("!=", TyInteger) => "ne"
    case ("<",  TyInteger) => "slt"
    case ("<=", TyInteger) => "sle"
    case (">",  TyInteger) => "sgt"
    case (">=", TyInteger) => "sge"
    case ("==", TyReal)    => "oeq"
    case ("!=", TyReal)    => "une"
    case ("<",  TyReal)    => "olt"
    case ("<=", TyReal)    => "ole"
    case (">",  TyReal)    => "ogt"
    case (">=", TyReal)    => "oge"
    case _                 => notYet(s"comparison predicate $op on $t")

  /** Emit a `sitofp` promotion inside a linalg.map / linalg.reduce
    * region body (indented at the region depth) and return the new
    * SSA name. Returns `srcName` unchanged when no promotion is
    * needed.
    */
  private def promoteInRegion(srcName: String, fromT: Type, toT: Type): String =
    if fromT == toT then srcName
    else (fromT, toT) match
      case (TyInteger, TyReal) =>
        val r = fresh("pr")
        out.append(s"      $r = arith.sitofp $srcName : i64 to f64\n")
        r
      case _ => notYet(s"in-region promote $fromT to $toT")

  /** Scalar binary `min` / `max`. Promotes mixed `int × real` operands
    * to real before dispatching to the matching `arith.{minsi, maxsi,
    * minimumf, maximumf}` op. Same NaN-propagating spelling we use
    * for array min/max.
    */
  private def emitScalarMinMax(name: String, lv: MlirVal, rv: MlirVal): MlirVal =
    val (lp, rp) = (lv.ty, rv.ty) match
      case (MScalar(TyInteger), MScalar(TyReal))    => (promoteIntToReal(lv), rv)
      case (MScalar(TyReal), MScalar(TyInteger))    => (lv, promoteIntToReal(rv))
      case _                                        => (lv, rv)
    (lp.ty, rp.ty) match
      case (MScalar(TyInteger), MScalar(TyInteger)) =>
        val opName = if name == "min" then "arith.minsi" else "arith.maxsi"
        val r      = fresh(name)
        out.append(s"  $r = $opName ${lp.reg}, ${rp.reg} : i64\n")
        MlirVal(r, MScalar(TyInteger))
      case (MScalar(TyReal), MScalar(TyReal)) =>
        val opName = if name == "min" then "arith.minimumf" else "arith.maximumf"
        val r      = fresh(name)
        out.append(s"  $r = $opName ${lp.reg}, ${rp.reg} : f64\n")
        MlirVal(r, MScalar(TyReal))
      case (lt, rt) =>
        notYet(s"$name on $lt and $rt")

  /** Scalar `abs`: branchless via `cmp + select`. Avoids depending on
    * the math dialect (which would force `--convert-math-to-llvm` into
    * the pass pipeline). Integer abs uses signed less-than against zero
    * and subtracts; real abs flips the sign with `arith.negf` when
    * `x < 0.0`. Note: `arith.cmpf olt(-0.0, 0.0)` is false (signed-zero
    * pair is equal in IEEE), so this preserves `-0.0` for the negative
    * zero — a known divergence the corpus does not currently test for
    * `abs(real)`.
    */
  private def emitScalarAbs(v: MlirVal): MlirVal = v.ty match
    case MScalar(TyInteger) =>
      val zero = fresh("z")
      out.append(s"  $zero = arith.constant 0 : i64\n")
      val neg = fresh("neg")
      out.append(s"  $neg = arith.subi $zero, ${v.reg} : i64\n")
      val cmp = fresh("cmp")
      out.append(s"  $cmp = arith.cmpi slt, ${v.reg}, $zero : i64\n")
      val r = fresh("abs")
      out.append(s"  $r = arith.select $cmp, $neg, ${v.reg} : i64\n")
      MlirVal(r, MScalar(TyInteger))
    case MScalar(TyReal) =>
      val zero = fresh("z")
      out.append(s"  $zero = arith.constant 0.0 : f64\n")
      val neg = fresh("neg")
      out.append(s"  $neg = arith.negf ${v.reg} : f64\n")
      val cmp = fresh("cmp")
      out.append(s"  $cmp = arith.cmpf olt, ${v.reg}, $zero : f64\n")
      val r = fresh("abs")
      out.append(s"  $r = arith.select $cmp, $neg, ${v.reg} : f64\n")
      MlirVal(r, MScalar(TyReal))
    case other =>
      notYet(s"abs of $other")

  /** Reduce a rank-1 tensor with the `min` or `max` prelude. Mirrors
    * `emitSumReduce` but seeds the accumulator with `arr[0]` rather
    * than a zero sentinel — matches `NexInterpreter`'s
    * `b.tail.foldLeft(b.head)` and dodges the need for an
    * IEEE-defined +∞ / -∞ constant in MLIR text.
    *
    * Reducer ops:
    *   - int  → `arith.minsi` / `arith.maxsi`
    *   - real → `arith.minimumf` / `arith.maximumf`
    *     (IEEE 754-2019 spelling, NaN-propagating like the
    *      `arith.cmpf` predicates we use elsewhere)
    *
    * Empty-array case is the caller's problem — the elaborator either
    * rejects empty literals or accepts them with a trap at runtime;
    * `tensor.extract` on an empty rank-1 tensor is UB by MLIR rules,
    * which matches NexInterpreter's `trap("min: empty array", ...)`.
    */
  private def emitMinMaxReduce(name: String, srcReg: String, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val outTy  = MTensor(elemT, Nil)
    val zeroIdx = fresh("z_idx")
    out.append(s"  $zeroIdx = arith.constant 0 : index\n")
    val initER = fresh("init_e")
    out.append(s"  $initER = tensor.extract $srcReg[$zeroIdx] : ${ty.text}\n")
    val initR = fresh("init")
    out.append(s"  $initR = tensor.from_elements $initER : ${outTy.text}\n")
    val opName = (name, elemT) match
      case ("min", TyInteger) => "arith.minsi"
      case ("max", TyInteger) => "arith.maxsi"
      case ("min", TyReal)    => "arith.minimumf"
      case ("max", TyReal)    => "arith.maximumf"
      case _                  => notYet(s"$name reducer on $elemT")
    val dims  = ty.shape.indices.mkString(", ")
    val redTR = fresh(s"${name}_t")
    out.append(
      s"  $redTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = $opName %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val redR = fresh(name)
    out.append(s"  $redR = tensor.extract $redTR[] : ${outTy.text}\n")
    MlirVal(redR, MScalar(elemT))

  /** Fold a unary-minus over a numeric literal. Returns `None` for
    * non-literal operands so the caller can decide whether to reject.
    * Constant folding past unary minus on arbitrary expressions is
    * out of scope until we have proper `arith.subi` / `arith.negf`
    * emission.
    */
  private def foldLiteralNeg(e: TExpr): Option[TExpr] = e match
    case TIntLit(v, p, t)  => Some(TIntLit(-v, p, t))
    case TRealLit(v, p, t) => Some(TRealLit(-v, p, t))
    case _                 => None

  private def scalarText(t: Type): String = t match
    case TyInteger => "i64"
    case TyReal    => "f64"
    case TyBool    => "i1"
    case TyString  => "i64"  // opaque pointer to a C-side nex_str descriptor
    case other     => notYet(s"scalar text for $other")

  /** Convert a Nex elaborator-level `Type` to the codegen's
    * [[MlirType]] when the backend can express it as a function
    * parameter or return type. Returns `None` for types this phase
    * doesn't yet handle (tensors, complex, tuples, structs, enums,
    * `TyUnit` — units are unit-returning defs which need a
    * different `func.return` shape).
    */
  private def mlirTypeOf(t: Type): Option[MlirType] = t match
    case TyInteger => Some(MScalar(TyInteger))
    case TyReal    => Some(MScalar(TyReal))
    case TyBool    => Some(MScalar(TyBool))
    case TyString  => Some(MString)
    case _         => None

  /** True when `e` is a scalar literal (after literal-fold of unary
    * minus) — i.e. an expression the codegen can re-emit at every
    * use site without changing semantics. Used to decide whether a
    * top-level binding is safe to inline at reference sites in user
    * `def` bodies (which run before `@main` and therefore can't
    * read the env-bound copy emitted at main's entry).
    */
  private def isLiteralScalarExpr(e: TExpr): Boolean = e match
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit       => true
    case TUnaryOp("-", inner, _, _)                                    => isLiteralScalarExpr(inner)
    case _                                                             => false

  /** Mangle a user `def`'s name into a unique MLIR symbol. The id
    * suffix prevents collisions with the runtime print/format
    * helpers, libm bridges, and any two user defs that happen to
    * share a name across scopes — uncommon, but cheap insurance.
    */
  private def mangleUserDef(sym: Symbol): String =
    s"@nex_user_${sym.name}_${sym.id}"

  /** Emit a call to a registered user def. Evaluates the args,
    * applies cheap numeric promotions to match the declared param
    * types, then writes a single `func.call`. Returns a fresh
    * `MlirVal` for value-returning calls; for unit-returning calls
    * the result has dummy register `%`-placeholder and type
    * `MScalar(TyInteger)` — the caller in [[emitBlockItem]] ignores
    * the result and uses the call purely for its side-effect.
    */
  private def emitUserDefCall(
      name:     String,
      paramTys: List[MlirType],
      retTyOpt: Option[MlirType],
      args:     List[TExpr],
      symName:  String,
  ): MlirVal =
    if args.size != paramTys.size then
      notYet(s"arity mismatch on user def `$symName` — ${args.size} args vs ${paramTys.size} params")
    val argVals = args.zip(paramTys).map { case (a, expectedTy) =>
      val av = emitExpr(a)
      (av.ty, expectedTy) match
        case (lt, rt) if lt == rt                  => av
        case (MScalar(TyInteger), MScalar(TyReal)) => promoteIntToReal(av)
        case (lt, rt) =>
          notYet(s"user def `$symName` arg type $lt does not match param type $rt")
    }
    val paramTyText = paramTys.map(_.text).mkString(", ")
    retTyOpt match
      case Some(retTy) =>
        val r = fresh("ucall")
        out.append(
          s"  $r = func.call $name(${argVals.map(_.reg).mkString(", ")}) : ($paramTyText) -> ${retTy.text}\n",
        )
        MlirVal(r, retTy)
      case None =>
        out.append(
          s"  func.call $name(${argVals.map(_.reg).mkString(", ")}) : ($paramTyText) -> ()\n",
        )
        MlirVal("%unused", MScalar(TyInteger))

  /** Emit a `func.func` for one registered user def, writing the
    * signature, the body, and the closing terminator into `out`.
    * The current `nextReg` and `env` are saved + reset so the body's
    * SSA names don't collide with the caller's region. Parameter
    * symbols bind to MLIR block args (`%arg0`, `%arg1`, …) in the
    * fresh env. Value-returning bodies are emitted via `emitExpr`
    * and yielded through `func.return %r : T`; unit-returning
    * bodies are emitted as a statement sequence ([[emitForBody]] is
    * already the right shape for that) and end with a bare
    * `func.return`.
    */
  private def emitUserDef(f: TFunDecl): Unit =
    val (name, paramTys, retTyOpt) = userDefs(f.sym.id)
    val sigParts = f.params.zip(paramTys).zipWithIndex.map { case ((_, ty), i) =>
      s"%arg$i: ${ty.text}"
    }
    val retStr = retTyOpt.fold("")(t => s" -> ${t.text}")
    out.append(s"func.func $name(${sigParts.mkString(", ")})$retStr {\n")
    val savedReg = nextReg
    val savedEnv = env.toMap
    nextReg = 0
    env.clear()
    f.params.zip(paramTys).zipWithIndex.foreach { case ((p, ty), i) =>
      env(p.id) = MlirVal(s"%arg$i", ty)
    }
    retTyOpt match
      case Some(retTy) =>
        val v = emitExpr(f.body)
        if v.ty != retTy then
          notYet(s"user def `${f.sym.name}` body type ${v.ty} does not match declared return ${retTy}")
        out.append(s"  func.return ${v.reg} : ${retTy.text}\n")
      case None =>
        emitForBody(f.body)
        out.append("  func.return\n")
    out.append("}\n")
    nextReg = savedReg
    env.clear()
    savedEnv.foreach { case (k, v) => env(k) = v }

  private def zeroLit(t: Type): String = t match
    case TyInteger => "0"
    case TyReal    => "0.0"
    case other     => notYet(s"zero literal for $other")

  /** Map a Nex binop + element type to the `arith.*` op that performs
    * it on a scalar. Used uniformly inside `linalg.reduce` and
    * `linalg.map` body regions, so both reductions and element-wise
    * ops share the dispatch table.
    *
    * Nex's `/` is always real division — the elaborator promotes
    * `int / int` to real-typed before this dispatcher sees it, so
    * `("/", TyInteger)` is never expected and isn't listed. `div`
    * (integer division) and `%` (integer modulo) stay on `i64`.
    */
  private def scalarBinop(op: String, t: Type): String = (op, t) match
    case ("+",   TyInteger) => "arith.addi"
    case ("-",   TyInteger) => "arith.subi"
    case ("*",   TyInteger) => "arith.muli"
    case ("div", TyInteger) => "arith.divsi"
    case ("%",   TyInteger) => "arith.remsi"
    case ("+",   TyReal)    => "arith.addf"
    case ("-",   TyReal)    => "arith.subf"
    case ("*",   TyReal)    => "arith.mulf"
    case ("/",   TyReal)    => "arith.divf"
    case _                  => notYet(s"scalar binop $op on $t")

  /** Render a `Double` so MLIR's FloatAttr parser accepts it. Whole
    * numbers get a trailing `.0`; everything else uses Scala's
    * shortest-round-trip `toString`. NaN / Inf intentionally rejected
    * — not reachable from a literal source program at this milestone.
    */
  private def formatReal(v: Double): String =
    if v.isNaN || v.isInfinite then notYet(s"non-finite real literal: $v")
    else
      val s = v.toString
      if s.contains('.') || s.contains('e') || s.contains('E') then s
      else s + ".0"

  private def fresh(prefix: String): String =
    val r = s"%${prefix}${nextReg}"
    nextReg += 1
    r

  private def notYet(msg: String): Nothing =
    throw new UnsupportedOperationException(s"NexMLIRCodegen: $msg")
