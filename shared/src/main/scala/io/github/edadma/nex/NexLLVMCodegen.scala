package io.github.edadma.nex

import scala.collection.mutable

/** First vertical slice of the AOT compiler — text-based LLVM IR
  * emission. The codegen takes a fully elaborated [[TProgram]] and
  * returns an LLVM IR module as a `String`. The CLI is responsible
  * for writing the `.ll`, shelling out to `clang` to produce a
  * native binary, and surfacing any tool errors.
  *
  * Coverage in this first cut:
  *   - Top-level `def main()` with no params, returning unit.
  *   - `print(integer)` / `print(real)` via libc `printf`.
  *   - Integer + real arithmetic binops: `+ - * /` and comparisons.
  *   - `val` / `var` bindings at function scope (alloca + load/store).
  *   - Integer / real / bool literals.
  *
  * Not yet supported (will fall through to a helpful "not yet" diag):
  *   - Other top-level functions, struct decls, top-bindings.
  *   - Lambdas, closures, higher-order calls, prelude HOFs.
  *   - Arrays (rank-1 / rank-2), tuples, strings, complex numbers.
  *   - Control flow (if, for, while), `return`.
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
class NexLLVMCodegen:

  private val out          = new StringBuilder
  private var regCounter   = 0
  private var labelCounter = 0
  private val locals       = mutable.Map.empty[Int, String] // symbol id → alloca register

  /** The name of the basic block currently accepting instructions.
    *
    * `Some(label)` while the block is open and instructions append to it.
    * `None` after a terminator (`br`, `ret`, `unreachable`) has been
    * emitted — further instruction emits are silently dropped until
    * [[startBlock]] opens a new block.
    *
    * Tracked so that early returns inside `if` / `while` branches don't
    * produce stray instructions after a terminator (which LLVM rejects).
    */
  private var currentBlock: Option[String] = None

  /** Currently-emitting function's declared return type. Needed by
    * [[TReturn]] to choose `ret <T> <v>` vs `ret void` when the spec'd
    * return type differs from the value's inferred type (e.g., the
    * elaborator left it `TyUnknown`).
    */
  private var currentReturnType: Type = TyUnit
  private var currentIsMain:     Boolean = false

  /** Symbol ids of top-level `val` / `var` / `const` bindings emitted as
    * LLVM `@<name> = global ...`. Looked up by [[emitExpr]] when a
    * [[TVarRef]] resolves neither to a local alloca nor a function — so
    * we know to emit a `load ... @<name>` (or, on the assign side, a
    * `store ... ptr @<name>`).
    */
  private val globalBindings = mutable.Set.empty[Int]

  /** Pool of string literals → `@.str.<N>` global names. Each unique
    * literal value gets a single global so duplicate literal text isn't
    * stored twice in the resulting binary.
    */
  private val stringPool = mutable.LinkedHashMap.empty[String, String]

  /** Function-level array slots — currently only array-typed parameters.
    * Block-level `val` / `var` bindings live in [[blockArrayScopes]] and
    * are dec'd at block end, not function end. Cleared per function.
    */
  private val arrayLocalSlots = mutable.LinkedHashMap.empty[Int, (String, Type)]

  /** A stack of array-slot maps, one per currently-open `TBlock`. Top of
    * the stack is the innermost block. Each `TBlockBinding(sym, _, …)`
    * whose `sym` is array-typed pushes onto the top. At block end we
    * dec every entry in the popped scope so the binding's owning share
    * is released. Early `return` consults the whole stack via
    * [[decAllLocalArrays]] so nothing leaks regardless of nesting.
    *
    * Maintained as a `List` of mutable maps so `push` / `pop` is cheap
    * and the natural iteration order matches innermost-first.
    */
  private var blockArrayScopes: List[mutable.LinkedHashMap[Int, (String, Type)]] = Nil

  /** Compile a program. Returns the full LLVM IR module text. */
  def compile(tp: TProgram): String =
    emitPreamble()

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

    // Pass 2: emit the init function (if there are any bindings) and
    // every user function. main has an implicit call to the init
    // function inserted before its body — see [[emitFunction]].
    if topBindings.nonEmpty then emitInitFunction(topBindings)

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

  /** Picks the appropriate `zeroinitializer` token for an LLVM type. */
  private def zeroInitFor(ty: String): String = ty match
    case "double" => "0.0"
    case "i1"     => "false"
    case "ptr"    => "null"
    case _        => "0"

  /** Add `s` to the literal pool if not already present, returning the
    * `@.str.<N>` global name as an SSA-usable pointer token. The actual
    * `@.str.<N> = constant [<len> x i8] c"<escaped>\00"` definitions are
    * flushed by [[flushStringPool]] right before the user code so they're
    * defined before any function references them.
    */
  private def internStringLiteral(s: String): String =
    stringPool.getOrElseUpdate(s, s"@.str.${stringPool.size}")

  /** Emit all pooled string-literal globals. Called once between the
    * preamble and the @-bindings/init-function/user-functions section so
    * every later reference can resolve.
    */
  private def flushStringPool(): Unit =
    if stringPool.nonEmpty then
      for (text, name) <- stringPool do
        val encoded = encodeIRString(text)
        val len     = encoded._2
        out.append(s"$name = private unnamed_addr constant [$len x i8] c\"${encoded._1}\"\n")
      out.append("\n")

  /** Encode a String into LLVM IR's c"..." form. Returns the encoded
    * string text (without surrounding quotes) and the total byte length
    * including the implicit trailing NUL. Non-ASCII / control bytes are
    * `\xx` hex-escaped; the trailing NUL is added as `\00`.
    */
  private def encodeIRString(s: String): (String, Int) =
    val sb = new StringBuilder
    var bytes = 0
    for c <- s.getBytes("UTF-8") do
      val b = c & 0xff
      if b == '"' || b == '\\' || b < 0x20 || b > 0x7e then
        sb.append(f"\\$b%02X")
      else
        sb.append(b.toChar)
      bytes += 1
    sb.append("\\00")
    bytes += 1
    (sb.toString, bytes)

  /** Runs every top-level binding initializer in declaration order. The
    * function returns void and is called once from `main`'s entry block.
    * Functions are already initialized at compile time (each is a
    * `define`), so an initializer body may freely call any function.
    */
  private def emitInitFunction(bindings: List[TTopBinding]): Unit =
    regCounter   = 0
    labelCounter = 0
    locals.clear()
    arrayLocalSlots.clear()
    blockArrayScopes = Nil
    currentReturnType = TyUnit
    currentIsMain     = false

    out.append("define void @__nex_init_globals() {\n")
    currentBlock = Some("entry")
    out.append("entry:\n")

    for b <- bindings do
      val rv = emitExpr(b.value)
      val ty = llvmType(b.sym.tpe)
      if currentBlock.isDefined && ty != "void" then
        emitLine(s"  store $ty $rv, ptr @${b.sym.name}\n")

    if currentBlock.isDefined then emitTerminator("  ret void\n")
    out.append("}\n\n")

  // ---------------------------------------------------------------------------
  // Preamble — external declarations, format strings.
  // ---------------------------------------------------------------------------

  private def emitPreamble(): Unit =
    out.append(
      """; Nex LLVM IR module (v0 scaffolding)
        |
        |declare i32 @printf(ptr, ...)
        |declare ptr @malloc(i64)
        |declare void @free(ptr)
        |declare void @abort()
        |
        |; --- Array descriptor types (§8.5) -------------------------------------
        |; rank-1: { refcount, length, data }                     ; 24 bytes
        |; rank-2: { refcount, rows,   cols, data }               ; 32 bytes
        |%nex_arr1 = type { i64, i64, ptr }
        |%nex_arr2 = type { i64, i64, i64, ptr }
        |
        |@.fmt_int     = private unnamed_addr constant [6 x i8] c"%lld\0A\00"
        |@.fmt_real    = private unnamed_addr constant [4 x i8] c"%g\0A\00"
        |@.fmt_bool_t  = private unnamed_addr constant [6 x i8] c"true\0A\00"
        |@.fmt_bool_f  = private unnamed_addr constant [7 x i8] c"false\0A\00"
        |@.fmt_str     = private unnamed_addr constant [4 x i8] c"%s\0A\00"
        |@.fmt_str_raw = private unnamed_addr constant [3 x i8] c"%s\00"
        |@.fmt_int_raw = private unnamed_addr constant [5 x i8] c"%lld\00"
        |@.fmt_real_raw = private unnamed_addr constant [3 x i8] c"%g\00"
        |@.nl          = private unnamed_addr constant [2 x i8] c"\0A\00"
        |@.arr_open    = private unnamed_addr constant [2 x i8] c"[\00"
        |@.arr_close   = private unnamed_addr constant [2 x i8] c"]\00"
        |@.arr_sep     = private unnamed_addr constant [3 x i8] c", \00"
        |@.oob_msg     = private unnamed_addr constant [27 x i8] c"trap: index out of bounds\0A\00"
        |@.fmt_real_int = private unnamed_addr constant [7 x i8] c"%lld.0\00"
        |@.fmt_real_g   = private unnamed_addr constant [3 x i8] c"%g\00"
        |
        |declare double @floor(double)
        |declare double @fabs(double)
        |
        |; Print a real value without a trailing newline. Matches the
        |; interpreter's formatValue: if v is a whole number with |v| < 1e15,
        |; print "<lld>.0"; otherwise "%g". Used by the print(real) and the
        |; array/interpolation paths.
        |define void @__nex_print_real_raw(double %v) {
        |entry:
        |  %f      = call double @floor(double %v)
        |  %is_int = fcmp oeq double %v, %f
        |  %a      = call double @fabs(double %v)
        |  %small  = fcmp olt double %a, 1.0e+15
        |  %both   = and i1 %is_int, %small
        |  br i1 %both, label %whole, label %generic
        |whole:
        |  %ll = fptosi double %v to i64
        |  call i32 (ptr, ...) @printf(ptr @.fmt_real_int, i64 %ll)
        |  ret void
        |generic:
        |  call i32 (ptr, ...) @printf(ptr @.fmt_real_g, double %v)
        |  ret void
        |}
        |
        |define void @__nex_print_real(double %v) {
        |entry:
        |  call void @__nex_print_real_raw(double %v)
        |  call i32 (ptr, ...) @printf(ptr @.nl)
        |  ret void
        |}
        |
        |; --- Rank-1 runtime helpers -------------------------------------------
        |
        |; Allocate a rank-1 array. Returns a fresh %nex_arr1* with refcount=1.
        |define ptr @__nex_arr1_alloc(i64 %len, i64 %elem_size) {
        |entry:
        |  %desc = call ptr @malloc(i64 24)
        |  %rcp  = getelementptr inbounds %nex_arr1, ptr %desc, i32 0, i32 0
        |  store i64 1, ptr %rcp
        |  %lp   = getelementptr inbounds %nex_arr1, ptr %desc, i32 0, i32 1
        |  store i64 %len, ptr %lp
        |  %dp   = getelementptr inbounds %nex_arr1, ptr %desc, i32 0, i32 2
        |  %bytes = mul i64 %len, %elem_size
        |  %buf   = call ptr @malloc(i64 %bytes)
        |  store ptr %buf, ptr %dp
        |  ret ptr %desc
        |}
        |
        |define void @__nex_arr1_inc(ptr %a) {
        |entry:
        |  %is_null = icmp eq ptr %a, null
        |  br i1 %is_null, label %done, label %inc
        |inc:
        |  %rcp = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 0
        |  %rc  = load i64, ptr %rcp
        |  %new = add i64 %rc, 1
        |  store i64 %new, ptr %rcp
        |  br label %done
        |done:
        |  ret void
        |}
        |
        |define void @__nex_arr1_dec(ptr %a) {
        |entry:
        |  %is_null = icmp eq ptr %a, null
        |  br i1 %is_null, label %done, label %dec
        |dec:
        |  %rcp = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 0
        |  %rc  = load i64, ptr %rcp
        |  %new = sub i64 %rc, 1
        |  store i64 %new, ptr %rcp
        |  %iz  = icmp eq i64 %new, 0
        |  br i1 %iz, label %free_it, label %done
        |free_it:
        |  %dp  = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 2
        |  %buf = load ptr, ptr %dp
        |  call void @free(ptr %buf)
        |  call void @free(ptr %a)
        |  br label %done
        |done:
        |  ret void
        |}
        |
        |; Returns the length of a rank-1 array (the `length` field).
        |define i64 @__nex_arr1_len(ptr %a) {
        |entry:
        |  %lp = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 1
        |  %l  = load i64, ptr %lp
        |  ret i64 %l
        |}
        |
        |; Returns a ptr to the i-th element slot for a rank-1 array of element
        |; size `elem_size` bytes. Bounds-checks against the array's length and
        |; aborts via @abort on overflow (after writing a trap message).
        |define ptr @__nex_arr1_slot(ptr %a, i64 %idx, i64 %elem_size) {
        |entry:
        |  %lp  = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 1
        |  %len = load i64, ptr %lp
        |  %lt  = icmp slt i64 %idx, 0
        |  %ge  = icmp sge i64 %idx, %len
        |  %bad = or i1 %lt, %ge
        |  br i1 %bad, label %trap, label %ok
        |trap:
        |  call i32 (ptr, ...) @printf(ptr @.oob_msg)
        |  call void @abort()
        |  unreachable
        |ok:
        |  %dp   = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 2
        |  %buf  = load ptr, ptr %dp
        |  %byte_off = mul i64 %idx, %elem_size
        |  %slot = getelementptr inbounds i8, ptr %buf, i64 %byte_off
        |  ret ptr %slot
        |}
        |
        |; --- Rank-2 runtime helpers -------------------------------------------
        |
        |define ptr @__nex_arr2_alloc(i64 %rows, i64 %cols, i64 %elem_size) {
        |entry:
        |  %desc = call ptr @malloc(i64 32)
        |  %rcp  = getelementptr inbounds %nex_arr2, ptr %desc, i32 0, i32 0
        |  store i64 1, ptr %rcp
        |  %rp   = getelementptr inbounds %nex_arr2, ptr %desc, i32 0, i32 1
        |  store i64 %rows, ptr %rp
        |  %cp   = getelementptr inbounds %nex_arr2, ptr %desc, i32 0, i32 2
        |  store i64 %cols, ptr %cp
        |  %total = mul i64 %rows, %cols
        |  %bytes = mul i64 %total, %elem_size
        |  %buf   = call ptr @malloc(i64 %bytes)
        |  %dp    = getelementptr inbounds %nex_arr2, ptr %desc, i32 0, i32 3
        |  store ptr %buf, ptr %dp
        |  ret ptr %desc
        |}
        |
        |define void @__nex_arr2_inc(ptr %a) {
        |entry:
        |  %is_null = icmp eq ptr %a, null
        |  br i1 %is_null, label %done, label %inc
        |inc:
        |  %rcp = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 0
        |  %rc  = load i64, ptr %rcp
        |  %new = add i64 %rc, 1
        |  store i64 %new, ptr %rcp
        |  br label %done
        |done:
        |  ret void
        |}
        |
        |define void @__nex_arr2_dec(ptr %a) {
        |entry:
        |  %is_null = icmp eq ptr %a, null
        |  br i1 %is_null, label %done, label %dec
        |dec:
        |  %rcp = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 0
        |  %rc  = load i64, ptr %rcp
        |  %new = sub i64 %rc, 1
        |  store i64 %new, ptr %rcp
        |  %iz  = icmp eq i64 %new, 0
        |  br i1 %iz, label %free_it, label %done
        |free_it:
        |  %dp  = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 3
        |  %buf = load ptr, ptr %dp
        |  call void @free(ptr %buf)
        |  call void @free(ptr %a)
        |  br label %done
        |done:
        |  ret void
        |}
        |
        |define i64 @__nex_arr2_rows(ptr %a) {
        |entry:
        |  %rp = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 1
        |  %r  = load i64, ptr %rp
        |  ret i64 %r
        |}
        |
        |define i64 @__nex_arr2_cols(ptr %a) {
        |entry:
        |  %cp = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 2
        |  %c  = load i64, ptr %cp
        |  ret i64 %c
        |}
        |
        |define i64 @__nex_arr2_len(ptr %a) {
        |entry:
        |  %rp = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 1
        |  %r  = load i64, ptr %rp
        |  %cp = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 2
        |  %c  = load i64, ptr %cp
        |  %t  = mul i64 %r, %c
        |  ret i64 %t
        |}
        |
        |define ptr @__nex_arr2_slot(ptr %a, i64 %i, i64 %j, i64 %elem_size) {
        |entry:
        |  %rp = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 1
        |  %r  = load i64, ptr %rp
        |  %cp = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 2
        |  %c  = load i64, ptr %cp
        |  %ilt = icmp slt i64 %i, 0
        |  %ige = icmp sge i64 %i, %r
        |  %ibad = or i1 %ilt, %ige
        |  %jlt = icmp slt i64 %j, 0
        |  %jge = icmp sge i64 %j, %c
        |  %jbad = or i1 %jlt, %jge
        |  %bad = or i1 %ibad, %jbad
        |  br i1 %bad, label %trap, label %ok
        |trap:
        |  call i32 (ptr, ...) @printf(ptr @.oob_msg)
        |  call void @abort()
        |  unreachable
        |ok:
        |  %flat = mul i64 %i, %c
        |  %idx  = add i64 %flat, %j
        |  %dp   = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 3
        |  %buf  = load ptr, ptr %dp
        |  %byte_off = mul i64 %idx, %elem_size
        |  %slot = getelementptr inbounds i8, ptr %buf, i64 %byte_off
        |  ret ptr %slot
        |}
        |
        |; Returns a ptr to the k-th flat element (k in 0..rows*cols-1).
        |define ptr @__nex_arr2_flat_slot(ptr %a, i64 %k, i64 %elem_size) {
        |entry:
        |  %dp  = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 3
        |  %buf = load ptr, ptr %dp
        |  %byte_off = mul i64 %k, %elem_size
        |  %slot = getelementptr inbounds i8, ptr %buf, i64 %byte_off
        |  ret ptr %slot
        |}
        |
        |""".stripMargin,
    )

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

  private def emitExpr(e: TExpr): String = e match
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

    case TVarRef(s, _, t) =>
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
        case other =>
          notYet(s"call to ${other.getClass.getSimpleName}"); "0"

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

  // ---------------------------------------------------------------------------
  // Arrays — §8.5 ARC descriptors. Rank-1 uses %nex_arr1; rank-2 uses
  // %nex_arr2. Element buffer layout is row-major (matches the interpreter's
  // VArray2 flat buf). Each emitted helper takes / returns a `ptr` to the
  // descriptor.
  // ---------------------------------------------------------------------------

  /** Lower a `TArrayLit` to an alloc + per-element store. The result is a
    * fresh array with refcount = 1 (set by the runtime alloc helper).
    *
    * Rank-1 case: every elem evaluates to a scalar, store into `data[i]`.
    *
    * Rank-2 case: outer `TArrayLit` is a list of inner `TArrayLit`s of the
    * same length. Rows = outer length; cols = inner length. The flat buffer
    * is filled row-major: `data[i*cols + j] = elems(i).elems(j)`.
    */
  private def emitArrayLit(elems: List[TExpr], t: Type): String =
    val rank   = arrayRank(t)
    val elem   = arrayElem(t)
    val esz    = elemSize(elem)
    val stType = storageType(elem)

    rank match
      case 1 =>
        val len  = elems.size
        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $len, i64 $esz)\n")
        if elems.nonEmpty then
          val dpr = newReg()
          emitLine(s"  $dpr = getelementptr inbounds %nex_arr1, ptr $desc, i32 0, i32 2\n")
          val buf = newReg()
          emitLine(s"  $buf = load ptr, ptr $dpr\n")
          for (e, i) <- elems.zipWithIndex do
            val v    = emitExpr(e)
            val slot = newReg()
            emitLine(s"  $slot = getelementptr inbounds $stType, ptr $buf, i64 $i\n")
            storeElem(stType, v, slot)
        desc

      case 2 =>
        // Validate inner shape — every elem should itself be a rank-1
        // `TArrayLit` of common length. The elaborator's type inference
        // accepts this shape; surface a clear diag if something else slipped
        // through.
        val rowExprs = elems.collect { case TArrayLit(inner, _, _) => inner }
        if rowExprs.size != elems.size then
          notYet("rank-2 array literal with non-literal rows")
          return "null"
        val rows = elems.size
        val cols = if rows == 0 then 0 else rowExprs.head.size

        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr2_alloc(i64 $rows, i64 $cols, i64 $esz)\n")
        if rows > 0 && cols > 0 then
          val dpr = newReg()
          emitLine(s"  $dpr = getelementptr inbounds %nex_arr2, ptr $desc, i32 0, i32 3\n")
          val buf = newReg()
          emitLine(s"  $buf = load ptr, ptr $dpr\n")
          for (row, i) <- rowExprs.zipWithIndex do
            for (e, j) <- row.zipWithIndex do
              val v       = emitExpr(e)
              val flatIdx = i * cols + j
              val slot    = newReg()
              emitLine(s"  $slot = getelementptr inbounds $stType, ptr $buf, i64 $flatIdx\n")
              storeElem(stType, v, slot)
        desc

      case other =>
        notYet(s"array literal of rank $other"); "null"

  /** Lower `arr[i]` (rank-1) or `arr[i, j]` (rank-2) to a slot-fetch via the
    * runtime helper and a load of the stored type. Bounds checks are inside
    * `__nex_arr*_slot` and abort on overflow.
    */
  private def emitIndex(arr: TExpr, indices: List[TExpr], resultT: Type): String =
    val rank  = arrayRank(arr.tpe)
    val elem  = arrayElem(arr.tpe)
    val esz   = elemSize(elem)
    val stT   = storageType(elem)
    val arrV  = emitExpr(arr)

    val result = (rank, indices) match
      case (1, List(idx)) =>
        val iv   = emitExpr(idx)
        val slot = newReg()
        emitLine(s"  $slot = call ptr @__nex_arr1_slot(ptr $arrV, i64 $iv, i64 $esz)\n")
        loadElem(stT, slot, llvmType(elem))
      case (2, List(i, j)) =>
        val iv   = emitExpr(i)
        val jv   = emitExpr(j)
        val slot = newReg()
        emitLine(s"  $slot = call ptr @__nex_arr2_slot(ptr $arrV, i64 $iv, i64 $jv, i64 $esz)\n")
        loadElem(stT, slot, llvmType(elem))
      case (r, ixs) =>
        notYet(s"index of rank $r with ${ixs.size} indices"); "0"
    // If the element is itself an array (nested), the loaded value needs
    // an inc since we shared it out of the slot — but v0 doesn't support
    // nested arrays via TArrayLit, so skip for now.
    emitArrDec(arrV, arr.tpe)
    result

  /** Store a value of LLVM type `t` (the *language* type) into a buffer slot
    * whose stored type is `storageT`. For bool, the i1 value is widened to
    * an i8 on store. Otherwise this is a straight `store T v, ptr slot`.
    */
  private def storeElem(storageT: String, value: String, slot: String): Unit =
    storageT match
      case "i8" =>
        // bool: widen i1 → i8 before storing
        val widened = newReg()
        emitLine(s"  $widened = zext i1 $value to i8\n")
        emitLine(s"  store i8 $widened, ptr $slot\n")
      case t =>
        emitLine(s"  store $t $value, ptr $slot\n")

  /** Load from a buffer slot whose stored type is `storageT`, producing a
    * value of language type `langT`. For bool, the i8 is truncated to i1.
    */
  private def loadElem(storageT: String, slot: String, langT: String): String =
    storageT match
      case "i8" =>
        val raw = newReg()
        emitLine(s"  $raw = load i8, ptr $slot\n")
        val out = newReg()
        emitLine(s"  $out = icmp ne i8 $raw, 0\n")
        out
      case t =>
        val r = newReg()
        emitLine(s"  $r = load $t, ptr $slot\n")
        r

  // ---------------------------------------------------------------------------
  // Prelude calls. The elaborator marks built-ins with [[SymKind.Prelude]];
  // the unrecognised-name path falls back to a `notYet` diag and a zero so
  // the rest of the IR still parses. Special-cased here:
  //   - `length(arr)` / `rows(arr)` / `cols(arr)`
  // Other prelude functions (sqrt, sin, map, reduce, ...) will land in
  // later chunks.
  // ---------------------------------------------------------------------------

  private def emitPreludeCall(name: String, args: List[TExpr], resultT: Type): String =
    (name, args) match
      case ("length", List(a)) => emitArrayLengthish(a, "len")
      case ("rows",   List(a)) => emitArrayLengthish(a, "rows")
      case ("cols",   List(a)) => emitArrayLengthish(a, "cols")
      case _ =>
        notYet(s"prelude `$name`/${args.size}"); "0"

  /** Emit a length-ish call (length / rows / cols). The runtime helper
    * picked depends on the array's static rank.
    *
    *   - rank-1 + `len`  → `__nex_arr1_len`
    *   - rank-1 + `rows` → 1 (no helper call needed)
    *   - rank-1 + `cols` → length
    *   - rank-2 + `len`  → `rows * cols`
    *   - rank-2 + `rows` → `__nex_arr2_rows`
    *   - rank-2 + `cols` → `__nex_arr2_cols`
    */
  private def emitArrayLengthish(arr: TExpr, which: String): String =
    val rank = arrayRank(arr.tpe)
    val av   = emitExpr(arr)
    val r    = newReg()
    val result = (rank, which) match
      case (1, "len" | "cols") =>
        emitLine(s"  $r = call i64 @__nex_arr1_len(ptr $av)\n"); r
      case (1, "rows") =>
        "1"
      case (2, "rows") | (2, "len") =>
        // length(rank-2) follows the interpreter convention: it returns
        // the row count, not rows*cols. (The flat element count is
        // available internally as __nex_arr2_len for codegen helpers.)
        emitLine(s"  $r = call i64 @__nex_arr2_rows(ptr $av)\n"); r
      case (2, "cols") =>
        emitLine(s"  $r = call i64 @__nex_arr2_cols(ptr $av)\n"); r
      case _ =>
        notYet(s"$which for rank $rank"); "0"
    // Release the owning share we took on the input array.
    emitArrDec(av, arr.tpe)
    result

  // ---------------------------------------------------------------------------
  // Element-wise ops, broadcasts, slices, clone, flat index, fused loop.
  // ---------------------------------------------------------------------------

  /** Open an iteration loop over `0..len` with a fresh counter slot and
    * call `genBody(i)` inside the body block. `genBody` receives the
    * SSA register for the current counter value and is responsible for
    * emitting all of the body's instructions before returning. The
    * returned exit label is left as the current block.
    *
    * Used by [[emitElementWise]] / [[emitBroadcast]] / slice / clone /
    * print-array — every per-element loop has the same shape.
    */
  private def emitCountingLoop(len: String, prefix: String)(genBody: String => Unit): Unit =
    val iSlot = newReg()
    emitLine(s"  $iSlot = alloca i64\n")
    emitLine(s"  store i64 0, ptr $iSlot\n")
    val condL = freshLabel(s"$prefix.cond")
    val bodyL = freshLabel(s"$prefix.body")
    val exitL = freshLabel(s"$prefix.exit")
    emitTerminator(s"  br label %$condL\n")
    startBlock(condL)
    val cur = newReg()
    emitLine(s"  $cur = load i64, ptr $iSlot\n")
    val ok  = newReg()
    emitLine(s"  $ok = icmp slt i64 $cur, $len\n")
    emitTerminator(s"  br i1 $ok, label %$bodyL, label %$exitL\n")
    startBlock(bodyL)
    genBody(cur)
    if currentBlock.isDefined then
      val nx = newReg()
      emitLine(s"  $nx = add i64 $cur, 1\n")
      emitLine(s"  store i64 $nx, ptr $iSlot\n")
      emitTerminator(s"  br label %$condL\n")
    startBlock(exitL)

  /** Emit a `length` lookup for either rank of array via the runtime
    * helpers. For rank-2 this returns the FLAT (rows*cols) element count
    * — different from [[emitArrayLengthish]]("len") which follows the
    * interpreter's "length = rows" convention.
    */
  private def flatLengthOf(arrV: String, t: Type): String =
    val r = newReg()
    arrayRank(t) match
      case 1 => emitLine(s"  $r = call i64 @__nex_arr1_len(ptr $arrV)\n"); r
      case 2 => emitLine(s"  $r = call i64 @__nex_arr2_len(ptr $arrV)\n"); r
      case other => notYet(s"flat length of rank $other"); "0"

  /** Allocate a fresh array shaped like `model` (same rank, same
    * dimensions). Returns the new descriptor's SSA value, with
    * `refcount = 1`. Element size comes from the result element type
    * (which may differ from the model's, e.g. a relational op result
    * has bool elements over reals).
    */
  private def allocLike(model: String, modelT: Type, resultElem: Type): String =
    val esz = elemSize(resultElem)
    val desc = newReg()
    arrayRank(modelT) match
      case 1 =>
        val lenR = newReg()
        emitLine(s"  $lenR = call i64 @__nex_arr1_len(ptr $model)\n")
        emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $lenR, i64 $esz)\n")
      case 2 =>
        val rowsR = newReg()
        emitLine(s"  $rowsR = call i64 @__nex_arr2_rows(ptr $model)\n")
        val colsR = newReg()
        emitLine(s"  $colsR = call i64 @__nex_arr2_cols(ptr $model)\n")
        emitLine(s"  $desc = call ptr @__nex_arr2_alloc(i64 $rowsR, i64 $colsR, i64 $esz)\n")
      case other => notYet(s"alloc-like of rank $other")
    desc

  /** Return a pointer to the flat element buffer of an array descriptor. */
  private def bufPtr(desc: String, t: Type): String =
    val r = newReg()
    arrayRank(t) match
      case 1 =>
        val dp = newReg()
        emitLine(s"  $dp = getelementptr inbounds %nex_arr1, ptr $desc, i32 0, i32 2\n")
        emitLine(s"  $r = load ptr, ptr $dp\n")
        r
      case 2 =>
        val dp = newReg()
        emitLine(s"  $dp = getelementptr inbounds %nex_arr2, ptr $desc, i32 0, i32 3\n")
        emitLine(s"  $r = load ptr, ptr $dp\n")
        r
      case other => notYet(s"buf ptr of rank $other"); "null"

  /** Emit `op` between two scalar values of the same Nex type, returning
    * the SSA register of the result. Reuses the existing [[binOpInst]]
    * table. Result type is implied by the operands (same as the
    * elementwise spec — Stage 2 lifts only valid op/operand combos).
    */
  private def emitScalarBinOp(op: String, lv: String, rv: String, opT: Type): String =
    op match
      case "and" | "or" =>
        // For element-wise paths these get evaluated eagerly (no
        // short-circuit possible at element level).
        val reg = newReg()
        val instr = if op == "and" then "and" else "or"
        emitLine(s"  $reg = $instr i1 $lv, $rv\n")
        reg
      case _ =>
        val (instr, _) = binOpInst(op, opT)
        val reg = newReg()
        emitLine(s"  $reg = $instr ${llvmType(opT)} $lv, $rv\n")
        reg

  /** Lower `lhs ⊙ rhs` element-wise when both sides are array-typed.
    * Allocates a fresh result of the same shape, iterates the flat
    * buffer, applies the scalar op per element. Both operands are
    * dec'd before returning the fresh array.
    */
  private def emitElementWise(op: String, lhs: TExpr, rhs: TExpr, resultT: Type): String =
    val elemL  = arrayElem(lhs.tpe)
    val elemR  = arrayElem(rhs.tpe)
    val resE   = arrayElem(resultT)
    val stL    = storageType(elemL)
    val stR    = storageType(elemR)
    val stRes  = storageType(resE)
    val langL  = llvmType(elemL)
    val langR  = llvmType(elemR)
    val esRes  = elemSize(resE)

    val lv = emitExpr(lhs)
    val rv = emitExpr(rhs)

    val desc = allocLike(lv, lhs.tpe, resE)
    val lenR = flatLengthOf(lv, lhs.tpe)
    val lBuf = bufPtr(lv, lhs.tpe)
    val rBuf = bufPtr(rv, rhs.tpe)
    val oBuf = bufPtr(desc, resultT)

    emitCountingLoop(lenR, "ew") { i =>
      val lSlot = newReg()
      emitLine(s"  $lSlot = getelementptr inbounds $stL, ptr $lBuf, i64 $i\n")
      val ll = loadElem(stL, lSlot, langL)
      val rSlot = newReg()
      emitLine(s"  $rSlot = getelementptr inbounds $stR, ptr $rBuf, i64 $i\n")
      val rr = loadElem(stR, rSlot, langR)
      val out = emitScalarBinOp(op, ll, rr, elemL)
      val oSlot = newReg()
      emitLine(s"  $oSlot = getelementptr inbounds $stRes, ptr $oBuf, i64 $i\n")
      storeElem(stRes, out, oSlot)
    }

    emitArrDec(lv, lhs.tpe)
    emitArrDec(rv, rhs.tpe)
    desc

  /** Lower a scalar × array (or array × scalar) broadcast. */
  private def emitBroadcast(scalar: TExpr, arr: TExpr, op: String, scalarFirst: Boolean, resultT: Type): String =
    val elem  = arrayElem(arr.tpe)
    val resE  = arrayElem(resultT)
    val stE   = storageType(elem)
    val stRes = storageType(resE)
    val langE = llvmType(elem)

    val sv = emitExpr(scalar)
    val av = emitExpr(arr)

    val desc = allocLike(av, arr.tpe, resE)
    val lenR = flatLengthOf(av, arr.tpe)
    val aBuf = bufPtr(av, arr.tpe)
    val oBuf = bufPtr(desc, resultT)

    emitCountingLoop(lenR, "bc") { i =>
      val aSlot = newReg()
      emitLine(s"  $aSlot = getelementptr inbounds $stE, ptr $aBuf, i64 $i\n")
      val e = loadElem(stE, aSlot, langE)
      val (l, r) = if scalarFirst then (sv, e) else (e, sv)
      val out = emitScalarBinOp(op, l, r, elem)
      val oSlot = newReg()
      emitLine(s"  $oSlot = getelementptr inbounds $stRes, ptr $oBuf, i64 $i\n")
      storeElem(stRes, out, oSlot)
    }

    emitArrDec(av, arr.tpe)
    desc

  /** Lower `arr[lo..hi]` / `arr[lo..=hi]` (rank-1) to a fresh array of
    * `hi-lo` (or `hi-lo+1`) elements copied from the source.
    */
  private def emitSlice(arr: TExpr, lo: TExpr, hi: TExpr, inclusive: Boolean, resultT: Type): String =
    val elem = arrayElem(arr.tpe)
    val stE  = storageType(elem)
    val langE = llvmType(elem)
    val esz  = elemSize(elem)

    val av  = emitExpr(arr)
    val loV = emitExpr(lo)
    val hiV = emitExpr(hi)

    // Slice length: hi - lo (exclusive) or hi - lo + 1 (inclusive).
    val rawLen = newReg()
    emitLine(s"  $rawLen = sub i64 $hiV, $loV\n")
    val length = if inclusive then
      val r = newReg()
      emitLine(s"  $r = add i64 $rawLen, 1\n")
      r
    else rawLen

    val desc = newReg()
    emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $length, i64 $esz)\n")
    val srcBuf = bufPtr(av, arr.tpe)
    val outBuf = bufPtr(desc, resultT)

    emitCountingLoop(length, "slice1") { i =>
      val srcIdx = newReg()
      emitLine(s"  $srcIdx = add i64 $loV, $i\n")
      val sSlot = newReg()
      emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $srcIdx\n")
      val v = loadElem(stE, sSlot, langE)
      val oSlot = newReg()
      emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $i\n")
      storeElem(stE, v, oSlot)
    }

    emitArrDec(av, arr.tpe)
    desc

  /** Lower a rank-2 axis-spec slice. The result rank depends on how
    * many axes are preserved; rank-0 (scalar) currently surfaces a diag.
    */
  private def emitSlice2(arr: TExpr, rowAx: TAxisSpec, colAx: TAxisSpec, resultT: Type): String =
    // Compute the (loR, hiR, isRangeR) for the row axis, similar for col.
    val elem = arrayElem(arr.tpe)
    val stE  = storageType(elem)
    val langE = llvmType(elem)
    val esz  = elemSize(elem)

    val av = emitExpr(arr)
    val rowsAll = newReg()
    emitLine(s"  $rowsAll = call i64 @__nex_arr2_rows(ptr $av)\n")
    val colsAll = newReg()
    emitLine(s"  $colsAll = call i64 @__nex_arr2_cols(ptr $av)\n")

    // For each axis: (loStart, loEnd, preserved?, isSingleton?).
    // `preserved` means this axis contributes to the result rank.
    def axis(spec: TAxisSpec, total: String): (String, String, Boolean, Option[String]) = spec match
      case TAxisAll => ("0", total, true, None)
      case TAxisIndex(idx) =>
        val iv = emitExpr(idx)
        val hi = newReg()
        emitLine(s"  $hi = add i64 $iv, 1\n")
        (iv, hi, false, Some(iv))
      case TAxisRange(lo, hi, inclusive) =>
        val loV = emitExpr(lo)
        val hiV = emitExpr(hi)
        val end = if inclusive then
          val r = newReg(); emitLine(s"  $r = add i64 $hiV, 1\n"); r
        else hiV
        (loV, end, true, None)

    val (rLo, rEnd, rPres, _) = axis(rowAx, rowsAll)
    val (cLo, cEnd, cPres, _) = axis(colAx, colsAll)
    val rLen = newReg()
    emitLine(s"  $rLen = sub i64 $rEnd, $rLo\n")
    val cLen = newReg()
    emitLine(s"  $cLen = sub i64 $cEnd, $cLo\n")

    val srcBuf = bufPtr(av, arr.tpe)

    val result = (rPres, cPres) match
      case (true, true) =>
        // Both axes preserved → rank-2 result.
        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr2_alloc(i64 $rLen, i64 $cLen, i64 $esz)\n")
        val outBuf = bufPtr(desc, resultT)
        emitCountingLoop(rLen, "slice2r") { i =>
          emitCountingLoop(cLen, "slice2c") { j =>
            val sr = newReg(); emitLine(s"  $sr = add i64 $rLo, $i\n")
            val sc = newReg(); emitLine(s"  $sc = add i64 $cLo, $j\n")
            val srcFlat = newReg(); emitLine(s"  $srcFlat = mul i64 $sr, $colsAll\n")
            val srcIdx  = newReg(); emitLine(s"  $srcIdx = add i64 $srcFlat, $sc\n")
            val sSlot = newReg()
            emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $srcIdx\n")
            val v = loadElem(stE, sSlot, langE)
            val outFlat = newReg(); emitLine(s"  $outFlat = mul i64 $i, $cLen\n")
            val outIdx  = newReg(); emitLine(s"  $outIdx = add i64 $outFlat, $j\n")
            val oSlot = newReg()
            emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $outIdx\n")
            storeElem(stE, v, oSlot)
          }
        }
        desc

      case (true, false) =>
        // Row axis preserved, col collapsed → rank-1 of rLen elements.
        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $rLen, i64 $esz)\n")
        val outBuf = bufPtr(desc, resultT)
        emitCountingLoop(rLen, "slice2rc") { i =>
          val sr = newReg(); emitLine(s"  $sr = add i64 $rLo, $i\n")
          val sFlat = newReg(); emitLine(s"  $sFlat = mul i64 $sr, $colsAll\n")
          val sIdx  = newReg(); emitLine(s"  $sIdx = add i64 $sFlat, $cLo\n")
          val sSlot = newReg()
          emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $sIdx\n")
          val v = loadElem(stE, sSlot, langE)
          val oSlot = newReg()
          emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $i\n")
          storeElem(stE, v, oSlot)
        }
        desc

      case (false, true) =>
        // Col axis preserved, row collapsed → rank-1 of cLen elements.
        val desc = newReg()
        emitLine(s"  $desc = call ptr @__nex_arr1_alloc(i64 $cLen, i64 $esz)\n")
        val outBuf = bufPtr(desc, resultT)
        emitCountingLoop(cLen, "slice2cr") { j =>
          val sc = newReg(); emitLine(s"  $sc = add i64 $cLo, $j\n")
          val sFlat = newReg(); emitLine(s"  $sFlat = mul i64 $rLo, $colsAll\n")
          val sIdx  = newReg(); emitLine(s"  $sIdx = add i64 $sFlat, $sc\n")
          val sSlot = newReg()
          emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $srcBuf, i64 $sIdx\n")
          val v = loadElem(stE, sSlot, langE)
          val oSlot = newReg()
          emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $j\n")
          storeElem(stE, v, oSlot)
        }
        desc

      case (false, false) =>
        // Both axes collapsed → scalar; should have been TIndex, not TSlice2.
        notYet("rank-2 slice with both axes collapsed")
        "null"

    emitArrDec(av, arr.tpe)
    result

  /** Lower [[TClone]] — deep-copy the source descriptor and its buffer
    * into a fresh allocation. Source's owning share is released.
    */
  private def emitClone(arr: TExpr, resultT: Type): String =
    val elem = arrayElem(arr.tpe)
    val stE  = storageType(elem)
    val langE = llvmType(elem)
    val esz  = elemSize(elem)

    val src   = emitExpr(arr)
    val desc  = allocLike(src, arr.tpe, elem)
    val len   = flatLengthOf(src, arr.tpe)
    val sBuf  = bufPtr(src, arr.tpe)
    val oBuf  = bufPtr(desc, resultT)

    emitCountingLoop(len, "clone") { i =>
      val sSlot = newReg()
      emitLine(s"  $sSlot = getelementptr inbounds $stE, ptr $sBuf, i64 $i\n")
      val v = loadElem(stE, sSlot, langE)
      val oSlot = newReg()
      emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $oBuf, i64 $i\n")
      storeElem(stE, v, oSlot)
    }

    emitArrDec(src, arr.tpe)
    desc

  /** Lower [[TFlatIndex]] — single flat-index access regardless of rank.
    * Used inside [[TFusedLoop]] bodies. Result is the loaded element
    * (a scalar of the array's element type).
    */
  private def emitFlatIndex(arr: TExpr, idx: TExpr, resultT: Type): String =
    val elem = arrayElem(arr.tpe)
    val esz  = elemSize(elem)
    val stE  = storageType(elem)
    val langE = llvmType(elem)
    val av   = emitExpr(arr)
    val iv   = emitExpr(idx)
    val slot = newReg()
    arrayRank(arr.tpe) match
      case 1 => emitLine(s"  $slot = call ptr @__nex_arr1_slot(ptr $av, i64 $iv, i64 $esz)\n")
      case 2 => emitLine(s"  $slot = call ptr @__nex_arr2_flat_slot(ptr $av, i64 $iv, i64 $esz)\n")
      case other => notYet(s"flat index of rank $other"); return "0"
    val v = loadElem(stE, slot, langE)
    emitArrDec(av, arr.tpe)
    v

  /** Lower [[TFusedLoop]] — the fusion-pass output. Allocates a fresh
    * array of `length` elements, then iterates 0..length binding
    * `loopVar` to the flat index and storing `body` into the buffer.
    * `cols=None` → rank-1; `cols=Some(c)` → rank-2 with rows = length/c.
    */
  private def emitFusedLoop(loopVar: Symbol, length: TExpr, body: TExpr, cols: Option[TExpr], resultT: Type): String =
    val resElem = arrayElem(resultT)
    val stE     = storageType(resElem)
    val esz     = elemSize(resElem)
    val len     = emitExpr(length)

    val desc = cols match
      case None =>
        val d = newReg()
        emitLine(s"  $d = call ptr @__nex_arr1_alloc(i64 $len, i64 $esz)\n")
        d
      case Some(cExpr) =>
        val c = emitExpr(cExpr)
        val rows = newReg()
        emitLine(s"  $rows = sdiv i64 $len, $c\n")
        val d = newReg()
        emitLine(s"  $d = call ptr @__nex_arr2_alloc(i64 $rows, i64 $c, i64 $esz)\n")
        d

    val outBuf = bufPtr(desc, resultT)

    // The loop variable is a per-function Symbol — register a fresh slot
    // for it so the body's TVarRef(loopVar) reads the current index.
    val ivSlot = newReg()
    emitLine(s"  $ivSlot = alloca i64\n")
    locals(loopVar.id) = ivSlot

    emitCountingLoop(len, "fused") { i =>
      emitLine(s"  store i64 $i, ptr $ivSlot\n")
      val v = emitExpr(body)
      val oSlot = newReg()
      emitLine(s"  $oSlot = getelementptr inbounds $stE, ptr $outBuf, i64 $i\n")
      storeElem(stE, v, oSlot)
    }
    desc

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

  /** Print a tuple value as `(a, b, c)` (no trailing newline). Used by
    * both the top-level print path and the interpolated-string element
    * path. The actual loop lives in [[emitPrintTupleValue]] so it can
    * be re-entered for tuple-of-tuple from inside [[emitPrintArrayElem]].
    */
  private def emitPrintTuple(arg: TExpr): Unit =
    val v   = emitExpr(arg)
    val tup = arg.tpe match
      case TyTuple(es) => es
      case other       =>
        notYet(s"print tuple — expected tuple type, got $other")
        return
    emitPrintTupleValue(v, arg.tpe, tup)

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

  /** Print a struct value as `Name { field=value, field=value }` (no
    * trailing newline). Used by the print path and tuple/array element
    * recursion.
    */
  private def emitPrintStructValue(v: String, t: Type, name: String, fields: List[(String, Type)]): Unit =
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

  private def emitPrintStruct(arg: TExpr): Unit =
    val v = emitExpr(arg)
    arg.tpe match
      case TyStruct(name, fields) => emitPrintStructValue(v, arg.tpe, name, fields)
      case other                  =>
        notYet(s"print struct — expected struct type, got $other")

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
  // `print(...)` — overloaded across integer / real / bool. Threads through
  // a single libc `printf` with the right format-string global.
  // ---------------------------------------------------------------------------

  private def emitPrintCall(arg: TExpr): Unit =
    arg match
      // Special-case interpolated strings: emit a printf for each part
      // and a trailing newline. Avoids needing an in-memory string
      // builder until we ship real string ops.
      case TInterpStringLit(parts, _, _) =>
        for p <- parts do p match
          case TInterpText(text) =>
            val ptr = internStringLiteral(text)
            emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $ptr)\n")
          case TInterpRef(sym) =>
            emitPrintValue(TVarRef(sym, None, sym.tpe))
          case TInterpExpr(e) =>
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
                emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str, ptr $v)\n")
              case other =>
                notYet(s"print(${other})")

  /** Emits a value-printing printf WITHOUT a trailing newline. Used by
    * the interpolated-string print path so each interpolated part lands
    * inline with the surrounding text.
    */
  private def emitPrintValue(arg: TExpr): Unit =
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
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $v)\n")
      case TyArray(_, _) =>
        emitPrintArray(arg)
      case TyTuple(_) =>
        emitPrintTuple(arg)
      case TyStruct(_, _) =>
        emitPrintStruct(arg)
      case other =>
        notYet(s"interpolated print($other)")

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
  private def emitPrintArray(arr: TExpr): Unit =
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
  private def emitPrintArr1Inline(arrV: String, elem: Type, esz: Int, stT: String, langT: String): Unit =
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
  private def emitPrintArrayElem(elem: Type, v: String): Unit =
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
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_str_raw, ptr $v)\n")
      case t @ TyTuple(es) =>
        // Inline tuple-element print using insertvalue/extractvalue.
        // Reuses emitPrintArrayElem recursively for each field.
        emitPrintTupleValue(v, t, es)
      case t @ TyStruct(name, fields) =>
        emitPrintStructValue(v, t, name, fields)
      case other =>
        notYet(s"print element of type $other")

  /** Print a tuple SSA value (struct aggregate) as `(a, b, c)` without
    * a trailing newline. Factored from [[emitPrintTuple]] so callers
    * holding an already-loaded struct (e.g., array-of-tuple element
    * printing) can reuse it.
    */
  private def emitPrintTupleValue(v: String, t: Type, es: List[Type]): Unit =
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

  // ---------------------------------------------------------------------------
  // Type and binop tables.
  // ---------------------------------------------------------------------------

  /** Map a Nex type to its LLVM type. Tuples lower to anonymous LLVM
    * struct types literal-style (`{ T0, T1, ... }`); no separate `%name
    * = type ...` registration is needed because LLVM accepts the
    * literal at every use site.
    */
  private def llvmType(t: Type): String = t match
    case TyInteger      => "i64"
    case TyReal         => "double"
    case TyBool         => "i1"
    case TyUnit         => "void"
    case TyString       => "ptr"
    case TyArray(_,_)   => "ptr"
    case TyTuple(elems)        => elems.map(llvmType).mkString("{ ", ", ", " }")
    case TyStruct(_, fields)   => fields.map(f => llvmType(f._2)).mkString("{ ", ", ", " }")
    case TyUnknown      => "i64" // best-effort placeholder for missing inference
    case other          => notYet(s"type `$other`"); "i64"

  /** Storage type for an array element. `i1` (bool) is stored as `i8` so the
    * buffer's stride is one byte per element rather than packed bits.
    * Everything else uses its natural LLVM type.
    */
  private def storageType(elem: Type): String = elem match
    case TyBool => "i8"
    case other  => llvmType(other)

  /** Size in bytes of one element in an array buffer (matches
    * [[storageType]]). For aggregates (tuples, structs) the size is
    * the sum of field sizes — this assumes every field is naturally
    * 8-byte aligned, which holds for v0's mix of integer/real/ptr/bool
    * fields (bool fields are stored as i8 inside arrays but as i1
    * inside tuples/structs; we round up to 8 for aggregates to keep
    * the alignment story simple).
    */
  private def elemSize(elem: Type): Int = elem match
    case TyInteger        => 8
    case TyReal           => 8
    case TyBool           => 1
    case TyString         => 8
    case TyArray(_, _)    => 8 // ptr to descriptor
    case TyTuple(es)      => es.map(aggregateFieldSize).sum
    case TyStruct(_, fs)  => fs.map(f => aggregateFieldSize(f._2)).sum
    case _                => 8

  /** Field-of-aggregate size: scalars and pointers are 8 bytes; bools
    * inside aggregates are 1 byte rounded to 8 for alignment; nested
    * aggregates contribute their own elemSize.
    */
  private def aggregateFieldSize(t: Type): Int = t match
    case TyBool => 8 // padded
    case other  => elemSize(other)

  /** Element type of an array Type; emits a diag and returns TyInteger when
    * the given type isn't a TyArray (which would mean the elaborator left the
    * type stage-1 — should be rare by Stage 3).
    */
  private def arrayElem(t: Type): Type = t match
    case TyArray(e, _) => e
    case _             => notYet(s"expected array type, got $t"); TyInteger

  private def arrayRank(t: Type): Int = t match
    case TyArray(_, r) => r
    case _             => 1

  private def isArrayType(t: Type): Boolean = t match
    case TyArray(_, _) => true
    case _             => false

  /** Pick the right ARC inc helper based on the array's static rank. */
  private def arrIncFor(t: Type): String = arrayRank(t) match
    case 1 => "@__nex_arr1_inc"
    case 2 => "@__nex_arr2_inc"
    case _ => "@__nex_arr1_inc"

  /** Pick the right ARC dec helper based on the array's static rank. */
  private def arrDecFor(t: Type): String = arrayRank(t) match
    case 1 => "@__nex_arr1_dec"
    case 2 => "@__nex_arr2_dec"
    case _ => "@__nex_arr1_dec"

  /** Emit `call void @__nex_arr*_inc(ptr value)`. No-op (silently skipped)
    * if no block is open or if [[t]] isn't an array type.
    */
  private def emitArrInc(value: String, t: Type): Unit =
    if isArrayType(t) then
      emitLine(s"  call void ${arrIncFor(t)}(ptr $value)\n")

  /** Emit `call void @__nex_arr*_dec(ptr value)`. */
  private def emitArrDec(value: String, t: Type): Unit =
    if isArrayType(t) then
      emitLine(s"  call void ${arrDecFor(t)}(ptr $value)\n")

  /** Decrement-ref every slot registered as an array-typed local in the
    * current function — innermost block first, then the function-level
    * (param) slots last. Called right before each function-exit `ret`
    * (including early `return`s) so that nothing leaks regardless of
    * how deeply nested the return site is.
    */
  private def decAllLocalArrays(): Unit =
    if currentBlock.isDefined then
      for scope <- blockArrayScopes do decBlockScope(scope)
      for (id, (slot, t)) <- arrayLocalSlots do
        val v = newReg()
        emitLine(s"  $v = load ptr, ptr $slot\n")
        emitArrDec(v, t)

  /** Dec every array slot recorded in a single block scope. Used at
    * block end (after popping) and as a building block for
    * [[decAllLocalArrays]].
    */
  private def decBlockScope(scope: mutable.LinkedHashMap[Int, (String, Type)]): Unit =
    if currentBlock.isDefined then
      for (id, (slot, t)) <- scope do
        val v = newReg()
        emitLine(s"  $v = load ptr, ptr $slot\n")
        emitArrDec(v, t)

  /** Push a fresh block scope; subsequent array bindings emit into it. */
  private def pushBlockScope(): Unit =
    blockArrayScopes = mutable.LinkedHashMap.empty[Int, (String, Type)] :: blockArrayScopes

  /** Pop the innermost block scope and return it so the caller can dec
    * its entries at the block-exit position.
    */
  private def popBlockScope(): mutable.LinkedHashMap[Int, (String, Type)] =
    val top = blockArrayScopes.head
    blockArrayScopes = blockArrayScopes.tail
    top

  /** Register an array-typed binding's slot under the appropriate scope:
    * the innermost block if we're inside one, otherwise the function-
    * level [[arrayLocalSlots]] (which is the right home for params).
    */
  private def registerArraySlot(id: Int, slot: String, t: Type): Unit =
    blockArrayScopes match
      case head :: _ => head(id) = (slot, t)
      case Nil       => arrayLocalSlots(id) = (slot, t)

  /** Map a Nex binary operator + operand type to (LLVM instruction,
    * result LLVM type). Integer / real overloads are picked here.
    */
  private def binOpInst(op: String, opT: Type): (String, String) =
    (op, opT) match
      case ("+", TyInteger) => ("add",  "i64")
      case ("-", TyInteger) => ("sub",  "i64")
      case ("*", TyInteger) => ("mul",  "i64")
      case ("/", TyInteger) => ("sdiv", "i64") // unreachable on scalars; / promotes
      case ("div", TyInteger) => ("sdiv", "i64") // truncation toward zero (≠ floor)
      case ("%", TyInteger) => ("srem", "i64")
      case ("+", TyReal)    => ("fadd", "double")
      case ("-", TyReal)    => ("fsub", "double")
      case ("*", TyReal)    => ("fmul", "double")
      case ("/", TyReal)    => ("fdiv", "double")
      case ("==", TyInteger) => ("icmp eq",  "i1")
      case ("!=", TyInteger) => ("icmp ne",  "i1")
      case ("<",  TyInteger) => ("icmp slt", "i1")
      case ("<=", TyInteger) => ("icmp sle", "i1")
      case (">",  TyInteger) => ("icmp sgt", "i1")
      case (">=", TyInteger) => ("icmp sge", "i1")
      case ("==", TyReal) => ("fcmp oeq", "i1")
      case ("!=", TyReal) => ("fcmp one", "i1")
      case ("<",  TyReal) => ("fcmp olt", "i1")
      case ("<=", TyReal) => ("fcmp ole", "i1")
      case (">",  TyReal) => ("fcmp ogt", "i1")
      case (">=", TyReal) => ("fcmp oge", "i1")
      case _                => notYet(s"binop `$op` on `$opT`"); ("add", "i64")

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  private def newReg(): String =
    regCounter += 1
    s"%t$regCounter"

  private def freshLabel(prefix: String): String =
    labelCounter += 1
    s"$prefix.$labelCounter"

  /** Emit a single line of LLVM IR into the current basic block. Silently
    * dropped if no block is currently open (i.e., after a terminator).
    */
  private def emitLine(s: String): Unit =
    if currentBlock.isDefined then out.append(s)

  /** Emit a terminator instruction (br, ret, unreachable). Closes the
    * current block — subsequent [[emitLine]] calls become no-ops until
    * [[startBlock]] opens a new one. Silently dropped if already
    * terminated.
    */
  private def emitTerminator(s: String): Unit =
    if currentBlock.isDefined then
      out.append(s)
      currentBlock = None

  /** Open a new basic block with the given label. If the prior block is
    * still open (no terminator emitted), an implicit `br label %<label>`
    * fall-through is added before opening the new one — this keeps the
    * block-structure obligation invariant without requiring callers to
    * remember to close every dangling block.
    */
  private def startBlock(label: String): Unit =
    if currentBlock.isDefined then
      out.append(s"  br label %$label\n")
    out.append(s"$label:\n")
    currentBlock = Some(label)

  private def formatReal(d: Double): String =
    // LLVM IR requires double constants to be in hex form (`0x...`) for
    // exact bit reproduction. Java's Double.doubleToLongBits + %016x
    // gives the bit pattern; LLVM parses it with the `0x` prefix.
    f"0x${java.lang.Double.doubleToLongBits(d)}%016X"

  /** Record a "not yet supported" feature inline in the output so the
    * resulting `.ll` is still readable; the CLI surfaces a friendlier
    * top-level error. We don't throw because the test surface wants to
    * see partial output for diagnosis.
    */
  private def notYet(what: String): Unit =
    out.append(s"  ; TODO: $what not yet supported by NexLLVMCodegen\n")
