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

  /** Compile a program. Returns the full LLVM IR module text. */
  def compile(tp: TProgram): String =
    emitPreamble()
    for d <- tp.decls do d match
      case f: TFunDecl =>
        emitFunction(f)
      case _: TModuleDecl | _: TImportDecl =>
        () // metadata only
      case other =>
        notYet(s"top-level decl `${other.getClass.getSimpleName}`")
    out.toString

  // ---------------------------------------------------------------------------
  // Preamble — external declarations, format strings.
  // ---------------------------------------------------------------------------

  private def emitPreamble(): Unit =
    out.append(
      """; Nex LLVM IR module (v0 scaffolding)
        |
        |declare i32 @printf(ptr, ...)
        |
        |@.fmt_int  = private unnamed_addr constant [6 x i8] c"%lld\0A\00"
        |@.fmt_real = private unnamed_addr constant [4 x i8] c"%g\0A\00"
        |@.fmt_bool_t = private unnamed_addr constant [6 x i8] c"true\0A\00"
        |@.fmt_bool_f = private unnamed_addr constant [7 x i8] c"false\0A\00"
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

    // Spill each param to an alloca so TVarRef loads work uniformly.
    for ((p, i) <- f.params.zipWithIndex) do
      val ty   = llvmType(p.tpe)
      val slot = newReg()
      emitLine(s"  $slot = alloca $ty\n")
      emitLine(s"  store $ty %arg$i, ptr $slot\n")
      locals(p.id) = slot

    val result = emitExpr(f.body)

    // Emit the final ret only if no terminator has fired yet. Inside the
    // body, an early `return` (or both branches of an if that return)
    // will already have closed the block.
    if currentBlock.isDefined then
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

    case TVarRef(s, _, t) =>
      locals.get(s.id) match
        case Some(slot) =>
          val reg = newReg()
          emitLine(s"  $reg = load ${llvmType(t)}, ptr $slot\n")
          reg
        case None =>
          notYet(s"reference to non-local `${s.name}`"); "0"

    case TBinOp("and", l, r, _, _) => emitShortCircuit(l, r, isAnd = true)
    case TBinOp("or",  l, r, _, _) => emitShortCircuit(l, r, isAnd = false)

    case TBinOp(op, l, r, _, _) =>
      val lv      = emitExpr(l)
      val rv      = emitExpr(r)
      val opT     = l.tpe // both sides have the same type for these ops
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
        case TVarRef(s, _, calleeT) if s.kind == SymKind.Function =>
          emitUserCall(s, calleeT, args)
        case other =>
          notYet(s"call to ${other.getClass.getSimpleName}"); "0"

    case TIf(cond, thenB, elseOpt, _, t) =>
      emitIf(cond, thenB, elseOpt, t)

    case TWhile(cond, body, _, _) =>
      emitWhile(cond, body); "void"

    case TReturn(v, _, _) =>
      emitReturn(v); "void"

    case TBlock(items, result, _, _) =>
      items.foreach {
        case TBlockBinding(sym, _, value) => emitLocalBinding(sym, value)
        case TBlockExpr(x)                => emitExpr(x)
      }
      emitExpr(result)

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

  private def emitReturn(v: Option[TExpr]): Unit =
    v match
      case None =>
        if currentIsMain then emitTerminator("  ret i32 0\n")
        else emitTerminator("  ret void\n")
      case Some(expr) =>
        val rv = emitExpr(expr)
        if currentIsMain then
          emitTerminator("  ret i32 0\n")
        else currentReturnType match
          case TyUnit =>
            emitTerminator("  ret void\n")
          case TyUnknown =>
            // Best-effort fallback: trust the expression's inferred type.
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
    val rv = emitExpr(value)
    target match
      case TVarRef(s, _, t) =>
        locals.get(s.id) match
          case Some(slot) => emitLine(s"  store ${llvmType(t)} $rv, ptr $slot\n")
          case None       => notYet(s"assign to non-local `${s.name}`")
      case other =>
        notYet(s"assign to ${other.getClass.getSimpleName}")

  private def emitLocalBinding(sym: Symbol, value: TExpr): Unit =
    val rv   = emitExpr(value)
    val ty   = llvmType(sym.tpe)
    val slot = newReg()
    emitLine(s"  $slot = alloca $ty\n")
    if ty != "void" then emitLine(s"  store $ty $rv, ptr $slot\n")
    locals(sym.id) = slot

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
    val v   = emitExpr(arg)
    arg.tpe match
      case TyInteger =>
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_int, i64 $v)\n")
      case TyReal =>
        emitLine(s"  call i32 (ptr, ...) @printf(ptr @.fmt_real, double $v)\n")
      case TyBool =>
        // Branch on the value: pick `true\n` vs `false\n` format string.
        val sel = newReg()
        emitLine(s"  $sel = select i1 $v, ptr @.fmt_bool_t, ptr @.fmt_bool_f\n")
        emitLine(s"  call i32 (ptr, ...) @printf(ptr $sel)\n")
      case other =>
        notYet(s"print(${other})")

  // ---------------------------------------------------------------------------
  // Type and binop tables.
  // ---------------------------------------------------------------------------

  /** Map a Nex type to its LLVM type. */
  private def llvmType(t: Type): String = t match
    case TyInteger => "i64"
    case TyReal    => "double"
    case TyBool    => "i1"
    case TyUnit    => "void"
    case TyUnknown => "i64" // best-effort placeholder for missing inference
    case other     => notYet(s"type `$other`"); "i64"

  /** Map a Nex binary operator + operand type to (LLVM instruction,
    * result LLVM type). Integer / real overloads are picked here.
    */
  private def binOpInst(op: String, opT: Type): (String, String) =
    (op, opT) match
      case ("+", TyInteger) => ("add",  "i64")
      case ("-", TyInteger) => ("sub",  "i64")
      case ("*", TyInteger) => ("mul",  "i64")
      case ("/", TyInteger) => ("sdiv", "i64")
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
