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

  private val out         = new StringBuilder
  private var regCounter  = 0
  private val locals      = mutable.Map.empty[Int, String] // symbol id → alloca register

  /** Compile a program. Returns the full LLVM IR module text. */
  def compile(tp: TProgram): String =
    emitPreamble()
    for d <- tp.decls do d match
      case f: TFunDecl if f.sym.name == "main" && f.params.isEmpty =>
        emitMain(f)
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
  // `def main()` — the only top-level shape supported in this slice.
  // The Nex `main` returns unit; the LLVM `main` returns i32 (0).
  // ---------------------------------------------------------------------------

  private def emitMain(f: TFunDecl): Unit =
    regCounter = 0
    locals.clear()
    out.append("define i32 @main() {\n")
    out.append("entry:\n")
    emitExpr(f.body)
    out.append("  ret i32 0\n")
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
          out.append(s"  $reg = load ${llvmType(t)}, ptr $slot\n")
          reg
        case None =>
          notYet(s"reference to non-local `${s.name}`"); "0"

    case TBinOp(op, l, r, _, t) =>
      val lv      = emitExpr(l)
      val rv      = emitExpr(r)
      val opT     = l.tpe // both sides have the same type for these ops
      val (instr, resultT) = binOpInst(op, opT)
      val reg     = newReg()
      out.append(s"  $reg = $instr ${llvmType(opT)} $lv, $rv\n")
      reg

    case TCall(callee, args, _, _) =>
      callee match
        case TVarRef(s, _, _) if s.name == "print" && args.size == 1 =>
          emitPrintCall(args.head); "void"
        case other =>
          notYet(s"call to ${other.getClass.getSimpleName}"); "0"

    case TBlock(items, result, _, _) =>
      items.foreach {
        case TBlockBinding(sym, _, value) => emitLocalBinding(sym, value)
        case TBlockExpr(x)                => emitExpr(x)
      }
      emitExpr(result)

    case other =>
      notYet(s"expression ${other.getClass.getSimpleName}"); "0"

  private def emitLocalBinding(sym: Symbol, value: TExpr): Unit =
    val rv   = emitExpr(value)
    val ty   = llvmType(sym.tpe)
    val slot = newReg()
    out.append(s"  $slot = alloca $ty\n")
    if ty != "void" then out.append(s"  store $ty $rv, ptr $slot\n")
    locals(sym.id) = slot

  // ---------------------------------------------------------------------------
  // `print(...)` — overloaded across integer / real / bool. Threads through
  // a single libc `printf` with the right format-string global.
  // ---------------------------------------------------------------------------

  private def emitPrintCall(arg: TExpr): Unit =
    val v   = emitExpr(arg)
    arg.tpe match
      case TyInteger =>
        out.append(s"  call i32 (ptr, ...) @printf(ptr @.fmt_int, i64 $v)\n")
      case TyReal =>
        out.append(s"  call i32 (ptr, ...) @printf(ptr @.fmt_real, double $v)\n")
      case TyBool =>
        // Branch on the value: pick `true\n` vs `false\n` format string.
        val sel = newReg()
        out.append(s"  $sel = select i1 $v, ptr @.fmt_bool_t, ptr @.fmt_bool_f\n")
        out.append(s"  call i32 (ptr, ...) @printf(ptr $sel)\n")
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
