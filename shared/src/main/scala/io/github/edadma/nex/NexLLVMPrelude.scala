package io.github.edadma.nex

/** Prelude dispatch: built-in functions marked by the elaborator with
  * [[SymKind.Prelude]] — libm bridges, `abs` / `sign` / `min` / `max`,
  * `length` / `rows` / `cols`, and the `assert*` family.
  */
protected trait NexLLVMPrelude extends NexLLVMState:

  // ---------------------------------------------------------------------------
  // Prelude calls. The elaborator marks built-ins with [[SymKind.Prelude]];
  // the unrecognised-name path falls back to a `notYet` diag and a zero so
  // the rest of the IR still parses. Special-cased here:
  //   - `length(arr)` / `rows(arr)` / `cols(arr)`
  // Other prelude functions (sqrt, sin, map, reduce, ...) will land in
  // later chunks.
  // ---------------------------------------------------------------------------

  protected def emitPreludeCall(name: String, args: List[TExpr], resultT: Type): String =
    (name, args) match
      case ("length", List(a)) => emitArrayLengthish(a, "len")
      case ("rows",   List(a)) => emitArrayLengthish(a, "rows")
      case ("cols",   List(a)) => emitArrayLengthish(a, "cols")

      // §10.2 scalar math — all unary fns lower to libm doubles. The
      // elaborator marks the result as TyReal; integer operands need
      // a sitofp lift before the call.
      case ("sqrt",  List(x)) => emitLibmUnary("sqrt",  x)
      case ("cbrt",  List(x)) => emitLibmUnary("cbrt",  x)
      case ("exp",   List(x)) => emitLibmUnary("exp",   x)
      case ("log",   List(x)) => emitLibmUnary("log",   x)
      case ("log2",  List(x)) => emitLibmUnary("log2",  x)
      case ("log10", List(x)) => emitLibmUnary("log10", x)
      case ("sin",   List(x)) => emitLibmUnary("sin",   x)
      case ("cos",   List(x)) => emitLibmUnary("cos",   x)
      case ("tan",   List(x)) => emitLibmUnary("tan",   x)
      case ("asin",  List(x)) => emitLibmUnary("asin",  x)
      case ("acos",  List(x)) => emitLibmUnary("acos",  x)
      case ("atan",  List(x)) => emitLibmUnary("atan",  x)
      case ("sinh",  List(x)) => emitLibmUnary("sinh",  x)
      case ("cosh",  List(x)) => emitLibmUnary("cosh",  x)
      case ("tanh",  List(x)) => emitLibmUnary("tanh",  x)
      case ("asinh", List(x)) => emitLibmUnary("__nex_asinh", x)
      case ("acosh", List(x)) => emitLibmUnary("__nex_acosh", x)
      case ("atanh", List(x)) => emitLibmUnary("__nex_atanh", x)
      case ("floor", List(x)) => emitLibmUnary("floor", x)
      case ("ceil",  List(x)) => emitLibmUnary("ceil",  x)
      case ("round", List(x)) => emitLibmUnary("round", x)
      case ("trunc", List(x)) => emitLibmUnary("trunc", x)
      case ("atan2", List(y, x)) =>
        val yv = liftToReal(y)
        val xv = liftToReal(x)
        val reg = newReg()
        emitLine(s"  $reg = call double @atan2(double $yv, double $xv)\n")
        reg

      // `abs` is overloaded — integer abs vs real fabs. The elaborator
      // leaves the call's result type to follow the argument's type.
      case ("abs", List(x)) =>
        x.tpe match
          case TyInteger =>
            val xv  = emitExpr(x)
            val reg = newReg()
            emitLine(s"  $reg = call i64 @llabs(i64 $xv)\n")
            reg
          case _ =>
            val xv  = liftToReal(x)
            val reg = newReg()
            emitLine(s"  $reg = call double @fabs(double $xv)\n")
            reg

      // sign(x) returns -1, 0, or +1 — integer or real result follows arg.
      case ("sign", List(x)) =>
        x.tpe match
          case TyInteger =>
            val xv  = emitExpr(x)
            val pos = newReg()
            emitLine(s"  $pos = icmp sgt i64 $xv, 0\n")
            val neg = newReg()
            emitLine(s"  $neg = icmp slt i64 $xv, 0\n")
            // zext (NOT sext) — we want true → 1, not true → -1.
            val s1  = newReg()
            emitLine(s"  $s1 = zext i1 $pos to i64\n")
            val s2  = newReg()
            emitLine(s"  $s2 = zext i1 $neg to i64\n")
            val r   = newReg()
            emitLine(s"  $r = sub i64 $s1, $s2\n")
            r
          case _ =>
            val xv = liftToReal(x)
            // Compare against 0.0 in both directions; pick 1.0 / -1.0 / 0.0.
            val pos = newReg()
            emitLine(s"  $pos = fcmp ogt double $xv, 0.0\n")
            val neg = newReg()
            emitLine(s"  $neg = fcmp olt double $xv, 0.0\n")
            val s1  = newReg()
            emitLine(s"  $s1 = select i1 $pos, double 1.0, double 0.0\n")
            val s2  = newReg()
            emitLine(s"  $s2 = select i1 $neg, double -1.0, double $s1\n")
            s2

      case ("min", List(a, b)) => emitMinMax(a, b, isMin = true)
      case ("max", List(a, b)) => emitMinMax(a, b, isMin = false)

      // §10.8 assertions
      case ("assert", List(c)) =>
        val cv = emitExpr(c)
        emitLine(s"  call void @__nex_assert(i1 $cv)\n")
        "void"
      case ("assert", List(c, _)) =>
        // Two-arg form: we drop the message and reuse the generic trap
        // for v0. Matches the interpreter's print-then-abort contract.
        val cv = emitExpr(c)
        emitLine(s"  call void @__nex_assert(i1 $cv)\n")
        "void"
      case ("assert_eq", List(a, b)) =>
        emitAssertEq(a, b); "void"
      case ("assert_approx", List(a, b, eps)) =>
        emitAssertApprox(a, b, eps); "void"

      case _ =>
        notYet(s"prelude `$name`/${args.size}"); "0"

  /** Lift an integer-typed expression to double via `sitofp`; pass-through
    * for double-typed expressions. Used by every libm bridge so callers
    * can write `sqrt(4)` and get `2.0`.
    */
  private def liftToReal(e: TExpr): String =
    val v = emitExpr(e)
    e.tpe match
      case TyInteger =>
        val r = newReg()
        emitLine(s"  $r = sitofp i64 $v to double\n")
        r
      case _ => v

  /** Emit `%r = call double @fn(double <x>)`. */
  private def emitLibmUnary(fn: String, x: TExpr): String =
    val xv  = liftToReal(x)
    val reg = newReg()
    emitLine(s"  $reg = call double @$fn(double $xv)\n")
    reg

  /** Emit `min(a, b)` / `max(a, b)` using fcmp+select for reals and
    * icmp+select for integers. Mixed-type results promote both operands
    * to double.
    */
  private def emitMinMax(a: TExpr, b: TExpr, isMin: Boolean): String =
    val bothInt = a.tpe == TyInteger && b.tpe == TyInteger
    if bothInt then
      val av  = emitExpr(a)
      val bv  = emitExpr(b)
      val cmp = newReg()
      val op  = if isMin then "icmp sle" else "icmp sge"
      emitLine(s"  $cmp = $op i64 $av, $bv\n")
      val sel = newReg()
      emitLine(s"  $sel = select i1 $cmp, i64 $av, i64 $bv\n")
      sel
    else
      val av  = liftToReal(a)
      val bv  = liftToReal(b)
      val cmp = newReg()
      val op  = if isMin then "fcmp ole" else "fcmp oge"
      emitLine(s"  $cmp = $op double $av, $bv\n")
      val sel = newReg()
      emitLine(s"  $sel = select i1 $cmp, double $av, double $bv\n")
      sel

  /** Emit an equality check between two values, aborting via
    * `__nex_assert(false)` on mismatch. Supports integer / real / bool
    * (with optional sitofp promotion when types differ).
    */
  private def emitAssertEq(a: TExpr, b: TExpr): Unit =
    val isReal = a.tpe == TyReal || b.tpe == TyReal
    val eq = newReg()
    if isReal then
      val av = liftToReal(a)
      val bv = liftToReal(b)
      emitLine(s"  $eq = fcmp oeq double $av, $bv\n")
    else
      val av = emitExpr(a)
      val bv = emitExpr(b)
      val ty = llvmType(a.tpe)
      emitLine(s"  $eq = icmp eq $ty $av, $bv\n")
    emitLine(s"  call void @__nex_assert(i1 $eq)\n")

  /** Emit `|a - b| <= eps` check, aborting on mismatch. Mixed numeric
    * types promote to double.
    */
  private def emitAssertApprox(a: TExpr, b: TExpr, eps: TExpr): Unit =
    val av = liftToReal(a)
    val bv = liftToReal(b)
    val ev = liftToReal(eps)
    val d  = newReg()
    emitLine(s"  $d = fsub double $av, $bv\n")
    val ad = newReg()
    emitLine(s"  $ad = call double @fabs(double $d)\n")
    val ok = newReg()
    emitLine(s"  $ok = fcmp ole double $ad, $ev\n")
    emitLine(s"  call void @__nex_assert(i1 $ok)\n")

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
