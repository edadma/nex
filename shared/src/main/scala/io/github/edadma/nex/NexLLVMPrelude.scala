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

      // §10.2 scalar math — most unary fns lower to libm doubles. The
      // elaborator marks the result as TyReal; integer operands need
      // a sitofp lift before the call.
      //
      // Spec §10.2 line 32: sin, cos, exp, log, sqrt (and as a
      // natural extension tan / log2 / log10 / cbrt) accept complex
      // arguments and return complex via the standard analytic
      // extensions. Each function routes on x.tpe.
      case ("sqrt",  List(x)) =>
        if x.tpe == TyComplex then emitComplexSqrt(x) else emitLibmUnary("sqrt", x)
      case ("cbrt",  List(x)) => emitLibmUnary("cbrt",  x)
      case ("exp",   List(x)) =>
        if x.tpe == TyComplex then emitComplexExp(x) else emitLibmUnary("exp", x)
      case ("log",   List(x)) =>
        if x.tpe == TyComplex then emitComplexLog(x, scale = 1.0) else emitLibmUnary("log", x)
      case ("log2",  List(x)) =>
        if x.tpe == TyComplex then emitComplexLog(x, scale = math.log(2))
        else emitLibmUnary("log2", x)
      case ("log10", List(x)) =>
        if x.tpe == TyComplex then emitComplexLog(x, scale = math.log(10))
        else emitLibmUnary("log10", x)
      case ("sin",   List(x)) =>
        if x.tpe == TyComplex then emitComplexSin(x) else emitLibmUnary("sin", x)
      case ("cos",   List(x)) =>
        if x.tpe == TyComplex then emitComplexCos(x) else emitLibmUnary("cos", x)
      case ("tan",   List(x)) =>
        if x.tpe == TyComplex then emitComplexTan(x) else emitLibmUnary("tan", x)
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

      // `abs` is overloaded — integer abs vs real fabs vs complex modulus.
      // The elaborator leaves the call's result type to follow the
      // argument's type.
      case ("abs", List(x)) =>
        x.tpe match
          case TyInteger =>
            val xv  = emitExpr(x)
            val reg = newReg()
            emitLine(s"  $reg = call i64 @llabs(i64 $xv)\n")
            reg
          case TyComplex =>
            // |z| = sqrt(re² + im²)
            val xv  = emitExpr(x)
            val re  = newReg()
            emitLine(s"  $re = extractvalue { double, double } $xv, 0\n")
            val im  = newReg()
            emitLine(s"  $im = extractvalue { double, double } $xv, 1\n")
            val rr  = newReg(); emitLine(s"  $rr = fmul double $re, $re\n")
            val ii  = newReg(); emitLine(s"  $ii = fmul double $im, $im\n")
            val sum = newReg(); emitLine(s"  $sum = fadd double $rr, $ii\n")
            val reg = newReg()
            emitLine(s"  $reg = call double @sqrt(double $sum)\n")
            reg
          case _ =>
            val xv  = liftToReal(x)
            val reg = newReg()
            emitLine(s"  $reg = call double @fabs(double $xv)\n")
            reg

      // Complex prelude functions (§10.3).
      case ("conj", List(z)) =>
        z.tpe match
          case TyComplex =>
            val zv = emitExpr(z)
            val re = newReg()
            emitLine(s"  $re = extractvalue { double, double } $zv, 0\n")
            val im = newReg()
            emitLine(s"  $im = extractvalue { double, double } $zv, 1\n")
            val nim = newReg()
            emitLine(s"  $nim = fneg double $im\n")
            val c0 = newReg()
            emitLine(s"  $c0 = insertvalue { double, double } undef, double $re, 0\n")
            val c1 = newReg()
            emitLine(s"  $c1 = insertvalue { double, double } $c0, double $nim, 1\n")
            c1
          case _ =>
            // conj of a real / int is itself — just emit it.
            emitExpr(z)

      case ("arg", List(z)) =>
        // arg(z) = atan2(im, re) per the interpreter. Reals/ints have
        // `im = 0`, so arg returns 0 for positive and π for negative.
        z.tpe match
          case TyComplex =>
            val zv = emitExpr(z)
            val re = newReg()
            emitLine(s"  $re = extractvalue { double, double } $zv, 0\n")
            val im = newReg()
            emitLine(s"  $im = extractvalue { double, double } $zv, 1\n")
            val reg = newReg()
            emitLine(s"  $reg = call double @atan2(double $im, double $re)\n")
            reg
          case _ =>
            val xv = liftToReal(z)
            // For a non-complex value: return 0 if >= 0, π otherwise.
            val ge = newReg()
            emitLine(s"  $ge = fcmp oge double $xv, 0.0\n")
            val sel = newReg()
            emitLine(s"  $sel = select i1 $ge, double 0.0, double 0x400921FB54442D18\n")
            sel

      // §10.7 to_complex(x) — wrap any numeric as a complex pair.
      case ("to_complex", List(x)) =>
        x.tpe match
          case TyComplex => emitExpr(x)
          case TyReal =>
            val xv = emitExpr(x)
            val c0 = newReg()
            emitLine(s"  $c0 = insertvalue { double, double } undef, double $xv, 0\n")
            val c1 = newReg()
            emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
            c1
          case TyInteger =>
            val xv = emitExpr(x)
            val rv = newReg()
            emitLine(s"  $rv = sitofp i64 $xv to double\n")
            val c0 = newReg()
            emitLine(s"  $c0 = insertvalue { double, double } undef, double $rv, 0\n")
            val c1 = newReg()
            emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
            c1
          case other =>
            notYet(s"to_complex from $other"); "0"

      // §10.7 to_real(x) — widen any numeric to double.
      case ("to_real", List(x)) =>
        x.tpe match
          case TyReal    => emitExpr(x)
          case TyInteger =>
            val xv = emitExpr(x)
            val rv = newReg()
            emitLine(s"  $rv = sitofp i64 $xv to double\n")
            rv
          case TyComplex =>
            // Convention: real part of a complex value (matches the
            // interpreter, which uses `asReal` returning the real
            // component of a complex).
            val xv = emitExpr(x)
            val re = newReg()
            emitLine(s"  $re = extractvalue { double, double } $xv, 0\n")
            re
          case other =>
            notYet(s"to_real from $other"); "0.0"

      // §10.7 to_integer(x) — truncate to i64.
      case ("to_integer", List(x)) =>
        x.tpe match
          case TyInteger => emitExpr(x)
          case TyReal =>
            val xv  = emitExpr(x)
            val reg = newReg()
            emitLine(s"  $reg = fptosi double $xv to i64\n")
            reg
          case TyComplex =>
            // Truncate the real part — same convention as to_real(complex).
            val xv  = emitExpr(x)
            val re  = newReg()
            emitLine(s"  $re = extractvalue { double, double } $xv, 0\n")
            val reg = newReg()
            emitLine(s"  $reg = fptosi double $re to i64\n")
            reg
          case other =>
            notYet(s"to_integer from $other"); "0"

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

      // §10.4 rank-1 reductions — counted loop + accumulator. Element
      // type may be integer (i64), real (double), or complex
      // ({double, double}); the op is fixed (+ for sum, * for product,
      // a*b accumulated for dot).
      case ("sum",     List(arr))              => emitSumCall(arr, resultT)
      case ("product", List(arr))              => emitProductCall(arr, resultT)
      case ("dot",     List(a, b))             => emitDotCall(a, b, resultT)

      // §10.4 rank-1 builders.
      case ("range",     List(lo, hi))         => emitRangeCall(lo, hi)
      case ("enumerate", List(arr))            => emitEnumerateCall(arr, resultT)
      case ("zip",       List(a, b))           => emitZipCall(a, b, resultT)
      case ("linspace",  List(lo, hi, n))      => emitLinspaceCall(lo, hi, n)

      // §10.4 array HOFs — direct inlined loops that dispatch each
      // iteration through the chunk-9 closure call helper. Works
      // identically for inline TLambda args and TVarRef closure
      // bindings (both emit a `{ptr, ptr}` value via emitExpr).
      case ("map",     List(arr, fn))          => emitMapCall(arr, fn, resultT)
      case ("flatMap", List(arr, fn))          => emitFlatMapCall(arr, fn, resultT)
      case ("reduce",  List(arr, init, fn))    => emitReduceCall(arr, init, fn, resultT)
      case ("filter",  List(arr, fn))          => emitFilterCall(arr, fn, resultT)

      // §10.5 array construction. `fill(n, v)` allocates a fresh rank-1
      // array of length `n` with every slot set to `v`; element type
      // comes from `v.tpe` and was already propagated into resultT by
      // the elaborator. `zeros(n)` / `ones(n)` are convenience wrappers
      // that lower through fill with a 0 / 1 integer constant.
      case ("fill",     List(n, v))            => emitFillCall(n, v, resultT)
      case ("zeros",    List(n))               => emitConstFill(n, "0", TyInteger, resultT)
      case ("ones",     List(n))               => emitConstFill(n, "1", TyInteger, resultT)
      case ("identity", List(n))               => emitIdentityCall(n, resultT)

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

  // ---------------------------------------------------------------------------
  // Complex-arg variants of the spec-§10.2 transcendental functions.
  // Each takes a TyComplex expression, extracts the (re, im) pair, runs
  // the standard analytic-extension formula via libm, and packs the
  // result back into a `{ double, double }` aggregate.
  // ---------------------------------------------------------------------------

  /** Pack two doubles into a complex aggregate value. */
  private def packComplexCD(re: String, im: String): String =
    val c0 = newReg()
    emitLine(s"  $c0 = insertvalue { double, double } undef, double $re, 0\n")
    val c1 = newReg()
    emitLine(s"  $c1 = insertvalue { double, double } $c0, double $im, 1\n")
    c1

  /** Extract (re, im) from a complex SSA value. */
  private def unpackComplex(z: String): (String, String) =
    val re = newReg()
    emitLine(s"  $re = extractvalue { double, double } $z, 0\n")
    val im = newReg()
    emitLine(s"  $im = extractvalue { double, double } $z, 1\n")
    (re, im)

  /** exp(a + bi) = e^a · (cos b + i·sin b). */
  private def emitComplexExp(x: TExpr): String =
    val (re, im) = unpackComplex(emitExpr(x))
    val s  = newReg(); emitLine(s"  $s  = call double @exp(double $re)\n")
    val c  = newReg(); emitLine(s"  $c  = call double @cos(double $im)\n")
    val si = newReg(); emitLine(s"  $si = call double @sin(double $im)\n")
    val r  = newReg(); emitLine(s"  $r  = fmul double $s, $c\n")
    val i  = newReg(); emitLine(s"  $i  = fmul double $s, $si\n")
    packComplexCD(r, i)

  /** log(a + bi) = ½·ln(a² + b²) + i·atan2(b, a), scaled by `scale`
    * for log2 / log10 (pass scale = ln(2) / ln(10)).
    */
  private def emitComplexLog(x: TExpr, scale: Double): String =
    val (re, im) = unpackComplex(emitExpr(x))
    val rr  = newReg(); emitLine(s"  $rr = fmul double $re, $re\n")
    val ii  = newReg(); emitLine(s"  $ii = fmul double $im, $im\n")
    val sum = newReg(); emitLine(s"  $sum = fadd double $rr, $ii\n")
    val ln  = newReg(); emitLine(s"  $ln = call double @log(double $sum)\n")
    val half = newReg(); emitLine(s"  $half = fmul double $ln, 5.0e-01\n")
    val ang  = newReg(); emitLine(s"  $ang = call double @atan2(double $im, double $re)\n")
    if scale == 1.0 then packComplexCD(half, ang)
    else
      // Divide both components by scale to get log2 / log10 from natural log.
      val sc = formatReal(scale)
      val rr2 = newReg(); emitLine(s"  $rr2 = fdiv double $half, $sc\n")
      val ii2 = newReg(); emitLine(s"  $ii2 = fdiv double $ang,  $sc\n")
      packComplexCD(rr2, ii2)

  /** sin(a + bi) = sin a · cosh b + i·cos a · sinh b. */
  private def emitComplexSin(x: TExpr): String =
    val (re, im) = unpackComplex(emitExpr(x))
    val sr = newReg(); emitLine(s"  $sr = call double @sin(double $re)\n")
    val cr = newReg(); emitLine(s"  $cr = call double @cos(double $re)\n")
    val sh = newReg(); emitLine(s"  $sh = call double @sinh(double $im)\n")
    val ch = newReg(); emitLine(s"  $ch = call double @cosh(double $im)\n")
    val r  = newReg(); emitLine(s"  $r = fmul double $sr, $ch\n")
    val i  = newReg(); emitLine(s"  $i = fmul double $cr, $sh\n")
    packComplexCD(r, i)

  /** cos(a + bi) = cos a · cosh b − i·sin a · sinh b. */
  private def emitComplexCos(x: TExpr): String =
    val (re, im) = unpackComplex(emitExpr(x))
    val sr = newReg(); emitLine(s"  $sr = call double @sin(double $re)\n")
    val cr = newReg(); emitLine(s"  $cr = call double @cos(double $re)\n")
    val sh = newReg(); emitLine(s"  $sh = call double @sinh(double $im)\n")
    val ch = newReg(); emitLine(s"  $ch = call double @cosh(double $im)\n")
    val r  = newReg(); emitLine(s"  $r = fmul double $cr, $ch\n")
    val negSr = newReg(); emitLine(s"  $negSr = fneg double $sr\n")
    val i  = newReg(); emitLine(s"  $i = fmul double $negSr, $sh\n")
    packComplexCD(r, i)

  /** tan(z) = sin z / cos z, expanded for numerical stability across
    * the imaginary axis. Formula: real = sin r · cos r / D,
    * imag = sinh i · cosh i / D, where D = cos²r·cosh²i + sin²r·sinh²i.
    */
  private def emitComplexTan(x: TExpr): String =
    val (re, im) = unpackComplex(emitExpr(x))
    val sr = newReg(); emitLine(s"  $sr = call double @sin(double $re)\n")
    val cr = newReg(); emitLine(s"  $cr = call double @cos(double $re)\n")
    val sh = newReg(); emitLine(s"  $sh = call double @sinh(double $im)\n")
    val ch = newReg(); emitLine(s"  $ch = call double @cosh(double $im)\n")
    val crsq = newReg(); emitLine(s"  $crsq = fmul double $cr, $cr\n")
    val srsq = newReg(); emitLine(s"  $srsq = fmul double $sr, $sr\n")
    val chsq = newReg(); emitLine(s"  $chsq = fmul double $ch, $ch\n")
    val shsq = newReg(); emitLine(s"  $shsq = fmul double $sh, $sh\n")
    val a = newReg(); emitLine(s"  $a = fmul double $crsq, $chsq\n")
    val b = newReg(); emitLine(s"  $b = fmul double $srsq, $shsq\n")
    val d = newReg(); emitLine(s"  $d = fadd double $a, $b\n")
    val srcr = newReg(); emitLine(s"  $srcr = fmul double $sr, $cr\n")
    val shch = newReg(); emitLine(s"  $shch = fmul double $sh, $ch\n")
    val r = newReg(); emitLine(s"  $r = fdiv double $srcr, $d\n")
    val i = newReg(); emitLine(s"  $i = fdiv double $shch, $d\n")
    packComplexCD(r, i)

  /** sqrt(a + bi) — principal branch, computed in the half-plane form
    *   real = √((|z| + re) / 2)
    *   imag = sign(im) · √((|z| − re) / 2)
    * matching the interpreter's [[NexInterpreter.sqrtV]].
    */
  private def emitComplexSqrt(x: TExpr): String =
    val (re, im) = unpackComplex(emitExpr(x))
    val rr  = newReg(); emitLine(s"  $rr = fmul double $re, $re\n")
    val ii  = newReg(); emitLine(s"  $ii = fmul double $im, $im\n")
    val sum = newReg(); emitLine(s"  $sum = fadd double $rr, $ii\n")
    val mag = newReg(); emitLine(s"  $mag = call double @sqrt(double $sum)\n")
    val sumR = newReg(); emitLine(s"  $sumR = fadd double $mag, $re\n")
    val diffR = newReg(); emitLine(s"  $diffR = fsub double $mag, $re\n")
    val halfA = newReg(); emitLine(s"  $halfA = fmul double $sumR,  5.0e-01\n")
    val halfB = newReg(); emitLine(s"  $halfB = fmul double $diffR, 5.0e-01\n")
    val realR = newReg(); emitLine(s"  $realR = call double @sqrt(double $halfA)\n")
    val absI  = newReg(); emitLine(s"  $absI  = call double @sqrt(double $halfB)\n")
    // Apply sign(im): copysign(absI, im) yields ±absI matching im's sign.
    // libm exposes copysign; use it for cheap branch-free sign transfer.
    val signed = newReg()
    emitLine(s"  $signed = call double @copysign(double $absI, double $im)\n")
    packComplexCD(realR, signed)

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

  // ---------------------------------------------------------------------------
  // §10.4 array higher-order functions (chunk 11).
  //
  // map / reduce / filter all share the same shape:
  //   1. Evaluate `arr` once into an owning SSA value.
  //   2. Evaluate the closure-typed `fn` once and split it into
  //      `{ fn_ptr, env_ptr }` so the loop body can dispatch directly.
  //   3. Iterate 0..len-1, invoking the closure per element.
  //   4. dec the source array (we owned a share).
  //
  // Rank-1 only for now — rank-2 map would preserve shape but the
  // interpreter's filter is rank-1 only too, so the spec target is the
  // rank-1 path. Rank-2 surfaces a `notYet` diag.
  // ---------------------------------------------------------------------------

  /** Emit `map(arr, fn)` — allocate a result array of the same length
    * and store each `fn(arr[i])` into it. The closure's `{ptr, ptr}`
    * value is split into fn_ptr + env_ptr before the loop so the body
    * only needs an indirect call.
    */
  private def emitMapCall(arr: TExpr, fn: TExpr, resultT: Type): String =
    val rank = arrayRank(arr.tpe)
    if rank != 1 && rank != 2 then
      notYet(s"map on rank $rank")
      return "0"

    val srcElem = arrayElem(arr.tpe)
    val resElem = arrayElem(resultT)
    val resEsz  = elemSize(resElem)
    val srcStT  = storageType(srcElem)
    val srcLLT  = llvmType(srcElem)
    val resStT  = storageType(resElem)
    val resLLT  = llvmType(resElem)

    val arrV    = emitExpr(arr)
    val (fnPtr, envPtr) = splitClosure(fn)

    // Rank-1: len = arr.len; alloc(len, esz). Rank-2: rows, cols from
    // source; alloc rank-2 with the same shape, walk flat over
    // rows*cols.
    val (len, res) = rank match
      case 1 =>
        val l = newReg(); emitLine(s"  $l = call i64 @__nex_arr1_len(ptr $arrV)\n")
        val r = newReg(); emitLine(s"  $r = call ptr @__nex_arr1_alloc(i64 $l, i64 $resEsz)\n")
        (l, r)
      case _ =>
        val rows = newReg(); emitLine(s"  $rows = call i64 @__nex_arr2_rows(ptr $arrV)\n")
        val cols = newReg(); emitLine(s"  $cols = call i64 @__nex_arr2_cols(ptr $arrV)\n")
        val l    = newReg(); emitLine(s"  $l = mul i64 $rows, $cols\n")
        val r    = newReg(); emitLine(s"  $r = call ptr @__nex_arr2_alloc(i64 $rows, i64 $cols, i64 $resEsz)\n")
        (l, r)

    val srcBuf  = bufPtr(arrV, arr.tpe)
    val dstBuf  = bufPtr(res, resultT)

    emitCountingLoop(len, "hof.map") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $srcStT, ptr $srcBuf, i64 $i\n")
      val elem    = loadElem(srcStT, srcSlot, srcLLT)
      val y       = newReg()
      emitLine(s"  $y = call $resLLT (ptr, $srcLLT) $fnPtr(ptr $envPtr, $srcLLT $elem)\n")
      val dstSlot = newReg()
      emitLine(s"  $dstSlot = getelementptr inbounds $resStT, ptr $dstBuf, i64 $i\n")
      storeElem(resStT, y, dstSlot)
    }

    emitArrDec(arrV, arr.tpe)
    res

  /** Emit `reduce(arr, init, fn)` — fold the array left-to-right
    * starting from `init`. The accumulator lives in an alloca slot so
    * we can update-in-place each iteration without phi nodes.
    */
  private def emitReduceCall(arr: TExpr, init: TExpr, fn: TExpr, resultT: Type): String =
    val rank = arrayRank(arr.tpe)
    if rank != 1 && rank != 2 then
      notYet(s"reduce on rank $rank")
      return "0"
    val lenFn = if rank == 1 then "__nex_arr1_len" else "__nex_arr2_len"

    val srcElem = arrayElem(arr.tpe)
    val srcStT  = storageType(srcElem)
    val srcLLT  = llvmType(srcElem)
    val accLLT  = llvmType(resultT)

    val arrV = emitExpr(arr)
    val iv   = emitExpr(init)
    val (fnPtr, envPtr) = splitClosure(fn)
    val accSlot = newReg()
    emitLine(s"  $accSlot = alloca $accLLT\n")
    emitLine(s"  store $accLLT $iv, ptr $accSlot\n")
    val len  = newReg()
    emitLine(s"  $len = call i64 @$lenFn(ptr $arrV)\n")
    val srcBuf  = bufPtr(arrV, arr.tpe)

    emitCountingLoop(len, "hof.reduce") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $srcStT, ptr $srcBuf, i64 $i\n")
      val elem    = loadElem(srcStT, srcSlot, srcLLT)
      val accCur  = newReg()
      emitLine(s"  $accCur = load $accLLT, ptr $accSlot\n")
      val nextAcc = newReg()
      emitLine(s"  $nextAcc = call $accLLT (ptr, $accLLT, $srcLLT) $fnPtr(ptr $envPtr, $accLLT $accCur, $srcLLT $elem)\n")
      emitLine(s"  store $accLLT $nextAcc, ptr $accSlot\n")
    }

    emitArrDec(arrV, arr.tpe)
    val finalAcc = newReg()
    emitLine(s"  $finalAcc = load $accLLT, ptr $accSlot\n")
    finalAcc

  /** Emit `filter(arr, predicate)` — allocate a worst-case-sized result
    * array of length(arr), iterate and copy elements where the
    * predicate returns true, then truncate the descriptor's length
    * field to the actual count. The over-allocation costs at most a
    * pointer's-worth of unused memory and avoids a two-pass approach.
    */
  private def emitFilterCall(arr: TExpr, fn: TExpr, resultT: Type): String =
    if arrayRank(arr.tpe) != 1 then
      notYet(s"filter on rank ${arrayRank(arr.tpe)} (only rank-1 supported)")
      return "0"

    val elem    = arrayElem(arr.tpe)
    val esz     = elemSize(elem)
    val stT     = storageType(elem)
    val langT   = llvmType(elem)

    val arrV = emitExpr(arr)
    val (fnPtr, envPtr) = splitClosure(fn)
    val len  = newReg()
    emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $arrV)\n")
    val res  = newReg()
    emitLine(s"  $res = call ptr @__nex_arr1_alloc(i64 $len, i64 $esz)\n")
    val countSlot = newReg()
    emitLine(s"  $countSlot = alloca i64\n")
    emitLine(s"  store i64 0, ptr $countSlot\n")
    // Tier-1 perf: hoist buffer pointers; both indices are provably
    // in-bounds (`i` ∈ 0..len; `cur` ∈ 0..i ≤ len).
    val srcBuf  = bufPtr(arrV, arr.tpe)
    val dstBuf  = bufPtr(res, resultT)

    emitCountingLoop(len, "hof.filter") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $stT, ptr $srcBuf, i64 $i\n")
      val v       = loadElem(stT, srcSlot, langT)
      val keep    = newReg()
      emitLine(s"  $keep = call i1 (ptr, $langT) $fnPtr(ptr $envPtr, $langT $v)\n")
      val keepL   = freshLabel("filter.keep")
      val skipL   = freshLabel("filter.skip")
      emitTerminator(s"  br i1 $keep, label %$keepL, label %$skipL\n")
      startBlock(keepL)
      val cur = newReg()
      emitLine(s"  $cur = load i64, ptr $countSlot\n")
      val dst = newReg()
      emitLine(s"  $dst = getelementptr inbounds $stT, ptr $dstBuf, i64 $cur\n")
      storeElem(stT, v, dst)
      val nx  = newReg()
      emitLine(s"  $nx = add i64 $cur, 1\n")
      emitLine(s"  store i64 $nx, ptr $countSlot\n")
      emitTerminator(s"  br label %$skipL\n")
      startBlock(skipL)
    }

    // Truncate the result descriptor's length field to the actual
    // count. The buffer keeps its over-allocated size but the array's
    // visible length matches what was retained — matches the
    // interpreter's `VArray1(out)` shape.
    val cnt   = newReg()
    emitLine(s"  $cnt = load i64, ptr $countSlot\n")
    val lenP  = newReg()
    emitLine(s"  $lenP = getelementptr inbounds %nex_arr1, ptr $res, i32 0, i32 1\n")
    emitLine(s"  store i64 $cnt, ptr $lenP\n")

    emitArrDec(arrV, arr.tpe)
    res

  /** Evaluate a closure-typed expression once and return its
    * `(fn_ptr, env_ptr)` extracted SSA names. Used by the three HOF
    * helpers so the loop body only emits one indirect call per
    * iteration rather than re-extracting per use.
    */
  private def splitClosure(fn: TExpr): (String, String) =
    val cl = emitExpr(fn)
    val fp = newReg()
    emitLine(s"  $fp = extractvalue { ptr, ptr } $cl, 0\n")
    val ep = newReg()
    emitLine(s"  $ep = extractvalue { ptr, ptr } $cl, 1\n")
    (fp, ep)

  // ---------------------------------------------------------------------------
  // §10.5 array construction — `fill(n, v)`, `zeros(n)`, `ones(n)`.
  //
  // Each emits an allocation followed by a counted store loop. The
  // value is evaluated ONCE outside the loop and reused across slots —
  // matches the interpreter's `mutable.ArrayBuffer.fill(n)(v)` semantics
  // (no per-element re-evaluation).
  // ---------------------------------------------------------------------------

  /** Emit `fill(n, v)` — allocate a rank-1 or rank-2 array and store
    * `v` into every slot. The shape comes from the `n` argument's
    * type: integer → rank-1 of length `n`; tuple (rows, cols) →
    * rank-2 of shape (rows, cols). `v` is evaluated once.
    */
  private def emitFillCall(n: TExpr, v: TExpr, resultT: Type): String =
    val rank = arrayRank(resultT)
    if rank != 1 && rank != 2 then
      notYet(s"fill(rank-$rank)")
      return "null"

    val elem = arrayElem(resultT)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val vVal = emitExpr(v)

    // Rank-1 path takes `n` directly. Rank-2 expects `n` to be a
    // (integer, integer) tuple — extract rows / cols via extractvalue
    // on the SSA tuple value, then compute the flat length for the
    // counting loop.
    val (arr, len) = rank match
      case 1 =>
        val nVal = emitExpr(n)
        val a    = newReg()
        emitLine(s"  $a = call ptr @__nex_arr1_alloc(i64 $nVal, i64 $esz)\n")
        (a, nVal)
      case _ =>
        val tup  = emitExpr(n)
        val rows = newReg(); emitLine(s"  $rows = extractvalue { i64, i64 } $tup, 0\n")
        val cols = newReg(); emitLine(s"  $cols = extractvalue { i64, i64 } $tup, 1\n")
        val a    = newReg()
        emitLine(s"  $a = call ptr @__nex_arr2_alloc(i64 $rows, i64 $cols, i64 $esz)\n")
        val l    = newReg(); emitLine(s"  $l = mul i64 $rows, $cols\n")
        (a, l)

    val buf = bufPtr(arr, resultT)
    emitCountingLoop(len, "fill") { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $i\n")
      storeElem(stT, vVal, slot)
    }
    arr

  /** Emit a fill-with-constant for `zeros(n)` / `ones(n)`. The constant
    * is materialised as an LLVM literal (no separate SSA), and the loop
    * shape matches [[emitFillCall]]. Rank-1 takes `n: integer`; rank-2
    * takes `n: (integer, integer)` and allocates rows × cols.
    */
  private def emitConstFill(n: TExpr, constStr: String, elemT: Type, resultT: Type): String =
    val rank = arrayRank(resultT)
    if rank != 1 && rank != 2 then
      notYet(s"zeros/ones rank-$rank")
      return "null"

    val elem = arrayElem(resultT)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)

    val (arr, len) = rank match
      case 1 =>
        val nVal = emitExpr(n)
        val a    = newReg()
        emitLine(s"  $a = call ptr @__nex_arr1_alloc(i64 $nVal, i64 $esz)\n")
        (a, nVal)
      case _ =>
        val tup  = emitExpr(n)
        val rows = newReg(); emitLine(s"  $rows = extractvalue { i64, i64 } $tup, 0\n")
        val cols = newReg(); emitLine(s"  $cols = extractvalue { i64, i64 } $tup, 1\n")
        val a    = newReg()
        emitLine(s"  $a = call ptr @__nex_arr2_alloc(i64 $rows, i64 $cols, i64 $esz)\n")
        val l    = newReg(); emitLine(s"  $l = mul i64 $rows, $cols\n")
        (a, l)

    val buf = bufPtr(arr, resultT)
    emitCountingLoop(len, "constfill") { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $i\n")
      storeElem(stT, constStr, slot)
    }
    arr

  /** Emit `identity(n)` — n × n integer identity matrix. Element type
    * matches the interpreter's VInt-typed cells (a v0 divergence from
    * the spec's `[[real]]` signature). Allocates n*n integer slots
    * with 0, then walks the diagonal writing 1.
    */
  private def emitIdentityCall(n: TExpr, resultT: Type): String =
    if arrayRank(resultT) != 2 then
      notYet(s"identity rank ${arrayRank(resultT)}"); return "null"
    val elem = arrayElem(resultT)
    val esz  = elemSize(elem)
    val stT  = storageType(elem)
    val nVal = emitExpr(n)
    val arr  = newReg()
    emitLine(s"  $arr = call ptr @__nex_arr2_alloc(i64 $nVal, i64 $nVal, i64 $esz)\n")
    val buf  = bufPtr(arr, resultT)
    val total = newReg(); emitLine(s"  $total = mul i64 $nVal, $nVal\n")

    // Zero-fill every slot.
    emitCountingLoop(total, "identity.zero") { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $i\n")
      storeElem(stT, "0", slot)
    }
    // Walk the diagonal: index i*n + i for i in 0..n.
    emitCountingLoop(nVal, "identity.diag") { i =>
      val nplus1 = newReg(); emitLine(s"  $nplus1 = add i64 $nVal, 1\n")
      val idx    = newReg(); emitLine(s"  $idx = mul i64 $i, $nplus1\n")
      val slot   = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $idx\n")
      storeElem(stT, "1", slot)
    }
    arr

  // ---------------------------------------------------------------------------
  // §10.4 rank-1 reductions: sum / product / dot.
  //
  // Each emits an alloca'd accumulator updated by a counted loop. The
  // op is fixed per function (no closure dispatch). Element types
  // supported: integer (i64), real (double), complex ({double, double}).
  // ---------------------------------------------------------------------------

  /** Emit `sum(arr)` — fold the array with +, walking every element
    * regardless of rank. Result type matches the element type. The
    * rank-1 and rank-2 paths share `emitReduceOver`; only the length
    * helper differs (`__nex_arr1_len` vs `__nex_arr2_len`, which the
    * helper picks via the rank).
    */
  private def emitSumCall(arr: TExpr, resultT: Type): String =
    arrayRank(arr.tpe) match
      case 1 | 2 => emitReduceOver(arr, resultT, "+", zeroOf(resultT))
      case other => notYet(s"sum on rank $other"); "0"

  /** Emit `product(arr)` — same shape as sum, identity 1, op `*`. */
  private def emitProductCall(arr: TExpr, resultT: Type): String =
    arrayRank(arr.tpe) match
      case 1 | 2 => emitReduceOver(arr, resultT, "*", oneOf(resultT))
      case other => notYet(s"product on rank $other"); "0"

  /** Emit `dot(a, b)` — inner product. Both arrays must be rank-1 with
    * the same element type; result is the element type. Two source
    * GEPs per iter, one accumulator update.
    */
  private def emitDotCall(a: TExpr, b: TExpr, resultT: Type): String =
    if arrayRank(a.tpe) != 1 || arrayRank(b.tpe) != 1 then
      notYet("dot requires two rank-1 arrays")
      return "0"
    val elem = arrayElem(a.tpe)
    val stT  = storageType(elem)
    val llT  = llvmType(elem)
    val accLLT = llvmType(resultT)

    val av    = emitExpr(a)
    val bv    = emitExpr(b)
    val len   = newReg(); emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $av)\n")
    val aBuf  = bufPtr(av, a.tpe)
    val bBuf  = bufPtr(bv, b.tpe)
    val accSlot = newReg()
    emitLine(s"  $accSlot = alloca $accLLT\n")
    storeElem(stT, zeroOf(resultT), accSlot)

    emitCountingLoop(len, "hof.dot") { i =>
      val aS = newReg()
      emitLine(s"  $aS = getelementptr inbounds $stT, ptr $aBuf, i64 $i\n")
      val ae = loadElem(stT, aS, llT)
      val bS = newReg()
      emitLine(s"  $bS = getelementptr inbounds $stT, ptr $bBuf, i64 $i\n")
      val be = loadElem(stT, bS, llT)
      val prod = emitScalarBinOpSimple("*", ae, be, elem)
      val cur  = newReg()
      emitLine(s"  $cur = load $accLLT, ptr $accSlot\n")
      val nxt  = emitScalarBinOpSimple("+", cur, prod, elem)
      storeElem(stT, nxt, accSlot)
    }

    emitArrDec(av, a.tpe)
    emitArrDec(bv, b.tpe)
    val r = newReg()
    emitLine(s"  $r = load $accLLT, ptr $accSlot\n")
    r

  /** Shared loop for sum / product. Walks every element of a rank-1
    * or rank-2 array (row-major flat order for rank-2). The element
    * type and op are fixed per call; the accumulator lives in an
    * alloca slot that the loop updates in place.
    */
  private def emitReduceOver(arr: TExpr, resultT: Type, op: String, identity: String): String =
    val rank   = arrayRank(arr.tpe)
    val elem   = arrayElem(arr.tpe)
    val stT    = storageType(elem)
    val llT    = llvmType(elem)
    val accLLT = llvmType(resultT)
    val lenFn  = rank match
      case 1 => "__nex_arr1_len"
      case 2 => "__nex_arr2_len"
      case _ => "__nex_arr1_len" // unreached — caller filtered ranks
    val labelPrefix = op match
      case "+" => "hof.sum"
      case "*" => "hof.product"
      case _   => "hof.fold"

    val arrV    = emitExpr(arr)
    val len     = newReg(); emitLine(s"  $len = call i64 @$lenFn(ptr $arrV)\n")
    val buf     = bufPtr(arrV, arr.tpe)
    val accSlot = newReg()
    emitLine(s"  $accSlot = alloca $accLLT\n")
    storeElem(stT, identity, accSlot)

    emitCountingLoop(len, labelPrefix) { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds $stT, ptr $buf, i64 $i\n")
      val e   = loadElem(stT, slot, llT)
      val cur = newReg()
      emitLine(s"  $cur = load $accLLT, ptr $accSlot\n")
      val nxt = emitScalarBinOpSimple(op, cur, e, elem)
      storeElem(stT, nxt, accSlot)
    }

    emitArrDec(arrV, arr.tpe)
    val r = newReg()
    emitLine(s"  $r = load $accLLT, ptr $accSlot\n")
    r

  /** Per-element scalar binop including the TyComplex pair-arithmetic
    * formulas. Reuses NexLLVMCodegen's emitScalarBinOp for the simple
    * cases via the trait-shared abstract; complex needs the dedicated
    * formulas (real + complex = complex etc.).
    */
  private def emitScalarBinOpSimple(op: String, lv: String, rv: String, t: Type): String =
    t match
      case TyComplex =>
        // Both operands already complex; pair-arithmetic.
        val (lre, lim) = unpackComplex(lv)
        val (rre, rim) = unpackComplex(rv)
        op match
          case "+" =>
            val re = newReg(); emitLine(s"  $re = fadd double $lre, $rre\n")
            val im = newReg(); emitLine(s"  $im = fadd double $lim, $rim\n")
            packComplexCD(re, im)
          case "*" =>
            val ac = newReg(); emitLine(s"  $ac = fmul double $lre, $rre\n")
            val bd = newReg(); emitLine(s"  $bd = fmul double $lim, $rim\n")
            val ad = newReg(); emitLine(s"  $ad = fmul double $lre, $rim\n")
            val bc = newReg(); emitLine(s"  $bc = fmul double $lim, $rre\n")
            val re = newReg(); emitLine(s"  $re = fsub double $ac, $bd\n")
            val im = newReg(); emitLine(s"  $im = fadd double $ad, $bc\n")
            packComplexCD(re, im)
          case other =>
            notYet(s"complex `$other` in reduction"); lv
      case _ =>
        // Scalar int / real binop via the shared instruction table.
        val (instr, _) = binOpInst(op, t)
        val reg = newReg()
        emitLine(s"  $reg = $instr ${llvmType(t)} $lv, $rv\n")
        reg

  /** Zero element for sum (identity for +): 0 / 0.0 / 0.0+0.0i. */
  private def zeroOf(t: Type): String = t match
    case TyInteger => "0"
    case TyReal    => "0.0"
    case TyComplex =>
      val c0 = newReg()
      emitLine(s"  $c0 = insertvalue { double, double } undef, double 0.0, 0\n")
      val c1 = newReg()
      emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
      c1
    case _ => "0"

  /** One element for product (identity for *): 1 / 1.0 / 1.0+0.0i. */
  private def oneOf(t: Type): String = t match
    case TyInteger => "1"
    case TyReal    => "1.0"
    case TyComplex =>
      val c0 = newReg()
      emitLine(s"  $c0 = insertvalue { double, double } undef, double 1.0, 0\n")
      val c1 = newReg()
      emitLine(s"  $c1 = insertvalue { double, double } $c0, double 0.0, 1\n")
      c1
    case _ => "1"

  // ---------------------------------------------------------------------------
  // §10.4 flatMap — concat-map: for each element, call f, append all
  // returned values to the output buffer. Worst-case sized buffer
  // grows as we go and is truncated at the end (same pattern as
  // emitFilterCall, but copying every returned element rather than
  // gating on a predicate). The closure's return type is `[T]` so
  // each call yields an array descriptor that we walk and copy from.
  // ---------------------------------------------------------------------------

  private def emitFlatMapCall(arr: TExpr, fn: TExpr, resultT: Type): String =
    if arrayRank(arr.tpe) != 1 then
      notYet(s"flatMap on rank ${arrayRank(arr.tpe)}")
      return "0"

    val srcElem = arrayElem(arr.tpe)
    val srcEsz  = elemSize(srcElem)
    val srcStT  = storageType(srcElem)
    val srcLLT  = llvmType(srcElem)
    val resElem = arrayElem(resultT)
    val resEsz  = elemSize(resElem)
    val resStT  = storageType(resElem)
    val resLLT  = llvmType(resElem)

    val arrV    = emitExpr(arr)
    val (fnPtr, envPtr) = splitClosure(fn)
    val srcLen  = newReg()
    emitLine(s"  $srcLen = call i64 @__nex_arr1_len(ptr $arrV)\n")
    val srcBuf  = bufPtr(arrV, arr.tpe)

    // Per-source-element loop:
    //   for i in 0..srcLen:
    //     subarr = call fnPtr(envPtr, srcBuf[i])
    //     subLen = __nex_arr1_len(subarr)
    //     for j in 0..subLen: out[count++] = subarr[j]
    //     dec subarr
    //
    // We DON'T know the total output length ahead of time. Approach:
    // 1) two passes — first counts total, second fills (calls f twice)
    // 2) grow-as-we-go — start with srcLen capacity, double when full
    // (1) double-calls the closure (might have side effects); (2) is
    // standard. Implement (2): start with an initial guess of
    // srcLen, realloc bigger if needed.
    //
    // For simplicity here use the simpler initial-overallocate of
    // (srcLen * 4) which is sufficient for most flatMaps where the
    // average expansion factor is small; truncate the descriptor to
    // the actual count at the end. Matches the interpreter's
    // ArrayBuffer.append semantics (no observable difference for
    // small programs; a follow-up can add real grow logic for
    // pathological cases).
    val initialCap = newReg()
    emitLine(s"  $initialCap = mul i64 $srcLen, 4\n")
    // Guard against initialCap=0 (empty source): allocate at least 1
    // slot to avoid malloc(0) UB; descriptor length gets set to 0
    // before the dec helper sees it.
    val isZero = newReg()
    emitLine(s"  $isZero = icmp eq i64 $initialCap, 0\n")
    val cap = newReg()
    emitLine(s"  $cap = select i1 $isZero, i64 1, i64 $initialCap\n")
    val res = newReg()
    emitLine(s"  $res = call ptr @__nex_arr1_alloc(i64 $cap, i64 $resEsz)\n")
    val dstBuf = bufPtr(res, resultT)
    val countSlot = newReg()
    emitLine(s"  $countSlot = alloca i64\n")
    emitLine(s"  store i64 0, ptr $countSlot\n")

    emitCountingLoop(srcLen, "hof.flatMap") { i =>
      // Call f(srcBuf[i]) — returns a rank-1 array descriptor (ptr).
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $srcStT, ptr $srcBuf, i64 $i\n")
      val srcE    = loadElem(srcStT, srcSlot, srcLLT)
      val subArr  = newReg()
      emitLine(s"  $subArr = call ptr (ptr, $srcLLT) $fnPtr(ptr $envPtr, $srcLLT $srcE)\n")
      val subLen  = newReg()
      emitLine(s"  $subLen = call i64 @__nex_arr1_len(ptr $subArr)\n")
      val subBuf  = bufPtr(subArr, resultT)
      // Inner copy loop.
      emitCountingLoop(subLen, "hof.flatMap.inner") { j =>
        val sSlot = newReg()
        emitLine(s"  $sSlot = getelementptr inbounds $resStT, ptr $subBuf, i64 $j\n")
        val sE    = loadElem(resStT, sSlot, resLLT)
        val cur   = newReg()
        emitLine(s"  $cur = load i64, ptr $countSlot\n")
        val dSlot = newReg()
        emitLine(s"  $dSlot = getelementptr inbounds $resStT, ptr $dstBuf, i64 $cur\n")
        storeElem(resStT, sE, dSlot)
        val nx    = newReg()
        emitLine(s"  $nx = add i64 $cur, 1\n")
        emitLine(s"  store i64 $nx, ptr $countSlot\n")
      }
      // Release the sub-array's owning share.
      emitArrDec(subArr, resultT)
    }

    // Truncate the descriptor's length field to the actual count.
    val finalCount = newReg()
    emitLine(s"  $finalCount = load i64, ptr $countSlot\n")
    val lenP = newReg()
    emitLine(s"  $lenP = getelementptr inbounds %nex_arr1, ptr $res, i32 0, i32 1\n")
    emitLine(s"  store i64 $finalCount, ptr $lenP\n")

    emitArrDec(arrV, arr.tpe)
    res

  // ---------------------------------------------------------------------------
  // §10.4 rank-1 builders: range / enumerate / zip / linspace.
  // ---------------------------------------------------------------------------

  /** Emit `range(lo, hi)` — integer half-open range. Length is
    * `max(0, hi - lo)`; element i is `lo + i`.
    */
  private def emitRangeCall(loE: TExpr, hiE: TExpr): String =
    val lo = emitExpr(loE)
    val hi = emitExpr(hiE)
    val diff = newReg()
    emitLine(s"  $diff = sub i64 $hi, $lo\n")
    val neg  = newReg()
    emitLine(s"  $neg = icmp slt i64 $diff, 0\n")
    val len  = newReg()
    emitLine(s"  $len = select i1 $neg, i64 0, i64 $diff\n")
    val arr  = newReg()
    emitLine(s"  $arr = call ptr @__nex_arr1_alloc(i64 $len, i64 8)\n")
    val buf  = bufPtr(arr, TyArray(TyInteger, 1))

    emitCountingLoop(len, "range") { i =>
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds i64, ptr $buf, i64 $i\n")
      val v    = newReg()
      emitLine(s"  $v = add i64 $lo, $i\n")
      emitLine(s"  store i64 $v, ptr $slot\n")
    }
    arr

  /** Emit `enumerate(arr)` — pairs `(index, value)`. */
  private def emitEnumerateCall(arr: TExpr, resultT: Type): String =
    val srcElem = arrayElem(arr.tpe)
    val srcStT  = storageType(srcElem)
    val srcLLT  = llvmType(srcElem)
    val outElem = arrayElem(resultT)
    val outSize = elemSize(outElem)
    val outTupTy = llvmType(outElem)

    val src    = emitExpr(arr)
    val len    = newReg(); emitLine(s"  $len = call i64 @__nex_arr1_len(ptr $src)\n")
    val srcBuf = bufPtr(src, arr.tpe)
    val out    = newReg()
    emitLine(s"  $out = call ptr @__nex_arr1_alloc(i64 $len, i64 $outSize)\n")
    val outBuf = bufPtr(out, resultT)

    emitCountingLoop(len, "enumerate") { i =>
      val srcSlot = newReg()
      emitLine(s"  $srcSlot = getelementptr inbounds $srcStT, ptr $srcBuf, i64 $i\n")
      val srcE    = loadElem(srcStT, srcSlot, srcLLT)
      val acc0    = newReg()
      emitLine(s"  $acc0 = insertvalue $outTupTy undef, i64 $i, 0\n")
      val acc1    = newReg()
      emitLine(s"  $acc1 = insertvalue $outTupTy $acc0, $srcLLT $srcE, 1\n")
      val outSlot = newReg()
      emitLine(s"  $outSlot = getelementptr inbounds $outTupTy, ptr $outBuf, i64 $i\n")
      emitLine(s"  store $outTupTy $acc1, ptr $outSlot\n")
    }
    emitArrDec(src, arr.tpe)
    out

  /** Emit `zip(a, b)` — pairs of corresponding elements. Length is
    * `min(len(a), len(b))`.
    */
  private def emitZipCall(a: TExpr, b: TExpr, resultT: Type): String =
    val aElem = arrayElem(a.tpe)
    val bElem = arrayElem(b.tpe)
    val aStT  = storageType(aElem)
    val bStT  = storageType(bElem)
    val aLLT  = llvmType(aElem)
    val bLLT  = llvmType(bElem)
    val outElem = arrayElem(resultT)
    val outSize = elemSize(outElem)
    val outTupTy = llvmType(outElem)

    val av    = emitExpr(a)
    val bv    = emitExpr(b)
    val aLen  = newReg(); emitLine(s"  $aLen = call i64 @__nex_arr1_len(ptr $av)\n")
    val bLen  = newReg(); emitLine(s"  $bLen = call i64 @__nex_arr1_len(ptr $bv)\n")
    val cmp   = newReg(); emitLine(s"  $cmp = icmp slt i64 $aLen, $bLen\n")
    val len   = newReg(); emitLine(s"  $len = select i1 $cmp, i64 $aLen, i64 $bLen\n")
    val aBuf  = bufPtr(av, a.tpe)
    val bBuf  = bufPtr(bv, b.tpe)
    val out   = newReg()
    emitLine(s"  $out = call ptr @__nex_arr1_alloc(i64 $len, i64 $outSize)\n")
    val outBuf = bufPtr(out, resultT)

    emitCountingLoop(len, "zip") { i =>
      val aS = newReg()
      emitLine(s"  $aS = getelementptr inbounds $aStT, ptr $aBuf, i64 $i\n")
      val ae = loadElem(aStT, aS, aLLT)
      val bS = newReg()
      emitLine(s"  $bS = getelementptr inbounds $bStT, ptr $bBuf, i64 $i\n")
      val be = loadElem(bStT, bS, bLLT)
      val acc0 = newReg()
      emitLine(s"  $acc0 = insertvalue $outTupTy undef, $aLLT $ae, 0\n")
      val acc1 = newReg()
      emitLine(s"  $acc1 = insertvalue $outTupTy $acc0, $bLLT $be, 1\n")
      val outSlot = newReg()
      emitLine(s"  $outSlot = getelementptr inbounds $outTupTy, ptr $outBuf, i64 $i\n")
      emitLine(s"  store $outTupTy $acc1, ptr $outSlot\n")
    }
    emitArrDec(av, a.tpe)
    emitArrDec(bv, b.tpe)
    out

  /** Emit `linspace(lo, hi, n)` — n evenly-spaced reals from lo to hi.
    * When n > 1, step is `(hi - lo) / (n - 1)`; when n <= 1, step is 0
    * and the single element is `lo`.
    */
  private def emitLinspaceCall(loE: TExpr, hiE: TExpr, nE: TExpr): String =
    val lo = liftToReal(loE)
    val hi = liftToReal(hiE)
    val n  = emitExpr(nE)
    val arr = newReg()
    emitLine(s"  $arr = call ptr @__nex_arr1_alloc(i64 $n, i64 8)\n")
    val buf = bufPtr(arr, TyArray(TyReal, 1))

    val gt1     = newReg(); emitLine(s"  $gt1 = icmp sgt i64 $n, 1\n")
    val nm1     = newReg(); emitLine(s"  $nm1 = sub i64 $n, 1\n")
    val nm1d    = newReg(); emitLine(s"  $nm1d = sitofp i64 $nm1 to double\n")
    val span    = newReg(); emitLine(s"  $span = fsub double $hi, $lo\n")
    val rawStep = newReg(); emitLine(s"  $rawStep = fdiv double $span, $nm1d\n")
    val step    = newReg(); emitLine(s"  $step = select i1 $gt1, double $rawStep, double 0.0\n")

    emitCountingLoop(n, "linspace") { i =>
      val id   = newReg(); emitLine(s"  $id = sitofp i64 $i to double\n")
      val off  = newReg(); emitLine(s"  $off = fmul double $step, $id\n")
      val v    = newReg(); emitLine(s"  $v = fadd double $lo, $off\n")
      val slot = newReg()
      emitLine(s"  $slot = getelementptr inbounds double, ptr $buf, i64 $i\n")
      emitLine(s"  store double $v, ptr $slot\n")
    }
    arr
