package io.github.edadma.nex

/** Prelude dispatch for the LLVM backend.
  *
  * Owns [[emitPreludeCall]], the master `name → emitter` switch the
  * elaborator routes every `SymKind.Prelude` call through, plus the
  * scalar primitives the dispatch uses directly:
  *
  *   - [[liftToReal]] / [[emitLibmUnary]] — i64 → double promotion plus
  *     the shared shape for libm-style unary bridges.
  *   - [[packComplexCD]] / [[unpackComplex]] — `{double, double}` layout
  *     helpers shared with [[NexLLVMArrayPrelude]]'s reduction kernels.
  *   - [[emitMinMax]] — the scalar two-argument `min`/`max` (the unary
  *     array overload lives in [[NexLLVMArrayPrelude]]).
  *   - [[emitArrayLengthish]] — `length` / `rows` / `cols`, which are
  *     small enough to live next to the dispatch instead of carving out
  *     their own trait.
  *
  * The bulkier emitters live in sibling traits keyed by section:
  *
  *   - [[NexLLVMAsserts]] — the §10.8 assertion family.
  *   - [[NexLLVMArrayPrelude]] — §10.4 HOFs / reductions / builders,
  *     §10.5 array construction, `view`.
  *   - [[NexLLVMMatrixPrelude]] — §10.4 rank-2 ops (matmul, transpose,
  *     reshape, flatten, sum_axis, …).
  *
  * Cross-trait calls happen through the abstract declarations in
  * [[NexLLVMState]], so this file's dispatch never names a sibling
  * trait directly.
  */
protected trait NexLLVMPrelude extends NexLLVMState:

  protected def emitPreludeCall(name: String, args: List[TExpr], resultT: Type): String =
    (name, args) match
      case ("length", List(a)) => emitArrayLengthish(a, "len")
      case ("rows",   List(a)) => emitArrayLengthish(a, "rows")
      case ("cols",   List(a)) => emitArrayLengthish(a, "cols")

      // The scalar transcendentals (sqrt/exp/log/sin/cos/tan and their
      // companion entries) all live in `prelude/scalar.nex` as either
      // direct `@intrinsic("libm.X")` bridges or overloaded source-body
      // defs that compose those bridges (complex variants). The Prelude-
      // path dispatch below only handles names that vary their return
      // type by argument type (abs / conj / arg / min / max / sign /
      // length / rows / cols / shape / fill / zeros / ones / linspace /
      // identity / map / reduce / filter / range / enumerate / zip /
      // dot / sum / product / sum_axis / transpose / matmul / diag /
      // reshape / flatten / print / format / to_real / to_integer /
      // to_complex / assert*).

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
      case ("assert", List(c, msg)) =>
        // Two-arg form: thread the user message into the trap text so
        // a downstream `assert_traps(fn, "<substr>")` matches the
        // interpreter's behavior.
        val cv = emitExpr(c)
        val mv = emitExpr(msg)
        emitLine(s"  call void @__nex_assert_with_msg(i1 $cv, ptr $mv)\n")
        "void"
      case ("assert_eq", List(a, b)) =>
        emitAssertEq(a, b); "void"
      case ("assert_approx", List(a, b, eps)) =>
        (a.tpe, b.tpe) match
          case (TyArray(_, 1), TyArray(_, 1)) =>
            emitAssertApproxArr1(a, b, eps); "void"
          case (TyArray(_, 2), TyArray(_, 2)) =>
            emitAssertApproxArr2(a, b, eps); "void"
          case (TyComplex, TyComplex) =>
            emitAssertApproxComplex(a, b, eps); "void"
          case _ =>
            emitAssertApprox(a, b, eps); "void"
      case ("assert_traps", List(fn)) =>
        emitAssertTraps(fn, expectedSubstr = None); "void"
      case ("assert_traps", List(fn, sub)) =>
        emitAssertTraps(fn, expectedSubstr = Some(sub)); "void"

      // §10.4 rank-1 reductions — counted loop + accumulator. Element
      // type may be integer (i64), real (double), or complex
      // ({double, double}); the op is fixed (+ for sum, * for product,
      // a*b accumulated for dot).
      case ("sum",     List(arr))              => emitSumCall(arr, resultT)
      case ("product", List(arr))              => emitProductCall(arr, resultT)
      case ("dot",     List(a, b))             => emitDotCall(a, b, resultT)

      // §10.4 unary min/max on an array. The binary scalar form is
      // handled by the elaborator's regular call path (it generates a
      // TCall with two args and the prelude dispatcher above handles
      // that via the lifted scalar entry); only the array overload
      // routes here.
      case ("min", List(arr)) if isArrayType(arr.tpe) =>
        emitMinMaxCall(arr, resultT, isMin = true)
      case ("max", List(arr)) if isArrayType(arr.tpe) =>
        emitMinMaxCall(arr, resultT, isMin = false)

      // §10.4 rank-1 builders.
      case ("range",     List(lo, hi))         => emitRangeCall(lo, hi)
      case ("enumerate", List(arr))            => emitEnumerateCall(arr, resultT)
      case ("zip",       List(a, b))           => emitZipCall(a, b, resultT)
      case ("linspace",  List(lo, hi, n))      => emitLinspaceCall(lo, hi, n)

      // §10.4 array HOFs — direct inlined loops that dispatch each
      // iteration through the indirect-call closure helper. Works
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

      // view-style slicing. The range arg is a `TBinOp("..", lo, hi)`
      // (exclusive) or `("..=", lo, hi)` (inclusive) — we unpack both
      // bounds, lower an inclusive range to the equivalent exclusive
      // `hi+1`, then call the runtime helper. With a third arg the
      // view picks every k-th element (rank-1 strided form).
      case ("view", List(arr, r))               => emitViewCall(arr, r, None, resultT)
      case ("view", List(arr, r, step))         => emitViewCall(arr, r, Some(step), resultT)

      // §10.4 rank-2 ops.
      case ("shape",     List(a))              => emitShapeCall(a, resultT)
      case ("transpose", List(a))              => emitTransposeCall(a, resultT)
      case ("matmul",    List(a, b))           => emitMatMulCall(a, b, resultT)
      case ("diag",      List(a))              => emitDiagCall(a, resultT)
      case ("reshape",   List(a, r, c))        => emitReshapeCall(a, r, c, resultT)
      case ("flatten",   List(a))              => emitFlattenCall(a, resultT)
      case ("sum_axis",  List(m, ax))          => emitSumAxisCall(m, ax, resultT)

      case _ =>
        notYet(s"prelude `$name`/${args.size}"); "0"

  /** Lift an integer-typed expression to double via `sitofp`; pass-through
    * for double-typed expressions. Used by every libm bridge so callers
    * can write `sqrt(4)` and get `2.0`.
    */
  protected def liftToReal(e: TExpr): String =
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

  /** Pack two doubles into a `{ double, double }` aggregate matching
    * Nex's complex value layout. Used by the pair-arithmetic paths in
    * complex `+`/`*`/`/` (NexLLVMCodegen) and by complex reductions
    * here in NexLLVMPrelude.
    */
  protected def packComplexCD(re: String, im: String): String =
    val c0 = newReg()
    emitLine(s"  $c0 = insertvalue { double, double } undef, double $re, 0\n")
    val c1 = newReg()
    emitLine(s"  $c1 = insertvalue { double, double } $c0, double $im, 1\n")
    c1

  /** Extract `(re, im)` from a complex SSA value. */
  protected def unpackComplex(z: String): (String, String) =
    val re = newReg()
    emitLine(s"  $re = extractvalue { double, double } $z, 0\n")
    val im = newReg()
    emitLine(s"  $im = extractvalue { double, double } $z, 1\n")
    (re, im)

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
