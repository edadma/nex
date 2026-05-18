package io.github.edadma.nex

/** External declarations, runtime helpers, format-string constants, and
  * the `__nex_init_globals` function that initializes top-level bindings
  * before `main` runs.
  */
protected trait NexLLVMPreamble extends NexLLVMState:

  // ---------------------------------------------------------------------------
  // Preamble — external declarations, format strings.
  // ---------------------------------------------------------------------------

  protected def emitPreamble(): Unit =
    out.append(
      """; Nex LLVM IR module (v0 scaffolding)
        |
        |declare i32 @printf(ptr, ...)
        |declare ptr @malloc(i64)
        |declare void @free(ptr)
        |declare void @abort()
        |declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg)
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
        |declare double @sqrt(double)
        |declare double @cbrt(double)
        |declare double @exp(double)
        |declare double @log(double)
        |declare double @log2(double)
        |declare double @log10(double)
        |declare double @sin(double)
        |declare double @cos(double)
        |declare double @tan(double)
        |declare double @asin(double)
        |declare double @acos(double)
        |declare double @atan(double)
        |declare double @atan2(double, double)
        |declare double @sinh(double)
        |declare double @cosh(double)
        |declare double @tanh(double)
        |declare double @ceil(double)
        |declare double @round(double)
        |declare double @trunc(double)
        |declare i64 @llabs(i64)
        |
        |@.assert_fail_msg = private unnamed_addr constant [21 x i8] c"trap: assert failed\0A\00"
        |@.assert_eq_msg   = private unnamed_addr constant [24 x i8] c"trap: assert_eq failed\0A\00"
        |@.assert_approx_msg = private unnamed_addr constant [28 x i8] c"trap: assert_approx failed\0A\00"
        |
        |define void @__nex_assert(i1 %cond) {
        |entry:
        |  br i1 %cond, label %ok, label %fail
        |fail:
        |  call i32 (ptr, ...) @printf(ptr @.assert_fail_msg)
        |  call void @abort()
        |  unreachable
        |ok:
        |  ret void
        |}
        |
        |; asinh(x) = log(x + sqrt(x^2 + 1))
        |define double @__nex_asinh(double %x) {
        |entry:
        |  %x2 = fmul double %x, %x
        |  %p1 = fadd double %x2, 1.0
        |  %s  = call double @sqrt(double %p1)
        |  %xs = fadd double %x, %s
        |  %r  = call double @log(double %xs)
        |  ret double %r
        |}
        |
        |; acosh(x) = log(x + sqrt(x^2 - 1))
        |define double @__nex_acosh(double %x) {
        |entry:
        |  %x2 = fmul double %x, %x
        |  %m1 = fsub double %x2, 1.0
        |  %s  = call double @sqrt(double %m1)
        |  %xs = fadd double %x, %s
        |  %r  = call double @log(double %xs)
        |  ret double %r
        |}
        |
        |; atanh(x) = 0.5 * log((1+x) / (1-x))
        |define double @__nex_atanh(double %x) {
        |entry:
        |  %p1 = fadd double 1.0, %x
        |  %m1 = fsub double 1.0, %x
        |  %q  = fdiv double %p1, %m1
        |  %l  = call double @log(double %q)
        |  %r  = fmul double %l, 5.0e-01
        |  ret double %r
        |}
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

  /** Runs every top-level binding initializer in declaration order. The
    * function returns void and is called once from `main`'s entry block.
    * Functions are already initialized at compile time (each is a
    * `define`), so an initializer body may freely call any function.
    */
  protected def emitInitFunction(bindings: List[TTopBinding]): Unit =
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
