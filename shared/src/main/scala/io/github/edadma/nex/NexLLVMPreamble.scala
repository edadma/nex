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
        |declare i32 @snprintf(ptr, i64, ptr, ...)
        |declare ptr @malloc(i64)
        |declare void @free(ptr)
        |declare void @abort()
        |declare i64 @strlen(ptr)
        |declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg)
        |
        |; setjmp / longjmp — used by __nex_assert_traps to catch traps.
        |; setjmp must be marked `returns_twice` so LLVM does not optimize
        |; assuming a single control-flow exit from the call.
        |declare i32 @setjmp(ptr) returns_twice
        |declare void @longjmp(ptr, i32)
        |
        |; Thread-local pointer to the active trap jmp_buf chain head, or
        |; null when no assert_traps is on the stack. The buffer is large
        |; enough to hold any platform's `jmp_buf` (Darwin arm64 needs
        |; ~192 bytes; 2 KB gives substantial headroom).
        |@__nex_trap_buf = internal thread_local global ptr null, align 8
        |
        |; Thread-local pointer to the trap message that would be shown if
        |; this trap escapes to the top level. Trap sites stash a message
        |; here BEFORE calling `__nex_trap` instead of printing eagerly,
        |; so a caught trap stays silent (matching the interpreter, where
        |; `NexTrap.msg` is only printed when uncaught).
        |@__nex_trap_msg = internal thread_local global ptr null, align 8
        |
        |@.assert_traps_fail_msg = private unnamed_addr constant [48 x i8] c"trap: assert_traps: expected trap, got no trap\0A\00"
        |
        |; __nex_trap is the single trap-emission point. If an enclosing
        |; assert_traps has set up a jmp_buf, longjmp out of it (silently,
        |; clearing the stashed message); otherwise print the stashed
        |; message and abort. Every trap site stashes its message via
        |; @__nex_trap_msg before calling here.
        |define void @__nex_trap() noreturn {
        |entry:
        |  %buf = load ptr, ptr @__nex_trap_buf, align 8
        |  %has = icmp ne ptr %buf, null
        |  br i1 %has, label %lj, label %ab
        |lj:
        |  store ptr null, ptr @__nex_trap_msg, align 8
        |  call void @longjmp(ptr %buf, i32 1)
        |  unreachable
        |ab:
        |  %msg  = load ptr, ptr @__nex_trap_msg, align 8
        |  %hasm = icmp ne ptr %msg, null
        |  br i1 %hasm, label %prn, label %doab
        |prn:
        |  call i32 (ptr, ...) @printf(ptr %msg)
        |  br label %doab
        |doab:
        |  call void @abort()
        |  unreachable
        |}
        |
        |; __nex_trap_msg sets the stashed message and calls __nex_trap.
        |; Trap sites use this helper for the common "print this message,
        |; then trap" pattern.
        |define void @__nex_trap_with(ptr %msg) noreturn {
        |entry:
        |  store ptr %msg, ptr @__nex_trap_msg, align 8
        |  call void @__nex_trap()
        |  unreachable
        |}
        |
        |; --- Array descriptor types (§8.5) -------------------------------------
        |; rank-1: { refcount, length, data }                     ; 24 bytes
        |; rank-2: { refcount, rows,   cols, data }               ; 32 bytes
        |; string: { refcount, length, data }                     ; 24 bytes
        |; Strings are refcounted just like rank-1 arrays. Literal strings get
        |; static descriptors with refcount=-1 (immortal sentinel); the inc/dec
        |; helpers no-op when they see that sentinel. Computed strings (concat,
        |; format, value-to-string) malloc fresh descriptors with refcount=1.
        |; A string's `data` field always points at a NUL-terminated buffer so
        |; we can hand it straight to libc (`printf("%s", data)`); `length`
        |; tracks the byte count NOT counting the terminator.
        |%nex_arr1 = type { i64, i64, ptr }
        |%nex_arr2 = type { i64, i64, i64, ptr }
        |%nex_str  = type { i64, i64, ptr }
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
        |declare double @copysign(double, double)
        |
        |@.assert_fail_msg = private unnamed_addr constant [21 x i8] c"trap: assert failed\0A\00"
        |@.assert_eq_msg   = private unnamed_addr constant [24 x i8] c"trap: assert_eq failed\0A\00"
        |@.assert_approx_msg = private unnamed_addr constant [28 x i8] c"trap: assert_approx failed\0A\00"
        |
        |define void @__nex_assert(i1 %cond) {
        |entry:
        |  br i1 %cond, label %ok, label %fail
        |fail:
        |  call void @__nex_trap_with(ptr @.assert_fail_msg)
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
        |  call void @__nex_trap_with(ptr @.oob_msg)
        |  unreachable
        |ok:
        |  %dp   = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 2
        |  %buf  = load ptr, ptr %dp
        |  %byte_off = mul i64 %idx, %elem_size
        |  %slot = getelementptr inbounds i8, ptr %buf, i64 %byte_off
        |  ret ptr %slot
        |}
        |
        |; --- String runtime helpers -------------------------------------------
        |;
        |; Strings parallel rank-1 arrays — same { refcount, length, data }
        |; descriptor, but `data` always points at a NUL-terminated byte
        |; buffer (so libc's printf("%s", data) and strlen(data) just work).
        |;
        |; Two allocation paths: __nex_str_alloc mallocs a heap descriptor +
        |; data buffer (refcount=1); literals are emitted as static globals
        |; with refcount=-1 (immortal sentinel). __nex_str_inc and _dec
        |; no-op on the immortal sentinel so literals can be passed around
        |; freely without leaking the descriptor.
        |
        |; Allocate a fresh string descriptor for a string of `len` bytes
        |; (not counting the NUL terminator). The data buffer is `len+1`
        |; bytes; the caller is responsible for filling bytes 0..len-1 and
        |; writing a NUL at byte len.
        |define ptr @__nex_str_alloc(i64 %len) {
        |entry:
        |  %desc = call ptr @malloc(i64 24)
        |  %rcp  = getelementptr inbounds %nex_str, ptr %desc, i32 0, i32 0
        |  store i64 1, ptr %rcp
        |  %lp   = getelementptr inbounds %nex_str, ptr %desc, i32 0, i32 1
        |  store i64 %len, ptr %lp
        |  %dp   = getelementptr inbounds %nex_str, ptr %desc, i32 0, i32 2
        |  %total = add i64 %len, 1
        |  %buf   = call ptr @malloc(i64 %total)
        |  store ptr %buf, ptr %dp
        |  ret ptr %desc
        |}
        |
        |; Bump refcount unless the descriptor is the immortal sentinel.
        |define void @__nex_str_inc(ptr %s) {
        |entry:
        |  %is_null = icmp eq ptr %s, null
        |  br i1 %is_null, label %done, label %check
        |check:
        |  %rcp = getelementptr inbounds %nex_str, ptr %s, i32 0, i32 0
        |  %rc  = load i64, ptr %rcp
        |  %im  = icmp eq i64 %rc, -1
        |  br i1 %im, label %done, label %inc
        |inc:
        |  %new = add i64 %rc, 1
        |  store i64 %new, ptr %rcp
        |  br label %done
        |done:
        |  ret void
        |}
        |
        |; Drop a share. On the immortal sentinel this is a no-op. On the
        |; last live share, free the data buffer and the descriptor.
        |define void @__nex_str_dec(ptr %s) {
        |entry:
        |  %is_null = icmp eq ptr %s, null
        |  br i1 %is_null, label %done, label %check
        |check:
        |  %rcp = getelementptr inbounds %nex_str, ptr %s, i32 0, i32 0
        |  %rc  = load i64, ptr %rcp
        |  %im  = icmp eq i64 %rc, -1
        |  br i1 %im, label %done, label %dec
        |dec:
        |  %new = sub i64 %rc, 1
        |  store i64 %new, ptr %rcp
        |  %iz  = icmp eq i64 %new, 0
        |  br i1 %iz, label %free_it, label %done
        |free_it:
        |  %dp  = getelementptr inbounds %nex_str, ptr %s, i32 0, i32 2
        |  %buf = load ptr, ptr %dp
        |  call void @free(ptr %buf)
        |  call void @free(ptr %s)
        |  br label %done
        |done:
        |  ret void
        |}
        |
        |; Length of a string descriptor (NOT including the NUL terminator).
        |define i64 @__nex_str_len(ptr %s) {
        |entry:
        |  %lp = getelementptr inbounds %nex_str, ptr %s, i32 0, i32 1
        |  %l  = load i64, ptr %lp
        |  ret i64 %l
        |}
        |
        |; Pointer to the NUL-terminated data buffer.
        |define ptr @__nex_str_data(ptr %s) {
        |entry:
        |  %dp = getelementptr inbounds %nex_str, ptr %s, i32 0, i32 2
        |  %d  = load ptr, ptr %dp
        |  ret ptr %d
        |}
        |
        |; Build a fresh string descriptor from an i64 (decimal). Uses
        |; snprintf-twice (size first, then write). Always heap-allocated
        |; with refcount=1.
        |define ptr @__nex_str_from_i64(i64 %n) {
        |entry:
        |  %len = call i32 (ptr, i64, ptr, ...) @snprintf(ptr null, i64 0, ptr @.fmt_int_raw, i64 %n)
        |  %len64 = sext i32 %len to i64
        |  %res = call ptr @__nex_str_alloc(i64 %len64)
        |  %dp  = getelementptr inbounds %nex_str, ptr %res, i32 0, i32 2
        |  %d   = load ptr, ptr %dp
        |  %cap = add i64 %len64, 1
        |  %ignored = call i32 (ptr, i64, ptr, ...) @snprintf(ptr %d, i64 %cap, ptr @.fmt_int_raw, i64 %n)
        |  ret ptr %res
        |}
        |
        |; Build a fresh string descriptor from a bool. Returns immortal
        |; "true" / "false" literals (no malloc — they're already in the
        |; literal pool, so we route through the runtime helper that picks
        |; the matching static descriptor at codegen time). Implemented
        |; codegen-side to avoid threading two literal-pool descriptors
        |; through a runtime select.
        |
        |; Build a fresh string descriptor from a real (double). Mirrors
        |; the interpreter's formatValue: whole numbers under 1e15 print
        |; as "<lld>.0", everything else via %g.
        |define ptr @__nex_str_from_double(double %v) {
        |entry:
        |  %f      = call double @floor(double %v)
        |  %is_int = fcmp oeq double %v, %f
        |  %a      = call double @fabs(double %v)
        |  %small  = fcmp olt double %a, 1.0e+15
        |  %both   = and i1 %is_int, %small
        |  br i1 %both, label %whole, label %generic
        |whole:
        |  %ll  = fptosi double %v to i64
        |  %wlen = call i32 (ptr, i64, ptr, ...) @snprintf(ptr null, i64 0, ptr @.fmt_real_int, i64 %ll)
        |  %wlen64 = sext i32 %wlen to i64
        |  %wres = call ptr @__nex_str_alloc(i64 %wlen64)
        |  %wdp  = getelementptr inbounds %nex_str, ptr %wres, i32 0, i32 2
        |  %wd   = load ptr, ptr %wdp
        |  %wcap = add i64 %wlen64, 1
        |  %wig  = call i32 (ptr, i64, ptr, ...) @snprintf(ptr %wd, i64 %wcap, ptr @.fmt_real_int, i64 %ll)
        |  ret ptr %wres
        |generic:
        |  %glen = call i32 (ptr, i64, ptr, ...) @snprintf(ptr null, i64 0, ptr @.fmt_real_g, double %v)
        |  %glen64 = sext i32 %glen to i64
        |  %gres = call ptr @__nex_str_alloc(i64 %glen64)
        |  %gdp  = getelementptr inbounds %nex_str, ptr %gres, i32 0, i32 2
        |  %gd   = load ptr, ptr %gdp
        |  %gcap = add i64 %glen64, 1
        |  %gig  = call i32 (ptr, i64, ptr, ...) @snprintf(ptr %gd, i64 %gcap, ptr @.fmt_real_g, double %v)
        |  ret ptr %gres
        |}
        |
        |; Allocate a new string descriptor for the concatenation of `a` and
        |; `b`. Inputs may be immortal or heap-allocated; the result is a
        |; fresh heap descriptor with refcount=1. Inputs are NOT dec'd — the
        |; caller owns their shares and is responsible for releasing them.
        |define ptr @__nex_str_concat(ptr %a, ptr %b) {
        |entry:
        |  %lap = getelementptr inbounds %nex_str, ptr %a, i32 0, i32 1
        |  %la  = load i64, ptr %lap
        |  %lbp = getelementptr inbounds %nex_str, ptr %b, i32 0, i32 1
        |  %lb  = load i64, ptr %lbp
        |  %total = add i64 %la, %lb
        |  %res = call ptr @__nex_str_alloc(i64 %total)
        |  %dap = getelementptr inbounds %nex_str, ptr %a, i32 0, i32 2
        |  %da  = load ptr, ptr %dap
        |  %dbp = getelementptr inbounds %nex_str, ptr %b, i32 0, i32 2
        |  %db  = load ptr, ptr %dbp
        |  %drp = getelementptr inbounds %nex_str, ptr %res, i32 0, i32 2
        |  %dr  = load ptr, ptr %drp
        |  call void @llvm.memcpy.p0.p0.i64(ptr %dr, ptr %da, i64 %la, i1 false)
        |  %off = getelementptr inbounds i8, ptr %dr, i64 %la
        |  call void @llvm.memcpy.p0.p0.i64(ptr %off, ptr %db, i64 %lb, i1 false)
        |  %nulp = getelementptr inbounds i8, ptr %dr, i64 %total
        |  store i8 0, ptr %nulp
        |  ret ptr %res
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
        |  call void @__nex_trap_with(ptr @.oob_msg)
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
        |; ---------- Closure env refcount (negative-offset header) ----------
        |; Layout: malloc returns a block of (size + 16) bytes. The first 16
        |; bytes hold `[ refcount(i64) | dtor(ptr) ]`; the env pointer we hand
        |; out points 16 bytes past the start so the capture struct's field
        |; indices stay 0..N-1 unchanged. inc/dec GEP back -16 to find the
        |; refcount; the dtor lives 8 bytes after the refcount. When rc hits
        |; zero, dec calls `dtor(env)` if non-null (the dtor walks refcounted
        |; captures and calls free itself), otherwise plain-frees the header.
        |; null env is the empty-capture sentinel and is silently skipped.
        |define ptr @__nex_env_alloc(i64 %sz, ptr %dtor) {
        |entry:
        |  %total = add i64 %sz, 16
        |  %raw   = call ptr @malloc(i64 %total)
        |  store i64 1, ptr %raw
        |  %dp    = getelementptr inbounds i8, ptr %raw, i64 8
        |  store ptr %dtor, ptr %dp
        |  %env   = getelementptr inbounds i8, ptr %raw, i64 16
        |  ret ptr %env
        |}
        |
        |define void @__nex_env_inc(ptr %env) {
        |entry:
        |  %z = icmp eq ptr %env, null
        |  br i1 %z, label %nul, label %do
        |do:
        |  %hdr = getelementptr inbounds i8, ptr %env, i64 -16
        |  %rc  = load i64, ptr %hdr
        |  %rc1 = add i64 %rc, 1
        |  store i64 %rc1, ptr %hdr
        |  ret void
        |nul:
        |  ret void
        |}
        |
        |define void @__nex_env_dec(ptr %env) {
        |entry:
        |  %z = icmp eq ptr %env, null
        |  br i1 %z, label %nul, label %do
        |do:
        |  %hdr = getelementptr inbounds i8, ptr %env, i64 -16
        |  %rc  = load i64, ptr %hdr
        |  %rc1 = sub i64 %rc, 1
        |  store i64 %rc1, ptr %hdr
        |  %dead = icmp eq i64 %rc1, 0
        |  br i1 %dead, label %fr, label %ok
        |fr:
        |  %dp   = getelementptr inbounds i8, ptr %hdr, i64 8
        |  %dtor = load ptr, ptr %dp
        |  %has  = icmp ne ptr %dtor, null
        |  br i1 %has, label %dispatch, label %plain
        |dispatch:
        |  call void %dtor(ptr %env)
        |  ret void
        |plain:
        |  call void @free(ptr %hdr)
        |  ret void
        |ok:
        |  ret void
        |nul:
        |  ret void
        |}
        |
        |""".stripMargin,
    )

  /** Flush every pending per-element-type deep-dec helper to the module.
    * Drains [[deepDecPending]] (which may grow during emission — a deep
    * dec for `[[String]]` registers an inner helper for `[String]`)
    * until stable.
    */
  protected def flushDeepDecs(): Unit =
    while deepDecPending.nonEmpty do
      val key @ (rank, elem) = deepDecPending.head
      deepDecPending -= key
      if !deepDecEmitted.contains(key) then
        deepDecEmitted += key
        rank match
          case 1 => emitDeepDec1Helper(elem)
          case 2 => emitDeepDec2Helper(elem)
          case _ => ()

  /** Flush every pending per-aggregate-type inc / drop helper to the
    * module. Drains [[aggHelperPending]] (which may grow during
    * emission — a drop for `(string, (string, integer))` registers an
    * inner drop helper for the inner tuple) until stable.
    */
  protected def flushAggHelpers(): Unit =
    while aggHelperPending.nonEmpty do
      val t = aggHelperPending.head
      aggHelperPending -= t
      if !aggHelperEmitted.contains(t) then
        aggHelperEmitted += t
        emitAggIncHelper(t)
        emitAggDropHelper(t)

  /** Return the (type, index) list of fields for an aggregate type.
    * Tuples are positional; structs use their declared field order.
    * Non-aggregates return Nil — emitters should guard with
    * [[aggregateContainsRefCounted]] before requesting helpers.
    */
  private def aggFields(t: Type): List[(Type, Int)] = t match
    case TyTuple(es)     => es.zipWithIndex
    case TyStruct(_, fs) => fs.zipWithIndex.map { case ((_, ft), i) => (ft, i) }
    case _               => Nil

  /** IR text for a single inc / dec call on a field's loaded value
    * `fv`. Aggregate fields recurse via their own helpers (and request
    * them for emission); other refcounted leaves call the direct
    * runtime helper. Non-refcounted fields produce an empty string.
    */
  private def fieldIncIR(fT: Type, fv: String, freshLocal: () => String): String =
    if isArrayType(fT) then
      s"  call void ${arrIncFor(fT)}(ptr $fv)\n"
    else if isClosureType(fT) then
      val env = freshLocal()
      s"  $env = extractvalue { ptr, ptr } $fv, 1\n" +
        s"  call void @__nex_env_inc(ptr $env)\n"
    else if fT == TyString then
      s"  call void @__nex_str_inc(ptr $fv)\n"
    else if aggregateContainsRefCounted(fT) then
      requestAggHelper(fT)
      s"  call void ${aggIncHelperName(fT)}(${llvmType(fT)} $fv)\n"
    else ""

  private def fieldDecIR(fT: Type, fv: String, freshLocal: () => String): String =
    if isArrayType(fT) then
      s"  call void ${arrDecFor(fT)}(ptr $fv)\n"
    else if isClosureType(fT) then
      val env = freshLocal()
      s"  $env = extractvalue { ptr, ptr } $fv, 1\n" +
        s"  call void @__nex_env_dec(ptr $env)\n"
    else if fT == TyString then
      s"  call void @__nex_str_dec(ptr $fv)\n"
    else if aggregateContainsRefCounted(fT) then
      requestAggHelper(fT)
      s"  call void ${aggDropHelperName(fT)}(${llvmType(fT)} $fv)\n"
    else ""

  /** Emit `define void @__nex_inc_<mangle>(<aggTy> %v)` — extracts each
    * refcounted field by index and inc's the share. Used when an
    * aggregate-typed local var is loaded into a fresh consumer (the
    * symmetric inc to the slot-end drop) and when an aggregate is
    * stashed into a closure env. Non-refcounted fields are skipped.
    */
  protected def emitAggIncHelper(t: Type): Unit =
    val name = aggIncHelperName(t).drop(1)
    val tyL  = llvmType(t)
    val sb   = new StringBuilder
    sb.append(s"define void @$name($tyL %v) {\n")
    sb.append("entry:\n")
    var ctr = 0
    def fresh(): String =
      ctr += 1
      s"%r$ctr"
    for ((fT, idx) <- aggFields(t)) do
      if isRefCountedType(fT) then
        val fv = fresh()
        sb.append(s"  $fv = extractvalue $tyL %v, $idx\n")
        sb.append(fieldIncIR(fT, fv, () => fresh()))
    sb.append("  ret void\n")
    sb.append("}\n\n")
    out.append(sb.toString)

  /** Emit `define void @__nex_drop_<mangle>(<aggTy> %v)` — extracts
    * each refcounted field by index and dec's the share. Aggregate
    * fields recurse via their own drop helper. Called when an
    * aggregate-typed slot leaves scope (block / function end) or when
    * a parent aggregate's drop walks a nested aggregate field.
    */
  protected def emitAggDropHelper(t: Type): Unit =
    val name = aggDropHelperName(t).drop(1)
    val tyL  = llvmType(t)
    val sb   = new StringBuilder
    sb.append(s"define void @$name($tyL %v) {\n")
    sb.append("entry:\n")
    var ctr = 0
    def fresh(): String =
      ctr += 1
      s"%r$ctr"
    for ((fT, idx) <- aggFields(t)) do
      if isRefCountedType(fT) then
        val fv = fresh()
        sb.append(s"  $fv = extractvalue $tyL %v, $idx\n")
        sb.append(fieldDecIR(fT, fv, () => fresh()))
    sb.append("  ret void\n")
    sb.append("}\n\n")
    out.append(sb.toString)

  /** Element-dec call text for a single element of a deep-dec body. The
    * value register `valReg` holds the slot's loaded value (a ptr for
    * string / array element types, an aggregate by value for tuple /
    * struct element types). Calling [[arrDecFor]] / [[requestAggHelper]]
    * here may register additional deep helpers for nested arrays or
    * aggregates.
    */
  private def elemDecCallIR(elem: Type, valReg: String): String = elem match
    case TyString                            =>
      s"  call void @__nex_str_dec(ptr $valReg)\n"
    case TyArray(_, _)                       =>
      s"  call void ${arrDecFor(elem)}(ptr $valReg)\n"
    case t if aggregateContainsRefCounted(t) =>
      requestAggHelper(t)
      s"  call void ${aggDropHelperName(t)}(${llvmType(t)} $valReg)\n"
    case other                               =>
      s"  ; unsupported deep-dec elem $other\n"

  /** Storage type and LLVM type used inside a deep-dec helper's loop
    * body. For pointer-keyed elements (string / array) we load through
    * `ptr` (matching what the buffer holds and what the existing inner
    * helpers expect); for aggregates we load by the element's
    * natural llvm type (a struct value).
    */
  private def deepDecSlotTy(elem: Type): String = elem match
    case TyString | TyArray(_, _) => "ptr"
    case _                        => llvmType(elem)

  /** Emit `define void @__nex_arr1_dec_<mangle>(ptr %a)` — the deep-dec
    * variant that walks each refcounted element before freeing the
    * descriptor and buffer. Body otherwise mirrors `__nex_arr1_dec`.
    * The slot's load type depends on the element: pointer-keyed
    * elements (string / array) keep `ptr`, aggregates load the struct
    * by value and route through the per-aggregate drop helper.
    */
  protected def emitDeepDec1Helper(elem: Type): Unit =
    val name    = s"__nex_arr1_dec_${typeMangle(elem)}"
    val slotTy  = deepDecSlotTy(elem)
    val elemDec = elemDecCallIR(elem, "%v")
    out.append(
      s"""define void @$name(ptr %a) {
         |entry:
         |  %is_null = icmp eq ptr %a, null
         |  br i1 %is_null, label %done, label %dec
         |dec:
         |  %rcp = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 0
         |  %rc  = load i64, ptr %rcp
         |  %new = sub i64 %rc, 1
         |  store i64 %new, ptr %rcp
         |  %iz  = icmp eq i64 %new, 0
         |  br i1 %iz, label %walk, label %done
         |walk:
         |  %lp  = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 1
         |  %len = load i64, ptr %lp
         |  %dp  = getelementptr inbounds %nex_arr1, ptr %a, i32 0, i32 2
         |  %buf = load ptr, ptr %dp
         |  br label %loop_hdr
         |loop_hdr:
         |  %i = phi i64 [0, %walk], [%i1, %loop_body]
         |  %cmp = icmp slt i64 %i, %len
         |  br i1 %cmp, label %loop_body, label %free_it
         |loop_body:
         |  %slot = getelementptr inbounds $slotTy, ptr %buf, i64 %i
         |  %v = load $slotTy, ptr %slot
         |${elemDec}  %i1 = add i64 %i, 1
         |  br label %loop_hdr
         |free_it:
         |  call void @free(ptr %buf)
         |  call void @free(ptr %a)
         |  br label %done
         |done:
         |  ret void
         |}
         |
         |""".stripMargin,
    )

  /** Rank-2 sibling of [[emitDeepDec1Helper]]. Length is `rows * cols`;
    * the buffer is row-major (matches `emitArrayLit`). Slot load type
    * tracks the element kind — see [[emitDeepDec1Helper]].
    */
  protected def emitDeepDec2Helper(elem: Type): Unit =
    val name    = s"__nex_arr2_dec_${typeMangle(elem)}"
    val slotTy  = deepDecSlotTy(elem)
    val elemDec = elemDecCallIR(elem, "%v")
    out.append(
      s"""define void @$name(ptr %a) {
         |entry:
         |  %is_null = icmp eq ptr %a, null
         |  br i1 %is_null, label %done, label %dec
         |dec:
         |  %rcp = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 0
         |  %rc  = load i64, ptr %rcp
         |  %new = sub i64 %rc, 1
         |  store i64 %new, ptr %rcp
         |  %iz  = icmp eq i64 %new, 0
         |  br i1 %iz, label %walk, label %done
         |walk:
         |  %rp  = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 1
         |  %rs  = load i64, ptr %rp
         |  %cp  = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 2
         |  %cs  = load i64, ptr %cp
         |  %len = mul i64 %rs, %cs
         |  %dp  = getelementptr inbounds %nex_arr2, ptr %a, i32 0, i32 3
         |  %buf = load ptr, ptr %dp
         |  br label %loop_hdr
         |loop_hdr:
         |  %i = phi i64 [0, %walk], [%i1, %loop_body]
         |  %cmp = icmp slt i64 %i, %len
         |  br i1 %cmp, label %loop_body, label %free_it
         |loop_body:
         |  %slot = getelementptr inbounds $slotTy, ptr %buf, i64 %i
         |  %v = load $slotTy, ptr %slot
         |${elemDec}  %i1 = add i64 %i, 1
         |  br label %loop_hdr
         |free_it:
         |  call void @free(ptr %buf)
         |  call void @free(ptr %a)
         |  br label %done
         |done:
         |  ret void
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
