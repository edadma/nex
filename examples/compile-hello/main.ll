; Nex LLVM IR module (v0 scaffolding)

declare i32 @printf(ptr, ...)

@.fmt_int  = private unnamed_addr constant [6 x i8] c"%lld\0A\00"
@.fmt_real = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@.fmt_bool_t = private unnamed_addr constant [6 x i8] c"true\0A\00"
@.fmt_bool_f = private unnamed_addr constant [7 x i8] c"false\0A\00"

define i32 @main() {
entry:
  call i32 (ptr, ...) @printf(ptr @.fmt_int, i64 42)
  %t1 = alloca i64
  store i64 10, ptr %t1
  %t2 = alloca i64
  store i64 32, ptr %t2
  %t3 = load i64, ptr %t1
  %t4 = load i64, ptr %t2
  %t5 = add i64 %t3, %t4
  call i32 (ptr, ...) @printf(ptr @.fmt_int, i64 %t5)
  call i32 (ptr, ...) @printf(ptr @.fmt_real, double 0x40091EB851EB851F)
  %t6 = fmul double 0x4000000000000000, 0x4035000000000000
  call i32 (ptr, ...) @printf(ptr @.fmt_real, double %t6)
  ret i32 0
}

