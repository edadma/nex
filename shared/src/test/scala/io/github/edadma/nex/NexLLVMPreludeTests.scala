package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Prelude (chunk 10): scalar math (sqrt, trig, abs, min/max, sign,
  * constants pi/e/inf/nan, atan2, asinh/acosh/atanh) and assertion
  * helpers (assert, assert_eq, assert_approx, failing-assert trap).
  */
class NexLLVMPreludeTests extends AnyWordSpec with NexCodegenTestBase:

  "prelude scalar math (chunk 10)" should {
    "sqrt routes to libm @sqrt" in {
      val ir = compile("def main() = print(sqrt(16.0))")
      ir should include("declare double @sqrt(double)")
      ir should include regex """call double @sqrt\(double 0x4030000000000000\)"""
    }

    "integer arg to sqrt is sitofp-lifted before the libm call" in {
      val ir = compile("def main() = print(sqrt(4))")
      ir should include regex """sitofp i64 4 to double"""
      ir should include regex """call double @sqrt\(double %t\d+\)"""
    }

    "all unary trig functions emit their libm bridge" in {
      val ir = compile("""
        |def main() =
        |  print(sin(0.0))
        |  print(cos(0.0))
        |  print(tan(0.0))
        |  print(asin(0.0))
        |  print(acos(0.0))
        |  print(atan(0.0))
      """.stripMargin)
      for fn <- List("sin", "cos", "tan", "asin", "acos", "atan") do
        ir should include(s"call double @$fn(double 0x0000000000000000)")
    }

    "abs(integer) uses llabs; abs(real) uses fabs" in {
      val intIr = compile("def main() = print(abs(-7))")
      // `-7` is parsed as TUnaryOp(-, 7) so we feed an SSA register to
      // llabs rather than a literal — match the regex accordingly.
      intIr should include regex """call i64 @llabs\(i64 %t\d+\)"""
      val realIr = compile("def main() = print(abs(-3.14))")
      realIr should include regex """call double @fabs\(double %t\d+\)"""
    }

    "min(i, i) lowers to icmp+select; max(r, r) to fcmp+select" in {
      val iMinIr = compile("def main() = print(min(3, 7))")
      iMinIr should include regex """icmp sle i64 3, 7"""
      iMinIr should include regex """select i1 %t\d+, i64 3, i64 7"""
      val rMaxIr = compile("def main() = print(max(3.0, 7.0))")
      rMaxIr should include("fcmp oge double")
      // LLVM IR accepts both hex and decimal forms for double constants;
      // our formatReal helper emits the hex form via doubleToLongBits.
      rMaxIr should include regex """select i1 %t\d+, double 0x[0-9A-F]+, double 0x[0-9A-F]+"""
    }

    "sign(integer) uses zext (so true → 1, not -1)" in {
      val ir = compile("def main() = print(sign(-5))")
      // The subtraction of two zext'd bools is the integer-sign idiom.
      ir should include regex """zext i1 %t\d+ to i64"""
      ir should include regex """sub i64 %t\d+, %t\d+"""
    }

    "sign(real) uses fcmp + select" in {
      val ir = compile("def main() = print(sign(-3.14))")
      ir should include("fcmp ogt double")
      ir should include("fcmp olt double")
      // LLVM accepts both `1.0` / `-1.0` decimals and hex bit-reps. Our
      // sign-of-real emitter writes them as plain decimals.
      ir should include regex """select i1 %t\d+, double 1.0, double 0.0"""
      ir should include regex """select i1 %t\d+, double -1.0, double %t\d+"""
    }

    "prelude constants pi / e / inf / nan inline as IR double literals" in {
      val ir = compile("""
        |def main() =
        |  print(pi)
        |  print(e)
        |  print(inf)
        |  print(nan)
      """.stripMargin)
      ir should include("0x400921FB54442D18") // pi
      ir should include("0x4005BF0A8B145769") // e
      ir should include("0x7FF0000000000000") // inf
      ir should include("0x7FF8000000000000") // nan
    }

    "atan2 takes two args and lowers to libm @atan2" in {
      val ir = compile("def main() = print(atan2(1.0, 1.0))")
      ir should include regex """call double @atan2\(double 0x3FF0000000000000, double 0x3FF0000000000000\)"""
    }

    "asinh / acosh / atanh route through our log-based helpers" in {
      val ir = compile("""
        |def main() =
        |  print(asinh(1.0))
        |  print(acosh(2.0))
        |  print(atanh(0.5))
      """.stripMargin)
      ir should include("call double @__nex_asinh(double")
      ir should include("call double @__nex_acosh(double")
      ir should include("call double @__nex_atanh(double")
    }
  }

  "prelude assertions (chunk 10)" should {
    "assert(bool) routes to @__nex_assert" in {
      val ir = compile("def main() = assert(1 + 1 == 2)")
      ir should include("define void @__nex_assert(i1 %cond)")
      ir should include regex """call void @__nex_assert\(i1 %t\d+\)"""
    }

    "assert_eq lowers to icmp eq + trap_with(assert_eq_msg)" in {
      val ir = compile("def main() = assert_eq(3 * 4, 12)")
      ir should include regex """icmp eq i64 %t\d+, 12"""
      // The check branches to a fail block that traps with the
      // specific message — so a parent assert_traps' substring check
      // for "assert_eq" finds it.
      ir should include("call void @__nex_trap_with(ptr @.assert_eq_msg)")
    }

    "assert_eq on reals uses fcmp oeq with double promotion" in {
      val ir = compile("def main() = assert_eq(3.0, 3.0)")
      ir should include("fcmp oeq double")
    }

    "assert_approx emits fabs(diff) <= eps + trap_with(assert_approx_msg)" in {
      val ir = compile("def main() = assert_approx(0.1, 0.2, 0.5)")
      ir should include regex """fsub double 0x3FB[0-9A-F]+, 0x3FC[0-9A-F]+"""
      ir should include("call double @fabs(double")
      ir should include("fcmp ole double")
      ir should include("call void @__nex_trap_with(ptr @.assert_approx_msg)")
    }

    "failing assert path routes through __nex_trap_with for catch-aware printing" in {
      val ir = compile("def main() = assert(false)")
      ir should include("@.assert_fail_msg")
      // The trap site stashes the message and calls __nex_trap; the
      // actual printf is deferred to __nex_trap's abort path so an
      // enclosing assert_traps catches silently.
      ir should include("call void @__nex_trap_with(ptr @.assert_fail_msg)")
    }
  }

  "assert_traps (catches a trap from a thunk)" should {
    "emits setjmp + closure-call dispatch inline at the call site" in {
      val ir = compile("""
        |def main() =
        |  assert_traps(() -> assert(false))
      """.stripMargin)
      // setjmp/longjmp must be declared and the trap-buf global allocated.
      ir should include("declare i32 @setjmp(ptr) returns_twice")
      ir should include("declare void @longjmp(ptr, i32)")
      ir should include("@__nex_trap_buf = internal thread_local global ptr null")
      ir should include("@__nex_trap_msg = internal thread_local global ptr null")
      // __nex_trap_with helper exists for printing on the abort path.
      ir should include("define void @__nex_trap_with(ptr %msg)")
      // The call site has a setjmp + branch and a labelled caught path.
      ir should include regex """call i32 @setjmp\(ptr %t\d+\)"""
      ir should include("@.assert_traps_fail_msg")
      // The failure path (thunk returned without trapping) routes through
      // __nex_trap_with so an enclosing assert_traps catches silently.
      ir should include("call void @__nex_trap_with(ptr @.assert_traps_fail_msg)")
    }

    "saves the previous trap_buf for nested assert_traps" in {
      val ir = compile("""
        |def main() =
        |  assert_traps(() -> assert_traps(() -> assert(true)))
      """.stripMargin)
      // Two setjmp call sites — one per assert_traps frame.
      val sjs = """call i32 @setjmp""".r.findAllIn(ir).toList
      sjs.size should be >= 2
      // The volatile stack-slot save/restore of the previous buf appears
      // (twice for nested) so a longjmp doesn't smash callee-saved regs.
      ir should include regex """load volatile ptr, ptr %t\d+, align 8"""
    }
  }

  "prelude array HOFs (chunk 11)" should {
    "map(arr, lambda) allocates a result array of the same length" in {
      val ir = compile("""
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(map(xs, x -> x * 2))
      """.stripMargin)
      // Allocate result first, then loop.
      ir should include regex """call ptr @__nex_arr1_alloc\(i64 %t\d+, i64 8\)"""
      // Closure is extracted into fn+env once outside the loop.
      ir should include regex """extractvalue \{ ptr, ptr \} %t\d+, 0"""
      ir should include regex """extractvalue \{ ptr, ptr \} %t\d+, 1"""
      // Loop body dispatches via the chunk-9 indirect call.
      ir should include regex """call i64 \(ptr, i64\) %t\d+\(ptr %t\d+, i64 %t\d+\)"""
    }

    "map's per-iteration loop uses the hof.map label prefix" in {
      val ir = compile("""
        |def main() = print(map([1, 2, 3], x -> x + 1))
      """.stripMargin)
      ir should include("hof.map.cond")
      ir should include("hof.map.body")
      ir should include("hof.map.exit")
    }

    "map(arr, namedClosureVar) loads the closure from its slot then dispatches" in {
      val ir = compile("""
        |def main() =
        |  val xs = [1, 2, 3]
        |  val f: (integer -> integer) = x -> x * 10
        |  print(map(xs, f))
      """.stripMargin)
      // The bound closure value lives in a `{ ptr, ptr }` alloca; the
      // HOF loads it, splits it, and calls indirectly inside the loop.
      ir should include regex """load \{ ptr, ptr \}, ptr %t\d+"""
      ir should include regex """call i64 \(ptr, i64\) %t\d+\(ptr %t\d+, i64 %t\d+\)"""
    }

    "reduce(arr, init, lambda) uses an alloca'd accumulator slot" in {
      val ir = compile("""
        |def main() = print(reduce([1, 2, 3, 4], 0, (a, x) -> a + x))
      """.stripMargin)
      // 2-arg closure with prepended env, so the signature is `(ptr, i64, i64) -> i64`.
      ir should include regex """call i64 \(ptr, i64, i64\) %t\d+\(ptr %t\d+, i64 %t\d+, i64 %t\d+\)"""
      ir should include("hof.reduce.cond")
      ir should include("hof.reduce.body")
      // alloca for the accumulator slot.
      ir should include regex """alloca i64"""
    }

    "filter(arr, predicate) over-allocates then truncates the descriptor length" in {
      val ir = compile("""
        |def main() =
        |  val xs = [1, 2, 3, 4, 5]
        |  print(filter(xs, x -> x % 2 == 0))
      """.stripMargin)
      // Predicate returns i1, so the indirect-call signature is `(ptr, i64) -> i1`.
      ir should include regex """call i1 \(ptr, i64\) %t\d+\(ptr %t\d+, i64 %t\d+\)"""
      ir should include("hof.filter.cond")
      ir should include("hof.filter.body")
      ir should include("filter.keep")
      ir should include("filter.skip")
      // Truncation: store the running count into the descriptor's
      // length field (field index 1).
      ir should include regex """getelementptr inbounds %nex_arr1, ptr %t\d+, i32 0, i32 1"""
    }

    "map on rank-2 lowers to __nex_arr2_alloc + a flat counting loop (Wave 3)" in {
      val ir = compile("""
        |def main() =
        |  val m: [[integer]] = [[1, 2], [3, 4]]
        |  val r = map(m, x -> x + 1)
        |  print(r)
      """.stripMargin)
      ir should not include "not yet supported"
      // Rank-2 result allocation: __nex_arr2_alloc(rows, cols, esz).
      ir should include regex """call ptr @__nex_arr2_alloc\(i64 %t\d+, i64 %t\d+, i64 8\)"""
      // Element-wise loop indexes the flat buffer.
      ir should include("hof.map")
    }

    "map preserves the result element type (real → real)" in {
      val ir = compile("""
        |def main() = print(map([1.0, 2.0, 3.0], x -> x * 2.0))
      """.stripMargin)
      // Real elements use 8 bytes per slot, same as integer; but the
      // indirect call's signature uses `double` types.
      ir should include regex """call double \(ptr, double\) %t\d+\(ptr %t\d+, double %t\d+\)"""
    }
  }

  "complex numbers (chunk 12)" should {
    "TyComplex lowers to a `{ double, double }` aggregate" in {
      val ir = compile("""
        |def main() =
        |  val z = 3.0 + 4.0 * i
        |  print(z.re)
      """.stripMargin)
      ir should include("alloca { double, double }")
      ir should include regex """store \{ double, double \} %t\d+, ptr %t\d+"""
    }

    "prelude `i` constant inlines as `{ 0.0, 1.0 }`" in {
      val ir = compile("def main() = print((1.0 + 0.0 * i).re)")
      ir should include("insertvalue { double, double } undef, double 0.0, 0")
      ir should include regex """insertvalue \{ double, double \} %t\d+, double 1.0, 1"""
    }

    "complex addition uses per-component fadd" in {
      val ir = compile("""
        |def main() =
        |  val a = 1.0 + 2.0 * i
        |  val b = 3.0 + 4.0 * i
        |  print(a + b)
      """.stripMargin)
      // Two fadds for the (a.re+b.re, a.im+b.im) pair.
      ir should include regex """fadd double %t\d+, %t\d+"""
    }

    "complex multiplication uses the (ac-bd, ad+bc) formula" in {
      val ir = compile("""
        |def main() =
        |  val a = 1.0 + 2.0 * i
        |  val b = 3.0 + 4.0 * i
        |  print(a * b)
      """.stripMargin)
      // 4 fmuls, 1 fsub, 1 fadd for the components.
      ir should include regex """fmul double %t\d+, %t\d+"""
      ir should include regex """fsub double %t\d+, %t\d+"""
      ir should include regex """fadd double %t\d+, %t\d+"""
    }

    "complex division uses the canonical 4-fmul + fadd-denominator formula" in {
      val ir = compile("""
        |def main() =
        |  val a = 1.0 + 2.0 * i
        |  val b = 3.0 + 4.0 * i
        |  print(a / b)
      """.stripMargin)
      // Two fdivs (one per output component).
      ir should include regex """fdiv double %t\d+, %t\d+"""
    }

    ".re and .im extract field 0 and field 1 respectively" in {
      val ir = compile("""
        |def main() =
        |  val z = 7.0 + 8.0 * i
        |  print(z.re)
        |  print(z.im)
      """.stripMargin)
      ir should include regex """extractvalue \{ double, double \} %t\d+, 0"""
      ir should include regex """extractvalue \{ double, double \} %t\d+, 1"""
    }

    "abs(complex) computes sqrt(re² + im²)" in {
      val ir = compile("""
        |def main() = print(abs(3.0 + 4.0 * i))
      """.stripMargin)
      // The modulus formula: fmul re*re, fmul im*im, fadd, sqrt.
      ir should include("call double @sqrt(double")
    }

    "conj negates the imaginary part" in {
      val ir = compile("""
        |def main() = print(conj(3.0 + 4.0 * i))
      """.stripMargin)
      ir should include regex """fneg double %t\d+"""
    }

    "arg(complex) routes to atan2(im, re)" in {
      val ir = compile("""
        |def main() = print(arg(3.0 + 4.0 * i))
      """.stripMargin)
      ir should include regex """call double @atan2\(double %t\d+, double %t\d+\)"""
    }

    "unary minus negates both components" in {
      val ir = compile("""
        |def main() =
        |  val z = 3.0 + 4.0 * i
        |  print(-z)
      """.stripMargin)
      // Two fnegs — one per component.
      ir should include regex """fneg double %t\d+"""
    }

    "complex == uses two fcmp oeq + and i1" in {
      val ir = compile("""
        |def main() =
        |  val z = 3.0 + 4.0 * i
        |  print(z == z)
      """.stripMargin)
      ir should include("fcmp oeq double")
      ir should include regex """and i1 %t\d+, %t\d+"""
    }

    "to_complex(integer) sitofp-lifts the real part and zeroes imag" in {
      val ir = compile("def main() = print(to_complex(5))")
      ir should include regex """sitofp i64 5 to double"""
      ir should include regex """insertvalue \{ double, double \} %t\d+, double 0.0, 1"""
    }

    "print(complex) emits `<re>+<im>i` via the per-component path" in {
      val ir = compile("""
        |def main() = print(3.0 + 4.0 * i)
      """.stripMargin)
      // Two real-prints (re + abs(im)) plus a sign string + "i" trailer.
      ir should include("call void @__nex_print_real_raw(double")
      ir should include("call double @fabs(double")
      ir should include("fcmp oge double")
    }
  }

  "prelude array construction (§10.5)" should {
    "fill(n, v: integer) allocates a length-n rank-1 array and stores v in each slot" in {
      val ir = compile("def main() = print(fill(4, 7))")
      ir should include regex """call ptr @__nex_arr1_alloc\(i64 4, i64 8\)"""
      ir should include("store i64 7")
    }

    "fill uses the `fill.cond` / `fill.body` / `fill.exit` loop prefix" in {
      val ir = compile("def main() = print(fill(4, 7))")
      ir should include("fill.cond")
      ir should include("fill.body")
      ir should include("fill.exit")
    }

    "fill(n, v: real) uses 8-byte slots and stores doubles" in {
      val ir = compile("def main() = print(fill(3, 3.14))")
      ir should include regex """call ptr @__nex_arr1_alloc\(i64 3, i64 8\)"""
      // The stored value is a double constant.
      ir should include("store double")
    }

    "fill(n, v: complex) uses 16-byte slots and stores `{ double, double }`" in {
      val ir = compile("def main() = print(fill(5, 1.0 + 2.0 * i))")
      ir should include regex """call ptr @__nex_arr1_alloc\(i64 5, i64 16\)"""
      ir should include("store { double, double }")
    }

    "fill evaluates `v` once outside the loop, not per iteration" in {
      // The complex value `1.0 + 2.0 * i` builds up via several
      // insertvalue ops. They should appear ONCE in main (before the
      // loop), not inside the body block. We approximate by counting
      // insertvalue ops in main — a per-iteration build would show up
      // inside the fill.body block and inflate the count well beyond
      // the constant-size builder chain (~6-7 ops).
      val ir = compile("def main() = print(fill(5, 1.0 + 2.0 * i))")
      val mainBody = ir.linesIterator
        .dropWhile(l => !l.startsWith("define i32 @main"))
        .takeWhile(l => !l.startsWith("}"))
        .mkString("\n")
      val insertCount =
        "insertvalue \\{ double, double \\}".r.findAllMatchIn(mainBody).size
      // 6 insertvalues for the constant builder chain (undef→re→im for
      // each of the 3 sub-expressions: `2.0`, `2.0 * i`, `1.0 + 2.0*i`).
      // The loop body adds one `store { double, double }` per iter but
      // NO insertvalues — so this count is stable regardless of n.
      insertCount should be <= 10
    }

    "fill(n: integer-variable, v) uses the variable length at runtime" in {
      val ir = compile("""
        |def main() =
        |  val n = 10
        |  print(fill(n, 0))
      """.stripMargin)
      // `n` loaded then passed to alloc.
      ir should include regex """call ptr @__nex_arr1_alloc\(i64 %t\d+, i64 8\)"""
    }

    "zeros(n) is fill(n, 0) — emits @__nex_arr1_alloc with elem_size 8 + a const-fill loop" in {
      val ir = compile("def main() = print(zeros(6))")
      ir should include regex """call ptr @__nex_arr1_alloc\(i64 6, i64 8\)"""
      ir should include("constfill.cond")
      ir should include("store i64 0")
    }

    "ones(n) stores 1 in each slot" in {
      val ir = compile("def main() = print(ones(4))")
      ir should include("store i64 1")
    }

    "fill produces a writable array (var binding can mutate slots)" in {
      val ir = compile("""
        |def main() =
        |  var xs = fill(5, 0)
        |  xs[2] = 99
      """.stripMargin)
      // We just check that the allocation went through fill and that
      // we get a slot pointer for the store at index 2.
      ir should include regex """call ptr @__nex_arr1_alloc\(i64 5, i64 8\)"""
      ir should include regex """call ptr @__nex_arr1_slot\(ptr %t\d+, i64 2, i64 8\)"""
    }
  }

  "complex-arg transcendental functions (spec §10.2)" should {
    // Spec §10.2 line 32: sin, cos, exp, log, sqrt apply to real AND
    // complex. The complex paths emit per-component IR using libm
    // helpers (exp/cos/sin/sinh/cosh/atan2/sqrt). Each test pins one
    // of the formulas via instructions only that branch would emit.

    "exp(complex) uses cos+sin of the imag part and exp of the real" in {
      val ir = compile("def main() = print(exp(1.0 + 2.0 * i))")
      ir should include("call double @exp(double")
      ir should include("call double @cos(double")
      ir should include("call double @sin(double")
    }

    "log(complex) uses atan2 + half-of-log of magnitude²" in {
      val ir = compile("def main() = print(log(3.0 + 4.0 * i))")
      ir should include("call double @log(double")
      ir should include("call double @atan2(double")
      // The 0.5 multiplier on the log² magnitude.
      ir should include("fmul double") // (loose, but the 0.5 lives in the IR)
    }

    "sin(complex) uses sin/cos of real AND sinh/cosh of imag" in {
      val ir = compile("def main() = print(sin(0.5 + 0.5 * i))")
      ir should include("call double @sin(double")
      ir should include("call double @cos(double")
      ir should include("call double @sinh(double")
      ir should include("call double @cosh(double")
    }

    "cos(complex) negates the imag-part product (cos·cosh − sin·sinh)" in {
      val ir = compile("def main() = print(cos(0.5 + 0.5 * i))")
      ir should include("call double @sin(double")
      ir should include("call double @cos(double")
      ir should include("call double @sinh(double")
      ir should include("call double @cosh(double")
      // The `−sin · sinh` term emits an fneg.
      ir should include regex """fneg double"""
    }

    "sqrt(complex) uses copysign for the imag sign transfer" in {
      val ir = compile("def main() = print(sqrt(-1.0 + 0.0 * i))")
      ir should include("declare double @copysign(double, double)")
      ir should include("call double @copysign(double")
      ir should include("call double @sqrt(double")
    }

    "regression: exp(2pi * i) compiles without trapping" in {
      // Was emitting `call double @exp(double <complex>)` and
      // failing to link. Now the elaborator routes to TyComplex
      // result type and the codegen emits the per-component
      // formula.
      val ir = compile("def main() = print(exp(2pi * i))")
      // The result of exp(...) is now a `{ double, double }` aggregate
      // that flows into the complex-print path.
      ir should include("insertvalue { double, double }")
    }
  }
