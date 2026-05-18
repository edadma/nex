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

    "assert_eq lowers to icmp eq + assert" in {
      val ir = compile("def main() = assert_eq(3 * 4, 12)")
      ir should include regex """icmp eq i64 %t\d+, 12"""
      ir should include regex """call void @__nex_assert\(i1 %t\d+\)"""
    }

    "assert_eq on reals uses fcmp oeq with double promotion" in {
      val ir = compile("def main() = assert_eq(3.0, 3.0)")
      ir should include("fcmp oeq double")
    }

    "assert_approx emits fabs(diff) <= eps + assert" in {
      val ir = compile("def main() = assert_approx(0.1, 0.2, 0.5)")
      ir should include regex """fsub double 0x3FB[0-9A-F]+, 0x3FC[0-9A-F]+"""
      ir should include("call double @fabs(double")
      ir should include("fcmp ole double")
      ir should include("call void @__nex_assert")
    }

    "failing assert path includes a trap message and abort" in {
      val ir = compile("def main() = assert(false)")
      ir should include("@.assert_fail_msg")
      ir should include("call void @abort()")
    }
  }
