package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Basics: preamble, main function, print, integer/real arithmetic,
  * val bindings, multi-function programs.
  */
class NexLLVMBasicsTests extends AnyWordSpec with NexCodegenTestBase:

  "preamble" should {
    "always include an extern printf declaration" in {
      val ir = compile("def main() = ()")
      ir should include("declare i32 @printf(ptr, ...)")
    }

    "always include the integer/real format-string globals" in {
      val ir = compile("def main() = ()")
      ir should include("@.fmt_int")
      ir should include("@.fmt_real")
    }
  }

  "main function" should {
    "emit a single `define i32 @main()` with `ret i32 0`" in {
      val ir = compile("def main() = ()")
      ir should include("define i32 @main()")
      ir should include("ret i32 0")
    }
  }

  "print" should {
    "emit a printf call with the integer format and i64 value for `print(integer)`" in {
      val ir = compile("def main() = print(42)")
      ir should include("call i32 (ptr, ...) @printf(ptr @.fmt_int, i64 42)")
    }

    "emit a printf call with the real format and double value for `print(real)`" in {
      val ir = compile("def main() = print(3.14)")
      ir should include("@.fmt_real")
      ir should include("double 0x")  // doubles encoded as hex bit-patterns
    }

    "emit a select + printf for `print(bool)`" in {
      val ir = compile("def main() = print(true)")
      ir should include("select i1 1, ptr @.fmt_bool_t, ptr @.fmt_bool_f")
    }
  }

  "integer arithmetic" should {
    "emit add/sub/mul/sdiv for + - * /" in {
      val ir = compile("""
        |def main() =
        |  print(1 + 2)
        |  print(7 - 3)
        |  print(4 * 5)
        |  print(20 / 4)
      """.stripMargin)
      ir should include("add i64 1, 2")
      ir should include("sub i64 7, 3")
      ir should include("mul i64 4, 5")
      // `/` between integers is real-divide in Nex (the elaborator marks
      // the result TyReal): both sides sitofp'd, then fdiv.
      ir should include("sitofp i64 20 to double")
      ir should include("sitofp i64 4 to double")
      ir should include("fdiv double")
    }

    "emit sdiv for `div` (integer floor division)" in {
      val ir = compile("""
        |def main() = print(20 div 4)
      """.stripMargin)
      // `div` stays integer-typed.
      ir should include("sdiv i64 20, 4")
    }

    "emit icmp for integer comparisons" in {
      val ir = compile("""
        |def main() =
        |  print(1 < 2)
        |  print(3 == 3)
      """.stripMargin)
      ir should include("icmp slt i64 1, 2")
      ir should include("icmp eq i64 3, 3")
    }
  }

  "real arithmetic" should {
    "emit fadd/fsub/fmul/fdiv for + - * /" in {
      val ir = compile("""
        |def main() =
        |  print(1.0 + 2.0)
        |  print(7.0 - 3.0)
        |  print(4.0 * 5.0)
        |  print(20.0 / 4.0)
      """.stripMargin)
      ir should include("fadd double")
      ir should include("fsub double")
      ir should include("fmul double")
      ir should include("fdiv double")
    }
  }

  "val bindings" should {
    "emit an alloca + store for `val x = ...` and a load for the reference" in {
      val ir = compile("""
        |def main() =
        |  val x = 17
        |  print(x)
      """.stripMargin)
      ir should include("alloca i64")
      ir should include("store i64 17, ptr")
      ir should include regex """%t\d+ = load i64, ptr"""
    }

    "compose: `val x = a + b; print(x * 2)` emits the full chain" in {
      val ir = compile("""
        |def main() =
        |  val x = 3 + 4
        |  print(x * 2)
      """.stripMargin)
      // The val's RHS is computed once; reads load from the slot; the
      // outer `print(x * 2)` multiplies the loaded value by 2 and
      // prints — that's add + alloca/store + load + mul + printf.
      ir should include("add i64 3, 4")
      ir should include("alloca i64")
      ir should include("store i64")
      ir should include("mul i64")
      ir should include("@.fmt_int")
    }
  }

  "multi-function programs" should {
    "emit each top-level def with its declared signature" in {
      val ir = compile("""
        |def helper(x: integer): integer = x + 1
        |def main() = print(helper(41))
      """.stripMargin)
      ir should include("define i64 @helper(i64 %arg0)")
      ir should include("define i32 @main()")
      ir should not include "; TODO:"
    }

    "emit a call to a user function with correct arg types" in {
      val ir = compile("""
        |def add(a: integer, b: integer): integer = a + b
        |def main() = print(add(40, 2))
      """.stripMargin)
      ir should include("call i64 @add(i64 40, i64 2)")
    }

    "real-typed args are passed as double" in {
      val ir = compile("""
        |def scale(x: real, k: real): real = x * k
        |def main() = print(scale(3.0, 2.0))
      """.stripMargin)
      ir should include("define double @scale(double %arg0, double %arg1)")
      ir should include("call double @scale(double")
    }

    "unit-returning function uses ret void" in {
      val ir = compile("""
        |def announce(n: integer) = print(n)
        |def main() = announce(7)
      """.stripMargin)
      ir should include("define void @announce(i64 %arg0)")
      ir should include("ret void")
      ir should include("call void @announce(i64 7)")
    }
  }
