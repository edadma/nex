package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Unit-level coverage for the first vertical slice of the LLVM IR
  * backend. We don't shell out to `clang` here — invoking it has a real
  * runtime cost on CI and would couple every check to host toolchain
  * availability. Instead we look at the generated `.ll` text for the
  * expected instructions; the `examples/compile-hello/` example
  * exercises the full clang round-trip as an integration smoke.
  */
class NexLLVMCodegenTests extends AnyWordSpec with Matchers:

  private def compile(src: String): String =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    val tp = new NexElaborator().elaborate(ast) match
      case Right(p)   => p
      case Left(errs) => fail(s"elab errors: ${errs.map(_.toString).mkString("; ")}")
    new NexLLVMCodegen().compile(tp)

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

  "unsupported features" should {
    "leave a 'not yet supported' comment when a non-main top-level decl is seen" in {
      val ir = compile("""
        |def helper(x: integer) = x + 1
        |def main() = print(42)
      """.stripMargin)
      ir should include("; TODO:")
      ir should include("TFunDecl")
    }
  }
