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

  "control flow" should {

    "emit conditional branch + merge phi for an if expression" in {
      val ir = compile("""
        |def sign(x: integer): integer =
        |  if x < 0 then -1 else 1
        |def main() = print(sign(0))
      """.stripMargin)
      ir should include("br i1")
      ir should include("if.then")
      ir should include("if.else")
      ir should include("phi i64")
      ir should not include "; TODO:"
    }

    "emit if-without-else as a two-block diamond" in {
      val ir = compile("""
        |def announce(x: integer) =
        |  if x > 0 then print(x)
        |def main() = announce(5)
      """.stripMargin)
      ir should include("br i1")
      ir should include("if.then")
      ir should include("if.end")
      ir should not include "; TODO:"
    }

    "emit a while loop with cond / body / exit blocks" in {
      val ir = compile("""
        |def count_down(n: integer): integer =
        |  var i = n
        |  while i > 0 do
        |    i = i - 1
        |  i
        |def main() = print(count_down(3))
      """.stripMargin)
      ir should include("while.cond")
      ir should include("while.body")
      ir should include("while.exit")
      ir should not include "; TODO:"
    }

    "emit a return statement that terminates the current block" in {
      val ir = compile("""
        |def first_neg(x: integer, y: integer): integer =
        |  if x < 0 then return x
        |  if y < 0 then return y
        |  0
        |def main() = print(first_neg(1, -2))
      """.stripMargin)
      ir should include("ret i64")
      ir should not include "; TODO:"
    }

    "emit short-circuit and as a branch + phi" in {
      val ir = compile("""
        |def in_range(x: integer, lo: integer, hi: integer): bool =
        |  x >= lo and x <= hi
        |def main() = print(in_range(5, 0, 10))
      """.stripMargin)
      ir should include("and.rhs")
      ir should include("and.end")
      ir should include("phi i1")
      ir should not include "; TODO:"
    }

    "emit short-circuit or as a branch + phi" in {
      val ir = compile("""
        |def either(x: integer): bool =
        |  x == 0 or x == 42
        |def main() = print(either(0))
      """.stripMargin)
      ir should include("or.rhs")
      ir should include("or.end")
      ir should include("phi i1")
      ir should not include "; TODO:"
    }

    "emit unary minus on integer and real" in {
      val ir = compile("""
        |def negi(x: integer): integer = -x
        |def negr(x: real): real = -x
        |def main() = print(negi(7))
      """.stripMargin)
      ir should include("sub i64 0,")
      ir should include("fneg double")
      ir should not include "; TODO:"
    }
  }

  "top-level bindings" should {

    "emit @<name> = global zeroinitializer for each top-level val/var/const" in {
      val ir = compile("""
        |val x: integer = 42
        |var y: integer = 0
        |def main() = print(x)
      """.stripMargin)
      ir should include("@x = global i64")
      ir should include("@y = global i64")
    }

    "emit __nex_init_globals when bindings exist" in {
      val ir = compile("""
        |val x: integer = 42
        |def main() = print(x)
      """.stripMargin)
      ir should include("define void @__nex_init_globals()")
      ir should include("store i64 42, ptr @x")
    }

    "main calls __nex_init_globals at the entry block" in {
      val ir = compile("""
        |val x: integer = 7
        |def main() = print(x)
      """.stripMargin)
      ir should include("call void @__nex_init_globals()")
    }

    "TVarRef to a top-level binding loads from @<name>" in {
      val ir = compile("""
        |val MAX: integer = 100
        |def main() = print(MAX)
      """.stripMargin)
      ir should include("load i64, ptr @MAX")
    }

    "TAssign to a top-level var stores to @<name>" in {
      val ir = compile("""
        |var counter: integer = 0
        |def bump() = counter = counter + 1
        |def main() = bump()
      """.stripMargin)
      ir should include("store i64")
      ir should include("ptr @counter")
    }

    "no __nex_init_globals when there are no top bindings" in {
      val ir = compile("""
        |def main() = print(42)
      """.stripMargin)
      ir should not include "@__nex_init_globals"
      ir should not include "call void @__nex_init_globals"
    }

    "real-typed top binding gets a double global" in {
      val ir = compile("""
        |val PI: real = 3.14159
        |def main() = print(PI)
      """.stripMargin)
      ir should include("@PI = global double 0.0")
      ir should include("store double")
      ir should include("load double, ptr @PI")
    }
  }

  "strings" should {

    "intern a string literal as @.str.<N>" in {
      val ir = compile("""
        |def main() = print("hello, world")
      """.stripMargin)
      ir should include("@.str.")
      ir should include("c\"hello, world\\00\"")
    }

    "print(string-literal) uses %s\\n" in {
      val ir = compile("""
        |def main() = print("hi")
      """.stripMargin)
      ir should include("@.fmt_str")
      ir should include("call i32 (ptr, ...) @printf(ptr @.fmt_str, ptr")
    }

    "string-typed function param is `ptr`" in {
      val ir = compile("""
        |def greet(name: string) = print(name)
        |def main() = greet("Alice")
      """.stripMargin)
      ir should include("define void @greet(ptr %arg0)")
    }

    "duplicate string literals share one global" in {
      val ir = compile("""
        |def main() =
        |  print("hi")
        |  print("hi")
      """.stripMargin)
      // Count occurrences of `c"hi\00"` — should be exactly 1.
      val count = """c"hi\\00"""".r.findAllIn(ir).size
      count shouldBe 1
    }

    "interpolated string in print produces a printf-per-part + newline" in {
      val ir = compile("""
        |def main() =
        |  val n = 42
        |  print(s"answer = $n")
      """.stripMargin)
      // The text chunk is interned; the int part uses fmt_int_raw; the
      // closing newline comes from @.nl.
      ir should include("c\"answer = \\00\"")
      ir should include("@.fmt_int_raw")
      ir should include("@.nl")
    }

    "interpolated string with a ${expr} embeds the inner value" in {
      val ir = compile("""
        |def main() =
        |  print(s"sum = ${1 + 2}")
      """.stripMargin)
      ir should include("@.fmt_int_raw")
      ir should include("@.nl")
    }
  }

  "for loops over ranges" should {

    "emit cond/body/exit blocks for `for i in lo..hi`" in {
      val ir = compile("""
        |def main() =
        |  for i in 0..5 do
        |    print(i)
      """.stripMargin)
      ir should include("for.cond")
      ir should include("for.body")
      ir should include("for.exit")
      ir should not include "; TODO:"
    }

    "exclusive upper bound uses icmp slt" in {
      val ir = compile("""
        |def main() =
        |  for i in 0..5 do
        |    print(i)
      """.stripMargin)
      ir should include("icmp slt i64")
    }

    "inclusive upper bound uses icmp sle" in {
      val ir = compile("""
        |def main() =
        |  for i in 1..=5 do
        |    print(i)
      """.stripMargin)
      ir should include("icmp sle i64")
    }

    "body increments the loop counter by 1" in {
      val ir = compile("""
        |def main() =
        |  for i in 0..3 do
        |    print(i)
      """.stripMargin)
      ir should include("add i64")
      ir should include(", 1")
    }

    "nested for loops emit independent label sets" in {
      val ir = compile("""
        |def main() =
        |  for i in 1..=3 do
        |    for j in 1..=3 do
        |      print(i * j)
      """.stripMargin)
      // Each loop generates its own .1, .2, .3 fresh-label suffix; the
      // outer + inner together produce multiple `for.cond.<n>` labels.
      val condLabels = """for\.cond\.\d+""".r.findAllIn(ir).toList.distinct
      condLabels.size shouldBe 2
    }
  }
