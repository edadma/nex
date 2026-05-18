package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** String literals, interning, string-typed params, and `s"..."`
  * interpolation lowering.
  */
class NexLLVMStringsTests extends AnyWordSpec with NexCodegenTestBase:

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
