package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** `for i in lo..hi` and `for i in lo..=hi` range loops. */
class NexLLVMLoopsTests extends AnyWordSpec with NexCodegenTestBase:

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

  "range at value position" should {

    "exclusive range allocates an i64-element array and fills it" in {
      val ir = compile("""
        |def main() = print(0..5)
      """.stripMargin)
      // Element-size 8 (i64); not 0-clamped (length is positive).
      ir should include("@__nex_arr1_alloc(")
      ir should include("i64 8)")
      // The fill loop stores `lo + i` at each slot.
      ir should include regex """store i64 %t\d+, ptr %t\d+"""
      // Length picks max(0, hi - lo).
      ir should include("select i1")
      ir should not include "; TODO:"
    }

    "inclusive range adds 1 to the raw length" in {
      val ir = compile("""
        |def main() = print(0..=5)
      """.stripMargin)
      // sub then add 1 → inclusive form.
      ir should include regex """sub i64 \S+, \S+"""
      ir should include regex """add i64 %t\d+, 1"""
    }

    "negative-direction range clamps length to 0 via select" in {
      val ir = compile("""
        |def main() = print(7..3)
      """.stripMargin)
      // The select i1 is the max(0, raw-len) clamp.
      ir should include("icmp slt i64")
      ir should include("select i1")
    }
  }
