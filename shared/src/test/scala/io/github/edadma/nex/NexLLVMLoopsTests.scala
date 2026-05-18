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
