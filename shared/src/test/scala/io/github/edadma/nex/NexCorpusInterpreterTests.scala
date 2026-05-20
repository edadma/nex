package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, PrintStream}

/** Runs every program in [[NexProgramCorpus]] through the interpreter
  * and asserts the captured stdout matches the case's `expected` field.
  *
  * Cross-platform (JVM, JS, Native) — exercises the interpreter only.
  * Parity against the AOT compiler is enforced by
  * [[NexCorpusParityTests]], which is JVM-only because it shells out
  * to `clang`.
  *
  * Adding a new case to `NexProgramCorpus.all` automatically registers
  * a test here AND a parity test on JVM, so the two paths cannot drift
  * apart through forgetfulness.
  */
class NexCorpusInterpreterTests extends AnyWordSpec with Matchers:

  private def runOut(src: String): String =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    val tp = new NexElaborator().elaborate(ast) match
      case Right(p)   => p
      case Left(errs) => fail(s"elab errors: ${errs.map(_.toString).mkString("; ")}")
    val baos = new ByteArrayOutputStream
    Console.withOut(new PrintStream(baos))(new NexInterpreter().runProgram(tp))
    baos.toString

  // Group cases by category so the WordSpec test report mirrors the
  // structure of NexInterpreterTests — easier to scan for failures.
  NexProgramCorpus.all.groupBy(_.category).toSeq.sortBy(_._1).foreach { case (category, cases) =>
    category should {
      cases.foreach { c =>
        c.name in { runOut(c.src) shouldBe c.expected }
      }
    }
  }
