package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for the parser bug where a single-line
  * `for ... do <assignment>` body produced a malformed AST with the
  * for-loop tree as the LHS of an outer `AssignExpr`, dropping the
  * loop pattern from the RHS's scope.
  *
  * Fixed by adding `assignment` ahead of `exprNoTuple` in `branchBody`
  * (`NexParser.scala`).
  */
class NexForInlineBugRepro extends AnyWordSpec with Matchers:

  private def parseOk(src: String): ProgramAST =
    new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")

  private def elabOk(src: String): Unit =
    val ast = parseOk(src)
    new NexElaborator().elaborate(ast) match
      case Right(_)   => ()
      case Left(errs) => fail(s"elab errors: ${errs.mkString("; ")}")

  "inline for-do with assignment body" should {
    "parse and elab a for-loop whose inline body is an assignment" in {
      elabOk(
        """def main() =
          |  var s = 0
          |  val v = [1, 2, 3]
          |  for x in v do s = s + x*x
          |  print(s)
          |""".stripMargin,
      )
    }

    "place the assignment inside the for-loop body (loop pattern in scope)" in {
      val ast = parseOk(
        """def main() =
          |  var s = 0
          |  val v = [1, 2, 3]
          |  for x in v do s = s + x
          |  print(s)
          |""".stripMargin,
      )
      val rendered = ast.toString
      rendered should include("ForExpr")
      rendered should include("AssignExpr(VarRefExpr(s)")
      rendered should not include "AssignExpr(ForExpr"
    }

    "still accept inline function-call bodies (regression of the working form)" in {
      elabOk("""def main() = for k in 0..3 do print(k)""")
    }
  }
