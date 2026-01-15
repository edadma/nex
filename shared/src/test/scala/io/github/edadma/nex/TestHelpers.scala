package io.github.edadma.nex

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait TestHelpers extends AnyFreeSpec with Matchers:
  def eval(input: String): Value =
    val evaluator = new Evaluator()
    Parser.parse(input) match
      case Left(err) => fail(s"Parse error: $err")
      case Right(ast) =>
        val rewritten = Rewriter.rewrite(ast)
        evaluator.eval(rewritten)

  def evalStr(input: String): String =
    Value.display(eval(input))

  def withEvaluator(f: (String => String) => Unit): Unit =
    val evaluator = new Evaluator()
    def evalWith(input: String): String =
      Parser.parse(input) match
        case Left(err) => fail(s"Parse error: $err")
        case Right(ast) =>
          val rewritten = Rewriter.rewrite(ast)
          Value.display(evaluator.eval(rewritten))
    f(evalWith)
