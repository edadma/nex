package io.github.edadma.nex

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.dal.DALNumber

class Tests extends AnyFreeSpec with Matchers {

  def eval(input: String): Value =
    val evaluator = new Evaluator()
    Parser.parse(input) match
      case Left(err) => fail(s"Parse error: $err")
      case Right(ast) =>
        val rewritten = Rewriter.rewrite(ast)
        evaluator.eval(rewritten)

  def evalStr(input: String): String =
    Value.display(eval(input))

  "Arithmetic" - {
    "integer addition" in {
      evalStr("1 + 2") shouldBe "3"
    }

    "integer subtraction" in {
      evalStr("5 - 3") shouldBe "2"
    }

    "integer multiplication" in {
      evalStr("3 * 4") shouldBe "12"
    }

    "exact rational division" in {
      evalStr("1 / 3") shouldBe "1/3"
    }

    "rational addition" in {
      evalStr("1/3 + 1/6") shouldBe "1/2"
    }

    "unary negation" in {
      evalStr("-5") shouldBe "-5"
    }

    "operator precedence" in {
      evalStr("2 + 3 * 4") shouldBe "14"
    }

    "parentheses" in {
      evalStr("(2 + 3) * 4") shouldBe "20"
    }
  }

  "Arrays" - {
    "array literal" in {
      evalStr("[1, 2, 3]") shouldBe "[1, 2, 3]"
    }

    "array addition" in {
      evalStr("[1, 2, 3] + [4, 5, 6]") shouldBe "[5, 7, 9]"
    }

    "array scalar multiplication" in {
      evalStr("[1, 2, 3] * 2") shouldBe "[2, 4, 6]"
    }

    "scalar array multiplication" in {
      evalStr("2 * [1, 2, 3]") shouldBe "[2, 4, 6]"
    }

    "empty array" in {
      evalStr("[]") shouldBe "[]"
    }
  }

  "Built-in functions" - {
    "iota" in {
      evalStr("iota(5)") shouldBe "[0, 1, 2, 3, 4]"
    }

    "sum" in {
      evalStr("sum([1, 2, 3])") shouldBe "6"
    }

    "sum of iota" in {
      evalStr("sum(iota(10))") shouldBe "45"
    }

    "product" in {
      evalStr("product([1, 2, 3, 4])") shouldBe "24"
    }

    "count" in {
      evalStr("count([1, 2, 3])") shouldBe "3"
    }

    "range" in {
      evalStr("range(2, 7)") shouldBe "[2, 3, 4, 5, 6]"
    }

    "take" in {
      evalStr("take(3, iota(10))") shouldBe "[0, 1, 2]"
    }

    "drop" in {
      evalStr("drop(2, [1, 2, 3, 4, 5])") shouldBe "[3, 4, 5]"
    }

    "shape of 1D array" in {
      evalStr("shape([1, 2, 3])") shouldBe "[3]"
    }

    "reshape" in {
      evalStr("reshape([2, 3], iota(6))") shouldBe "[[0, 1, 2], [3, 4, 5]]"
    }
  }

  "Variables" - {
    "assignment and retrieval" in {
      val evaluator = new Evaluator()
      def evalWith(input: String): String =
        Parser.parse(input) match
          case Left(err) => fail(s"Parse error: $err")
          case Right(ast) =>
            val rewritten = Rewriter.rewrite(ast)
            Value.display(evaluator.eval(rewritten))

      evalWith("x = 5") shouldBe "5"
      evalWith("x * 2") shouldBe "10"
    }
  }

  "Lambda/map" - {
    "map with placeholder" in {
      evalStr("map(_ * 2, [1, 2, 3])") shouldBe "[2, 4, 6]"
    }

    "map with addition" in {
      evalStr("map(_ + 10, [1, 2, 3])") shouldBe "[11, 12, 13]"
    }
  }

  "Expressions with mean" - {
    "average calculation" in {
      evalStr("sum([1, 2, 3]) / count([1, 2, 3])") shouldBe "2"
    }
  }

  "Complex numbers" - {
    "imaginary unit" in {
      evalStr("i") shouldBe "0+1i"
    }

    "complex expression" in {
      evalStr("3 + 2*i") shouldBe "3+2i"
    }

    "i squared" in {
      evalStr("i * i") shouldBe "-1"
    }

    "complex arithmetic" in {
      evalStr("(1 + i) * (1 - i)") shouldBe "2"
    }
  }

  "Constants" - {
    "pi" in {
      evalStr("pi") shouldBe "3.141592653589793"
    }

    "e" in {
      evalStr("e") shouldBe "2.718281828459045"
    }
  }

}
