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

  "Pipeline operator" - {
    "simple pipeline" in {
      evalStr("[1, 2, 3] | sum") shouldBe "6"
    }

    "chained pipeline" in {
      evalStr("iota(10) | sum") shouldBe "45"
    }

    "pipeline with map" in {
      evalStr("[1, 2, 3] | map(_ * 2)") shouldBe "[2, 4, 6]"
    }

    "pipeline with filter" in {
      evalStr("[1, 2, 3, 4, 5] | filter(_ > 2)") shouldBe "[3, 4, 5]"
    }

    "multi-stage pipeline" in {
      evalStr("iota(10) | filter(_ > 5) | sum") shouldBe "30"
    }

    "complex pipeline" in {
      evalStr("iota(10) | filter(_ > 5) | map(_ * 2) | sum") shouldBe "60"
    }

    "pipeline preserves precedence" in {
      evalStr("5 + 3 | _ * 2") shouldBe "16"
    }
  }

  "Currying" - {
    "partial application of take" in {
      evalStr("[1, 2, 3, 4, 5] | take(3)") shouldBe "[1, 2, 3]"
    }

    "partial application of drop" in {
      evalStr("[1, 2, 3, 4, 5] | drop(2)") shouldBe "[3, 4, 5]"
    }

    "partial application of map" in {
      evalStr("[1, 2, 3] | map(_ + 10)") shouldBe "[11, 12, 13]"
    }

    "partial application of filter" in {
      evalStr("[1, 2, 3, 4, 5] | filter(_ > 3)") shouldBe "[4, 5]"
    }

    "curried function as variable" in {
      val evaluator = new Evaluator()
      def evalWith(input: String): String =
        Parser.parse(input) match
          case Left(err) => fail(s"Parse error: $err")
          case Right(ast) =>
            val rewritten = Rewriter.rewrite(ast)
            Value.display(evaluator.eval(rewritten))

      evalWith("take3 = take(3)")
      evalWith("[1, 2, 3, 4, 5] | take3") shouldBe "[1, 2, 3]"
    }

    "pipeline with multiple curried functions" in {
      evalStr("iota(10) | filter(_ > 5) | map(_ * 2) | take(3)") shouldBe "[12, 14, 16]"
    }
  }

  "Composition operator" - {
    "compose two functions" in {
      val evaluator = new Evaluator()
      def evalWith(input: String): String =
        Parser.parse(input) match
          case Left(err) => fail(s"Parse error: $err")
          case Right(ast) =>
            val rewritten = Rewriter.rewrite(ast)
            Value.display(evaluator.eval(rewritten))

      evalWith("double = _ * 2")
      evalWith("increment = _ + 1")
      evalWith("f = increment . double")
      evalWith("5 | f") shouldBe "11"
    }

    "compose with map" in {
      val evaluator = new Evaluator()
      def evalWith(input: String): String =
        Parser.parse(input) match
          case Left(err) => fail(s"Parse error: $err")
          case Right(ast) =>
            val rewritten = Rewriter.rewrite(ast)
            Value.display(evaluator.eval(rewritten))

      evalWith("double = _ * 2")
      evalWith("increment = _ + 1")
      evalWith("f = increment . double")
      evalWith("[1, 2, 3] | map(f)") shouldBe "[3, 5, 7]"
    }

    "composition is right-associative" in {
      val evaluator = new Evaluator()
      def evalWith(input: String): String =
        Parser.parse(input) match
          case Left(err) => fail(s"Parse error: $err")
          case Right(ast) =>
            val rewritten = Rewriter.rewrite(ast)
            Value.display(evaluator.eval(rewritten))

      evalWith("a = _ + 1")
      evalWith("b = _ * 2")
      evalWith("c = _ + 10")
      // f . g . h means f(g(h(x))), so a . b . c means a(b(c(x)))
      // For x = 5: c(5) = 15, b(15) = 30, a(30) = 31
      evalWith("f = a . b . c")
      evalWith("5 | f") shouldBe "31"
    }
  }

  "Complete examples" - {
    "sum of doubled values" in {
      evalStr("iota(5) | map(_ * 2) | sum") shouldBe "20"
    }

    "product via pipeline" in {
      evalStr("[1, 2, 3, 4] | product") shouldBe "24"
    }

    "count filtered" in {
      evalStr("iota(10) | filter(_ > 5) | count") shouldBe "4"
    }
  }

}

