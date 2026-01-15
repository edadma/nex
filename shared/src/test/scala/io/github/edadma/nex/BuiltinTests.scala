package io.github.edadma.nex

class BuiltinTests extends TestHelpers:

  "Unary builtins" - {
    "iota" in {
      evalStr("iota(5)") shouldBe "[0, 1, 2, 3, 4]"
    }

    "iota zero" in {
      evalStr("iota(0)") shouldBe "[]"
    }

    "sum" in {
      evalStr("sum([1, 2, 3])") shouldBe "6"
    }

    "sum of iota" in {
      evalStr("sum(iota(10))") shouldBe "45"
    }

    "sum of scalar" in {
      evalStr("sum(5)") shouldBe "5"
    }

    "product" in {
      evalStr("product([1, 2, 3, 4])") shouldBe "24"
    }

    "count" in {
      evalStr("count([1, 2, 3])") shouldBe "3"
    }
  }

  "Multi-arg builtins" - {
    "range" in {
      evalStr("range(2, 7)") shouldBe "[2, 3, 4, 5, 6]"
    }

    "range empty" in {
      evalStr("range(5, 5)") shouldBe "[]"
    }
  }

  "Curried builtins" - {
    "take with curried syntax" in {
      evalStr("take(3)(iota(10))") shouldBe "[0, 1, 2]"
    }

    "drop with curried syntax" in {
      evalStr("drop(2)([1, 2, 3, 4, 5])") shouldBe "[3, 4, 5]"
    }

    "map with curried syntax" in {
      evalStr("map(_ * 2)([1, 2, 3])") shouldBe "[2, 4, 6]"
    }

    "filter with curried syntax" in {
      evalStr("filter(_ > 2)([1, 2, 3, 4, 5])") shouldBe "[3, 4, 5]"
    }

    "take as first-class value" in {
      evalStr("take") shouldBe "<take>"
    }

    "partially applied take as first-class value" in {
      withEvaluator { evalWith =>
        evalWith("take3 = take(3)")
        evalWith("take3") shouldBe "<take(3)>"
      }
    }

    "map as first-class value" in {
      evalStr("map") shouldBe "<map>"
    }

    "partially applied map as first-class value" in {
      withEvaluator { evalWith =>
        evalWith("double = map(_ * 2)")
        evalWith("double") shouldBe "<map(_)>"
      }
    }
  }

  "Edge cases" - {
    "empty array sum" in {
      evalStr("sum([])") shouldBe "0"
    }

    "empty array product" in {
      evalStr("product([])") shouldBe "1"
    }

    "empty array count" in {
      evalStr("count([])") shouldBe "0"
    }

    "take zero" in {
      evalStr("take(0)([1, 2, 3])") shouldBe "[]"
    }

    "drop zero" in {
      evalStr("drop(0)([1, 2, 3])") shouldBe "[1, 2, 3]"
    }

    "take more than available" in {
      evalStr("take(10)([1, 2, 3])") shouldBe "[1, 2, 3]"
    }

    "drop more than available" in {
      evalStr("drop(10)([1, 2, 3])") shouldBe "[]"
    }

    "filter none match" in {
      evalStr("filter(_ > 100)([1, 2, 3])") shouldBe "[]"
    }

    "filter all match" in {
      evalStr("filter(_ > 0)([1, 2, 3])") shouldBe "[1, 2, 3]"
    }

    "filter on scalar" in {
      evalStr("5 | filter(_ > 0)") shouldBe "[5]"
    }

    "map on scalar treated as array" in {
      evalStr("5 | map(_ * 2)") shouldBe "[10]"
    }
  }

  "Math functions" - {
    "abs of positive" in {
      evalStr("abs(5)") shouldBe "5"
    }

    "abs of negative" in {
      evalStr("abs(-5)") shouldBe "5"
    }

    "abs of array" in {
      evalStr("abs([-3, -2, -1, 0, 1, 2, 3])") shouldBe "[3, 2, 1, 0, 1, 2, 3]"
    }

    "sqrt of perfect square" in {
      evalStr("sqrt(4)") shouldBe "2"
    }

    "sqrt of 9" in {
      evalStr("sqrt(9)") shouldBe "3"
    }

    "sqrt of negative" in {
      evalStr("sqrt(-1)") shouldBe "i"
    }

    "floor of positive decimal" in {
      evalStr("floor(3.7)") shouldBe "3"
    }

    "floor of negative decimal" in {
      evalStr("floor(-3.2)") shouldBe "-4"
    }

    "floor of integer" in {
      evalStr("floor(5)") shouldBe "5"
    }

    "ceil of positive decimal" in {
      evalStr("ceil(3.2)") shouldBe "4"
    }

    "ceil of negative decimal" in {
      evalStr("ceil(-3.7)") shouldBe "-3"
    }

    "ceil of integer" in {
      evalStr("ceil(5)") shouldBe "5"
    }

    "round of positive up" in {
      evalStr("round(3.7)") shouldBe "4"
    }

    "round of positive down" in {
      evalStr("round(3.2)") shouldBe "3"
    }

    "round of half up" in {
      evalStr("round(2.5)") shouldBe "3"
    }

    "round of negative" in {
      evalStr("round(-2.3)") shouldBe "-2"
    }

    "round of integer" in {
      evalStr("round(5)") shouldBe "5"
    }

    "sin of zero" in {
      evalStr("sin(0)") shouldBe "0"
    }

    "cos of zero" in {
      evalStr("cos(0)") shouldBe "1"
    }

    "tan of zero" in {
      evalStr("tan(0)") shouldBe "0"
    }

    "exp of zero" in {
      evalStr("exp(0)") shouldBe "1"
    }

    "exp of one approximates e" in {
      evalStr("floor(exp(1) * 100) / 100") shouldBe "271/100"
    }

    "ln of one" in {
      evalStr("ln(1)") shouldBe "0"
    }

    "ln of e approximates one" in {
      evalStr("floor(ln(e) * 100) / 100") shouldBe "1"
    }

    "asin of zero" in {
      evalStr("asin(0)") shouldBe "0"
    }

    "acos of one" in {
      evalStr("acos(1)") shouldBe "0"
    }

    "atan of zero" in {
      evalStr("atan(0)") shouldBe "0"
    }

    "pow scalar" in {
      evalStr("pow(2, 3)") shouldBe "8"
    }

    "pow with array base" in {
      evalStr("pow([2, 3, 4], 2)") shouldBe "[4, 9, 16]"
    }

    "mod scalar" in {
      evalStr("mod(10, 3)") shouldBe "1"
    }

    "mod with array" in {
      evalStr("mod([10, 11, 12], 3)") shouldBe "[1, 2, 0]"
    }

    "min of array" in {
      evalStr("min([3, 1, 4, 1, 5])") shouldBe "1"
    }

    "min of scalars" in {
      evalStr("min(3, 7)") shouldBe "3"
    }

    "min of multiple scalars" in {
      evalStr("min(5, 2, 8, 1)") shouldBe "1"
    }

    "max of array" in {
      evalStr("max([3, 1, 4, 1, 5])") shouldBe "5"
    }

    "max of scalars" in {
      evalStr("max(3, 7)") shouldBe "7"
    }

    "max of multiple scalars" in {
      evalStr("max(5, 2, 8, 1)") shouldBe "8"
    }
  }
