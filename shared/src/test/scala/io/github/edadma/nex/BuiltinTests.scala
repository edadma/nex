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
