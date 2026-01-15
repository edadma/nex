package io.github.edadma.nex

class FunctionTests extends TestHelpers:

  "Variables" - {
    "assignment and retrieval" in {
      withEvaluator { evalWith =>
        evalWith("x = 5") shouldBe "5"
        evalWith("x * 2") shouldBe "10"
      }
    }
  }

  "Lambdas" - {
    "lambda stored as variable" in {
      withEvaluator { evalWith =>
        evalWith("f = _ * 2")
        evalWith("f(5)") shouldBe "10"
      }
    }

    "lambda with addition" in {
      withEvaluator { evalWith =>
        evalWith("add10 = _ + 10")
        evalWith("add10(5)") shouldBe "15"
      }
    }

    "lambda with division" in {
      withEvaluator { evalWith =>
        evalWith("half = _ / 2")
        evalWith("half(10)") shouldBe "5"
      }
    }

    "lambda in pipeline" in {
      withEvaluator { evalWith =>
        evalWith("f = _ * 3")
        evalWith("5 | f") shouldBe "15"
      }
    }
  }

  "Lambda/map" - {
    "map with placeholder via pipeline" in {
      evalStr("[1, 2, 3] | map(_ * 2)") shouldBe "[2, 4, 6]"
    }

    "map with addition via pipeline" in {
      evalStr("[1, 2, 3] | map(_ + 10)") shouldBe "[11, 12, 13]"
    }

    "map with curried call syntax" in {
      evalStr("map(_ * 2)([1, 2, 3])") shouldBe "[2, 4, 6]"
    }
  }

  "Expressions with mean" - {
    "average calculation" in {
      evalStr("sum([1, 2, 3]) / count([1, 2, 3])") shouldBe "2"
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

    "pipeline with take" in {
      evalStr("[1, 2, 3, 4, 5] | take(3)") shouldBe "[1, 2, 3]"
    }

    "pipeline with drop" in {
      evalStr("[1, 2, 3, 4, 5] | drop(2)") shouldBe "[3, 4, 5]"
    }

    "pipeline with multiple curried functions" in {
      evalStr("iota(10) | filter(_ > 5) | map(_ * 2) | take(3)") shouldBe "[12, 14, 16]"
    }
  }

  "Curried functions as variables" - {
    "store and use curried take" in {
      withEvaluator { evalWith =>
        evalWith("take3 = take(3)")
        evalWith("[1, 2, 3, 4, 5] | take3") shouldBe "[1, 2, 3]"
      }
    }

    "store and use curried map" in {
      withEvaluator { evalWith =>
        evalWith("double = map(_ * 2)")
        evalWith("[1, 2, 3] | double") shouldBe "[2, 4, 6]"
      }
    }

    "store and use curried filter" in {
      withEvaluator { evalWith =>
        evalWith("positive = filter(_ > 0)")
        evalWith("[-2, -1, 0, 1, 2] | positive") shouldBe "[1, 2]"
      }
    }

    "apply curried function directly" in {
      withEvaluator { evalWith =>
        evalWith("take3 = take(3)")
        evalWith("take3([1, 2, 3, 4, 5])") shouldBe "[1, 2, 3]"
      }
    }
  }

  "Composition operator" - {
    "compose two functions" in {
      withEvaluator { evalWith =>
        evalWith("double = _ * 2")
        evalWith("increment = _ + 1")
        evalWith("f = increment . double")
        evalWith("5 | f") shouldBe "11"
      }
    }

    "compose with map" in {
      withEvaluator { evalWith =>
        evalWith("double = _ * 2")
        evalWith("increment = _ + 1")
        evalWith("f = increment . double")
        evalWith("[1, 2, 3] | map(f)") shouldBe "[3, 5, 7]"
      }
    }

    "composition is right-associative" in {
      withEvaluator { evalWith =>
        evalWith("a = _ + 1")
        evalWith("b = _ * 2")
        evalWith("c = _ + 10")
        // f . g . h means f(g(h(x))), so a . b . c means a(b(c(x)))
        // For x = 5: c(5) = 15, b(15) = 30, a(30) = 31
        evalWith("f = a . b . c")
        evalWith("5 | f") shouldBe "31"
      }
    }

    "compose curried builtins" in {
      withEvaluator { evalWith =>
        evalWith("f = sum . take(3)")
        evalWith("iota(10) | f") shouldBe "3"
      }
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

    "complex data transformation" in {
      evalStr("iota(20) | filter(_ > 10) | map(_ * 2) | take(5) | sum") shouldBe "130"
    }

    "nested curried calls" in {
      evalStr("map(_ + 1)(filter(_ > 2)([1, 2, 3, 4, 5]))") shouldBe "[4, 5, 6]"
    }
  }
