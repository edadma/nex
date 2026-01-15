package io.github.edadma.nex

class ArrayTests extends TestHelpers:

  "Arrays" - {
    "array literal" in {
      evalStr("[1, 2, 3]") shouldBe "[1, 2, 3]"
    }

    "array with negative numbers" in {
      evalStr("[-1, -2, -3]") shouldBe "[-1, -2, -3]"
    }

    "array addition" in {
      evalStr("[1, 2, 3] + [4, 5, 6]") shouldBe "[5, 7, 9]"
    }

    "array subtraction" in {
      evalStr("[5, 6, 7] - [1, 2, 3]") shouldBe "[4, 4, 4]"
    }

    "array scalar multiplication" in {
      evalStr("[1, 2, 3] * 2") shouldBe "[2, 4, 6]"
    }

    "scalar array multiplication" in {
      evalStr("2 * [1, 2, 3]") shouldBe "[2, 4, 6]"
    }

    "array scalar division" in {
      evalStr("[2, 4, 6] / 2") shouldBe "[1, 2, 3]"
    }

    "negation of array" in {
      evalStr("-[1, 2, 3]") shouldBe "[-1, -2, -3]"
    }

    "empty array" in {
      evalStr("[]") shouldBe "[]"
    }

    "nested array flattens" in {
      evalStr("[[1, 2], [3, 4]]") shouldBe "[1, 2, 3, 4]"
    }
  }

  "Strings" - {
    "string literal" in {
      evalStr("\"hello\"") shouldBe "\"hello\""
    }

    "shape of string" in {
      evalStr("shape(\"hello\")") shouldBe "[5]"
    }
  }

  "Shape" - {
    "shape of 1D array" in {
      evalStr("shape([1, 2, 3])") shouldBe "[3]"
    }

    "shape of scalar" in {
      evalStr("shape(5)") shouldBe "[]"
    }

    "shape of 2D array" in {
      evalStr("shape(reshape([2, 3], iota(6)))") shouldBe "[2, 3]"
    }
  }

  "Reshape" - {
    "reshape" in {
      evalStr("reshape([2, 3], iota(6))") shouldBe "[[0, 1, 2], [3, 4, 5]]"
    }

    "reshape with cycling" in {
      evalStr("reshape([2, 3], [1, 2])") shouldBe "[[1, 2, 1], [2, 1, 2]]"
    }
  }
