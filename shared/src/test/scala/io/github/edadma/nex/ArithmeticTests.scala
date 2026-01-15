package io.github.edadma.nex

class ArithmeticTests extends TestHelpers:

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

    "rational division" in {
      evalStr("1 / (1/3)") shouldBe "3"
    }

    "decimal numbers" in {
      evalStr("3.14 + 0.01") shouldBe "3.15"
    }

    "unary negation" in {
      evalStr("-5") shouldBe "-5"
    }

    "unary negation of expression" in {
      evalStr("-(2 + 3)") shouldBe "-5"
    }

    "operator precedence" in {
      evalStr("2 + 3 * 4") shouldBe "14"
    }

    "parentheses" in {
      evalStr("(2 + 3) * 4") shouldBe "20"
    }

    "power operator" in {
      evalStr("2^3") shouldBe "8"
    }

    "power right associative" in {
      evalStr("2^3^2") shouldBe "512"  // 2^(3^2) = 2^9 = 512
    }

    "power precedence over multiplication" in {
      evalStr("2 * 3^2") shouldBe "18"  // 2 * 9 = 18
    }

    "power with negative exponent" in {
      evalStr("2^-1") shouldBe "1/2"
    }

    "infix mod" in {
      evalStr("10 mod 3") shouldBe "1"
    }

    "infix mod precedence" in {
      evalStr("10 + 7 mod 3") shouldBe "11"  // 10 + (7 mod 3) = 10 + 1
    }

    "infix mod with multiplication" in {
      evalStr("2 * 5 mod 3") shouldBe "1"  // (2 * 5) mod 3 = 10 mod 3 = 1
    }

    "integer division" in {
      evalStr("7 \\ 3") shouldBe "2"
    }

    "integer division negative" in {
      evalStr("-7 \\ 3") shouldBe "-2"  // truncates toward zero
    }

    "integer division precedence" in {
      evalStr("10 + 7 \\ 3") shouldBe "12"  // 10 + 2 = 12
    }
  }

  "Comparison operators" - {
    "greater than true" in {
      evalStr("5 > 3") shouldBe "1"
    }

    "greater than false" in {
      evalStr("3 > 5") shouldBe "0"
    }

    "less than" in {
      evalStr("3 < 5") shouldBe "1"
    }

    "greater than or equal" in {
      evalStr("5 >= 5") shouldBe "1"
    }

    "less than or equal" in {
      evalStr("4 <= 5") shouldBe "1"
    }

    "equal" in {
      evalStr("5 == 5") shouldBe "1"
    }

    "not equal" in {
      evalStr("5 != 3") shouldBe "1"
    }

    "array comparison with scalar" in {
      evalStr("[1, 2, 3, 4, 5] > 3") shouldBe "[0, 0, 0, 1, 1]"
    }

    "scalar comparison with array" in {
      evalStr("3 < [1, 2, 3, 4, 5]") shouldBe "[0, 0, 0, 1, 1]"
    }

    "array comparison with array" in {
      evalStr("[1, 2, 3] == [1, 2, 4]") shouldBe "[1, 1, 0]"
    }
  }

  "Complex numbers" - {
    "imaginary unit" in {
      evalStr("i") shouldBe "i"
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
