package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** **THE parity testbed.** Runs each Nex program through BOTH the
  * tree-walking interpreter and the AOT compiler (codegen → clang ->
  * native binary), then asserts the captured stdouts agree byte-for-byte.
  *
  * The verification contract documented at `tooling/02-verification.md`
  * says these two paths MUST agree. This file enforces it.
  *
  * Coverage discipline: every language feature that's documented as
  * working should appear here at least once. When a parity test
  * FAILS, the failure mode is either:
  *
  *   1. **The compiler genuinely diverged from the interpreter.** A
  *      real bug in either path. Fix it before merging.
  *   2. **The compiler doesn't yet implement the feature** (surfaces
  *      via a `; TODO:` comment in the IR, then the binary mis-
  *      computes or clang fails). That's a known gap; either fix it,
  *      or mark the parity test pending and link to the fix issue.
  *   3. **A documented divergence** (currently only the `%g` 6-sig-fig
  *      issue on irrational reals). Don't include such cases here;
  *      use whole reals only, or use `assert_approx` inside the
  *      program if the divergence is expected.
  *
  * No "Slow" tag — runs on every `sbt nexJVM/test`. Each test shells
  * out to clang once (~0.3-1s); the corpus runs in well under a
  * minute end-to-end.
  */
class NexParityTests extends AnyWordSpec with NexParityBase:

  "literals and printing" should {
    "integer"              in parityCheck("def main() = print(42)",            "42\n")
    "negative integer"     in parityCheck("def main() = print(-17)",           "-17\n")
    "whole real"           in parityCheck("def main() = print(3.0)",           "3.0\n")
    "bool true"            in parityCheck("def main() = print(true)",          "true\n")
    "bool false"           in parityCheck("def main() = print(false)",         "false\n")
    "string"               in parityCheck("""def main() = print("hello")""",   "hello\n")
    "interpolated string"  in parityCheck(
      """
        |def main() =
        |  val k = 7
        |  print(s"k = $k")
      """.stripMargin,
      "k = 7\n",
    )
  }

  "integer arithmetic" should {
    "addition"        in parityCheck("def main() = print(2 + 3)",         "5\n")
    "subtraction"     in parityCheck("def main() = print(10 - 3)",        "7\n")
    "multiplication"  in parityCheck("def main() = print(6 * 7)",         "42\n")
    "integer divide"  in parityCheck("def main() = print(17 div 5)",      "3\n")
    "modulo"          in parityCheck("def main() = print(17 % 5)",        "2\n")
    "unary minus"     in parityCheck(
      """
        |def main() =
        |  val x = 5
        |  print(-x)
      """.stripMargin,
      "-5\n",
    )
    "precedence"      in parityCheck("def main() = print(2 + 3 * 4)",     "14\n")
    "parens"          in parityCheck("def main() = print((2 + 3) * 4)",   "20\n")
  }

  "real arithmetic" should {
    "real divide gives real"     in parityCheck("def main() = print(1.0 / 2.0)", "0.5\n")
    "int / int promotes to real" in parityCheck("def main() = print(1 / 2)",     "0.5\n")
    "mixed int + real"           in parityCheck("def main() = print(1 + 2.5)",   "3.5\n")
    "whole real result"          in parityCheck("def main() = print(1.5 + 1.5)", "3.0\n")
  }

  "comparison and logical" should {
    "less than"     in parityCheck("def main() = print(3 < 5)",        "true\n")
    "equal"         in parityCheck("def main() = print(3 == 3)",       "true\n")
    "not equal"     in parityCheck("def main() = print(3 != 4)",       "true\n")
    "and"           in parityCheck("def main() = print(true and false)",  "false\n")
    "or"            in parityCheck("def main() = print(true or false)",   "true\n")
    "not"           in parityCheck("def main() = print(not true)",        "false\n")
    "short-circuit" in parityCheck("def main() = print(false and (1 div 0 == 0))", "false\n")
  }

  "control flow" should {
    "if then else (true)"  in parityCheck("def main() = print(if 3 < 5 then 1 else 2)", "1\n")
    "if then else (false)" in parityCheck("def main() = print(if 5 < 3 then 1 else 2)", "2\n")

    "while loop" in parityCheck(
      """
        |def main() =
        |  var i = 0
        |  var s = 0
        |  while i < 5 do
        |    s = s + i
        |    i = i + 1
        |  print(s)
      """.stripMargin,
      "10\n",
    )

    "for range (exclusive)" in parityCheck(
      """
        |def main() =
        |  var s = 0
        |  for k in 0..5 do
        |    s = s + k
        |  print(s)
      """.stripMargin,
      "10\n",
    )

    "for range (inclusive)" in parityCheck(
      """
        |def main() =
        |  var s = 0
        |  for k in 0..=5 do
        |    s = s + k
        |  print(s)
      """.stripMargin,
      "15\n",
    )

    "early return" in parityCheck(
      """
        |def first_positive(xs: [integer]): integer =
        |  for x in xs do
        |    if x > 0 then return x
        |  -1
        |def main() = print(first_positive([-3, -1, 4, 2]))
      """.stripMargin,
      "4\n",
    )
  }

  "bindings" should {
    "val" in parityCheck(
      """
        |def main() =
        |  val x = 7
        |  print(x)
      """.stripMargin,
      "7\n",
    )
    "var mutation" in parityCheck(
      """
        |def main() =
        |  var x = 7
        |  x = 99
        |  print(x)
      """.stripMargin,
      "99\n",
    )
    // `;` as a statement separator IS spec §2.8 but not yet
    // implemented (the lexer doesn't tokenize `;` as a separator).
    // Shadowing inside the same block is spec §5.5 but the
    // elaborator currently errors with "redeclaration of `x` in the
    // same scope". Both noted; parity tests cover what works.
    "annotated val coerces narrower numeric (int → real)" in parityCheck(
      """
        |def main() =
        |  val x: real = 7
        |  print(x)
      """.stripMargin,
      "7.0\n",
    )
  }

  "functions" should {
    "single function" in parityCheck(
      """
        |def sq(x: integer): integer = x * x
        |def main() = print(sq(7))
      """.stripMargin,
      "49\n",
    )

    "recursion (factorial)" in parityCheck(
      """
        |def fact(n: integer): integer =
        |  if n <= 1 then 1 else n * fact(n - 1)
        |def main() = print(fact(6))
      """.stripMargin,
      "720\n",
    )

    "mutual recursion" in parityCheck(
      """
        |def isEven(n: integer): bool =
        |  if n == 0 then true else isOdd(n - 1)
        |def isOdd(n: integer): bool =
        |  if n == 0 then false else isEven(n - 1)
        |def main() =
        |  print(isEven(10))
        |  print(isOdd(7))
      """.stripMargin,
      "true\ntrue\n",
    )

    "function returning array (recursive)" in parityCheck(
      """
        |def rep(n: integer, v: integer): [integer] =
        |  if n == 0 then fill(0, v)
        |  else fill(n, v)
        |def main() = print(rep(4, 9))
      """.stripMargin,
      "[9, 9, 9, 9]\n",
    )
  }

  "lambdas and closures" should {
    "lambda" in parityCheck(
      """
        |def call(f: (integer -> integer), x: integer): integer = f(x)
        |def main() = print(call(x -> x * 2, 21))
      """.stripMargin,
      "42\n",
    )

    "val capture" in parityCheck(
      """
        |def call(f: (integer -> integer), x: integer): integer = f(x)
        |def main() =
        |  val k = 10
        |  print(call(x -> x + k, 5))
      """.stripMargin,
      "15\n",
    )

    "var capture observes parent mutation" in parityCheck(
      """
        |def call(f: (integer -> integer), x: integer): integer = f(x)
        |def main() =
        |  var k = 1
        |  print(call(x -> x + k, 10))
        |  k = 100
        |  print(call(x -> x + k, 10))
      """.stripMargin,
      "11\n110\n",
    )

    "multi-arg lambda" in parityCheck(
      """
        |def call2(f: ((integer, integer) -> integer), a: integer, b: integer) = f(a, b)
        |def main() = print(call2((x, y) -> x * y + 1, 6, 7))
      """.stripMargin,
      "43\n",
    )
  }

  "rank-1 arrays" should {
    "literal"     in parityCheck("def main() = print([1, 2, 3])",         "[1, 2, 3]\n")
    "length"      in parityCheck("def main() = print(length([1, 2, 3, 4]))", "4\n")
    "index"       in parityCheck("def main() = print([10, 20, 30][1])",      "20\n")
    "mutation" in parityCheck(
      """
        |def main() =
        |  var a = [1, 2, 3]
        |  a[1] = 99
        |  print(a)
      """.stripMargin,
      "[1, 99, 3]\n",
    )
    "for-each" in parityCheck(
      """
        |def main() =
        |  var s = 0
        |  for x in [3, 5, 7] do
        |    s = s + x
        |  print(s)
      """.stripMargin,
      "15\n",
    )
    "slice exclusive" in parityCheck("def main() = print([0, 1, 2, 3, 4][1..3])", "[1, 2]\n")
    "slice inclusive" in parityCheck("def main() = print([0, 1, 2, 3, 4][1..=3])", "[1, 2, 3]\n")

    "element-wise +" in parityCheck("def main() = print([1, 2, 3] + [10, 20, 30])", "[11, 22, 33]\n")
    "broadcast scalar * array" in parityCheck("def main() = print(2 * [1, 2, 3])", "[2, 4, 6]\n")
    "broadcast array - scalar" in parityCheck("def main() = print([10, 20, 30] - 1)", "[9, 19, 29]\n")
  }

  "rank-2 arrays" should {
    "literal"           in parityCheck("def main() = print([[1, 2], [3, 4]])",       "[[1, 2], [3, 4]]\n")
    "rows"              in parityCheck("def main() = print(rows([[1, 2], [3, 4], [5, 6]]))", "3\n")
    "cols"              in parityCheck("def main() = print(cols([[1, 2, 3], [4, 5, 6]]))",   "3\n")
    "index"             in parityCheck("def main() = print([[1, 2], [3, 4]][1, 0])", "3\n")
    "mutation" in parityCheck(
      """
        |def main() =
        |  var m = [[1, 2], [3, 4]]
        |  m[0, 1] = 99
        |  print(m)
      """.stripMargin,
      "[[1, 99], [3, 4]]\n",
    )
    "row slice [:, j]"  in parityCheck("def main() = print([[1, 2, 3], [4, 5, 6]][:, 1])", "[2, 5]\n")
    "row slice [i, :]"  in parityCheck("def main() = print([[1, 2, 3], [4, 5, 6]][0, :])", "[1, 2, 3]\n")

    "element-wise (rank-2)" in parityCheck(
      "def main() = print([[1, 2], [3, 4]] + [[10, 20], [30, 40]])",
      "[[11, 22], [33, 44]]\n",
    )
  }

  "complex numbers" should {
    "literal"       in parityCheck("def main() = print(3.0 + 4i)",     "3.0+4.0i\n")
    "i constant"    in parityCheck("def main() = print(i)",            "0.0+1.0i\n")
    ".re"           in parityCheck("def main() = print((3.0 + 4i).re)", "3.0\n")
    ".im"           in parityCheck("def main() = print((3.0 + 4i).im)", "4.0\n")
    "addition"      in parityCheck("def main() = print((1.0 + 2i) + (3.0 + 4i))", "4.0+6.0i\n")
    "multiplication" in parityCheck("def main() = print((1.0 + 2i) * (3.0 + 4i))", "-5.0+10.0i\n")
    "negation"      in parityCheck("def main() = print(-(3.0 + 4i))",  "-3.0-4.0i\n")
    "abs (modulus)" in parityCheck("def main() = print(abs(3.0 + 4i))", "5.0\n")
    "conj"          in parityCheck("def main() = print(conj(3.0 + 4i))", "3.0-4.0i\n")

    // exp(2pi*i) returns 1+εi where ε ≈ -2.4e-16; both paths produce
    // the same FP value, but the formatted output differs slightly
    // between Java's Double.toString and %g. Test the real component
    // via .re instead so we get a clean equality.
    "exp(2pi * i).re == 1.0 (Euler full rotation)" in parityCheck(
      "def main() = print(exp(2pi * i).re)",
      "1.0\n",
    )

    "to_complex" in parityCheck("def main() = print(to_complex(5))", "5.0+0.0i\n")
  }

  "tuples and structs" should {
    "tuple literal"        in parityCheck("def main() = print((1, 2, 3))", "(1, 2, 3)\n")
    "tuple destructuring" in parityCheck(
      """
        |def main() =
        |  val a, b = (10, 20)
        |  print(a + b)
      """.stripMargin,
      "30\n",
    )
    "struct construction + field" in parityCheck(
      """
        |struct Point
        |  x: real
        |  y: real
        |def main() =
        |  val p = Point(3.0, 4.0)
        |  print(p.x)
        |  print(p.y)
      """.stripMargin,
      "3.0\n4.0\n",
    )
    "nested struct" in parityCheck(
      """
        |struct Inner
        |  v: integer
        |struct Outer
        |  i: Inner
        |def main() = print(Outer(Inner(42)).i.v)
      """.stripMargin,
      "42\n",
    )
  }

  "rank-1 HOFs" should {
    "map (inline lambda)"   in parityCheck(
      "def main() = print(map([1, 2, 3, 4], x -> x * x))",
      "[1, 4, 9, 16]\n",
    )
    "reduce (sum)"          in parityCheck(
      "def main() = print(reduce([1, 2, 3, 4], 0, (a, x) -> a + x))",
      "10\n",
    )
    "filter (predicate)"    in parityCheck(
      "def main() = print(filter([1, 2, 3, 4, 5], x -> x % 2 == 0))",
      "[2, 4]\n",
    )
    "method-call sugar"     in parityCheck(
      "def main() = print([1, 2, 3].map(x -> x + 100))",
      "[101, 102, 103]\n",
    )
    "named closure variable" in parityCheck(
      """
        |def main() =
        |  val f: (integer -> integer) = x -> x * 10
        |  print(map([1, 2, 3], f))
      """.stripMargin,
      "[10, 20, 30]\n",
    )
    "HOF closure captures outer var" in parityCheck(
      """
        |def main() =
        |  val k = 100
        |  print(map([1, 2, 3], x -> x + k))
      """.stripMargin,
      "[101, 102, 103]\n",
    )
  }

  "rank-1 construction" should {
    "fill (int)"     in parityCheck("def main() = print(fill(4, 7))",  "[7, 7, 7, 7]\n")
    "fill (real)"    in parityCheck("def main() = print(fill(3, 1.5))", "[1.5, 1.5, 1.5]\n")
    "zeros"          in parityCheck("def main() = print(zeros(5))",     "[0, 0, 0, 0, 0]\n")
    "ones"           in parityCheck("def main() = print(ones(4))",      "[1, 1, 1, 1]\n")
  }

  "scalar prelude (whole-value subset)" should {
    "sqrt of perfect square"   in parityCheck("def main() = print(sqrt(16.0))", "4.0\n")
    "abs (integer)"            in parityCheck("def main() = print(abs(-7))",    "7\n")
    "abs (real, whole)"        in parityCheck("def main() = print(abs(-3.0))",  "3.0\n")
    "sign positive"            in parityCheck("def main() = print(sign(7))",     "1\n")
    "sign zero"                in parityCheck("def main() = print(sign(0))",     "0\n")
    "sign negative"            in parityCheck("def main() = print(sign(-7))",   "-1\n")
    "min (int)"                in parityCheck("def main() = print(min(3, 7))",   "3\n")
    "max (int)"                in parityCheck("def main() = print(max(3, 7))",   "7\n")
    "floor"                    in parityCheck("def main() = print(floor(3.7))", "3.0\n")
    "ceil"                     in parityCheck("def main() = print(ceil(3.2))",  "4.0\n")
    "round (half up)"          in parityCheck("def main() = print(round(3.5))", "4.0\n")
    "to_integer (real → int)"  in parityCheck("def main() = print(to_integer(3.7))", "3\n")
    "to_real (int → real)"     in parityCheck("def main() = print(to_real(5))", "5.0\n")
  }

  "rank-1 prelude reductions (Wave 1)" should {
    "sum of integers"   in parityCheck("def main() = print(sum([1, 2, 3, 4]))",          "10\n")
    "sum of reals"      in parityCheck("def main() = print(sum([1.0, 2.5, 3.5]))",       "7.0\n")
    "sum of mixed → real" in parityCheck("def main() = print(sum([1.0, 2.0, 3.0, 4.0]))", "10.0\n")
    "sum of an empty integer array is 0" in parityCheck(
      """
        |def main() =
        |  val xs: [integer] = []
        |  print(sum(xs))
      """.stripMargin,
      "0\n",
    )

    "product of integers"   in parityCheck("def main() = print(product([1, 2, 3, 4]))",       "24\n")
    "product of reals"      in parityCheck("def main() = print(product([1.0, 2.0, 0.5]))",    "1.0\n")
    "product of empty array is 1" in parityCheck(
      """
        |def main() =
        |  val xs: [integer] = []
        |  print(product(xs))
      """.stripMargin,
      "1\n",
    )

    "dot product (integer)" in parityCheck(
      "def main() = print(dot([1, 2, 3], [4, 5, 6]))",
      "32\n",   // 1*4 + 2*5 + 3*6
    )
    "dot product (real)" in parityCheck(
      "def main() = print(dot([1.0, 2.0, 3.0], [4.0, 5.0, 6.0]))",
      "32.0\n",
    )

    // Dot-notation forms (method-call sugar; spec §4.9 / §10.4).
    "sum via dot notation"      in parityCheck("def main() = print([1, 2, 3, 4].sum())",     "10\n")
    "product via dot notation"  in parityCheck("def main() = print([1, 2, 3, 4].product())", "24\n")
    "chained map.sum"           in parityCheck(
      "def main() = print([1, 2, 3, 4].map(x -> x * x).sum())",
      "30\n",
    )
  }

  "rank-1 HOFs added in Wave 1" should {
    "flatMap (inline)" in parityCheck(
      "def main() = print([1, 2, 3].flatMap(x -> [x, -x]))",
      "[1, -1, 2, -2, 3, -3]\n",
    )
    "flatMap (single-element acts like map)" in parityCheck(
      "def main() = print([1, 2, 3].flatMap(x -> [x * x]))",
      "[1, 4, 9]\n",
    )
  }

  "rank-1 builders (Wave 2)" should {
    "range produces a materialized integer array" in parityCheck(
      "def main() = print(range(0, 5))",
      "[0, 1, 2, 3, 4]\n",
    )
    "range can feed sum" in parityCheck(
      "def main() = print(range(1, 11).sum())",
      "55\n",
    )
    "range can be filtered" in parityCheck(
      "def main() = print(range(0, 10).filter(x -> x % 2 == 0))",
      "[0, 2, 4, 6, 8]\n",
    )
    "enumerate pairs index with value" in parityCheck(
      """
        |def main() =
        |  for (k, v) in enumerate([10, 20, 30]) do
        |    print(k)
        |    print(v)
      """.stripMargin,
      "0\n10\n1\n20\n2\n30\n",
    )
    "enumerate length matches source" in parityCheck(
      "def main() = print(length(enumerate([7, 8, 9])))",
      "3\n",
    )
    "zip walks two arrays in parallel" in parityCheck(
      """
        |def main() =
        |  for (a, b) in zip([1, 2, 3], [10, 20, 30]) do
        |    print(a + b)
      """.stripMargin,
      "11\n22\n33\n",
    )
    "zip stops at the shorter array" in parityCheck(
      "def main() = print(length(zip([1, 2, 3, 4], [10, 20])))",
      "2\n",
    )
    "linspace produces evenly-spaced reals" in parityCheck(
      "def main() = print(linspace(0.0, 1.0, 5))",
      "[0.0, 0.25, 0.5, 0.75, 1.0]\n",
    )
    "linspace endpoints exact" in parityCheck(
      """
        |def main() =
        |  val xs = linspace(0.0, 10.0, 11)
        |  print(xs[0])
        |  print(xs[10])
      """.stripMargin,
      "0.0\n10.0\n",
    )
  }

  "rank-2 HOFs (Wave 3)" should {
    "sum of a 2x3 integer matrix sums all elements" in parityCheck(
      """
        |def main() =
        |  val m = fill((2, 3), 4)
        |  print(sum(m))
      """.stripMargin,
      "24\n",
    )
    "sum of a real matrix" in parityCheck(
      """
        |def main() =
        |  val m = fill((2, 2), 1.5)
        |  print(sum(m))
      """.stripMargin,
      "6.0\n",
    )
    "product of a small integer matrix" in parityCheck(
      """
        |def main() =
        |  val m = fill((2, 2), 3)
        |  print(product(m))
      """.stripMargin,
      "81\n",
    )
    "map preserves shape (rank-2)" in parityCheck(
      """
        |def main() =
        |  val m: [[integer]] = [[1, 2, 3], [4, 5, 6]]
        |  val sq = m.map(x -> x * x)
        |  print(sq)
      """.stripMargin,
      "[[1, 4, 9], [16, 25, 36]]\n",
    )
    "map then sum on rank-2" in parityCheck(
      """
        |def main() =
        |  val m: [[integer]] = [[1, 2], [3, 4]]
        |  print(m.map(x -> x + 10).sum())
      """.stripMargin,
      "50\n",
    )
    "reduce over rank-2 walks every element" in parityCheck(
      """
        |def main() =
        |  val m: [[integer]] = [[1, 2], [3, 4]]
        |  print(reduce(m, 0, (acc, x) -> acc + x))
      """.stripMargin,
      "10\n",
    )
  }

  "assertions (positive cases — passing assertions exit cleanly)" should {
    "assert(true)" in parityCheck(
      """
        |def main() =
        |  assert(1 + 1 == 2)
        |  print(42)
      """.stripMargin,
      "42\n",
    )
    "assert_eq passes" in parityCheck(
      """
        |def main() =
        |  assert_eq(3 * 4, 12)
        |  print(99)
      """.stripMargin,
      "99\n",
    )
    "assert_approx" in parityCheck(
      """
        |def main() =
        |  assert_approx(0.1 + 0.2, 0.3, 1.0e-9)
        |  print(7)
      """.stripMargin,
      "7\n",
    )
  }
