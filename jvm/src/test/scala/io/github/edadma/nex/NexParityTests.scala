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

    // Closure-ARC stress: each iteration constructs a fresh capturing
    // lambda + calls it. Without refcounting the env would leak every
    // iteration; with refcounting the env from each iteration is freed
    // when emitClosureCall env_decs after dispatch.
    "many closures in a loop are freed each iteration" in parityCheck(
      """
        |def call(f: (integer -> integer), x: integer): integer = f(x)
        |def main() =
        |  var total = 0
        |  for i in 1..=10 do
        |    total = total + call(x -> x + i, i)
        |  print(total)
      """.stripMargin,
      "110\n",
    )

    // Closure stored in a val binding — the slot owns one share; each
    // call inc's via TVarRef then dec's after dispatch. Final scope
    // exit frees the original share.
    "named closure called multiple times" in parityCheck(
      """
        |def main() =
        |  val add3: (integer -> integer) = x -> x + 3
        |  print(add3(10))
        |  print(add3(20))
        |  print(add3(30))
      """.stripMargin,
      "13\n23\n33\n",
    )

    // Closure returned from a factory function. The factory's return
    // is a fresh closure (rc=1); the caller dispatches it once and
    // emitClosureCall env_decs at the end → freed.
    "closure returned from factory and called inline" in parityCheck(
      """
        |def make_adder(k: integer): (integer -> integer) = x -> x + k
        |def main() = print(make_adder(7)(35))
      """.stripMargin,
      "42\n",
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

  "struct field assignment (spec §4.15 lvalue form)" should {
    "single-level field write on `var`" in parityCheck(
      """
        |struct P
        |  x: integer
        |  y: integer
        |end P
        |
        |def main() =
        |  var p = P(1, 2)
        |  p.x = 99
        |  print(p.x)
      """.stripMargin,
      "99\n",
    )
    "single-level field write preserves other fields" in parityCheck(
      """
        |struct P
        |  x: integer
        |  y: integer
        |end P
        |
        |def main() =
        |  var p = P(1, 2)
        |  p.x = 99
        |  print(p.x)
        |  print(p.y)
      """.stripMargin,
      "99\n2\n",
    )
    "nested field write through chained TField" in parityCheck(
      """
        |struct Inner
        |  v: integer
        |end Inner
        |
        |struct Outer
        |  i: Inner
        |end Outer
        |
        |def main() =
        |  var o = Outer(Inner(1))
        |  o.i.v = 99
        |  print(o.i.v)
      """.stripMargin,
      "99\n",
    )
    "three-level nested field write" in parityCheck(
      """
        |struct A
        |  v: integer
        |end A
        |
        |struct B
        |  a: A
        |end B
        |
        |struct C
        |  b: B
        |end C
        |
        |def main() =
        |  var c = C(B(A(1)))
        |  c.b.a.v = 99
        |  print(c.b.a.v)
      """.stripMargin,
      "99\n",
    )
    "refcounted field write releases old share and stores new" in parityCheck(
      """
        |struct Item
        |  name: string
        |  qty: integer
        |end Item
        |
        |def main() =
        |  var it = Item("hello" + " world", 1)
        |  it.name = "foo" + "bar"
        |  print(it.name)
        |  print(it.qty)
      """.stripMargin,
      "foobar\n1\n",
    )
    "field write inside a loop" in parityCheck(
      """
        |struct Pt
        |  x: integer
        |end Pt
        |
        |def main() =
        |  var p = Pt(0)
        |  for i in 1..4 do
        |    p.x = p.x + i
        |  print(p.x)
      """.stripMargin,
      "6\n",
    )
    "field write on aggregate with real field" in parityCheck(
      """
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |def main() =
        |  var p = Point(1.0, 2.0)
        |  p.x = 99.0
        |  p.y = 4.5
        |  print(p.x)
        |  print(p.y)
      """.stripMargin,
      "99.0\n4.5\n",
    )
  }

  "range as a value-producing expression (spec §4.12)" should {
    "exclusive range bound to a `val` and printed" in parityCheck(
      "def main() = print(0..5)",
      "[0, 1, 2, 3, 4]\n",
    )
    "inclusive range bound to a `val` and printed" in parityCheck(
      "def main() = print(0..=5)",
      "[0, 1, 2, 3, 4, 5]\n",
    )
    "empty exclusive range (lo == hi)" in parityCheck(
      "def main() = print(3..3)",
      "[]\n",
    )
    "negative-direction range produces an empty array" in parityCheck(
      "def main() = print(7..3)",
      "[]\n",
    )
    "range stored in a val survives reuse" in parityCheck(
      """
        |def main() =
        |  val r = 1..4
        |  print(r)
        |  print(r)
      """.stripMargin,
      "[1, 2, 3]\n[1, 2, 3]\n",
    )
    "range with negative bounds" in parityCheck(
      "def main() = print(-2..=2)",
      "[-2, -1, 0, 1, 2]\n",
    )
    "range with variable bounds" in parityCheck(
      """
        |def main() =
        |  val lo = 2
        |  val hi = 6
        |  print(lo..hi)
      """.stripMargin,
      "[2, 3, 4, 5]\n",
    )
    "for-loop over a range still consumes lazily (no array allocated)" in parityCheck(
      """
        |def main() =
        |  var s = 0
        |  for i in 0..5 do
        |    s = s + i
        |  print(s)
      """.stripMargin,
      "10\n",
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

  "rank-1 prelude reductions" should {
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

  "rank-1 HOFs (extended set)" should {
    "flatMap (inline)" in parityCheck(
      "def main() = print([1, 2, 3].flatMap(x -> [x, -x]))",
      "[1, -1, 2, -2, 3, -3]\n",
    )
    "flatMap (single-element acts like map)" in parityCheck(
      "def main() = print([1, 2, 3].flatMap(x -> [x * x]))",
      "[1, 4, 9]\n",
    )
  }

  "rank-1 builders" should {
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

  "rank-2 HOFs" should {
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

  "rank-2 construction" should {
    "zeros((2, 3)) builds a zero matrix" in parityCheck(
      """
        |def main() =
        |  val m = zeros((2, 3))
        |  print(m)
      """.stripMargin,
      "[[0, 0, 0], [0, 0, 0]]\n",
    )
    "ones((3, 2)) builds a ones matrix" in parityCheck(
      """
        |def main() =
        |  val m = ones((3, 2))
        |  print(m)
      """.stripMargin,
      "[[1, 1], [1, 1], [1, 1]]\n",
    )
    "zeros and ones interact with sum" in parityCheck(
      """
        |def main() =
        |  print(sum(zeros((4, 5))))
        |  print(sum(ones((4, 5))))
      """.stripMargin,
      "0\n20\n",
    )
    "identity(3) puts 1 on the diagonal" in parityCheck(
      """
        |def main() =
        |  val m = identity(3)
        |  print(m)
      """.stripMargin,
      "[[1, 0, 0], [0, 1, 0], [0, 0, 1]]\n",
    )
    "identity(n).sum() equals n" in parityCheck(
      """
        |def main() =
        |  print(sum(identity(5)))
      """.stripMargin,
      "5\n",
    )
  }

  "rank-2 matrix ops" should {
    "shape of a rank-1 array prints as a 1-tuple" in parityCheck(
      """
        |def main() =
        |  print(shape([1, 2, 3, 4, 5]))
      """.stripMargin,
      "(5)\n",
    )
    "shape of a rank-2 array prints rows, cols" in parityCheck(
      """
        |def main() =
        |  print(shape(zeros((2, 3))))
      """.stripMargin,
      "(2, 3)\n",
    )
    "transpose of a 2x3 matrix" in parityCheck(
      // reshape is column-major: [1..6] into (2,3) is [[1,3,5],[2,4,6]],
      // so transpose is [[1,2],[3,4],[5,6]].
      """
        |def main() =
        |  val m = reshape(range(1, 7), 2, 3)
        |  print(transpose(m))
      """.stripMargin,
      "[[1, 2], [3, 4], [5, 6]]\n",
    )
    "transpose of a square matrix is involutive" in parityCheck(
      """
        |def main() =
        |  val m = identity(4)
        |  print(transpose(transpose(m)))
      """.stripMargin,
      "[[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]\n",
    )
    "matmul of two 2x2 integer matrices" in parityCheck(
      // a = [[1,3],[2,4]], b = [[5,7],[6,8]] (column-major reshape).
      // a @ b = [[1*5+3*6, 1*7+3*8], [2*5+4*6, 2*7+4*8]] = [[23,31],[34,46]].
      """
        |def main() =
        |  val a = reshape([1, 2, 3, 4], 2, 2)
        |  val b = reshape([5, 6, 7, 8], 2, 2)
        |  print(matmul(a, b))
      """.stripMargin,
      "[[23, 31], [34, 46]]\n",
    )
    "@ operator on 2x2 matrices" in parityCheck(
      // a = [[1,3],[2,4]] (column-major reshape); identity leaves it alone.
      """
        |def main() =
        |  val a = reshape([1, 2, 3, 4], 2, 2)
        |  val b = identity(2)
        |  print(a @ b)
      """.stripMargin,
      "[[1, 3], [2, 4]]\n",
    )
    "matmul rank-2 times rank-1 (matrix x vector)" in parityCheck(
      // m = [[1,3,5],[2,4,6]] (column-major reshape).
      // m @ [1,2,3] = [1+6+15, 2+8+18] = [22, 28].
      """
        |def main() =
        |  val m = reshape([1, 2, 3, 4, 5, 6], 2, 3)
        |  val v = [1, 2, 3]
        |  print(matmul(m, v))
      """.stripMargin,
      "[22, 28]\n",
    )
    "matmul rank-1 times rank-2 (vector x matrix)" in parityCheck(
      // m = [[1,3,5],[2,4,6]] (column-major reshape).
      // [1,2] @ m = [1+4, 3+8, 5+12] = [5, 11, 17].
      """
        |def main() =
        |  val v = [1, 2]
        |  val m = reshape([1, 2, 3, 4, 5, 6], 2, 3)
        |  print(matmul(v, m))
      """.stripMargin,
      "[5, 11, 17]\n",
    )
    "matmul rank-1 times rank-1 is the dot product" in parityCheck(
      """
        |def main() =
        |  print(matmul([1, 2, 3], [4, 5, 6]))
      """.stripMargin,
      "32\n",
    )
    "diag of a rank-1 array" in parityCheck(
      """
        |def main() =
        |  print(diag([1, 2, 3]))
      """.stripMargin,
      "[[1, 0, 0], [0, 2, 0], [0, 0, 3]]\n",
    )
    "reshape from rank-1 to rank-2 (column-major)" in parityCheck(
      """
        |def main() =
        |  print(reshape([1, 2, 3, 4, 5, 6], 2, 3))
      """.stripMargin,
      "[[1, 3, 5], [2, 4, 6]]\n",
    )
    "flatten rank-2 is column-major" in parityCheck(
      """
        |def main() =
        |  val m = reshape([1, 2, 3, 4, 5, 6], 2, 3)
        |  print(flatten(m))
      """.stripMargin,
      "[1, 2, 3, 4, 5, 6]\n",
    )
    "sum_axis(m, 0) collapses rows into a row-vector of len cols" in parityCheck(
      """
        |def main() =
        |  val m: [[integer]] = [[1, 2, 3], [4, 5, 6]]
        |  print(sum_axis(m, 0))
      """.stripMargin,
      "[5, 7, 9]\n",
    )

    "sum_axis(m, 1) collapses cols into a col-vector of len rows" in parityCheck(
      """
        |def main() =
        |  val m: [[integer]] = [[1, 2, 3], [4, 5, 6]]
        |  print(sum_axis(m, 1))
      """.stripMargin,
      "[6, 15]\n",
    )

    "sum_axis on a real matrix" in parityCheck(
      """
        |def main() =
        |  val m: [[real]] = [[1.0, 2.0], [3.0, 4.0]]
        |  print(sum_axis(m, 0))
      """.stripMargin,
      "[4.0, 6.0]\n",
    )

    "sum_axis on a single-row matrix is just the row" in parityCheck(
      """
        |def main() =
        |  val m: [[integer]] = [[10, 20, 30]]
        |  print(sum_axis(m, 0))
      """.stripMargin,
      "[10, 20, 30]\n",
    )

    "sum_axis 1 on a single-col matrix is the per-row sum" in parityCheck(
      """
        |def main() =
        |  val m: [[integer]] = [[1], [2], [3]]
        |  print(sum_axis(m, 1))
      """.stripMargin,
      "[1, 2, 3]\n",
    )

    "flatten round-trips with reshape" in parityCheck(
      """
        |def main() =
        |  val m = reshape([1, 2, 3, 4, 5, 6], 2, 3)
        |  val r = reshape(flatten(m), 2, 3)
        |  print(r)
      """.stripMargin,
      "[[1, 3, 5], [2, 4, 6]]\n",
    )
  }

  "string concat" should {
    "literal + literal" in parityCheck(
      """
        |def main() = print("foo" + "bar")
      """.stripMargin,
      "foobar\n",
    )
    "concat via val bindings" in parityCheck(
      """
        |def main() =
        |  val a = "hello, "
        |  val b = "world"
        |  print(a + b)
      """.stripMargin,
      "hello, world\n",
    )
    "associates left" in parityCheck(
      """
        |def main() = print("a" + "b" + "c" + "d")
      """.stripMargin,
      "abcd\n",
    )
    "empty string is the identity" in parityCheck(
      """
        |def main() =
        |  print("" + "non-empty")
        |  print("non-empty" + "")
      """.stripMargin,
      "non-empty\nnon-empty\n",
    )
    "concat result is a real string (can be re-concatenated)" in parityCheck(
      """
        |def main() =
        |  val ab = "a" + "b"
        |  val cd = "c" + "d"
        |  print(ab + cd)
      """.stripMargin,
      "abcd\n",
    )
    "concat inside an interpolated print" in parityCheck(
      """
        |def main() =
        |  val greeting = "hello" + ", " + "world"
        |  print(s"msg: $greeting")
      """.stripMargin,
      "msg: hello, world\n",
    )

    // TyString-ARC stress: each iteration concats a fresh result. Without
    // refcounting every iteration leaks an intermediate descriptor; with
    // refcounting concat operands and the val slot all release cleanly.
    "many concats in a loop release intermediates each iteration" in parityCheck(
      """
        |def main() =
        |  var i = 0
        |  while i < 5 do
        |    val s = "step " + "done"
        |    print(s)
        |    i = i + 1
      """.stripMargin,
      "step done\nstep done\nstep done\nstep done\nstep done\n",
    )

    "long left-associated concat chain has no double-free" in parityCheck(
      """
        |def main() =
        |  val s = "a" + "b" + "c" + "d" + "e" + "f"
        |  print(s)
      """.stripMargin,
      "abcdef\n",
    )

    "interpolated string in a loop releases each iteration's result" in parityCheck(
      """
        |def main() =
        |  for i in 1..=3 do
        |    val msg = s"step $i complete"
        |    print(msg)
      """.stripMargin,
      "step 1 complete\nstep 2 complete\nstep 3 complete\n",
    )
  }

  "deep ARC for arrays of refcounted elements" should {
    // Reading the same element twice used to corrupt the slot: index-load
    // gave a borrowed ptr; print's dec drove the string to rc=0 and freed
    // the descriptor while the slot still pointed at it. The deep-dec
    // patch adds an inc on element load so the caller has its own share.
    "reading the same string element twice is safe" in parityCheck(
      """
        |def main() =
        |  val arr = ["x" + "y"]
        |  print(arr[0])
        |  print(arr[0])
      """.stripMargin,
      "xy\nxy\n",
    )

    "freeing an array of computed strings doesn't leak the elements" in parityCheck(
      """
        |def main() =
        |  var i = 0
        |  while i < 4 do
        |    val arr = ["a" + "b", "c" + "d"]
        |    print(arr[0])
        |    print(arr[1])
        |    i = i + 1
      """.stripMargin,
      "ab\ncd\nab\ncd\nab\ncd\nab\ncd\n",
    )

    "var-captured string + array share the same descriptor cleanly" in parityCheck(
      """
        |def main() =
        |  val s = "hi" + " there"
        |  val arr = [s, s]
        |  print(arr[0])
        |  print(arr[1])
        |  print(s)
      """.stripMargin,
      "hi there\nhi there\nhi there\n",
    )

    "stress: array of strings built and released in a loop" in parityCheck(
      """
        |def main() =
        |  var i = 0
        |  while i < 8 do
        |    val arr = ["k=" + "v", "n=" + "m"]
        |    val first = arr[0]
        |    print(first)
        |    i = i + 1
      """.stripMargin,
      "k=v\nk=v\nk=v\nk=v\nk=v\nk=v\nk=v\nk=v\n",
    )

    "rank-2 array of strings releases every element on free" in parityCheck(
      """
        |def main() =
        |  val grid = [["a" + "1", "b" + "2"], ["c" + "3", "d" + "4"]]
        |  print(grid[0, 0])
        |  print(grid[0, 1])
        |  print(grid[1, 0])
        |  print(grid[1, 1])
      """.stripMargin,
      "a1\nb2\nc3\nd4\n",
    )
  }

  "deep ARC for tuples and structs" should {
    // Tuple holding a heap string. The slot's drop must release the
    // string when the tuple goes out of scope. Tuples are addressed
    // by destructuring in Nex source.
    "tuple with a heap string destructures and the source var still works" in parityCheck(
      """
        |def main() =
        |  val s = "hi" + "!"
        |  val t = (s, 42)
        |  val a, b = t
        |  print(a)
        |  print(s)
      """.stripMargin,
      "hi!\nhi!\n",
    )

    "stress: tuples-with-strings built and released in a loop" in parityCheck(
      """
        |def main() =
        |  var i = 0
        |  while i < 6 do
        |    val s = "k=" + "v"
        |    val t = (s, i)
        |    val a, b = t
        |    print(a)
        |    i = i + 1
      """.stripMargin,
      "k=v\nk=v\nk=v\nk=v\nk=v\nk=v\n",
    )

    "struct with a string field releases it cleanly" in parityCheck(
      """
        |struct Wrap
        |  msg: string
        |  n: integer
        |def main() =
        |  val w = Wrap("hello" + " world", 7)
        |  print(w.msg)
        |  print(w.n)
      """.stripMargin,
      "hello world\n7\n",
    )

    "stress: struct-with-string built and released in a loop" in parityCheck(
      """
        |struct Wrap
        |  msg: string
        |  n: integer
        |def main() =
        |  var i = 0
        |  while i < 5 do
        |    val w = Wrap("tag" + "X", i)
        |    print(w.msg)
        |    i = i + 1
      """.stripMargin,
      "tagX\ntagX\ntagX\ntagX\ntagX\n",
    )

    "nested struct (Inner of string) releases every share" in parityCheck(
      """
        |struct Inner
        |  s: string
        |struct Outer
        |  inner: Inner
        |  tag: string
        |def main() =
        |  val o = Outer(Inner("a" + "1"), "b" + "2")
        |  print(o.inner.s)
        |  print(o.tag)
      """.stripMargin,
      "a1\nb2\n",
    )

    "struct re-binding inc's shares so both vars stay valid" in parityCheck(
      """
        |struct Wrap
        |  s: string
        |def main() =
        |  val a = Wrap("x" + "y")
        |  val b = a
        |  print(a.s)
        |  print(b.s)
      """.stripMargin,
      "xy\nxy\n",
    )

    "array of struct-with-string releases every element on free" in parityCheck(
      """
        |struct Wrap
        |  msg: string
        |  n: integer
        |def main() =
        |  val xs = [Wrap("k" + "v", 1), Wrap("a" + "b", 2)]
        |  print(xs[0].msg)
        |  print(xs[1].msg)
      """.stripMargin,
      "kv\nab\n",
    )

    "stress: array of struct-with-string built and released in a loop" in parityCheck(
      """
        |struct Wrap
        |  msg: string
        |  n: integer
        |def main() =
        |  var i = 0
        |  while i < 5 do
        |    val xs = [Wrap("tag" + "X", i), Wrap("end" + "Y", i + 1)]
        |    print(xs[0].msg)
        |    print(xs[1].msg)
        |    i = i + 1
      """.stripMargin,
      "tagX\nendY\ntagX\nendY\ntagX\nendY\ntagX\nendY\ntagX\nendY\n",
    )

    "rank-2 array of tuple-with-string releases every cell" in parityCheck(
      """
        |def main() =
        |  val xs = [[("a" + "1", 1), ("b" + "2", 2)], [("c" + "3", 3), ("d" + "4", 4)]]
        |  val a0, n0 = xs[0, 0]
        |  val a1, n1 = xs[1, 1]
        |  print(a0)
        |  print(a1)
      """.stripMargin,
      "a1\nd4\n",
    )
  }

  "deep ARC for closure env captures" should {
    // Capture a heap string by value into a closure. The dtor must dec
    // it when the env's last share dies, so the descriptor doesn't leak.
    "closure capturing a string releases the share when env is freed" in parityCheck(
      """
        |def callIt(f: (integer -> string)) = f(0)
        |def main() =
        |  val s = "hello" + " world"
        |  print(callIt(_ -> s))
      """.stripMargin,
      "hello world\n",
    )

    "stress: many closures each capturing a fresh string, in a loop" in parityCheck(
      """
        |def callIt(f: (integer -> string)) = f(0)
        |def main() =
        |  var i = 0
        |  while i < 6 do
        |    val s = "tick " + "done"
        |    print(callIt(_ -> s))
        |    i = i + 1
      """.stripMargin,
      "tick done\ntick done\ntick done\ntick done\ntick done\ntick done\n",
    )

    "closure capturing an array releases the array on env free" in parityCheck(
      """
        |def callIt(f: (integer -> integer)) = f(0)
        |def main() =
        |  val xs = [10, 20, 30]
        |  print(callIt(_ -> xs[1]))
      """.stripMargin,
      "20\n",
    )

    "stress: closures capturing arrays in a loop" in parityCheck(
      """
        |def callIt(f: (integer -> integer)) = f(0)
        |def main() =
        |  var i = 0
        |  while i < 5 do
        |    val xs = [1, 2, 3, 4]
        |    print(callIt(_ -> xs[2]))
        |    i = i + 1
      """.stripMargin,
      "3\n3\n3\n3\n3\n",
    )
  }

  "interpolated `s\"...\"` at value position" should {
    "with an integer ref" in parityCheck(
      """
        |def main() =
        |  val k = 42
        |  val msg = s"k = $k"
        |  print(msg)
      """.stripMargin,
      "k = 42\n",
    )
    "with an inline ${expr}" in parityCheck(
      """
        |def main() =
        |  val msg = s"sum = ${1 + 2}"
        |  print(msg)
      """.stripMargin,
      "sum = 3\n",
    )
    "with a real" in parityCheck(
      """
        |def main() =
        |  val x = 3.5
        |  val msg = s"x = $x"
        |  print(msg)
      """.stripMargin,
      "x = 3.5\n",
    )
    "with a whole real (formats as N.0)" in parityCheck(
      """
        |def main() =
        |  val x = 7.0
        |  val msg = s"x = $x"
        |  print(msg)
      """.stripMargin,
      "x = 7.0\n",
    )
    "with a bool" in parityCheck(
      """
        |def main() =
        |  val b = true
        |  val msg = s"flag = $b"
        |  print(msg)
      """.stripMargin,
      "flag = true\n",
    )
    "with a string ref" in parityCheck(
      """
        |def main() =
        |  val name = "world"
        |  val msg = s"hello, $name"
        |  print(msg)
      """.stripMargin,
      "hello, world\n",
    )
    "round-trips through concat" in parityCheck(
      """
        |def main() =
        |  val k = 10
        |  val a = s"k=$k"
        |  val b = " end"
        |  print(a + b)
      """.stripMargin,
      "k=10 end\n",
    )
    "multiple interpolations in one string" in parityCheck(
      """
        |def main() =
        |  val a = 1
        |  val b = 2
        |  val c = 3
        |  val msg = s"a=$a b=$b c=$c"
        |  print(msg)
      """.stripMargin,
      "a=1 b=2 c=3\n",
    )
  }

  "aggregate value-to-string" should {
    "interpolate a complex" in parityCheck(
      """
        |def main() =
        |  val z = 1.0 + 2.0i
        |  print(s"z = $z")
      """.stripMargin,
      "z = 1.0+2.0i\n",
    )
    "interpolate a complex with negative imaginary" in parityCheck(
      """
        |def main() =
        |  val z = 3.0 - 4.0i
        |  print(s"z = $z")
      """.stripMargin,
      "z = 3.0-4.0i\n",
    )
    "interpolate a tuple" in parityCheck(
      """
        |def main() =
        |  val t = (1, 2.5, true)
        |  print(s"t = $t")
      """.stripMargin,
      "t = (1, 2.5, true)\n",
    )
    "interpolate a rank-1 array of integers" in parityCheck(
      """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(s"xs = $xs")
      """.stripMargin,
      "xs = [1, 2, 3, 4]\n",
    )
    "interpolate an empty rank-1 array" in parityCheck(
      """
        |def main() =
        |  val xs: [integer] = []
        |  print(s"xs = $xs")
      """.stripMargin,
      "xs = []\n",
    )
    "interpolate a rank-2 array" in parityCheck(
      """
        |def main() =
        |  val m = identity(3)
        |  print(s"m = $m")
      """.stripMargin,
      "m = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]\n",
    )
    "interpolate a struct" in parityCheck(
      """
        |struct Point
        |  x: integer
        |  y: integer
        |end Point
        |def main() =
        |  val p = Point(3, 4)
        |  print(s"p = $p")
      """.stripMargin,
      "p = Point { x=3, y=4 }\n",
    )
    "interpolate nested aggregates" in parityCheck(
      """
        |def main() =
        |  val pairs = [(1, "a"), (2, "b")]
        |  print(s"pairs = $pairs")
      """.stripMargin,
      "pairs = [(1, a), (2, b)]\n",
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

  "`;` as a statement separator (spec §2.8)" should {
    "join two block-level decls on a single source line" in parityCheck(
      """
        |def main() =
        |  val x = 1; val y = 2
        |  print(x + y)
      """.stripMargin,
      "3\n",
    )
    "three decls separated by `;` on one line" in parityCheck(
      """
        |def main() =
        |  val a = 10; val b = 20; val c = 30
        |  print(a + b + c)
      """.stripMargin,
      "60\n",
    )
    "two top-level functions on a single source line" in parityCheck(
      """
        |def f() = 1; def g() = 2
        |def main() = print(f() + g())
      """.stripMargin,
      "3\n",
    )
    "trailing `;` is harmless" in parityCheck(
      """
        |def main() =
        |  val x = 7;
        |  print(x)
      """.stripMargin,
      "7\n",
    )
  }

  "assert_traps (catches a trap from the thunk)" should {
    "thunk that fails an assert is caught" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> assert(false))
        |  print(42)
      """.stripMargin,
      "42\n",
    )
    "thunk that fails assert_eq is caught" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> assert_eq(1, 2))
        |  print(7)
      """.stripMargin,
      "7\n",
    )
    "thunk that hits an array out-of-bounds trap is caught" in parityCheck(
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  assert_traps(() -> assert(xs[5] == 0))
        |  print(99)
      """.stripMargin,
      "99\n",
    )
    "multiple sequential assert_traps each catch their trap" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> assert(false))
        |  assert_traps(() -> assert_eq(1, 2))
        |  assert_traps(() -> assert_approx(0.0, 1.0, 0.001))
        |  print("ok")
      """.stripMargin,
      "ok\n",
    )
    "nested assert_traps — outer catches a non-trapping inner" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> assert_traps(() -> assert(true)))
        |  print("nested-ok")
      """.stripMargin,
      "nested-ok\n",
    )
    "control flow continues after a caught trap" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> assert(false))
        |  val x = 10
        |  assert_traps(() -> assert(x == 11))
        |  print(x + 1)
      """.stripMargin,
      "11\n",
    )
    "assert_traps that captures a local var sees the right value" in parityCheck(
      """
        |def main() =
        |  val target = 99
        |  assert_traps(() -> assert(target == 100))
        |  print(target)
      """.stripMargin,
      "99\n",
    )
  }

  "two-arg `assert(cond, msg)` and `assert_traps(fn, substr)`" should {
    "assert(true, msg) is silent" in parityCheck(
      """
        |def main() =
        |  assert(1 + 1 == 2, "math should work")
        |  print("ok")
      """.stripMargin,
      "ok\n",
    )
    "assert_traps catches assert(false, msg) and the substring check sees the msg" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> assert(false, "boom"), "boom")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "substring match against the generic assertion-failure text" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> assert(false), "assertion failed")
        |  print("ok")
      """.stripMargin,
      "ok\n",
    )
    "substring match against an assert_eq failure" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> assert_eq(1, 2), "assert_eq")
        |  print("eq-caught")
      """.stripMargin,
      "eq-caught\n",
    )
    "assert_traps with mismatched substring itself traps and is caught by an outer trap" in parityCheck(
      """
        |def main() =
        |  val inner = () -> assert_traps(() -> assert(false, "alpha"), "beta")
        |  assert_traps(inner, "expected substring")
        |  print("nested-caught")
      """.stripMargin,
      "nested-caught\n",
    )
    "user-provided message containing `%` is safe (no format-string injection)" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> assert(false, "100% broken: %s %d"), "100%")
        |  print("safe")
      """.stripMargin,
      "safe\n",
    )
  }

  "division by zero traps (matches interpreter)" should {
    "real `/` by zero (int/int promotes to real / 0.0) traps with `division by zero`" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> print(1 / 0), "division by zero")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "real `/` by zero (literal real) traps" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> print(1.0 / 0.0), "division by zero")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "integer `div` by zero traps with `integer division by zero`" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> print(7 div 0), "integer division by zero")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "integer `%` by zero traps with the modulo-by-zero message" in parityCheck(
      """
        |def main() =
        |  assert_traps(() -> print(7 % 0), "by zero")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "non-zero divisors keep working" in parityCheck(
      """
        |def main() =
        |  print(10 / 4)
        |  print(10 div 3)
        |  print(10 % 3)
        |  print(1.0 / 4.0)
      """.stripMargin,
      "2.5\n3\n1\n0.25\n",
    )
  }

  "slice out-of-bounds traps (matches interpreter)" should {
    "rank-1 slice with hi past end traps" in parityCheck(
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  assert_traps(() -> print(xs[0..10]), "out of bounds")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "rank-1 slice with negative lo traps" in parityCheck(
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  assert_traps(() -> print(xs[-1..2]), "out of bounds")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "rank-1 slice with hi < lo traps" in parityCheck(
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  assert_traps(() -> print(xs[2..1]), "out of bounds")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "rank-1 inclusive slice with hi == size traps (one past end)" in parityCheck(
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  assert_traps(() -> print(xs[0..=3]), "out of bounds")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "rank-2 slice with row range out of bounds traps" in parityCheck(
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  assert_traps(() -> print(m[0..5, :]), "out of bounds")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "rank-2 slice with col-index out of bounds traps" in parityCheck(
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  assert_traps(() -> print(m[:, 5]), "out of bounds")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "well-formed rank-1 and rank-2 slices keep working" in parityCheck(
      """
        |def main() =
        |  val xs = [10, 20, 30, 40, 50]
        |  print(xs[1..4])
        |  print(xs[0..=2])
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m[0, :])
        |  print(m[:, 1])
      """.stripMargin,
      "[20, 30, 40]\n[10, 20, 30]\n[1, 2, 3]\n[2, 5]\n",
    )
  }

  "remaining runtime checks (dot, sum_axis)" should {
    "dot with mismatched lengths traps" in parityCheck(
      """
        |def main() =
        |  val a = [1, 2, 3]
        |  val b = [4, 5]
        |  assert_traps(() -> print(dot(a, b)), "length mismatch")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
    "sum_axis with axis out of {0, 1} traps" in parityCheck(
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  assert_traps(() -> print(sum_axis(m, 2)), "axis must be 0 or 1")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    )
  }


  "real shortest-round-trip print (Ryu-equivalent via iterative %.Ng)" should {
    "0.1 + 0.2 prints all 17 round-trip digits, not %g's 6" in parityCheck(
      "def main() = print(0.1 + 0.2)",
      "0.30000000000000004\n",
    )
    "1.0 / 3.0 prints 16-digit repeating mantissa" in parityCheck(
      "def main() = print(1.0 / 3.0)",
      "0.3333333333333333\n",
    )
    "2.0 / 7.0 prints its shortest round-trip" in parityCheck(
      "def main() = print(2.0 / 7.0)",
      "0.2857142857142857\n",
    )
    "values that round-trip at low precision stay short" in parityCheck(
      """
        |def main() =
        |  print(0.5)
        |  print(0.25)
        |  print(0.125)
      """.stripMargin,
      "0.5\n0.25\n0.125\n",
    )
    "whole reals still print with the `.0` suffix" in parityCheck(
      """
        |def main() =
        |  print(3.0)
        |  print(1.5 + 1.5)
      """.stripMargin,
      "3.0\n3.0\n",
    )
    "non-whole reals printed inside an array match the interpreter" in parityCheck(
      """
        |def main() = print([0.1 + 0.2, 1.0 / 3.0])
      """.stripMargin,
      "[0.30000000000000004, 0.3333333333333333]\n",
    )
    "non-whole reals printed inside a tuple match the interpreter" in parityCheck(
      """
        |def main() =
        |  val t = (0.1 + 0.2, 1.0 / 3.0)
        |  print(t)
      """.stripMargin,
      "(0.30000000000000004, 0.3333333333333333)\n",
    )
    "very large reals: Java's `1.0E20` instead of C's `1e+20`" in parityCheck(
      "def main() = print(1e20)",
      "1.0E20\n",
    )
    "very small reals: Java's `1.0E-5` instead of C's `1e-05`" in parityCheck(
      "def main() = print(1e-5)",
      "1.0E-5\n",
    )
    "whole-magnitude scientific gets `.0` injected in mantissa" in parityCheck(
      "def main() = print(2e30)",
      "2.0E30\n",
    )
    "non-whole-mantissa scientific has no `.0` injection" in parityCheck(
      "def main() = print(1.5e20)",
      "1.5E20\n",
    )
    "negative-exponent leading zeros stripped" in parityCheck(
      "def main() = print(1e-7)",
      "1.0E-7\n",
    )
  }

  "slice assignment (spec §4.14 lvalue form, Fortran-90 array-section)" should {
    "rank-1 exclusive slice-assign" in parityCheck(
      """
        |def main() =
        |  var xs = [10, 20, 30, 40, 50]
        |  xs[1..4] = [200, 300, 400]
        |  print(xs)
      """.stripMargin,
      "[10, 200, 300, 400, 50]\n",
    )
    "rank-1 inclusive slice-assign" in parityCheck(
      """
        |def main() =
        |  var xs = [10, 20, 30, 40, 50]
        |  xs[0..=2] = [1, 2, 3]
        |  print(xs)
      """.stripMargin,
      "[1, 2, 3, 40, 50]\n",
    )
    "rank-1 slice-assign covering full array" in parityCheck(
      """
        |def main() =
        |  var xs = [0, 0, 0]
        |  xs[0..3] = [9, 8, 7]
        |  print(xs)
      """.stripMargin,
      "[9, 8, 7]\n",
    )
    "rank-1 slice-assign empty range is a no-op" in parityCheck(
      """
        |def main() =
        |  var xs = [10, 20, 30]
        |  val empty: [integer] = []
        |  xs[1..1] = empty
        |  print(xs)
      """.stripMargin,
      "[10, 20, 30]\n",
    )
    "rank-1 slice-assign from another array variable" in parityCheck(
      """
        |def main() =
        |  var xs = [0, 0, 0, 0, 0]
        |  val ys = [11, 22]
        |  xs[2..4] = ys
        |  print(xs)
      """.stripMargin,
      "[0, 0, 11, 22, 0]\n",
    )
    "rank-2 row replacement via row-index + col-all" in parityCheck(
      """
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  m[1, :] = [40, 50, 60]
        |  print(m)
      """.stripMargin,
      "[[1, 2, 3], [40, 50, 60], [7, 8, 9]]\n",
    )
    "rank-2 column replacement via col-index + row-all" in parityCheck(
      """
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  m[:, 1] = [20, 50, 80]
        |  print(m)
      """.stripMargin,
      "[[1, 20, 3], [4, 50, 6], [7, 80, 9]]\n",
    )
    "rank-2 submatrix replacement" in parityCheck(
      """
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  m[0..2, 0..2] = [[10, 20], [30, 40]]
        |  print(m)
      """.stripMargin,
      "[[10, 20, 3], [30, 40, 6], [7, 8, 9]]\n",
    )
    "rank-2 submatrix replacement (inclusive)" in parityCheck(
      """
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  m[0..=1, 1..=2] = [[20, 30], [50, 60]]
        |  print(m)
      """.stripMargin,
      "[[1, 20, 30], [4, 50, 60], [7, 8, 9]]\n",
    )
    "rank-2 partial row replacement: m[i, lo..hi]" in parityCheck(
      """
        |def main() =
        |  var m = [[1, 2, 3, 4], [5, 6, 7, 8]]
        |  m[0, 1..3] = [20, 30]
        |  print(m)
      """.stripMargin,
      "[[1, 20, 30, 4], [5, 6, 7, 8]]\n",
    )
    "rank-1 slice-assign with shape mismatch traps" in parityCheck(
      """
        |def bad_len(xs: mut [integer]) = xs[1..4] = [9, 9]
        |def main() =
        |  var xs = [1, 2, 3, 4, 5]
        |  assert_traps(() -> bad_len(xs), "length mismatch")
        |  print(xs)
      """.stripMargin,
      "[1, 2, 3, 4, 5]\n",
    )
    "rank-2 slice-assign with shape mismatch traps" in parityCheck(
      """
        |def bad_shape(m: mut [[integer]]) = m[0..2, 0..2] = [[1, 2]]
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  assert_traps(() -> bad_shape(m), "shape mismatch")
        |  print(m)
      """.stripMargin,
      "[[1, 2, 3], [4, 5, 6], [7, 8, 9]]\n",
    )
    "rank-1 slice-assign with hi past end traps" in parityCheck(
      """
        |def bad_oob(xs: mut [integer]) = xs[0..10] = [9, 9, 9, 9, 9, 9, 9, 9, 9, 9]
        |def main() =
        |  var xs = [1, 2, 3]
        |  assert_traps(() -> bad_oob(xs), "out of bounds")
        |  print(xs)
      """.stripMargin,
      "[1, 2, 3]\n",
    )
    "rank-1 slice-assign of reals" in parityCheck(
      """
        |def main() =
        |  var xs = [1.0, 2.0, 3.0, 4.0]
        |  xs[1..3] = [20.0, 30.0]
        |  print(xs)
      """.stripMargin,
      "[1.0, 20.0, 30.0, 4.0]\n",
    )
    "rank-1 slice-assign within a loop (sweep update)" in parityCheck(
      """
        |def main() =
        |  var xs = [0, 0, 0, 0, 0, 0]
        |  for i in 0..3 do
        |    xs[2 * i..2 * i + 2] = [i + 1, i + 1]
        |  print(xs)
      """.stripMargin,
      "[1, 1, 2, 2, 3, 3]\n",
    )
    "interleaved slice-assigns build up a result" in parityCheck(
      """
        |def main() =
        |  var xs = [0, 0, 0, 0, 0, 0]
        |  xs[0..2] = [1, 2]
        |  xs[2..4] = [3, 4]
        |  xs[4..6] = [5, 6]
        |  print(xs)
      """.stripMargin,
      "[1, 2, 3, 4, 5, 6]\n",
    )
  }
