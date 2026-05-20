package io.github.edadma.nex

/** Shared corpus of small, parity-compatible Nex programs used by both
  * the cross-platform interpreter runner ([[NexCorpusInterpreterTests]])
  * and the JVM-only parity runner ([[NexCorpusParityTests]]).
  *
  * Anything added here is automatically:
  *
  *  - Run through the interpreter on JVM, JS, and Native (smoke-tests
  *    the cross-platform interpreter remains correct).
  *  - Run through both the interpreter and the AOT compiler on JVM,
  *    with stdouts compared byte-for-byte (catches silent interp-vs-
  *    AOT miscompiles).
  *
  * The two runners share this single source of truth, so adding a new
  * program here gives mandatory parity coverage with no extra work and
  * no possibility of forgetting one side.
  *
  * `NexInterpreterTests` is retained for tests that are intentionally
  * interp-only — `shouldTrap` cases (AOT trap surface is via
  * `assert_traps`, exercised separately) and tests that directly poke
  * the elaborator's error machinery via `elabExpect`. New programs
  * that don't fit those categories should go HERE.
  */
object NexProgramCorpus:

  /** One program in the corpus.
    *
    * @param category the test category heading (matches the
    *                 `"category" should { ... }` group in the
    *                 generated test class).
    * @param name     the individual test name (the `"name" in { ... }`
    *                 case label).
    * @param src      the Nex source to run.
    * @param expected the exact stdout the program must produce —
    *                 stripped of leading `|` if multi-line, matching
    *                 the `stripMargin` convention already used
    *                 throughout the test suite.
    * @param pending  when `Some(reason)`, the parity runner marks the
    *                 test as `pending` instead of running it. Used for
    *                 cases where the interpreter is correct but the AOT
    *                 has a known divergence the corpus surfaced — the
    *                 reason text becomes a visible TODO until the fix
    *                 lands and the flag goes away. The interpreter
    *                 runner ignores `pending` so the corpus stays
    *                 useful as an interpreter smoke check.
    */
  case class Case(
      category: String,
      name:     String,
      src:      String,
      expected: String,
      pending:  Option[String] = None,
  )

  /** Every parity-compatible program from NexInterpreterTests, plus
    * any programs added since the migration. Grouped by category for
    * readability; the two runners iterate `all` directly so ordering
    * within a group doesn't matter functionally.
    */
  val all: Seq[Case] = Seq(

    // ========================================================================
    // top-level bindings + print
    // ========================================================================

    Case(
      "top-level bindings + print",
      "evaluate integer literal",
      """
        |val x = 42
        |def main() = print(x)
      """.stripMargin,
      "42\n",
    ),
    Case(
      "top-level bindings + print",
      "evaluate real literal",
      """
        |val x = 3.5
        |def main() = print(x)
      """.stripMargin,
      "3.5\n",
    ),
    Case(
      "top-level bindings + print",
      "evaluate bool literal",
      """
        |def main() = print(true)
      """.stripMargin,
      "true\n",
    ),
    Case(
      "top-level bindings + print",
      "evaluate string literal",
      """
        |def main() = print("hello")
      """.stripMargin,
      "hello\n",
    ),

    // ========================================================================
    // scalar arithmetic
    // ========================================================================

    Case(
      "scalar arithmetic",
      "add integers",
      """def main() = print(2 + 3)""",
      "5\n",
    ),
    Case(
      "scalar arithmetic",
      "promote int + real to real",
      """def main() = print(2 + 0.5)""",
      "2.5\n",
    ),
    Case(
      "scalar arithmetic",
      "use real division on `/`",
      """def main() = print(7 / 2)""",
      "3.5\n",
    ),
    Case(
      "scalar arithmetic",
      "use integer division on `div`",
      """def main() = print(7 div 2)""",
      "3\n",
    ),
    Case(
      "scalar arithmetic",
      "modulo on integers",
      """def main() = print(7 % 3)""",
      "1\n",
    ),
    Case(
      "scalar arithmetic",
      "power (integer)",
      """def main() = print(2 ^ 10)""",
      "1024\n",
    ),
    Case(
      "scalar arithmetic",
      "power (integer) — odd exponent path",
      """def main() = print(3 ^ 5)""",
      "243\n",
    ),
    Case(
      "scalar arithmetic",
      "power (integer) — zero exponent is one",
      """def main() = print(7 ^ 0)""",
      "1\n",
    ),
    Case(
      "scalar arithmetic",
      "power (real ^ real) routes through libm",
      """def main() = print(2.0 ^ 3.0)""",
      "8.0\n",
    ),
    Case(
      "scalar arithmetic",
      "unary minus",
      """def main() = print(-7)""",
      "-7\n",
    ),
    Case(
      "scalar arithmetic",
      "complex from `i`",
      """def main() = print(3 + 4i)""",
      "3.0+4.0i\n",
    ),
    Case(
      "scalar arithmetic",
      "complex multiply",
      """
        |def main() =
        |  val a = 1 + 1i
        |  val b = 1 - 1i
        |  print(a * b)
      """.stripMargin,
      "2.0+0.0i\n",
    ),
    Case(
      "scalar arithmetic",
      "unary minus binds tighter than power",
      """def main() = print(-2^2)""",
      "4\n",
    ),

    // ========================================================================
    // comparison + logical
    // ========================================================================

    Case(
      "comparison + logical",
      "==",
      """def main() = print(3 == 3)""",
      "true\n",
    ),
    Case(
      "comparison + logical",
      "!=",
      """def main() = print(3 != 4)""",
      "true\n",
    ),
    Case(
      "comparison + logical",
      "<",
      """def main() = print(3 < 4)""",
      "true\n",
    ),
    Case(
      "comparison + logical",
      "and short-circuits",
      """def main() = print(false and (1 / 0 == 0))""",
      "false\n",
    ),
    Case(
      "comparison + logical",
      "or short-circuits",
      """def main() = print(true or (1 / 0 == 0))""",
      "true\n",
    ),
    Case(
      "comparison + logical",
      "not",
      """def main() = print(not false)""",
      "true\n",
    ),
    Case(
      "comparison + logical",
      "string == compares by content",
      """
        |def main() =
        |  print("hi" == "hi")
        |  print("hi" == "bye")
      """.stripMargin,
      "true\nfalse\n",
    ),
    Case(
      "comparison + logical",
      "string != is the negation of ==",
      """
        |def main() =
        |  print("a" != "b")
        |  print("a" != "a")
      """.stripMargin,
      "true\nfalse\n",
    ),
    Case(
      "comparison + logical",
      "string == works on concatenated descriptors",
      """
        |def main() =
        |  val a = "foo" + "bar"
        |  print(a == "foobar")
      """.stripMargin,
      "true\n",
    ),
    Case(
      "comparison + logical",
      "IEEE: every relational op with NaN returns false",
      """
        |def main() =
        |  val x = nan
        |  print(x < x)
        |  print(x <= x)
        |  print(x > x)
        |  print(x >= x)
        |  print(x == x)
        |  print(x != x)
      """.stripMargin,
      "false\nfalse\nfalse\nfalse\nfalse\ntrue\n",
    ),
    Case(
      "comparison + logical",
      "IEEE: -0.0 and +0.0 compare equal, neither less nor greater",
      """
        |def main() =
        |  val x = -0.0
        |  print(x == 0.0)
        |  print(x < 0.0)
        |  print(x > 0.0)
      """.stripMargin,
      "true\nfalse\nfalse\n",
    ),

    // ========================================================================
    // control flow
    // ========================================================================

    Case(
      "control flow",
      "if/else",
      """
        |def main() =
        |  val x = 10
        |  if x > 5 then print("big") else print("small")
      """.stripMargin,
      "big\n",
    ),
    Case(
      "control flow",
      "if expression yields a value",
      """
        |def main() =
        |  val x = if 3 < 4 then 1 else 0
        |  print(x)
      """.stripMargin,
      "1\n",
    ),
    Case(
      "control flow",
      "while loop",
      """
        |def main() =
        |  var i = 0
        |  while i < 3 do
        |    print(i)
        |    i = i + 1
      """.stripMargin,
      "0\n1\n2\n",
    ),
    Case(
      "control flow",
      "for over array",
      """
        |def main() =
        |  for x in [10, 20, 30] do
        |    print(x)
      """.stripMargin,
      "10\n20\n30\n",
    ),
    Case(
      "control flow",
      "for over range",
      """
        |def main() =
        |  for i in 0..3 do
        |    print(i)
      """.stripMargin,
      "0\n1\n2\n",
    ),
    Case(
      "control flow",
      "range at value position produces an integer array",
      """
        |def main() =
        |  val r = 0..5
        |  print(r)
      """.stripMargin,
      "[0, 1, 2, 3, 4]\n",
    ),
    Case(
      "control flow",
      "inclusive range at value position",
      """
        |def main() = print(1..=3)
      """.stripMargin,
      "[1, 2, 3]\n",
    ),
    Case(
      "control flow",
      "empty range at value position",
      """
        |def main() =
        |  print(5..5)
        |  print(8..3)
      """.stripMargin,
      "[]\n[]\n",
    ),
    Case(
      "control flow",
      "direct call refines a bound-then-called integer lambda",
      """
        |def main() =
        |  val f = x -> x + 1
        |  print(f(10))
      """.stripMargin,
      "11\n",
    ),
    Case(
      "control flow",
      "direct call refines a bound-then-called real lambda",
      """
        |def main() =
        |  val sqr = x -> x * x
        |  print(sqr(2.5))
      """.stripMargin,
      "6.25\n",
    ),
    Case(
      "control flow",
      "second direct call after refinement still works",
      """
        |def main() =
        |  val f = x -> x * 2
        |  print(f(3))
        |  print(f(4))
      """.stripMargin,
      "6\n8\n",
    ),
    Case(
      "control flow",
      "string-arg call refines a bound-then-called lambda using `+` for concat",
      """
        |def main() =
        |  val shout = s -> s + "!"
        |  print(shout("hi"))
      """.stripMargin,
      "hi!\n",
    ),
    Case(
      "control flow",
      "for with tuple destructuring on enumerate(xs)",
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  for i, x in enumerate(xs) do
        |    print(s"$i: $x")
      """.stripMargin,
      "0: 10\n1: 20\n2: 30\n",
    ),
    Case(
      "control flow",
      "early return",
      """
        |def f(x: integer) =
        |  if x < 0 then return -1
        |  x * 2
        |
        |def main() =
        |  print(f(-5))
        |  print(f(4))
      """.stripMargin,
      "-1\n8\n",
    ),
    Case(
      "control flow",
      "array of lambdas — each closure fits in 16-byte slots",
      """
        |def main() =
        |  val fs = [(x: integer) -> x + 1, (x: integer) -> x * 2, (x: integer) -> x - 1]
        |  for f in fs do
        |    print(f(10))
      """.stripMargin,
      "11\n20\n9\n",
    ),
    Case(
      "control flow",
      "rank-2 single-index returns the row as a rank-1 array",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  print(m[0])
        |  print(m[1])
        |  print(m[1][0])
        |  print(m[0][1])
      """.stripMargin,
      "[1, 2]\n[3, 4]\n3\n2\n",
    ),

    // ========================================================================
    // functions
    // ========================================================================

    Case(
      "functions",
      "plain function",
      """
        |def add(x: integer, y: integer) = x + y
        |def main() = print(add(3, 4))
      """.stripMargin,
      "7\n",
    ),
    Case(
      "functions",
      "recursion (fact)",
      """
        |def fact(n: integer): integer =
        |  if n <= 1 then 1 else n * fact(n - 1)
        |def main() = print(fact(6))
      """.stripMargin,
      "720\n",
    ),
    Case(
      "functions",
      "mutual recursion",
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
    ),
    Case(
      "functions",
      "lambda + capture",
      """
        |def main() =
        |  val k = 10
        |  val f = x -> x + k
        |  print(f(5))
      """.stripMargin,
      "15\n",
    ),
    Case(
      "functions",
      "closure captures live cells",
      """
        |def main() =
        |  var k = 1
        |  val f = x -> x + k
        |  print(f(10))
        |  k = 100
        |  print(f(10))
      """.stripMargin,
      "11\n110\n",
    ),
    Case(
      "functions",
      "unannotated lambda passed to plain call infers param type and runs",
      """
        |def apply(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  print(apply(x -> x * 2, 3))
      """.stripMargin,
      "6\n",
    ),
    Case(
      "functions",
      "unannotated lambda passed via method-call sugar runs",
      """
        |def apply(n: integer, f: (integer -> integer)) = f(n)
        |def main() =
        |  print((3).apply(x -> x * 2))
      """.stripMargin,
      "6\n",
    ),
    Case(
      "functions",
      "lambda bound to a val with a declared function type runs",
      """
        |def main() =
        |  val f: (integer -> integer) = x -> x * 2
        |  print(f(7))
      """.stripMargin,
      "14\n",
    ),
    Case(
      "functions",
      "unannotated multi-arg lambda passed to plain call infers and runs",
      """
        |def apply2(f: ((integer, integer) -> integer), a: integer, b: integer) = f(a, b)
        |def main() =
        |  print(apply2((x, y) -> x + y, 4, 5))
      """.stripMargin,
      "9\n",
    ),
    Case(
      "functions",
      "prelude HOF `map` with unannotated lambda infers and runs",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  val ys = map(xs, x -> x * 2)
        |  print(sum(ys))
      """.stripMargin,
      "12\n",
    ),
    Case(
      "functions",
      "prelude HOF `reduce` with unannotated lambda infers and runs",
      """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(reduce(xs, 0, (a, x) -> a + x))
      """.stripMargin,
      "10\n",
    ),
    Case(
      "functions",
      "prelude HOF `filter` with unannotated lambda infers and runs",
      """
        |def main() =
        |  val xs = [1, 2, 3, 4, 5]
        |  val ev = filter(xs, x -> x % 2 == 0)
        |  print(sum(ev))
      """.stripMargin,
      "6\n",
    ),
    Case(
      "functions",
      "prelude HOF `flatMap` concatenates inner arrays",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(flatMap(xs, x -> [x, x * 10]))
      """.stripMargin,
      "[1, 10, 2, 20, 3, 30]\n",
    ),
    Case(
      "functions",
      "flatMap via method-call sugar",
      """
        |def main() =
        |  print([1, 2, 3].flatMap(x -> [x, -x]))
      """.stripMargin,
      "[1, -1, 2, -2, 3, -3]\n",
    ),
    Case(
      "functions",
      "flatMap with single-element inner arrays acts like map",
      """
        |def main() =
        |  print([1, 2, 3].flatMap(x -> [x * x]))
      """.stripMargin,
      "[1, 4, 9]\n",
    ),
    Case(
      "functions",
      "prelude HOF method-call sugar `xs.map(x -> ...)` runs",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(sum(xs.map(x -> x * 10)))
      """.stripMargin,
      "60\n",
    ),
    Case(
      "functions",
      "bound-then-called lambda with no declared type runs after deferred resolve",
      """
        |def apply(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  val f = x -> x * 2
        |  print(apply(f, 3))
      """.stripMargin,
      "6\n",
    ),
    Case(
      "functions",
      "bound-then-called lambda runs through prelude HOF",
      """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  val f  = x -> x * 10
        |  print(sum(map(xs, f)))
      """.stripMargin,
      "100\n",
    ),

    // ========================================================================
    // arrays
    // ========================================================================

    Case(
      "arrays",
      "literal + length",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(xs.length())
      """.stripMargin,
      "3\n",
    ),
    Case(
      "arrays",
      "index access",
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  print(xs[1])
      """.stripMargin,
      "20\n",
    ),
    Case(
      "arrays",
      "sum",
      """
        |def main() = print(sum([1, 2, 3, 4]))
      """.stripMargin,
      "10\n",
    ),
    Case(
      "arrays",
      "min over an integer array",
      """
        |def main() = print(min([3, 1, 4, 1, 5, 9, 2, 6]))
      """.stripMargin,
      "1\n",
    ),
    Case(
      "arrays",
      "max over an integer array",
      """
        |def main() = print(max([3, 1, 4, 1, 5, 9, 2, 6]))
      """.stripMargin,
      "9\n",
    ),
    Case(
      "arrays",
      "min over a real array",
      """
        |def main() = print(min([2.5, 1.5, 3.5]))
      """.stripMargin,
      "1.5\n",
    ),
    Case(
      "arrays",
      "max over a real array",
      """
        |def main() = print(max([2.5, 1.5, 3.5]))
      """.stripMargin,
      "3.5\n",
    ),
    Case(
      "arrays",
      "min/max binary scalar form still works",
      """
        |def main() =
        |  print(min(3, 7))
        |  print(max(3, 7))
      """.stripMargin,
      "3\n7\n",
    ),
    Case(
      "arrays",
      "element-wise add",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  val ys = [10, 20, 30]
        |  print(xs + ys)
      """.stripMargin,
      "[11, 22, 33]\n",
    ),
    Case(
      "arrays",
      "broadcast scalar * array",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(2 * xs)
      """.stripMargin,
      "[2, 4, 6]\n",
    ),
    Case(
      "arrays",
      "broadcast `arr - scalar` preserves operand order (regression)",
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  print(xs - 1)
      """.stripMargin,
      "[9, 19, 29]\n",
    ),
    Case(
      "arrays",
      "broadcast `scalar - arr` preserves operand order (regression)",
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  print(100 - xs)
      """.stripMargin,
      "[90, 80, 70]\n",
    ),
    Case(
      "arrays",
      "broadcast `arr / scalar` preserves operand order (regression)",
      """
        |def main() =
        |  val xs = [10.0, 20.0, 40.0]
        |  print(xs / 2.0)
      """.stripMargin,
      "[5.0, 10.0, 20.0]\n",
    ),
    Case(
      "arrays",
      "broadcast `arr < scalar` preserves operand order (regression)",
      """
        |def main() =
        |  val xs = [1, 5, 10]
        |  print(xs < 5)
      """.stripMargin,
      "[true, false, false]\n",
    ),
    Case(
      "arrays",
      "juxtaposition broadcast",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(2xs)
      """.stripMargin,
      "[2, 4, 6]\n",
    ),
    Case(
      "arrays",
      "rank-2 literal + transpose",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m.transpose())
      """.stripMargin,
      "[[1, 4], [2, 5], [3, 6]]\n",
    ),
    Case(
      "arrays",
      "matrix * vector via @",
      """
        |def main() =
        |  val a = [[1, 0], [0, 1]]
        |  val b = [3, 5]
        |  print(a @ b)
      """.stripMargin,
      "[3, 5]\n",
    ),
    Case(
      "arrays",
      "dot product via @ on rank-1 / rank-1",
      """
        |def main() = print([1, 2, 3] @ [4, 5, 6])
      """.stripMargin,
      "32\n",
    ),
    Case(
      "arrays",
      "map + sum chain",
      """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(xs.map(x -> x * x).sum())
      """.stripMargin,
      "30\n",
    ),
    Case(
      "arrays",
      "flatten produces column-major order (spec §10.4)",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(flatten(m))
      """.stripMargin,
      "[1, 4, 2, 5, 3, 6]\n",
    ),
    Case(
      "arrays",
      "flatten on a rank-1 array is a copy",
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  print(flatten(xs))
      """.stripMargin,
      "[10, 20, 30]\n",
    ),
    Case(
      "arrays",
      "reshape interprets input column-major (spec §10.4)",
      """
        |def main() =
        |  val flat = [1, 4, 2, 5, 3, 6]
        |  print(reshape(flat, 2, 3))
      """.stripMargin,
      "[[1, 2, 3], [4, 5, 6]]\n",
    ),
    Case(
      "arrays",
      "flatten + reshape round-trip preserves shape (spec §10.4)",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(reshape(flatten(m), 2, 3))
      """.stripMargin,
      "[[1, 2, 3], [4, 5, 6]]\n",
    ),
    Case(
      "arrays",
      "rank-1 slice with half-open range (spec §4.14)",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  print(a[0..2])
        |  print(a[1..3])
      """.stripMargin,
      "[10, 20]\n[20, 30]\n",
    ),
    Case(
      "arrays",
      "rank-1 slice with closed range",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  print(a[0..=4])
        |  print(a[1..=2])
      """.stripMargin,
      "[10, 20, 30, 40, 50]\n[20, 30]\n",
    ),
    Case(
      "arrays",
      "rank-1 slice with empty range",
      """
        |def main() =
        |  val a = [10, 20, 30]
        |  print(a[1..1])
      """.stripMargin,
      "[]\n",
    ),
    Case(
      "arrays",
      "rank-1 slice returns a fresh array (mutation doesn't alias)",
      """
        |def main() =
        |  var a = [10, 20, 30]
        |  var b = a[0..2]
        |  b[0] = 99
        |  print(a)
        |  print(b)
      """.stripMargin,
      "[10, 20, 30]\n[99, 20]\n",
    ),
    Case(
      "arrays",
      "slicing computed bounds works (spec §4.14: range expression)",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  val lo = 1
        |  val hi = 4
        |  print(a[lo..hi])
      """.stripMargin,
      "[20, 30, 40]\n",
    ),
    Case(
      "arrays",
      "rank-2 slice column with `:` (spec §4.14)",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m[:, 1])
      """.stripMargin,
      "[2, 5]\n",
    ),
    Case(
      "arrays",
      "rank-2 slice row with single integer index",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m[0, 0..2])
      """.stripMargin,
      "[1, 2]\n",
    ),
    Case(
      "arrays",
      "rank-2 slice with row range and full column axis",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  print(m[0..2, :])
      """.stripMargin,
      "[[1, 2, 3], [4, 5, 6]]\n",
    ),
    Case(
      "arrays",
      "rank-2 sub-matrix slice (both axes ranges)",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  print(m[0..2, 1..3])
      """.stripMargin,
      "[[2, 3], [5, 6]]\n",
    ),
    Case(
      "arrays",
      "rank-2 full copy with `m[:, :]`",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  var copy = m[:, :]
        |  copy[0, 0] = 99
        |  print(m)
        |  print(copy)
      """.stripMargin,
      "[[1, 2], [3, 4]]\n[[99, 2], [3, 4]]\n",
    ),
    Case(
      "arrays",
      "rank-2 slice with closed-range axis",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m[:, 0..=2])
      """.stripMargin,
      "[[1, 2, 3], [4, 5, 6]]\n",
    ),
    Case(
      "arrays",
      "var array index assign",
      """
        |def main() =
        |  var xs = [1, 2, 3]
        |  xs[1] = 99
        |  print(xs)
      """.stripMargin,
      "[1, 99, 3]\n",
    ),
    Case(
      "arrays",
      "rank-1 slice-assign overwrites underlying buffer",
      """
        |def main() =
        |  var xs = [1, 2, 3, 4, 5]
        |  xs[1..4] = [20, 30, 40]
        |  print(xs)
      """.stripMargin,
      "[1, 20, 30, 40, 5]\n",
    ),
    Case(
      "arrays",
      "rank-1 inclusive slice-assign",
      """
        |def main() =
        |  var xs = [1, 2, 3, 4]
        |  xs[0..=1] = [10, 20]
        |  print(xs)
      """.stripMargin,
      "[10, 20, 3, 4]\n",
    ),
    Case(
      "arrays",
      "rank-2 row replace via m[i, :] = rhs",
      """
        |def main() =
        |  var m = [[1, 2], [3, 4]]
        |  m[0, :] = [10, 20]
        |  print(m)
      """.stripMargin,
      "[[10, 20], [3, 4]]\n",
    ),
    Case(
      "arrays",
      "rank-2 column replace via m[:, j] = rhs",
      """
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6]]
        |  m[:, 2] = [30, 60]
        |  print(m)
      """.stripMargin,
      "[[1, 2, 30], [4, 5, 60]]\n",
    ),

    // ========================================================================
    // structs
    // ========================================================================

    Case(
      "structs",
      "construct + field access",
      """
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |def main() =
        |  val p = Point(3.0, 4.0)
        |  print(p.x)
        |  print(p.y)
      """.stripMargin,
      "3.0\n4.0\n",
    ),
    Case(
      "structs",
      "var struct field assign",
      """
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |def main() =
        |  var p = Point(1.0, 2.0)
        |  p.x = 99.0
        |  print(p.x)
      """.stripMargin,
      "99.0\n",
    ),
    Case(
      "structs",
      "field write preserves other fields",
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
    ),
    Case(
      "structs",
      "nested struct field write through chained TField",
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
    ),
    Case(
      "structs",
      "refcounted field write swaps the underlying string",
      """
        |struct Item
        |  name: string
        |end Item
        |
        |def main() =
        |  var it = Item("hello" + " world")
        |  it.name = "foo" + "bar"
        |  print(it.name)
      """.stripMargin,
      "foobar\n",
    ),
    Case(
      "structs",
      "var c = b on a struct copies — mutations don't alias",
      """
        |struct Box
        |  v: integer
        |end Box
        |
        |def main() =
        |  var b = Box(10)
        |  var c = b
        |  c.v = 99
        |  print(b.v)
        |  print(c.v)
        |  b.v = 1
        |  print(b.v)
        |  print(c.v)
      """.stripMargin,
      "10\n99\n1\n99\n",
    ),
    Case(
      "structs",
      "nested struct copy: inner mutation in copy doesn't leak to original",
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
        |  var a = Outer(Inner(1))
        |  var b = a
        |  b.i.v = 99
        |  print(a.i.v)
        |  print(b.i.v)
      """.stripMargin,
      "1\n99\n",
    ),
    Case(
      "structs",
      "field write through an array-element receiver mutates the slot",
      """
        |struct P
        |  x: integer
        |end P
        |
        |def main() =
        |  var ps = [P(1), P(2), P(3)]
        |  ps[0].x = 99
        |  print(ps[0].x)
        |  print(ps[1].x)
      """.stripMargin,
      "99\n2\n",
    ),

    // ========================================================================
    // prelude
    // ========================================================================

    Case(
      "prelude",
      "sqrt of real",
      """def main() = print(sqrt(9.0))""",
      "3.0\n",
    ),
    Case(
      "prelude",
      "sqrt of negative real returns NaN (spec §10.2)",
      """def main() = print(sqrt(-4.0))""",
      "nan\n",
    ),
    Case(
      "prelude",
      "sqrt of negative complex returns the principal imaginary root",
      """def main() = print(sqrt(-4.0 + 0i))""",
      "0.0+2.0i\n",
    ),
    Case(
      "prelude",
      "abs",
      """def main() = print(abs(-7))""",
      "7\n",
    ),
    Case(
      "prelude",
      "exp(complex) — exp(0 + 0i) = 1 + 0i exactly",
      """def main() = print(exp(0.0 + 0i))""",
      "1.0+0.0i\n",
    ),
    Case(
      "prelude",
      "sqrt(complex) — sqrt(-1 + 0i) = 0 + i",
      """def main() = print(sqrt(-1.0 + 0i))""",
      "0.0+1.0i\n",
    ),
    Case(
      "prelude",
      "range",
      """def main() = print(range(0, 5))""",
      "[0, 1, 2, 3, 4]\n",
    ),
    Case(
      "prelude",
      "linspace",
      """def main() = print(linspace(0.0, 1.0, 5))""",
      "[0.0, 0.25, 0.5, 0.75, 1.0]\n",
    ),
    Case(
      "prelude",
      "zeros + ones",
      """def main() =
        |  print(zeros(3))
        |  print(ones(3))
      """.stripMargin,
      "[0, 0, 0]\n[1, 1, 1]\n",
    ),
    Case(
      "prelude",
      "format returns a string",
      """
        |def main() =
        |  val s = format(2 + 3)
        |  print(s)
        |  print(s == "5")
      """.stripMargin,
      "5\ntrue\n",
    ),
    Case(
      "prelude",
      "format joins multiple args with a single space",
      """
        |def main() = print(format(1, 2, 3))
      """.stripMargin,
      "1 2 3\n",
    ),
    Case(
      "prelude",
      "format with mixed types (int / real / string)",
      """
        |def main() = print(format(1, 2.5, "ok"))
      """.stripMargin,
      "1 2.5 ok\n",
    ),
    Case(
      "prelude",
      "assert_eq passes silently",
      """
        |def main() =
        |  assert_eq(2 + 2, 4)
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_traps passes when the lambda traps",
      """
        |def main() =
        |  assert_traps(() -> assert(false, "boom"))
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_traps with substring match passes",
      """
        |def main() =
        |  assert_traps(() -> assert(false, "division by zero"), "division")
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_traps catches a runtime trap (division by zero)",
      """
        |def main() =
        |  assert_traps(() -> 1 / 0)
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),

    // ========================================================================
    // blocks + scoping
    // ========================================================================

    Case(
      "blocks + scoping",
      "sequential vals are scoped to the function body",
      """
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(a + b)
      """.stripMargin,
      "7\n",
    ),
    Case(
      "blocks + scoping",
      "val RHS can be an indented block",
      """
        |def main() =
        |  val total =
        |    val a = 1
        |    val b = 2
        |    val c = 3
        |    a + b + c
        |  print(total)
      """.stripMargin,
      "6\n",
    ),
    Case(
      "blocks + scoping",
      "top-level val RHS can be an indented block",
      """
        |val greeting =
        |  val name = "world"
        |  s"hello, $name"
        |
        |def main() = print(greeting)
      """.stripMargin,
      "hello, world\n",
    ),
    Case(
      "blocks + scoping",
      "inner def can shadow outer val",
      """
        |def f(x: integer) =
        |  val x = x + 100
        |  x
        |
        |def main() =
        |  print(f(1))
      """.stripMargin,
      "101\n",
    ),

    // ========================================================================
    // tuple destructuring
    // ========================================================================

    Case(
      "tuple destructuring",
      "bind block-level names from a tuple literal",
      """
        |def main() =
        |  val a, b = (3, 4)
        |  print(a)
        |  print(b)
        |  print(a + b)
      """.stripMargin,
      "3\n4\n7\n",
    ),
    Case(
      "tuple destructuring",
      "bind from an existing tuple variable",
      """
        |def main() =
        |  val pair = (10, 20)
        |  val x, y = pair
        |  print(x - y)
      """.stripMargin,
      "-10\n",
    ),
    Case(
      "tuple destructuring",
      "handle 3-tuples with mixed types",
      """
        |def main() =
        |  val p, q, r = (1.5, 2.5, 3.5)
        |  print(p + q + r)
      """.stripMargin,
      "7.5\n",
    ),
    Case(
      "tuple destructuring",
      "bind names at the top level",
      """
        |val a, b = (10, 20)
        |def main() = print(a + b)
      """.stripMargin,
      "30\n",
    ),
    Case(
      "tuple destructuring",
      "wildcard component is allowed",
      """
        |def main() =
        |  val _, b = (99, 7)
        |  print(b)
      """.stripMargin,
      "7\n",
    ),
    Case(
      "tuple destructuring",
      "preserves heterogeneous element types",
      """
        |def main() =
        |  val n, s = (42, "hi")
        |  print(s"n=$n s=$s")
      """.stripMargin,
      "n=42 s=hi\n",
    ),

    // ========================================================================
    // paren-less tuple construction
    // ========================================================================

    Case(
      "paren-less tuple construction",
      "bind a tuple value with no parens at all",
      """
        |def main() =
        |  val a, b = 1, 2
        |  print(a)
        |  print(b)
        |  print(a + b)
      """.stripMargin,
      "1\n2\n3\n",
    ),
    Case(
      "paren-less tuple construction",
      "bind a single name to a paren-less tuple RHS",
      """
        |def main() =
        |  val t = 1, 2
        |  print(t)
      """.stripMargin,
      "(1, 2)\n",
    ),
    Case(
      "paren-less tuple construction",
      "bind a paren-less 3-tuple at the top level",
      """
        |val a, b, c = 10, 20, 30
        |def main() = print(a + b + c)
      """.stripMargin,
      "60\n",
    ),
    Case(
      "paren-less tuple construction",
      "bind a paren-less heterogeneous tuple",
      """
        |def main() =
        |  val n, s = 42, "hello"
        |  print(s"n=$n s=$s")
      """.stripMargin,
      "n=42 s=hello\n",
    ),

    // ========================================================================
    // string interpolation
    // ========================================================================

    Case(
      "string interpolation",
      "embed variable",
      """
        |def main() =
        |  val x = 42
        |  print(s"x = $x")
      """.stripMargin,
      "x = 42\n",
    ),
    Case(
      "string interpolation",
      "embed multiple variables + literal text",
      """
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"$a + $b")
      """.stripMargin,
      "3 + 4\n",
    ),
    Case(
      "string interpolation",
      "evaluate a `${...}` arithmetic expression",
      """
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"sum = ${a + b}")
      """.stripMargin,
      "sum = 7\n",
    ),
    Case(
      "string interpolation",
      "evaluate multiple `${...}` expressions in one string",
      """
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"${a} * ${b} = ${a * b}")
      """.stripMargin,
      "3 * 4 = 12\n",
    ),
    Case(
      "string interpolation",
      "evaluate a function call inside `${...}`",
      """
        |def square(x: integer) = x * x
        |def main() =
        |  print(s"5^2 = ${square(5)}")
      """.stripMargin,
      "5^2 = 25\n",
    ),
    Case(
      "string interpolation",
      "evaluate an `if` expression inside `${...}`",
      """
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"max = ${if a < b then b else a}")
      """.stripMargin,
      "max = 4\n",
    ),
    Case(
      "string interpolation",
      "evaluate a `${...}` reading destructured names",
      """
        |def main() =
        |  val x, y = (10, 20)
        |  print(s"x+y = ${x + y}")
      """.stripMargin,
      "x+y = 30\n",
    ),

    // ========================================================================
    // mut by-ref
    // ========================================================================

    Case(
      "mut by-ref",
      "callee assignment to a mut param mutates the caller's var",
      """
        |def bump(x: mut integer) = x = x + 1
        |
        |def main() =
        |  var n = 41
        |  bump(n)
        |  print(n)
      """.stripMargin,
      "42\n",
    ),
    Case(
      "mut by-ref",
      "callee can both read AND write the aliased cell",
      """
        |def double(x: mut integer) = x = x * 2
        |
        |def main() =
        |  var n = 7
        |  double(n)
        |  double(n)
        |  print(n)
      """.stripMargin,
      "28\n",
    ),
    Case(
      "mut by-ref",
      "two mut params alias two independent vars",
      """
        |def swap(a: mut integer, b: mut integer) =
        |  val tmp = a
        |  a = b
        |  b = tmp
        |
        |def main() =
        |  var x = 1
        |  var y = 2
        |  swap(x, y)
        |  print(x)
        |  print(y)
      """.stripMargin,
      "2\n1\n",
    ),
    Case(
      "mut by-ref",
      "mut param forwarded into another mut param reaches the original var",
      """
        |def inner(x: mut integer) = x = x + 100
        |def outer(y: mut integer) = inner(y)
        |
        |def main() =
        |  var n = 5
        |  outer(n)
        |  print(n)
      """.stripMargin,
      "105\n",
    ),
    Case(
      "mut by-ref",
      "read-mode params still copy: caller's val is unchanged after the call",
      """
        |def use(x: integer) = x + 1
        |
        |def main() =
        |  val n = 10
        |  val r = use(n)
        |  print(n)
        |  print(r)
      """.stripMargin,
      "10\n11\n",
    ),
    Case(
      "mut by-ref",
      "mut param with same name as caller's var doesn't collide",
      """
        |def bump(n: mut integer) = n = n + 1
        |
        |def main() =
        |  var n = 0
        |  bump(n)
        |  bump(n)
        |  bump(n)
        |  print(n)
      """.stripMargin,
      "3\n",
    ),

    // ========================================================================
    // auto-clone (§8.3)
    // ========================================================================

    Case(
      "auto-clone (§8.3)",
      "var b = a clones when a is reused later",
      """
        |def main() =
        |  var a = [10, 20, 30]
        |  var b = a
        |  b[0] = 999
        |  print(a[0])
        |  print(b[0])
      """.stripMargin,
      "10\n999\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "var b = a does not clone when a has no later use",
      """
        |def main() =
        |  var a = [10, 20, 30]
        |  var b = a
        |  b[0] = 999
        |  print(b[0])
      """.stripMargin,
      "999\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "mut-call without later use mutates the caller's var (no clone)",
      """
        |def zero_first(xs: mut [integer]) = xs[0] = 0
        |
        |def main() =
        |  var a = [10, 20, 30]
        |  zero_first(a)
      """.stripMargin,
      "",
    ),
    Case(
      "auto-clone (§8.3)",
      "mut-call with later use of the same var clones (mutation is invisible)",
      """
        |def zero_first(xs: mut [integer]) = xs[0] = 0
        |
        |def main() =
        |  var a = [10, 20, 30]
        |  zero_first(a)
        |  print(a[0])
      """.stripMargin,
      "10\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "var b = a then mut-call on b: a and b are independent clones",
      """
        |def zero_first(xs: mut [integer]) = xs[0] = 0
        |
        |def main() =
        |  var a = [10, 20, 30]
        |  var b = a
        |  zero_first(b)
        |  print(a[0])
        |  print(b[0])
      """.stripMargin,
      "10\n10\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "assign b = a clones when a is reused later",
      """
        |def main() =
        |  var a = [1, 2, 3]
        |  var b = [9, 9, 9]
        |  b = a
        |  b[0] = 0
        |  print(a[0])
        |  print(b[0])
      """.stripMargin,
      "1\n0\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "rank-2 var binding clones independently",
      """
        |def main() =
        |  var m = [[1, 2], [3, 4]]
        |  var n = m
        |  n[0, 0] = 99
        |  print(m[0, 0])
        |  print(n[0, 0])
      """.stripMargin,
      "1\n99\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "val source bound to var does not need cloning (val is immutable)",
      """
        |def main() =
        |  val a = [10, 20, 30]
        |  var b = a
        |  b[0] = 999
        |  print(a[0])
        |  print(b[0])
      """.stripMargin,
      "999\n999\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "scalar var is never cloned (only arrays are unique-owned)",
      """
        |def main() =
        |  var x = 42
        |  var y = x
        |  y = 99
        |  print(x)
        |  print(y)
      """.stripMargin,
      "42\n99\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "chained var aliasing with intermediate clone",
      """
        |def main() =
        |  var a = [1, 2, 3]
        |  var b = a
        |  var c = b
        |  c[0] = 999
        |  print(a[0])
        |  print(b[0])
        |  print(c[0])
      """.stripMargin,
      "1\n1\n999\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "no clone needed when var is used only once after init",
      """
        |def main() =
        |  var a = [5, 10, 15]
        |  var b = a
        |  print(b[0])
        |  print(b[1])
        |  print(b[2])
      """.stripMargin,
      "5\n10\n15\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "rank-2 element mutation through clone does not affect original",
      """
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6]]
        |  var n = m
        |  n[1, 2] = 999
        |  print(m[1, 2])
        |  print(n[1, 2])
        |  print(m[0, 0])
        |  print(n[0, 0])
      """.stripMargin,
      "6\n999\n1\n1\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "function returning var-array doesn't break (clone at return)",
      """
        |def make_arr(n: integer) =
        |  var a = [0, 0, 0]
        |  a[0] = n
        |  a
        |
        |def main() =
        |  val xs = make_arr(42)
        |  print(xs[0])
      """.stripMargin,
      "42\n",
    ),
    Case(
      "auto-clone (§8.3)",
      "closure capture of var-array sees mutations through the captured cell",
      """
        |def main() =
        |  var a = [1, 2, 3]
        |  val f = () -> a[0]
        |  a[0] = 999
        |  print(f())
      """.stripMargin,
      "999\n",
    ),

    // ========================================================================
    // closure escape (§4.11) — captured `var` outlives the parent frame
    // ========================================================================

    Case(
      "closure escape (§4.11)",
      "returned closure increments its captured var across calls",
      """
        |def make_counter(): () -> integer =
        |  var n = 0
        |  () ->
        |    n = n + 1
        |    n
        |
        |def main() =
        |  val c = make_counter()
        |  print(c())
        |  print(c())
        |  print(c())
      """.stripMargin,
      "1\n2\n3\n",
    ),
    Case(
      "closure escape (§4.11)",
      "two counters returned from separate calls keep independent state",
      """
        |def make_counter(): () -> integer =
        |  var n = 0
        |  () ->
        |    n = n + 1
        |    n
        |
        |def main() =
        |  val a = make_counter()
        |  val b = make_counter()
        |  print(a())
        |  print(a())
        |  print(b())
        |  print(a())
        |  print(b())
      """.stripMargin,
      "1\n2\n1\n3\n2\n",
    ),
    Case(
      "closure escape (§4.11)",
      "returned closure reads captured var after parent returned",
      """
        |def make_reader(): () -> integer =
        |  var x = 42
        |  () -> x
        |
        |def main() =
        |  val r = make_reader()
        |  print(r())
        |  print(r())
      """.stripMargin,
      "42\n42\n",
    ),

    // ========================================================================
    // top-level initialization
    // ========================================================================

    Case(
      "top-level initialization",
      "allows a top-level val to call a function declared later in source order",
      """
        |val x: integer = compute()
        |
        |def compute(): integer = 42
        |
        |def main() = print(x)
      """.stripMargin,
      "42\n",
    ),
    Case(
      "top-level initialization",
      "supports mutual recursion between top-level functions",
      """
        |def is_even(n: integer): bool =
        |  if n == 0 then true else is_odd(n - 1)
        |
        |def is_odd(n: integer): bool =
        |  if n == 0 then false else is_even(n - 1)
        |
        |def main() =
        |  print(is_even(4))
        |  print(is_odd(7))
      """.stripMargin,
      "true\ntrue\n",
    ),
    Case(
      "top-level initialization",
      "supports top-level val that depends on a function",
      """
        |def square(n: integer): integer = n * n
        |
        |val s: integer = square(7)
        |
        |def main() = print(s)
      """.stripMargin,
      "49\n",
    ),
  )
