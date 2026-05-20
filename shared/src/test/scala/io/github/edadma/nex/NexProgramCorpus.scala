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
    * @param mlir     opt-in flag for the MLIR parity runner. When
    *                 `true`, [[NexCorpusMlirParityTests]] additionally
    *                 lowers this case through the MLIR backend and
    *                 asserts byte-for-byte agreement with the
    *                 interpreter. The count of `mlir = true` cases is
    *                 the strangler-fig progress bar for the MLIR
    *                 backend's coverage of the corpus.
    */
  case class Case(
      category: String,
      name:     String,
      src:      String,
      expected: String,
      pending:  Option[String] = None,
      mlir:     Boolean        = false,
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
      mlir = true,
    ),
    Case(
      "top-level bindings + print",
      "evaluate real literal",
      """
        |val x = 3.5
        |def main() = print(x)
      """.stripMargin,
      "3.5\n",
      mlir = true,
    ),
    Case(
      "top-level bindings + print",
      "evaluate bool literal",
      """
        |def main() = print(true)
      """.stripMargin,
      "true\n",
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "scalar arithmetic",
      "promote int + real to real",
      """def main() = print(2 + 0.5)""",
      "2.5\n",
      mlir = true,
    ),
    Case(
      "scalar arithmetic",
      "use real division on `/`",
      """def main() = print(7 / 2)""",
      "3.5\n",
      mlir = true,
    ),
    Case(
      "scalar arithmetic",
      "use integer division on `div`",
      """def main() = print(7 div 2)""",
      "3\n",
      mlir = true,
    ),
    Case(
      "scalar arithmetic",
      "modulo on integers",
      """def main() = print(7 % 3)""",
      "1\n",
      mlir = true,
    ),
    Case(
      "scalar arithmetic",
      "power (integer)",
      """def main() = print(2 ^ 10)""",
      "1024\n",
      mlir = true,
    ),
    Case(
      "scalar arithmetic",
      "power (integer) — odd exponent path",
      """def main() = print(3 ^ 5)""",
      "243\n",
      mlir = true,
    ),
    Case(
      "scalar arithmetic",
      "power (integer) — zero exponent is one",
      """def main() = print(7 ^ 0)""",
      "1\n",
      mlir = true,
    ),
    Case(
      "scalar arithmetic",
      "power (real ^ real) routes through libm",
      """def main() = print(2.0 ^ 3.0)""",
      "8.0\n",
      mlir = true,
    ),
    Case(
      "scalar arithmetic",
      "unary minus",
      """def main() = print(-7)""",
      "-7\n",
      mlir = true,
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
      mlir = true,
    ),

    // ========================================================================
    // comparison + logical
    // ========================================================================

    Case(
      "comparison + logical",
      "==",
      """def main() = print(3 == 3)""",
      "true\n",
      mlir = true,
    ),
    Case(
      "comparison + logical",
      "!=",
      """def main() = print(3 != 4)""",
      "true\n",
      mlir = true,
    ),
    Case(
      "comparison + logical",
      "<",
      """def main() = print(3 < 4)""",
      "true\n",
      mlir = true,
    ),
    Case(
      "comparison + logical",
      "and short-circuits",
      """def main() = print(false and (1 / 0 == 0))""",
      "false\n",
      mlir = true,
    ),
    Case(
      "comparison + logical",
      "or short-circuits",
      """def main() = print(true or (1 / 0 == 0))""",
      "true\n",
      mlir = true,
    ),
    Case(
      "comparison + logical",
      "not",
      """def main() = print(not false)""",
      "true\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),

    Case(
      "comparison + logical",
      "bool == bool (spec §4.6: only `==` / `!=` allowed on bool)",
      """
        |def main() =
        |  print(true == true)
        |  print(true == false)
        |  print(false != true)
        |  print(false != false)
      """.stripMargin,
      "true\nfalse\ntrue\nfalse\n",
    ),

    // ========================================================================
    // chained comparison (§4.6)
    // ========================================================================

    Case(
      "chained comparison",
      "3-way `<` chain returns true when in range",
      """
        |def main() =
        |  val x = 5
        |  print(0 < x < 10)
      """.stripMargin,
      "true\n",
    ),
    Case(
      "chained comparison",
      "3-way `<` chain returns false when out of range",
      """
        |def main() =
        |  val x = 11
        |  print(0 < x < 10)
      """.stripMargin,
      "false\n",
    ),
    Case(
      "chained comparison",
      "mixed operators: `0 <= x < 10 <= 100`",
      """
        |def main() =
        |  val x = 5
        |  print(0 <= x < 10 <= 100)
      """.stripMargin,
      "true\n",
    ),
    Case(
      "chained comparison",
      "agrees with the equivalent explicit `and` form",
      """
        |def main() =
        |  for x in -2..=12 do
        |    val chained  = 0 <= x < 10
        |    val explicit = 0 <= x and x < 10
        |    print(chained == explicit)
      """.stripMargin,
      "true\n" * 15,
    ),
    Case(
      "chained comparison",
      "inner operand is evaluated exactly once (side-effecting call)",
      """
        |var count = 0
        |def bumped(): integer =
        |  count = count + 1
        |  5
        |
        |def main() =
        |  val ok = 0 < bumped() < 10
        |  print(ok)
        |  print(count)
      """.stripMargin,
      "true\n1\n",
    ),
    Case(
      "chained comparison",
      "4-way chain with all distinct comparison operators",
      """
        |def main() =
        |  val a = 1
        |  val b = 2
        |  val c = 3
        |  val d = 3
        |  print(a < b < c <= d)
      """.stripMargin,
      "true\n",
    ),
    Case(
      "chained comparison",
      "short-circuit: side-effecting inner operand skipped when earlier cmp fails",
      """
        |var inner_calls = 0
        |var rhs_calls   = 0
        |def inner(): integer =
        |  inner_calls = inner_calls + 1
        |  100
        |def rhs(): integer =
        |  rhs_calls = rhs_calls + 1
        |  100
        |
        |def main() =
        |  val x = -1
        |  print(0 < x < inner() < rhs())
        |  print(inner_calls)
        |  print(rhs_calls)
      """.stripMargin,
      "false\n0\n0\n",
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
      mlir = true,
    ),
    Case(
      "control flow",
      "if expression printed directly",
      """
        |def main() =
        |  val n = 7
        |  print(if n > 5 then n * 2 else n)
      """.stripMargin,
      "14\n",
      mlir = true,
    ),
    Case(
      "control flow",
      "if expression yields a real-typed value",
      """
        |def main() =
        |  val x = 1.5
        |  print(if x > 1.0 then x * 2.0 else x)
      """.stripMargin,
      "3.0\n",
      mlir = true,
    ),
    Case(
      "control flow",
      "if expression yields a bool",
      """
        |def main() =
        |  val n = 5
        |  print(if n > 0 then true else false)
      """.stripMargin,
      "true\n",
      mlir = true,
    ),
    Case(
      "control flow",
      "nested if expression",
      """
        |def main() =
        |  val x = 5
        |  val y = if x > 0 then
        |    if x > 10 then 100 else x
        |  else 0
        |  print(y)
      """.stripMargin,
      "5\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "control flow",
      "for over val-bound array",
      """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  for x in xs do
        |    print(x * x)
      """.stripMargin,
      "1\n4\n9\n16\n",
      mlir = true,
    ),
    Case(
      "control flow",
      "for over array produced by range()",
      """
        |def main() =
        |  for x in range(0, 4) do
        |    print(x + 100)
      """.stripMargin,
      "100\n101\n102\n103\n",
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "control flow",
      "for over inclusive range",
      """
        |def main() =
        |  for i in 1..=3 do
        |    print(i)
      """.stripMargin,
      "1\n2\n3\n",
      mlir = true,
    ),
    Case(
      "control flow",
      "for over range computes a running sum",
      """
        |def main() =
        |  var s = 0
        |  for i in 1..=5 do
        |    s = s + i
        |  print(s)
      """.stripMargin,
      "15\n",
      mlir = true,
    ),
    Case(
      "control flow",
      "nested for over range",
      """
        |def main() =
        |  for i in 0..2 do
        |    for j in 0..2 do
        |      print(i * 10 + j)
      """.stripMargin,
      "0\n1\n10\n11\n",
      mlir = true,
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
      "default parameter values are filled when omitted",
      """
        |def scale(x: real, factor: real = 2.0, offset: real = 0.0) =
        |  print(factor * x + offset)
        |def main() =
        |  scale(10.0)
        |  scale(10.0, 3.0)
        |  scale(10.0, 5.0, 1.0)
      """.stripMargin,
      "20.0\n30.0\n51.0\n",
    ),
    Case(
      "functions",
      "named arguments resolve to positions in any order",
      """
        |def greet(name: string, greeting: string = "Hello") =
        |  print(s"${greeting}, ${name}!")
        |def main() =
        |  greet("World")
        |  greet(name = "Nex")
        |  greet(greeting = "Hi", name = "Foo")
        |  greet("Bar", greeting = "Yo")
      """.stripMargin,
      "Hello, World!\nHello, Nex!\nHi, Foo!\nYo, Bar!\n",
    ),
    Case(
      "functions",
      "named arg fills a non-trailing slot, default fills the rest",
      """
        |def scale(x: real, factor: real = 2.0, offset: real = 0.0) =
        |  print(factor * x + offset)
        |def main() =
        |  scale(10.0, offset = 5.0)
        |  scale(10.0, factor = 7.0)
      """.stripMargin,
      "25.0\n70.0\n",
    ),
    Case(
      "functions",
      "default expression is re-evaluated at every call site",
      // The default is captured untouched and re-evaluated per call —
      // observable here because `next_id()` increments a top-level
      // counter and the default reads it fresh on each call.
      """
        |var counter = 0
        |def next_id(): integer =
        |  counter = counter + 1
        |  counter
        |def tag(prefix: string, id: integer = next_id()) =
        |  print(s"${prefix}-${id}")
        |def main() =
        |  tag("a")
        |  tag("b")
        |  tag("c", 99)
        |  tag("d")
      """.stripMargin,
      "a-1\nb-2\nc-99\nd-3\n",
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
    Case(
      "functions",
      "top-level def passed as a function-value argument",
      // Regression: passing a top-level `def` name through a HOF arg
      // used to lower to `{ ptr, ptr } 0` in LLVM IR (clang rejected
      // it: "integer constant must have integer type"). Codegen now
      // materializes a closure-shape thunk per referenced def and
      // builds `{ <thunk>, null }` so the indirect call site reads
      // fn_ptr + env_ptr uniformly.
      """
        |def harmonic(t: real, y: [real]) = [y[1], -y[0]]
        |
        |def call_with(f: (real, [real]) -> [real], t: real, y: [real]) =
        |  f(t, y)
        |
        |def main() =
        |  print(call_with(harmonic, 0.0, [1.0, 0.0]))
      """.stripMargin,
      "[0.0, -1.0]\n",
    ),
    Case(
      "functions",
      "same top-level def passed twice in one expression",
      // Two TVarRef sites to the same def should produce two closure
      // literals sharing a single thunk emission (the request is
      // memoized on symbol id).
      """
        |def neg(x: integer) = -x
        |
        |def apply2(f: integer -> integer, g: integer -> integer, x: integer) =
        |  f(g(x))
        |
        |def main() =
        |  print(apply2(neg, neg, 7))
      """.stripMargin,
      "7\n",
    ),
    Case(
      "functions",
      "top-level def bound to a val, then called via the binding",
      // Mixed path: a `val f = top_level_def` binding stores the
      // closure literal; calling `f(x)` then goes through
      // `emitClosureCall` (indirect dispatch). Different from the
      // pre-existing val-bound-lambda test above.
      """
        |def square(x: integer) = x * x
        |
        |def main() =
        |  val f = square
        |  print(f(9))
      """.stripMargin,
      "81\n",
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "arrays",
      "negative index counts from the end (rank-1)",
      """
        |def main() =
        |  val xs = [10, 20, 30, 40, 50]
        |  print(xs[-1])
        |  print(xs[-2])
        |  print(xs[-5])
      """.stripMargin,
      "50\n40\n10\n",
    ),
    Case(
      "arrays",
      "negative index on rank-2 element and row",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  print(m[-1, -1])
        |  print(m[-1, 0])
        |  print(m[0, -1])
        |  print(m[-1])
      """.stripMargin,
      "9\n7\n3\n[7, 8, 9]\n",
    ),
    Case(
      "arrays",
      "negative index in slice-assign target",
      """
        |def main() =
        |  var xs = [10, 20, 30]
        |  xs[-1] = 999
        |  xs[-3] = 111
        |  print(xs)
      """.stripMargin,
      "[111, 20, 999]\n",
    ),
    Case(
      "arrays",
      "negative out-of-range index traps",
      """
        |def main() =
        |  val xs = [10, 20, 30]
        |  assert_traps(() -> print(xs[-4]), "out of bounds")
        |  assert_traps(() -> print(xs[3]),  "out of bounds")
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "arrays",
      "sum",
      """
        |def main() = print(sum([1, 2, 3, 4]))
      """.stripMargin,
      "10\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "min over an integer array",
      """
        |def main() = print(min([3, 1, 4, 1, 5, 9, 2, 6]))
      """.stripMargin,
      "1\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "max over an integer array",
      """
        |def main() = print(max([3, 1, 4, 1, 5, 9, 2, 6]))
      """.stripMargin,
      "9\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "min over a real array",
      """
        |def main() = print(min([2.5, 1.5, 3.5]))
      """.stripMargin,
      "1.5\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "max over a real array",
      """
        |def main() = print(max([2.5, 1.5, 3.5]))
      """.stripMargin,
      "3.5\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "arrays",
      "broadcast scalar-int * real-array promotes scalar before fmul",
      // Regression: AOT codegen used to emit `fmul double 2, %t` (with
      // the integer literal as a double operand), which clang rejects.
      // `emitBroadcast` now lifts the scalar IR value to the array's
      // element type via `liftScalarTo` before the per-element op.
      """
        |def main() =
        |  val a = [1.0, 2.0, 3.0]
        |  print(2 * a)
        |  print(2a)
        |  print(2 + a)
        |  print(a - 1)
      """.stripMargin,
      "[2.0, 4.0, 6.0]\n[2.0, 4.0, 6.0]\n[3.0, 4.0, 5.0]\n[0.0, 1.0, 2.0]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "broadcast scalar-int < real-array promotes scalar before fcmp",
      // Same fix path as the arithmetic-broadcast regression above —
      // comparisons go through `emitScalarBinOp` with elem=TyReal as
      // the op type, so the integer scalar needs sitofp first.
      """
        |def main() =
        |  val a = [1.0, 2.0, 3.0]
        |  print(a < 2)
        |  print(2 < a)
      """.stripMargin,
      "[true, false, false]\n[false, false, true]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "elementwise [int] + [real] promotes both elements to real",
      // Regression: when the two operand arrays have different element
      // types, the codegen loaded each element at its native LLVM type
      // (i64 vs double) and fed both to `add i64 ...`, mismatching the
      // result element type. `emitElementWise` now promotes both
      // loaded elements up to the result-element type before the op.
      """
        |def main() =
        |  val a = [1, 2, 3]
        |  val b = [10.0, 20.0, 30.0]
        |  print(a + b)
        |  print(b + a)
      """.stripMargin,
      "[11.0, 22.0, 33.0]\n[11.0, 22.0, 33.0]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "elementwise [complex] + - * / via componentwise lowering",
      // Regression: `emitScalarBinOp` used to route TyComplex through
      // the generic `binOpInst` path and emit `add { double, double }`
      // (invalid IR; clang rejects). Element-wise + broadcast over
      // `[complex]` now route through `emitComplexArith`, the same
      // helper the scalar TBinOp path already uses.
      """
        |def main() =
        |  val a: [complex] = [1.0 + 2i, 3.0 + 4i]
        |  val b: [complex] = [10.0 + 20i, 30.0 + 40i]
        |  print(a + b)
        |  print(a - b)
        |  print(a * b)
        |  print(b / a)
      """.stripMargin,
      "[11.0+22.0i, 33.0+44.0i]\n[-9.0-18.0i, -27.0-36.0i]\n[-30.0+40.0i, -70.0+240.0i]\n[10.0+0.0i, 10.0+0.0i]\n",
    ),
    Case(
      "arrays",
      "elementwise [complex] == / != via componentwise compare + and/or",
      // Same fix path for comparisons: complex equality folds
      // `(re == re) and (im == im)`, `!=` folds the unordered-or-
      // not-equal variant via `or`. Result is `[bool]`.
      """
        |def main() =
        |  val a: [complex] = [1.0 + 2i, 3.0 + 4i]
        |  val b: [complex] = [1.0 + 2i, 0.0 + 0i]
        |  print(a == b)
        |  print(a != b)
      """.stripMargin,
      "[true, false]\n[false, true]\n",
    ),
    Case(
      "arrays",
      "scalar-complex * [complex] broadcast",
      // Broadcast routes through the same `emitScalarBinOp` so it gets
      // the same complex fix. Real and integer scalars promote up the
      // numeric lattice to complex inside the loop body via
      // `liftScalarTo` (broadcast already does this for arithmetic).
      """
        |def main() =
        |  val a: [complex] = [1.0 + 2i, 3.0 + 4i]
        |  print((1.0 + 1i) * a)
        |  print(2.0 * a)
        |  print(a + (10.0 + 0i))
      """.stripMargin,
      "[-1.0+3.0i, -1.0+7.0i]\n[2.0+4.0i, 6.0+8.0i]\n[11.0+2.0i, 13.0+4.0i]\n",
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "arrays",
      "dot product via @ on rank-1 / rank-1",
      """
        |def main() = print([1, 2, 3] @ [4, 5, 6])
      """.stripMargin,
      "32\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "prelude",
      "sqrt of negative real returns NaN (spec §10.2)",
      """def main() = print(sqrt(-4.0))""",
      "nan\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "prelude",
      "linspace",
      """def main() = print(linspace(0.0, 1.0, 5))""",
      "[0.0, 0.25, 0.5, 0.75, 1.0]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "zeros + ones",
      """def main() =
        |  print(zeros(3))
        |  print(ones(3))
      """.stripMargin,
      "[0, 0, 0]\n[1, 1, 1]\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
    Case(
      "string interpolation",
      "real value via s-interpolation returned from a function",
      // Exercises the value-position interpolation path: the s"..."
      // string is built and returned, then printed by the caller. Goes
      // through `__nex_str_from_double`, which uses the same
      // shortest-round-trip + Java post-processing as `print(x)`.
      """
        |def fmt(x: real): string = s"x=${x}"
        |def main() =
        |  print(fmt(1.4142135623730951))
        |  print(fmt(1.0))
        |  print(fmt(0.0))
        |  print(fmt(-3.5))
        |  print(fmt(1.0e-20))
        |  print(fmt(1.0e20))
      """.stripMargin,
      "x=1.4142135623730951\nx=1.0\nx=0.0\nx=-3.5\nx=1.0E-20\nx=1.0E20\n",
    ),
    Case(
      "string interpolation",
      "real interpolation handles nan / inf / -inf via the value-position path",
      """
        |def fmt(x: real): string = s"x=${x}"
        |def main() =
        |  print(fmt(nan))
        |  print(fmt(inf))
        |  print(fmt(-inf))
      """.stripMargin,
      "x=nan\nx=inf\nx=-inf\n",
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

    // ========================================================================
    // docs/examples — programs lifted from docs/content/examples/. Each one
    // is the body of the corresponding `def main()`-bearing snippet in a
    // doc page, with output measured at the time the doc was last verified.
    // Adding them here gives byte-exact interpreter/AOT parity coverage on
    // every commit so the published docs cannot silently drift.
    // ========================================================================

    Case(
      "docs/examples",
      "01-hello: hypotenuse via sqrt and ^",
      """
        |def hypotenuse(a: real, b: real) = sqrt(a^2 + b^2)
        |
        |def main() =
        |  print(s"hypotenuse(3, 4) = ${hypotenuse(3.0, 4.0)}")
      """.stripMargin,
      "hypotenuse(3, 4) = 5.0\n",
    ),
    Case(
      "docs/examples",
      "02-arrays: element-wise fusion + Point distance",
      """
        |struct Point
        |  x: real
        |  y: real
        |end
        |
        |def distance(p1: Point, p2: Point) =
        |  val dx = p1.x - p2.x
        |  val dy = p1.y - p2.y
        |  sqrt(dx^2 + dy^2)
        |
        |def main() =
        |  val a = [1.0, 2.0, 3.0, 4.0, 5.0]
        |  val b = [10.0, 20.0, 30.0, 40.0, 50.0]
        |  val result = 2a + b - 1.0
        |  print(result)
        |
        |  val origin = Point(0.0, 0.0)
        |  val p = Point(3.0, 4.0)
        |  print(distance(origin, p))
      """.stripMargin,
      "[11.0, 23.0, 35.0, 47.0, 59.0]\n5.0\n",
    ),
    Case(
      "docs/examples",
      "03-higher-order: map captures mean, fuses into sum",
      """
        |def main() =
        |  val xs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
        |  val n = to_real(length(xs))
        |  val mean = sum(xs) / n
        |  val variance: real = xs.map(x -> (x - mean)^2).sum() / n
        |  val stddev = sqrt(variance)
        |  print(s"mean = $mean, stddev = $stddev")
      """.stripMargin,
      "mean = 5.5, stddev = 2.8722813232690143\n",
    ),
    Case(
      "docs/examples",
      "04-complex: complex arithmetic + .abs / .re / .im",
      """
        |def main() =
        |  val z1 = 1.0 + 2i
        |  val z2 = 3.0 - 1i
        |  print(s"z1 + z2 = ${z1 + z2}")
        |  print(s"z1 * z2 = ${z1 * z2}")
        |  print(s"|z1| = ${z1.abs()}")
        |  print(s"z1.re = ${z1.re}, z1.im = ${z1.im}")
      """.stripMargin,
      "z1 + z2 = 4.0+1.0i\nz1 * z2 = 5.0+5.0i\n|z1| = 2.23606797749979\nz1.re = 1.0, z1.im = 2.0\n",
    ),
    Case(
      "docs/examples",
      "06-numerical-methods: Newton's method for sqrt",
      """
        |def newton_sqrt(x: real, tol: real) =
        |  var guess = x / 2.0
        |  var delta = guess
        |  while abs(delta) > tol do
        |    val next = (guess + x / guess) / 2.0
        |    delta = next - guess
        |    guess = next
        |  end while
        |  guess
        |
        |def main() =
        |  print(newton_sqrt(2.0,    1.0e-12))
        |  print(newton_sqrt(1000.0, 1.0e-9))
      """.stripMargin,
      "1.414213562373095\n31.622776601683793\n",
    ),
    Case(
      "docs/examples",
      "06-numerical-methods: RK4 one step over a harmonic oscillator (100 steps)",
      """
        |def rk4_step(f: (real, [real]) -> [real], t: real, y: [real], h: real) =
        |  val k1 = f(t,       y)
        |  val k2 = f(t + h/2, y + h/2 * k1)
        |  val k3 = f(t + h/2, y + h/2 * k2)
        |  val k4 = f(t + h,   y + h   * k3)
        |  y + h/6 * (k1 + 2k2 + 2k3 + k4)
        |
        |def harmonic(t: real, y: [real]) =
        |  [y[1], -y[0]]
        |
        |def main() =
        |  var y = [1.0, 0.0]
        |  var t = 0.0
        |  val h = 0.01
        |  val steps = 100
        |
        |  for k in 0..steps do
        |    y = rk4_step(harmonic, t, y, h)
        |    t = t + h
        |  end for
        |
        |  print(s"position=${y[0]}, velocity=${y[1]}")
      """.stripMargin,
      "position=0.5403023059378852, velocity=-0.8414709847622888\n",
    ),
    Case(
      "docs/examples",
      "07-styles: three idioms — fused, explicit-loop, slice assignment",
      """
        |def normalize(v: [real]) =
        |  val mag = sqrt(sum(v * v))
        |  if mag == 0.0 then v
        |  else v / mag
        |
        |def normalize_loop(v: [real]) =
        |  val n = length(v)
        |  val mag = sqrt(sum(v * v))
        |  if mag == 0.0 then v
        |  else
        |    var out = fill(n, 0.0)
        |    for i in 0..n do
        |      out[i] = v[i] / mag
        |    end for
        |    out
        |
        |def main() =
        |  val a = [3.0, 4.0]
        |  val b = normalize(a)
        |  print(b)
        |
        |  val c = normalize_loop([3.0, 4.0])
        |  print(c)
        |
        |  var d = [9.0, 9.0, 9.0, 9.0]
        |  d[0..2] = [0.6, 0.8]
        |  print(d)
      """.stripMargin,
      "[0.6, 0.8]\n[0.6, 0.8]\n[0.6, 0.8, 9.0, 9.0]\n",
    ),
    Case(
      "docs/examples",
      "08-mandelbrot: per-row slice assignment of escape-time vector",
      """
        |def escape_iters(c: complex, max_iter: integer) =
        |  var z = 0i
        |  var k = 0
        |  while k < max_iter and z.abs() <= 2.0 do
        |    z = z*z + c
        |    k = k + 1
        |  end while
        |  k
        |
        |def row_at(py: integer, width: integer, height: integer, max_iter: integer) =
        |  var row = fill(width, 0)
        |  for px in 0..width do
        |    val x = -2.0 + 3.0 * to_real(px) / to_real(width)
        |    val y = -1.5 + 3.0 * to_real(py) / to_real(height)
        |    row[px] = escape_iters(x + y*i, max_iter)
        |  end for
        |  row
        |
        |def main() =
        |  var result = fill((3, 8), 0)
        |  for py in 0..3 do
        |    result[py, :] = row_at(py, 8, 3, 20)
        |  end for
        |  for py in 0..3 do
        |    print(max(result[py]))
        |  end for
      """.stripMargin,
      "2\n20\n20\n",
    ),
    Case(
      "docs/examples",
      "09-sinusoids: three-term wave at four sample points",
      """
        |def wave(t: real) =
        |  3sin(2pi*t) + 2cos(4pi*t) - sin(6pi*t)
        |
        |def main() =
        |  print(s"wave(0.0)  = ${wave(0.0)}")
        |  print(s"wave(0.25) = ${wave(0.25)}")
        |  print(s"wave(0.5)  = ${wave(0.5)}")
        |  print(s"wave(0.75) = ${wave(0.75)}")
      """.stripMargin,
      "wave(0.0)  = 2.0\nwave(0.25) = 2.0\nwave(0.5)  = 2.0\nwave(0.75) = -6.0\n",
    ),
    Case(
      "docs/examples",
      "10-power-iteration: dominant eigenvalue / eigenvector via @ and dot",
      """
        |def power_iteration(A: [[real]], iters: integer) =
        |  val n = rows(A)
        |  var v = fill(n, 1.0)
        |  var lambda = 0.0
        |
        |  for k in 0..iters do
        |    val Av = A @ v
        |    val norm = sqrt(dot(Av, Av))
        |    v = Av / norm
        |    lambda = dot(v, A @ v)
        |  end for
        |
        |  (lambda, v)
        |
        |def main() =
        |  val A = [[3.0, 2.0],
        |           [2.0, 3.0]]
        |
        |  val lambda, v = power_iteration(A, 50)
        |
        |  print(s"dominant eigenvalue ~ $lambda")
        |  print(s"corresponding eigenvector ~ $v")
      """.stripMargin,
      "dominant eigenvalue ~ 4.999999999999999\ncorresponding eigenvector ~ [0.7071067811865475, 0.7071067811865475]\n",
    ),
    Case(
      "docs/examples",
      "12-fft: recursive radix-2 FFT on a 4-point step input",
      """
        |def fft(x: [complex]): [complex] =
        |  val n = length(x)
        |  if n == 1 then return x
        |  val half = n div 2
        |  var even = fill(half, 0.0 + 0i)
        |  var odd  = fill(half, 0.0 + 0i)
        |  for k in 0..half do
        |    even[k] = x[2 * k]
        |    odd[k]  = x[2 * k + 1]
        |  val ef = fft(even)
        |  val of = fft(odd)
        |  var t = fill(half, 0.0 + 0i)
        |  for k in 0..half do
        |    val angle = -2.0 * pi * to_real(k) / to_real(n)
        |    t[k] = (cos(angle) + sin(angle) * i) * of[k]
        |  var y = fill(n, 0.0 + 0i)
        |  y[0..half] = ef + t
        |  y[half..n] = ef - t
        |  y
        |
        |def main() =
        |  val x: [complex] = [1.0, 1.0, 0.0, 0.0]
        |  val y = fft(x)
        |  for k in 0..length(y) do
        |    print(y[k])
      """.stripMargin,
      "2.0+0.0i\n1.0-1.0i\n0.0+0.0i\n0.9999999999999999+1.0i\n",
    ),

    // ========================================================================
    // sum types (enums)
    // ========================================================================

    Case(
      "sum types (enums)",
      "declare and print bare variants",
      """
        |enum Color =
        |  Red
        |  Green
        |  Blue
        |
        |def main() =
        |  print(Red)
        |  print(Green)
        |  print(Blue)
      """.stripMargin,
      "Red\nGreen\nBlue\n",
    ),
    Case(
      "sum types (enums)",
      "construct and print fielded variants",
      """
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |  MaxIters(iters: integer, last: real)
        |
        |def main() =
        |  print(Converged(3.14))
        |  print(Diverged)
        |  print(MaxIters(100, 0.5))
      """.stripMargin,
      "Converged(3.14)\nDiverged\nMaxIters(100, 0.5)\n",
    ),
    Case(
      "sum types (enums)",
      "use the enum name as a binding's type annotation",
      """
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |
        |def main() =
        |  val a: Solver = Converged(2.5)
        |  val b: Solver = Diverged
        |  print(a)
        |  print(b)
      """.stripMargin,
      "Converged(2.5)\nDiverged\n",
    ),
    Case(
      "sum types (enums)",
      "pass an enum value through a function and return it",
      """
        |enum Step =
        |  Continue(n: integer)
        |  Done
        |
        |def advance(s: Step): Step = s
        |
        |def main() =
        |  print(advance(Continue(7)))
        |  print(advance(Done))
      """.stripMargin,
      "Continue(7)\nDone\n",
    ),
    Case(
      "sum types (enums)",
      "match dispatches on a bare variant",
      """
        |enum Color =
        |  Red
        |  Green
        |  Blue
        |
        |def label(c: Color): string =
        |  c match
        |    Red   -> "stop"
        |    Green -> "go"
        |    Blue  -> "wait"
        |
        |def main() =
        |  print(label(Red))
        |  print(label(Green))
        |  print(label(Blue))
      """.stripMargin,
      "stop\ngo\nwait\n",
    ),
    Case(
      "sum types (enums)",
      "match binds variant fields into the arm body",
      """
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |  MaxIters(iters: integer, last: real)
        |
        |def describe(s: Solver): real =
        |  s match
        |    Converged(x)       -> x
        |    Diverged           -> -1.0
        |    MaxIters(n, last)  -> last
        |
        |def main() =
        |  print(describe(Converged(3.14)))
        |  print(describe(Diverged))
        |  print(describe(MaxIters(100, 2.5)))
      """.stripMargin,
      "3.14\n-1.0\n2.5\n",
    ),
    Case(
      "sum types (enums)",
      "match wildcard catches the remaining variants",
      """
        |enum Color =
        |  Red
        |  Green
        |  Blue
        |
        |def isRed(c: Color): bool =
        |  c match
        |    Red -> true
        |    _   -> false
        |
        |def main() =
        |  print(isRed(Red))
        |  print(isRed(Green))
        |  print(isRed(Blue))
      """.stripMargin,
      "true\nfalse\nfalse\n",
    ),
    Case(
      "sum types (enums)",
      "match ignores unused fields with `_`",
      """
        |enum Step =
        |  Continue(n: integer)
        |  Halt
        |
        |def kind(s: Step): string =
        |  s match
        |    Continue(_) -> "continue"
        |    Halt        -> "halt"
        |
        |def main() =
        |  print(kind(Continue(99)))
        |  print(kind(Halt))
      """.stripMargin,
      "continue\nhalt\n",
    ),
    Case(
      "sum types (enums)",
      "match arms unify to a single result type",
      """
        |enum Step =
        |  Continue(n: integer)
        |  Done(total: integer)
        |
        |def main() =
        |  val s: Step = Continue(7)
        |  val v: integer = s match
        |    Continue(n) -> n
        |    Done(t)     -> t
        |  print(v)
      """.stripMargin,
      "7\n",
    ),
    Case(
      "sum types (enums)",
      "match arm uses a variant field of type string",
      """
        |enum Cmd =
        |  Echo(msg: string)
        |  Quit
        |
        |def main() =
        |  val c: Cmd = Echo("hello")
        |  c match
        |    Echo(s) -> print(s)
        |    Quit    -> print("bye")
      """.stripMargin,
      "hello\n",
    ),
    Case(
      "sum types (enums)",
      "match arm uses a bool field",
      """
        |enum Flag =
        |  Set(v: bool)
        |  Unset
        |
        |def show(f: Flag): bool =
        |  f match
        |    Set(v) -> v
        |    Unset  -> false
        |
        |def main() =
        |  print(show(Set(true)))
        |  print(show(Set(false)))
        |  print(show(Unset))
      """.stripMargin,
      "true\nfalse\nfalse\n",
    ),
    Case(
      "sum types (enums)",
      "optional `end match` trailer closes the block",
      """
        |enum Color =
        |  Red
        |  Green
        |  Blue
        |
        |def label(c: Color): string =
        |  c match
        |    Red   -> "stop"
        |    Green -> "go"
        |    Blue  -> "wait"
        |  end match
        |
        |def main() = print(label(Green))
      """.stripMargin,
      "go\n",
    ),
    Case(
      "sum types (enums)",
      "interpolated print of a fielded variant in s\"...\"",
      """
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |
        |def main() =
        |  val s = Converged(1.25)
        |  print(s"result=${s}")
      """.stripMargin,
      "result=Converged(1.25)\n",
    ),
    Case(
      "docs/examples",
      "13-sum-types: Newton-solver describe() with field interpolation",
      """
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |  MaxIters(iters: integer, last: real)
        |
        |def newton(f: real -> real, fp: real -> real, x0: real, tol: real, max_iters: integer): Solver =
        |  var x = x0
        |  for i in 0..max_iters do
        |    val fx = f(x)
        |    if abs(fx) < tol then return Converged(x)
        |    val d = fp(x)
        |    if abs(d) < 1.0e-15 then return Diverged
        |    x = x - fx / d
        |  end for
        |  MaxIters(max_iters, x)
        |
        |def f(x: real) : real = x^2 - 2.0
        |def fp(x: real): real = 2.0 * x
        |
        |def describe(s: Solver): string =
        |  s match
        |    Converged(x)         -> s"converged at x=${x}"
        |    Diverged             -> "diverged"
        |    MaxIters(iters, x)   -> s"ran ${iters} iters, last x=${x}"
        |
        |def main() =
        |  print(describe(newton(f, fp, 1.0,    1.0e-12, 50)))
        |  print(describe(newton(f, fp, 0.0,    1.0e-12, 50)))
        |  print(describe(newton(f, fp, 1.0e9,  1.0e-12, 3)))
      """.stripMargin,
      "converged at x=1.4142135623730951\ndiverged\nran 3 iters, last x=125000000.0\n",
    ),
  )
