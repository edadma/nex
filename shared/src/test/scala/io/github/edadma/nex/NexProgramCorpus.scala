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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "control flow",
      "inclusive range at value position",
      """
        |def main() = print(1..=3)
      """.stripMargin,
      "[1, 2, 3]\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "control flow",
      "rank-2 two-integer index returns a scalar element",
      """
        |def main() =
        |  val m = [[10, 20, 30], [40, 50, 60]]
        |  print(m[0, 0])
        |  print(m[0, 2])
        |  print(m[1, 1])
        |  print(m[1, 2])
      """.stripMargin,
      "10\n30\n50\n60\n",
      mlir = true,
    ),
    Case(
      "control flow",
      "rank-2 indexing through a computed integer expression",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  val i = 1
        |  val j = 1 + 1
        |  print(m[i, j])
      """.stripMargin,
      "6\n",
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "functions",
      "tensor-typed param + scalar return",
      """
        |def total(xs: [integer]): integer = sum(xs)
        |
        |def main() =
        |  val v = [1, 2, 3, 4, 5]
        |  print(total(v))
      """.stripMargin,
      "15\n",
      mlir = true,
    ),
    Case(
      "functions",
      "rank-1 tensor param with prelude reductions",
      """
        |def stats(xs: [real]): real = sqrt(sum(xs)) * 2.0
        |
        |def main() =
        |  print(stats([4.0, 5.0, 7.0]))
      """.stripMargin,
      "8.0\n",
      mlir = true,
    ),
    Case(
      "functions",
      "rank-2 tensor param + integer return via length",
      """
        |def rows_count(m: [[integer]]): integer = length(m)
        |
        |def main() =
        |  print(rows_count([[1, 2, 3], [4, 5, 6]]))
      """.stripMargin,
      "2\n",
      mlir = true,
    ),
    Case(
      "functions",
      "def returning a tensor (rank-1 doubled via map)",
      """
        |def doubled(xs: [integer]): [integer] = map(xs, x -> x * 2)
        |
        |def main() =
        |  print(doubled([1, 2, 3]))
      """.stripMargin,
      "[2, 4, 6]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "tensor + scalar params, tensor return (broadcast scale)",
      """
        |def scale_by(v: [real], k: real): [real] = v * k
        |
        |def main() =
        |  print(scale_by([1.0, 2.0, 3.0], 3.5))
      """.stripMargin,
      "[3.5, 7.0, 10.5]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "two tensor params, element-wise sum returned",
      """
        |def add_arrays(a: [integer], b: [integer]): [integer] = a + b
        |
        |def main() =
        |  print(add_arrays([1, 2, 3], [10, 20, 30]))
      """.stripMargin,
      "[11, 22, 33]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "tensor-returning if-expression branches yield same tensor type",
      """
        |def pick(flag: bool, a: [integer], b: [integer]): [integer] =
        |  if flag then a else b
        |
        |def main() =
        |  print(pick(true, [1, 2, 3], [4, 5, 6]))
        |  print(pick(false, [1, 2, 3], [4, 5, 6]))
      """.stripMargin,
      "[1, 2, 3]\n[4, 5, 6]\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "functions",
      "filter prints the selected elements directly",
      """
        |def main() =
        |  val xs = [1, 2, 3, 4, 5, 6]
        |  print(filter(xs, x -> x > 3))
      """.stripMargin,
      "[4, 5, 6]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "filter where no element matches returns an empty array",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(filter(xs, x -> x > 100))
      """.stripMargin,
      "[]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "filter where every element matches returns a full-length copy",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(filter(xs, x -> x > 0))
      """.stripMargin,
      "[1, 2, 3]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "filter on a dynamic-bound range",
      """
        |def main() =
        |  val n = 10
        |  print(filter(range(0, n), x -> x % 3 == 0))
      """.stripMargin,
      "[0, 3, 6, 9]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "filter on real values picks NaN-safe predicate",
      """
        |def main() =
        |  val xs = [1.0, 2.5, 3.5, 4.5]
        |  print(filter(xs, x -> x > 2.0))
      """.stripMargin,
      "[2.5, 3.5, 4.5]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "length of filter result",
      """
        |def main() =
        |  val xs = [10, 20, 30, 40, 50]
        |  print(length(filter(xs, x -> x >= 30)))
      """.stripMargin,
      "3\n",
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "functions",
      "flatMap via method-call sugar",
      """
        |def main() =
        |  print([1, 2, 3].flatMap(x -> [x, -x]))
      """.stripMargin,
      "[1, -1, 2, -2, 3, -3]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "flatMap with single-element inner arrays acts like map",
      """
        |def main() =
        |  print([1, 2, 3].flatMap(x -> [x * x]))
      """.stripMargin,
      "[1, 4, 9]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "flatMap on a dynamic-bound range with two-element inner arrays",
      """
        |def main() =
        |  val n = 4
        |  print(flatMap(range(1, n), x -> [x, x + 10]))
      """.stripMargin,
      "[1, 11, 2, 12, 3, 13]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "flatMap where inner length itself depends on the element (variable inner length)",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(flatMap(xs, x -> range(0, x)))
      """.stripMargin,
      "[0, 0, 1, 0, 1, 2]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "length of flatMap result",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(length(flatMap(xs, x -> [x, x + 1, x + 2])))
      """.stripMargin,
      "9\n",
      mlir = true,
    ),
    Case(
      "functions",
      "flatMap on real elements promotes through inner arrays",
      """
        |def main() =
        |  val xs = [1.0, 2.0, 3.0]
        |  print(flatMap(xs, x -> [x, x * 2.0]))
      """.stripMargin,
      "[1.0, 2.0, 2.0, 4.0, 3.0, 6.0]\n",
      mlir = true,
    ),
    Case(
      "functions",
      "sum of flatMap result",
      """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(sum(flatMap(xs, x -> [x, x])))
      """.stripMargin,
      "20\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      "negative bounds on rank-1 slice (`a[-3..length(a)]`, `a[0..-1]`)",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  print(a[-3..length(a)])
        |  print(a[0..-1])
        |  print(a[-3..-1])
        |  print(a[0..=-1])
      """.stripMargin,
      "[30, 40, 50]\n[10, 20, 30, 40]\n[30, 40]\n[10, 20, 30, 40, 50]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "negative bounds on rank-2 slice axes",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  print(m[-1, :])
        |  print(m[:, -1])
        |  print(m[-2..length(m), -2..length(m)])
      """.stripMargin,
      "[7, 8, 9]\n[3, 6, 9]\n[[5, 6], [8, 9]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "negative bounds in slice-assign target",
      """
        |def main() =
        |  var v = [100, 200, 300, 400, 500]
        |  v[-3..length(v)] = [33, 44, 55]
        |  print(v)
      """.stripMargin,
      "[100, 200, 33, 44, 55]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "over-negative slice bound traps",
      """
        |def main() =
        |  val a = [10, 20, 30]
        |  assert_traps(() -> print(a[-10..2]), "out of bounds")
        |  assert_traps(() -> print(a[0..-10]), "out of bounds")
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "arrays",
      "open-ended slice bounds (`a[lo..]`, `a[..hi]`, `a[..]`)",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  print(a[2..])
        |  print(a[..3])
        |  print(a[..])
        |  print(a[..=2])
      """.stripMargin,
      "[30, 40, 50]\n[10, 20, 30]\n[10, 20, 30, 40, 50]\n[10, 20, 30]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "open-ended slice bounds combine with negative indices",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  print(a[-3..])
        |  print(a[..-1])
        |  print(a[..=-1])
      """.stripMargin,
      "[30, 40, 50]\n[10, 20, 30, 40]\n[10, 20, 30, 40, 50]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "open-ended axis bounds on rank-2 slices",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  print(m[1.., :])
        |  print(m[1.., 1..])
        |  print(m[..2, ..2])
      """.stripMargin,
      "[[4, 5, 6], [7, 8, 9]]\n[[5, 6], [8, 9]]\n[[1, 2], [4, 5]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "open-ended slice-assign target (`v[-2..] = rhs`)",
      """
        |def main() =
        |  var v = [100, 200, 300, 400, 500]
        |  v[-2..] = [44, 55]
        |  v[..2] = [11, 22]
        |  print(v)
      """.stripMargin,
      "[11, 22, 300, 44, 55]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "strided slice with closed bounds (`a[lo..hi by k]`)",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
        |  print(a[0..10 by 2])
        |  print(a[1..10 by 3])
        |  print(a[0..=9 by 2])
      """.stripMargin,
      "[10, 30, 50, 70, 90]\n[20, 50, 80]\n[10, 30, 50, 70, 90]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "strided slice combined with open bounds and negative indices",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
        |  print(a[..10 by 2])
        |  print(a[2.. by 2])
        |  print(a[.. by 5])
        |  print(a[-5.. by 2])
        |  print(a[..-1 by 3])
      """.stripMargin,
      "[10, 30, 50, 70, 90]\n[30, 50, 70, 90]\n[10, 60]\n[60, 80, 100]\n[10, 40, 70]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "strided slice edge cases (empty window, stride > span)",
      """
        |def main() =
        |  val a = [10, 20, 30]
        |  print(a[0..0 by 1])
        |  print(a[0..1 by 5])
      """.stripMargin,
      "[]\n[10]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "non-positive stride traps",
      """
        |def main() =
        |  val a = [10, 20, 30]
        |  assert_traps(() -> print(a[0..3 by 0]),  "slice")
        |  assert_traps(() -> print(a[0..3 by -1]), "slice")
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "arrays",
      "strided slice-assign target (`a[lo..hi by k] = rhs`)",
      """
        |def main() =
        |  var a = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        |  a[0..10 by 2] = [1, 1, 1, 1, 1]
        |  print(a)
        |  var b = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
        |  b[1..10 by 3] = [200, 500, 800]
        |  print(b)
      """.stripMargin,
      "[1, 0, 1, 0, 1, 0, 1, 0, 1, 0]\n[10, 200, 30, 40, 500, 60, 70, 800, 90, 100]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "strided slice-assign combines with open bounds and inclusive",
      """
        |def main() =
        |  var c = [10, 20, 30, 40, 50]
        |  c[..5 by 2] = [99, 99, 99]
        |  c[1.. by 2] = [42, 42]
        |  print(c)
        |  var d = [10, 20, 30, 40, 50]
        |  d[0..=4 by 2] = [77, 77, 77]
        |  print(d)
      """.stripMargin,
      "[99, 42, 99, 42, 99]\n[77, 20, 77, 40, 77]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "strided slice-assign length mismatch traps",
      """
        |def assign5by2(v: mut [integer]) = v[0..5 by 2] = [1, 2]
        |def main() =
        |  var v = [10, 20, 30, 40, 50]
        |  assert_traps(() -> assign5by2(v), "length mismatch")
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "arrays",
      "strided slice-assign with non-positive stride traps",
      """
        |def assignBadStride(v: mut [integer]) = v[0..5 by 0] = [1, 2, 3]
        |def main() =
        |  var v = [10, 20, 30, 40, 50]
        |  assert_traps(() -> assignBadStride(v), "slice")
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
      mlir = true,
    ),
    Case(
      "arrays",
      "map with inline lambda printed directly",
      """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(map(xs, x -> x * 10))
      """.stripMargin,
      "[10, 20, 30, 40]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "method-call sugar `xs.map(...)` printed directly",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(xs.map(x -> x + 100))
      """.stripMargin,
      "[101, 102, 103]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "map producing real output from int input",
      """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(xs.map(x -> x * 1.5))
      """.stripMargin,
      "[1.5, 3.0, 4.5]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "reduce with inline lambda computes a fold",
      """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(reduce(xs, 0, (acc, x) -> acc + x * x))
      """.stripMargin,
      "30\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "reduce with inline lambda computes product as fold",
      """
        |def main() =
        |  val xs = [2, 3, 4]
        |  print(reduce(xs, 1, (acc, x) -> acc * x))
      """.stripMargin,
      "24\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "map over a rank-2 array preserves rank",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  print(map(m, x -> x * 10))
      """.stripMargin,
      "[[10, 20], [30, 40]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "method-call sugar `m.map(...)` on rank-2 preserves rank",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m.map(x -> x + 100))
      """.stripMargin,
      "[[101, 102, 103], [104, 105, 106]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-2 map producing real output from int input",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  print(m.map(x -> x * 1.5))
      """.stripMargin,
      "[[1.5, 3.0], [4.5, 6.0]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "reduce over a rank-2 array folds left-to-right over all elements",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  print(reduce(m, 0, (acc, x) -> acc + x))
      """.stripMargin,
      "10\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-2 reduce computes sum-of-squares via inline lambda",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  print(reduce(m, 0, (acc, x) -> acc + x * x))
      """.stripMargin,
      "30\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "arrays",
      "sum_axis(m, 0) collapses rows producing the column totals",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(sum_axis(m, 0))
      """.stripMargin,
      "[5, 7, 9]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "sum_axis(m, 1) collapses columns producing the row totals",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(sum_axis(m, 1))
      """.stripMargin,
      "[6, 15]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "product of an integer array",
      """def main() = print(product([2, 3, 4]))""",
      "24\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "product of a real array",
      """def main() = print(product([1.5, 2.0, 0.5]))""",
      "1.5\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "diag(arr) builds a square matrix with arr on the diagonal",
      """
        |def main() =
        |  print(diag([1, 2, 3]))
      """.stripMargin,
      "[[1, 0, 0], [0, 2, 0], [0, 0, 3]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "identity(n) is the n×n integer identity matrix",
      """
        |def main() =
        |  print(identity(3))
      """.stripMargin,
      "[[1, 0, 0], [0, 1, 0], [0, 0, 1]]\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-1 slice with runtime inclusive bound",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  val lo = 1
        |  val hi = 3
        |  print(a[lo..=hi])
      """.stripMargin,
      "[20, 30, 40]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-1 leading-open slice with runtime hi",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  val hi = 3
        |  print(a[..hi])
      """.stripMargin,
      "[10, 20, 30]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-1 trailing-open slice with runtime lo",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  val lo = 2
        |  print(a[lo..])
      """.stripMargin,
      "[30, 40, 50]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-1 fully-open slice copies the array",
      """
        |def main() =
        |  val a = [10, 20, 30]
        |  print(a[..])
      """.stripMargin,
      "[10, 20, 30]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-1 slice with runtime stride",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50, 60]
        |  val k = 2
        |  print(a[0..6 by k])
      """.stripMargin,
      "[10, 30, 50]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-1 slice with runtime lo + hi + stride",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50, 60, 70]
        |  val lo = 1
        |  val hi = 7
        |  val k = 3
        |  print(a[lo..hi by k])
      """.stripMargin,
      "[20, 50]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-1 slice on a dynamic-bound range",
      """
        |def main() =
        |  val n = 8
        |  print(range(0, n)[2..6])
      """.stripMargin,
      "[2, 3, 4, 5]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "length of dynamic slice",
      """
        |def main() =
        |  val a = [1, 2, 3, 4, 5]
        |  val lo = 1
        |  val hi = 4
        |  print(length(a[lo..hi]))
      """.stripMargin,
      "3\n",
      mlir = true,
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
      mlir = true,
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
      "rank-2 slice with runtime row range",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  val lo = 1
        |  val hi = 3
        |  print(m[lo..hi, :])
      """.stripMargin,
      "[[4, 5, 6], [7, 8, 9]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-2 slice with runtime column range",
      """
        |def main() =
        |  val m = [[1, 2, 3, 4], [5, 6, 7, 8]]
        |  val lo = 1
        |  val hi = 3
        |  print(m[:, lo..hi])
      """.stripMargin,
      "[[2, 3], [6, 7]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-2 slice with runtime ranges on both axes",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  val r0 = 1
        |  val r1 = 3
        |  val c0 = 0
        |  val c1 = 2
        |  print(m[r0..r1, c0..c1])
      """.stripMargin,
      "[[4, 5], [7, 8]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-2 slice with runtime row index drops a dimension",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  val i = 1
        |  print(m[i, :])
      """.stripMargin,
      "[4, 5, 6]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-2 slice with runtime column index drops a dimension",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  val j = 2
        |  print(m[:, j])
      """.stripMargin,
      "[3, 6]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-2 leading-open row slice with runtime hi",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4], [5, 6], [7, 8]]
        |  val hi = 2
        |  print(m[..hi, :])
      """.stripMargin,
      "[[1, 2], [3, 4]]\n",
      mlir = true,
    ),
    Case(
      "arrays",
      "rank-2 trailing-open column slice with runtime lo",
      """
        |def main() =
        |  val m = [[1, 2, 3, 4], [5, 6, 7, 8]]
        |  val lo = 2
        |  print(m[:, lo..])
      """.stripMargin,
      "[[3, 4], [7, 8]]\n",
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      "assert_approx on complex (component-wise distance)",
      """
        |def main() =
        |  assert_approx(1.0 + 2.0i, 1.0 + 2.0i + 1.0e-13 * i, 1.0e-12)
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_approx element-wise over rank-1 real array",
      """
        |def main() =
        |  assert_approx([1.0, 2.0, 3.0], [1.0 + 1.0e-13, 2.0, 3.0 - 1.0e-13], 1.0e-12)
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_approx element-wise over rank-1 complex array",
      """
        |def main() =
        |  assert_approx([1.0 + 0i, 0.0 + 1.0i], [1.0 + 0i, 0.0 + 1.0i], 1.0e-12)
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_approx element-wise over rank-1 integer array (lifted)",
      """
        |def main() =
        |  assert_approx([1, 2, 3], [1, 2, 3], 1.0e-12)
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_approx traps when an array element exceeds the tolerance",
      """
        |def main() =
        |  assert_traps(() -> assert_approx([1.0, 2.0, 3.0], [1.0, 5.0, 3.0], 1.0e-6), "assert_approx")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    ),
    Case(
      "prelude",
      "assert_approx traps on length mismatch",
      """
        |def main() =
        |  assert_traps(() -> assert_approx([1.0, 2.0], [1.0, 2.0, 3.0], 1.0e-6), "assert_approx")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    ),
    Case(
      "prelude",
      "assert_approx traps when the complex distance exceeds the tolerance",
      """
        |def main() =
        |  assert_traps(() -> assert_approx(1.0 + 0i, 1.0 + 5.0i, 1.0e-6), "assert_approx")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    ),
    Case(
      "prelude",
      "assert_approx element-wise over rank-2 real array",
      """
        |def main() =
        |  val a = [[1.0, 2.0], [3.0, 4.0]]
        |  val b = [[1.0 + 1.0e-13, 2.0], [3.0, 4.0 - 1.0e-13]]
        |  assert_approx(a, b, 1.0e-12)
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_approx element-wise over rank-2 integer array (lifted)",
      """
        |def main() =
        |  assert_approx([[1, 2], [3, 4]], [[1, 2], [3, 4]], 1.0e-12)
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_approx element-wise over rank-2 complex array",
      """
        |def main() =
        |  val a = [[1.0 + 0i, 0.0 + 1.0i], [2.0 + 0i, 0.0 + 2.0i]]
        |  assert_approx(a, a, 1.0e-12)
        |  print("ok")
      """.stripMargin,
      "ok\n",
    ),
    Case(
      "prelude",
      "assert_approx traps when a rank-2 element exceeds the tolerance",
      """
        |def main() =
        |  val a = [[1.0, 2.0], [3.0, 4.0]]
        |  val b = [[1.0, 2.0], [3.0, 9.0]]
        |  assert_traps(() -> assert_approx(a, b, 1.0e-6), "assert_approx")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    ),
    Case(
      "prelude",
      "assert_approx traps on rank-2 row-count mismatch",
      """
        |def main() =
        |  assert_traps(() -> assert_approx([[1.0, 2.0], [3.0, 4.0]], [[1.0, 2.0]], 1.0e-6), "assert_approx")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    ),
    Case(
      "prelude",
      "assert_approx traps on rank-2 column-count mismatch",
      """
        |def main() =
        |  assert_traps(() -> assert_approx([[1.0, 2.0]], [[1.0, 2.0, 3.0]], 1.0e-6), "assert_approx")
        |  print("caught")
      """.stripMargin,
      "caught\n",
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "prelude",
      "sqrt(complex) — sqrt(-1 + 0i) = 0 + i",
      """def main() = print(sqrt(-1.0 + 0i))""",
      "0.0+1.0i\n",
      mlir = true,
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
        |  val xs = zeros(3)
        |  print(xs)
        |  print(ones(3))
      """.stripMargin,
      "[0, 0, 0]\n[1, 1, 1]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "range with runtime bounds",
      """def main() =
        |  val lo = 2
        |  val hi = 7
        |  print(range(lo, hi))
      """.stripMargin,
      "[2, 3, 4, 5, 6]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "range with runtime bounds — empty when hi <= lo",
      """def main() =
        |  val lo = 5
        |  val hi = 5
        |  print(range(lo, hi))
      """.stripMargin,
      "[]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "zeros with runtime length",
      """def main() =
        |  val n = 4
        |  print(zeros(n))
      """.stripMargin,
      "[0, 0, 0, 0]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "ones with runtime length",
      """def main() =
        |  val n = 5
        |  print(ones(n))
      """.stripMargin,
      "[1, 1, 1, 1, 1]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "linspace with runtime length",
      """def main() =
        |  val n = 5
        |  print(linspace(0.0, 1.0, n))
      """.stripMargin,
      "[0.0, 0.25, 0.5, 0.75, 1.0]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "linspace with runtime bounds and length",
      """def main() =
        |  val lo = 0.0
        |  val hi = 4.0
        |  val n = 5
        |  print(linspace(lo, hi, n))
      """.stripMargin,
      "[0.0, 1.0, 2.0, 3.0, 4.0]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "length of dynamic-bound range",
      """def main() =
        |  val lo = 1
        |  val hi = 8
        |  print(length(range(lo, hi)))
      """.stripMargin,
      "7\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "sum of dynamic-bound range",
      """def main() =
        |  val n = 5
        |  print(sum(range(0, n)))
      """.stripMargin,
      "10\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "scalar broadcast over dynamic-bound range",
      """def main() =
        |  val n = 4
        |  print(2 * range(0, n))
      """.stripMargin,
      "[0, 2, 4, 6]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "map over dynamic-bound range",
      """def main() =
        |  val n = 5
        |  print(map(range(0, n), x -> x * x))
      """.stripMargin,
      "[0, 1, 4, 9, 16]\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "for over dynamic-bound range — running sum",
      """def main() =
        |  val n = 4
        |  var s = 0
        |  val xs = range(1, n + 1)
        |  for x in xs do s = s + x
        |  print(s)
      """.stripMargin,
      "10\n",
      mlir = true,
    ),
    Case(
      "prelude",
      "comparison broadcast over dynamic-bound range",
      """def main() =
        |  val n = 5
        |  print(range(0, n) < 3)
      """.stripMargin,
      "[true, true, true, false, false]\n",
      mlir = true,
    ),
    Case(
      "string interpolation",
      "f-string without spec behaves like s-string",
      """
        |def main() =
        |  val x = 42
        |  print(f"x = $x")
        |  print(f"sum = ${1 + 2}")
      """.stripMargin,
      "x = 42\nsum = 3\n",
      mlir = true,
    ),
    Case(
      "string interpolation",
      "f-string width / left-align / zero-pad on integers",
      """
        |def main() =
        |  val x = 42
        |  print(f"|$x%5d|")
        |  print(f"|$x%-5d|")
        |  print(f"|$x%05d|")
      """.stripMargin,
      "|   42|\n|42   |\n|00042|\n",
      mlir = true,
    ),
    Case(
      "string interpolation",
      "f-string precision on reals",
      """
        |def main() =
        |  val pi = 3.14159265
        |  print(f"$pi%.3f")
        |  print(f"$pi%.10f")
        |  print(f"$pi%10.3f")
      """.stripMargin,
      "3.142\n3.1415926500\n     3.142\n",
      mlir = true,
    ),
    Case(
      "string interpolation",
      "f-string hex / octal / binary",
      """
        |def main() =
        |  val n = 255
        |  print(f"$n%x")
        |  print(f"$n%X")
        |  print(f"$n%o")
        |  print(f"$n%b")
        |  print(f"$n%016b")
      """.stripMargin,
      "ff\nFF\n377\n11111111\n0000000011111111\n",
      mlir = true,
    ),
    Case(
      "string interpolation",
      "f-string value-position (function returns formatted string)",
      """
        |def render(label: string, x: real): string = f"$label = $x%8.4f"
        |def main() =
        |  print(render("alpha", 3.14159))
        |  print(render("gamma", 12345.678))
      """.stripMargin,
      "alpha =   3.1416\ngamma = 12345.6780\n",
      mlir = true,
    ),
    Case(
      "string interpolation",
      "f-string literal `%` and `$$` escape",
      """
        |def main() =
        |  print(f"100% sure $$dollar")
      """.stripMargin,
      "100% sure $dollar\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "tuple destructuring",
      "bind names at the top level",
      """
        |val a, b = (10, 20)
        |def main() = print(a + b)
      """.stripMargin,
      "30\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "paren-less tuple construction",
      "bind a paren-less 3-tuple at the top level",
      """
        |val a, b, c = 10, 20, 30
        |def main() = print(a + b + c)
      """.stripMargin,
      "60\n",
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "paren-less tuple construction",
      "return a paren-less tuple from a def block body",
      """
        |def split(x: integer) =
        |  val y = x * 2
        |  y, -y
        |def main() =
        |  val a, b = split(3)
        |  print(s"$a $b")
      """.stripMargin,
      "6 -6\n",
      mlir = true,
    ),
    Case(
      "paren-less tuple construction",
      "return a paren-less tuple via the `return` keyword",
      """
        |def first_positive(xs: [real]) =
        |  for x in xs do
        |    if x > 0.0 then return x, true
        |  end for
        |  0.0, false
        |def main() =
        |  val a, b = first_positive([-1.0, 2.0, 3.0])
        |  print(s"$a $b")
      """.stripMargin,
      "2.0 true\n",
    ),
    Case(
      "paren-less tuple construction",
      "assign a paren-less tuple in a block (assignment RHS)",
      """
        |def main() =
        |  var p = 0, 0
        |  p = 7, 8
        |  val a, b = p
        |  print(s"$a $b")
      """.stripMargin,
      "7 8\n",
      mlir = true,
    ),
    Case(
      "paren-less tuple construction",
      "return a paren-less tuple as the last item of an indented if-branch block",
      """
        |def classify(x: integer) =
        |  if x > 0 then
        |    x, "pos"
        |  else
        |    -x, "non-pos"
        |def main() =
        |  val n, label = classify(-5)
        |  print(s"$n $label")
      """.stripMargin,
      "5 non-pos\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
    ),
    Case(
      "string interpolation",
      "embed a real variable in s-interpolation",
      """
        |def main() =
        |  val x = 1.5
        |  print(s"x = $x")
      """.stripMargin,
      "x = 1.5\n",
      mlir = true,
    ),
    Case(
      "string interpolation",
      "embed a whole-valued real (formats as `<int>.0`)",
      """
        |def main() =
        |  val x = 7.0
        |  print(s"x = $x")
      """.stripMargin,
      "x = 7.0\n",
      mlir = true,
    ),
    Case(
      "string interpolation",
      "embed a real in scientific range (Java-style E exponent)",
      """
        |def main() =
        |  val tiny = 1.0e-20
        |  val big  = 1.0e20
        |  print(s"tiny = $tiny, big = $big")
      """.stripMargin,
      "tiny = 1.0E-20, big = 1.0E20\n",
      mlir = true,
    ),
    Case(
      "string interpolation",
      "embed nan / inf / -inf as s-interpolation values",
      """
        |def main() =
        |  val a = nan
        |  val b = inf
        |  val c = -inf
        |  print(s"$a $b $c")
      """.stripMargin,
      "nan inf -inf\n",
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      mlir = true,
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
      "compound assignment",
      "+= on a scalar var",
      """
        |def main() =
        |  var n = 0
        |  n += 5
        |  n += 3
        |  print(n)
      """.stripMargin,
      "8\n",
      mlir = true,
    ),
    Case(
      "compound assignment",
      "-= on a scalar var",
      """
        |def main() =
        |  var n = 100
        |  n -= 1
        |  n -= 9
        |  print(n)
      """.stripMargin,
      "90\n",
      mlir = true,
    ),
    Case(
      "compound assignment",
      "*= on a scalar var",
      """
        |def main() =
        |  var n = 2
        |  n *= 3
        |  n *= 7
        |  print(n)
      """.stripMargin,
      "42\n",
      mlir = true,
    ),
    Case(
      "compound assignment",
      "/= on a real var",
      """
        |def main() =
        |  var x = 8.0
        |  x /= 2.0
        |  x /= 2.0
        |  print(x)
      """.stripMargin,
      "2.0\n",
      mlir = true,
    ),
    Case(
      "compound assignment",
      "%= on an integer var",
      """
        |def main() =
        |  var n = 23
        |  n %= 10
        |  print(n)
      """.stripMargin,
      "3\n",
      mlir = true,
    ),
    Case(
      "compound assignment",
      "+= accumulator inside a for-loop (Fourier-sum idiom)",
      """
        |def main() =
        |  var total = 0
        |  for k in 1..5 do
        |    total += k
        |  end for
        |  print(total)
      """.stripMargin,
      "10\n",
      mlir = true,
    ),
    Case(
      "compound assignment",
      "+= on an array index target",
      """
        |def main() =
        |  var a = [10, 20, 30]
        |  a[1] += 5
        |  print(a[0])
        |  print(a[1])
        |  print(a[2])
      """.stripMargin,
      "10\n25\n30\n",
      mlir = true,
    ),
    Case(
      "compound assignment",
      "+= RHS is the full expression",
      """
        |def main() =
        |  var n = 1
        |  n += 2 * 3 + 4
        |  print(n)
      """.stripMargin,
      "11\n",
      mlir = true,
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

    // ============================================================
    // view-style slicing — non-copying borrows via `a.view(lo..hi)`.
    // Spec §4.14: `a[lo..hi]` keeps copy semantics; `view` is the
    // opt-in non-copying counterpart, with writes through to source.
    // ============================================================
    Case(
      "view-style slicing",
      "view reads the source's elements through the window",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  val v = a.view(1..4)
        |  print(v[0])
        |  print(v[1])
        |  print(v[2])
      """.stripMargin,
      "20\n30\n40\n",
    ),
    Case(
      "view-style slicing",
      "view length reflects the window, not the source",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  print(length(a.view(1..4)))
      """.stripMargin,
      "3\n",
    ),
    Case(
      "view-style slicing",
      "writes through a view update the source array",
      """
        |def main() =
        |  var a = [10, 20, 30, 40, 50]
        |  val v = a.view(1..4)
        |  v[0] = 99
        |  v[2] = 77
        |  print(a)
      """.stripMargin,
      "[10, 99, 30, 77, 50]\n",
    ),
    Case(
      "view-style slicing",
      "sum / map / dot work transparently on a view",
      """
        |def main() =
        |  val a = [1, 2, 3, 4, 5, 6]
        |  val v = a.view(1..5)
        |  print(sum(v))
        |  print(v.map(x -> x * 10))
        |  print(v.dot(v))
      """.stripMargin,
      "14\n[20, 30, 40, 50]\n54\n",
    ),
    Case(
      "view-style slicing",
      "inclusive range form a.view(lo..=hi) includes hi",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  print(a.view(1..=3))
      """.stripMargin,
      "[20, 30, 40]\n",
    ),
    Case(
      "view-style slicing",
      "view-of-view sees the original elements through a narrower window",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  val v = a.view(1..5)
        |  val w = v.view(1..3)
        |  print(w)
      """.stripMargin,
      "[30, 40]\n",
    ),
    Case(
      "view-style slicing",
      "negative bounds wrap from the end",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  print(a.view(-3..-1))
      """.stripMargin,
      "[30, 40]\n",
    ),
    Case(
      "view-style slicing",
      "an out-of-bounds view traps",
      """
        |def main() =
        |  val a = [1, 2, 3]
        |  assert_traps(() -> a.view(0..10), "view")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    ),
    Case(
      "view-style slicing",
      "writes to the source are visible through a previously-created view",
      """
        |def main() =
        |  var a = [10, 20, 30, 40, 50]
        |  val v = a.view(1..4)
        |  a[2] = 99
        |  print(v[1])
      """.stripMargin,
      "99\n",
    ),

    // ============================================================
    // rank-2 row-range views — `m.view(rowLo..rowHi)` borrows a
    // contiguous row range. Row-major layout makes any row range
    // contiguous in memory, so no stride is needed. Sub-rectangles
    // (two ranges) are a separate deferred feature.
    // ============================================================
    Case(
      "rank-2 view-style slicing",
      "row-range view reads source rows through the window",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]
        |  val v = m.view(1..3)
        |  print(v[0, 0])
        |  print(v[0, 2])
        |  print(v[1, 1])
      """.stripMargin,
      "4\n6\n8\n",
    ),
    Case(
      "rank-2 view-style slicing",
      "rows/cols/shape reflect the view's window",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]
        |  val v = m.view(1..3)
        |  print(rows(v))
        |  print(cols(v))
        |  print(shape(v))
      """.stripMargin,
      "2\n3\n(2, 3)\n",
    ),
    Case(
      "rank-2 view-style slicing",
      "writes through a row-range view update the source matrix",
      """
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  val v = m.view(1..3)
        |  v[0, 1] = 99
        |  v[1, 2] = 77
        |  print(m)
      """.stripMargin,
      "[[1, 2, 3], [4, 99, 6], [7, 8, 77]]\n",
    ),
    Case(
      "rank-2 view-style slicing",
      "sum / transpose / sum_axis work transparently on a row-range view",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]
        |  val v = m.view(1..3)
        |  print(sum(v))
        |  print(transpose(v))
        |  print(sum_axis(v, 0))
        |  print(sum_axis(v, 1))
      """.stripMargin,
      "39\n[[4, 7], [5, 8], [6, 9]]\n[11, 13, 15]\n[15, 24]\n",
    ),
    Case(
      "rank-2 view-style slicing",
      "inclusive range form m.view(lo..=hi) includes the last row",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4], [5, 6], [7, 8]]
        |  print(m.view(1..=2))
      """.stripMargin,
      "[[3, 4], [5, 6]]\n",
    ),
    Case(
      "rank-2 view-style slicing",
      "view-of-view collapses to the underlying buffer",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4], [5, 6], [7, 8]]
        |  val v = m.view(0..4)
        |  val w = v.view(1..3)
        |  print(w)
      """.stripMargin,
      "[[3, 4], [5, 6]]\n",
    ),
    Case(
      "rank-2 view-style slicing",
      "negative bounds wrap from the bottom row",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4], [5, 6], [7, 8]]
        |  print(m.view(-3..-1))
      """.stripMargin,
      "[[3, 4], [5, 6]]\n",
    ),
    Case(
      "rank-2 view-style slicing",
      "an out-of-bounds row-range view traps",
      """
        |def main() =
        |  val m = [[1, 2], [3, 4], [5, 6]]
        |  assert_traps(() -> m.view(0..10), "view")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    ),
    Case(
      "rank-2 view-style slicing",
      "writes to the source matrix are visible through a previously-created view",
      """
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        |  val v = m.view(1..3)
        |  m[1, 1] = 99
        |  print(v[0, 1])
      """.stripMargin,
      "99\n",
    ),

    // ============================================================
    // rank-1 strided views — `a.view(lo..hi, k)` borrows every k-th
    // element starting at lo. Three-arg form keeps the parser simple;
    // descriptor carries a stride field. View-of-view multiplies
    // strides.
    // ============================================================
    Case(
      "rank-1 strided view-style slicing",
      "stride 2 borrows every other element",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50, 60]
        |  val v = a.view(0..6, 2)
        |  print(v)
      """.stripMargin,
      "[10, 30, 50]\n",
    ),
    Case(
      "rank-1 strided view-style slicing",
      "strided view length collapses by ceil((hi-lo)/k)",
      """
        |def main() =
        |  val a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        |  val v = a.view(1..9, 3)
        |  print(length(v))
        |  print(v)
      """.stripMargin,
      "3\n[2, 5, 8]\n",
    ),
    Case(
      "rank-1 strided view-style slicing",
      "strided view supports indexed read",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50, 60, 70]
        |  val v = a.view(0..7, 3)
        |  print(v[0])
        |  print(v[1])
        |  print(v[2])
      """.stripMargin,
      "10\n40\n70\n",
    ),
    Case(
      "rank-1 strided view-style slicing",
      "write through a strided view updates the source",
      """
        |def main() =
        |  var a = [1, 2, 3, 4, 5, 6]
        |  val v = a.view(0..6, 2)
        |  v[1] = 99
        |  print(a)
      """.stripMargin,
      "[1, 2, 99, 4, 5, 6]\n",
    ),
    Case(
      "rank-1 strided view-style slicing",
      "sum / map / reduce work transparently on a strided view",
      """
        |def main() =
        |  val a = [1, 2, 3, 4, 5, 6, 7, 8]
        |  val v = a.view(0..8, 2)
        |  print(sum(v))
        |  print(reduce(v, 0, (acc, x) -> acc + x * 10))
      """.stripMargin,
      "16\n160\n",
    ),
    Case(
      "rank-1 strided view-style slicing",
      "view-of-view multiplies strides",
      """
        |def main() =
        |  val a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
        |  val v1 = a.view(0..12, 2)
        |  val v2 = v1.view(0..6, 2)
        |  print(v2)
      """.stripMargin,
      "[1, 5, 9]\n",
    ),
    Case(
      "rank-1 strided view-style slicing",
      "stride 1 is equivalent to a contiguous view",
      """
        |def main() =
        |  val a = [10, 20, 30, 40, 50]
        |  val v = a.view(1..4, 1)
        |  print(v)
      """.stripMargin,
      "[20, 30, 40]\n",
    ),
    Case(
      "rank-1 strided view-style slicing",
      "non-positive stride traps",
      """
        |def main() =
        |  val a = [1, 2, 3, 4, 5]
        |  assert_traps(() -> a.view(0..5, 0), "view")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    ),

    // ============================================================
    // rank-2 sub-rectangle views — `m.view(rRange, cRange)` borrows
    // a non-contiguous sub-matrix. Descriptor carries the owner's
    // rowStride so per-element indexing skips the gap between
    // visible rows in the underlying flat buffer. View-of-view
    // composes via colOff + rowStride inheritance.
    // ============================================================
    Case(
      "rank-2 sub-rectangle view-style slicing",
      "sub-rect view reads source elements at the right offsets",
      """
        |def main() =
        |  val m = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
        |  val v = m.view(0..2, 1..3)
        |  print(v[0, 0])
        |  print(v[0, 1])
        |  print(v[1, 0])
        |  print(v[1, 1])
      """.stripMargin,
      "2\n3\n6\n7\n",
    ),
    Case(
      "rank-2 sub-rectangle view-style slicing",
      "sub-rect shape reflects the row+col window",
      """
        |def main() =
        |  val m = [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13, 14, 15]]
        |  val v = m.view(1..3, 1..4)
        |  print(rows(v))
        |  print(cols(v))
        |  print(shape(v))
      """.stripMargin,
      "2\n3\n(2, 3)\n",
    ),
    Case(
      "rank-2 sub-rectangle view-style slicing",
      "writes through a sub-rect view update the source matrix",
      """
        |def main() =
        |  var m = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
        |  val v = m.view(0..2, 1..3)
        |  v[0, 0] = 99
        |  v[1, 1] = 77
        |  print(m)
      """.stripMargin,
      "[[1, 99, 3, 4], [5, 6, 77, 8], [9, 10, 11, 12]]\n",
    ),
    Case(
      "rank-2 sub-rectangle view-style slicing",
      "row extraction (v[i]) on a sub-rect view returns the right elements",
      """
        |def main() =
        |  val m = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
        |  val v = m.view(0..3, 1..3)
        |  print(v[0])
        |  print(v[1])
        |  print(v[2])
      """.stripMargin,
      "[2, 3]\n[6, 7]\n[10, 11]\n",
    ),
    Case(
      "rank-2 sub-rectangle view-style slicing",
      "formatValue prints sub-rect view in the expected nested form",
      """
        |def main() =
        |  val m = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
        |  print(m.view(0..2, 1..3))
      """.stripMargin,
      "[[2, 3], [6, 7]]\n",
    ),
    Case(
      "rank-2 sub-rectangle view-style slicing",
      "sum on a sub-rect view accounts for row stride",
      """
        |def main() =
        |  val m = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
        |  val v = m.view(0..2, 1..3)
        |  print(sum(v))
      """.stripMargin,
      "18\n",
    ),
    Case(
      "rank-2 sub-rectangle view-style slicing",
      "view-of-view composes col offsets",
      """
        |def main() =
        |  val m = [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13, 14, 15], [16, 17, 18, 19, 20]]
        |  val v1 = m.view(0..4, 1..4)
        |  val v2 = v1.view(1..3, 1..3)
        |  print(v2)
      """.stripMargin,
      "[[8, 9], [13, 14]]\n",
    ),
    Case(
      "rank-2 sub-rectangle view-style slicing",
      "transpose of a sub-rect view produces the right transposed values",
      """
        |def main() =
        |  val m = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
        |  print(transpose(m.view(0..2, 1..3)))
      """.stripMargin,
      "[[2, 6], [3, 7]]\n",
    ),
    Case(
      "rank-2 sub-rectangle view-style slicing",
      "out-of-bounds col range traps",
      """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  assert_traps(() -> m.view(0..2, 0..10), "view")
        |  print("caught")
      """.stripMargin,
      "caught\n",
    ),

    // ============================================================
    // const expressions calling functions — spec §5.3. The RHS may
    // call any pure function (prelude or user-defined) so long as
    // the result type is scalar. Compile-time evaluation is optional
    // (spec says "may inline"); today the runtime computes the value
    // at module init, which is observable at the first print.
    // ============================================================
    Case(
      "const fn calls",
      "const calls a pure prelude function returning real",
      """
        |const SQRT2 = sqrt(2.0)
        |def main() =
        |  print(SQRT2 * SQRT2)
      """.stripMargin,
      "2.0000000000000004\n",
      mlir = true,
    ),
    Case(
      "const fn calls",
      "const calls a user-defined pure function",
      """
        |def square(x: real) = x * x
        |const NINE = square(3.0)
        |def main() = print(NINE)
      """.stripMargin,
      "9.0\n",
      mlir = true,
    ),
    Case(
      "const fn calls",
      "const calls a recursive pure function",
      """
        |def fact(n: integer): integer = if n <= 0 then 1 else n * fact(n - 1)
        |const FACT6 = fact(6)
        |def main() = print(FACT6)
      """.stripMargin,
      "720\n",
      mlir = true,
    ),
    Case(
      "const fn calls",
      "const chains a prelude call into arithmetic with another const",
      """
        |const HALF_PI = pi / 2.0
        |const COS_HP  = cos(HALF_PI)
        |def main() = print(abs(COS_HP) < 1.0e-10)
      """.stripMargin,
      "true\n",
      mlir = true,
    ),
    Case(
      "const fn calls",
      "user pure function declared after the const still works (declared return type)",
      """
        |const NINE = square(3.0)
        |def square(x: real): real = x * x
        |def main() = print(NINE)
      """.stripMargin,
      "9.0\n",
      mlir = true,
    ),

    // ========================================================================
    // user-defined generics — Stage 1 (functions only)
    // ========================================================================

    Case(
      "user generics",
      "identity[T] returns its integer argument unchanged",
      """
        |def id[T](x: T): T = x
        |def main() = print(id(42))
      """.stripMargin,
      "42\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "identity[T] returns its real argument unchanged",
      """
        |def id[T](x: T): T = x
        |def main() = print(id(3.5))
      """.stripMargin,
      "3.5\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "identity[T] returns its string argument unchanged",
      """
        |def id[T](x: T): T = x
        |def main() = print(id("hi"))
      """.stripMargin,
      "hi\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "identity[T] called with multiple concrete types in one program",
      """
        |def id[T](x: T): T = x
        |def main() =
        |  print(id(42))
        |  print(id(3.5))
        |  print(id("hi"))
      """.stripMargin,
      "42\n3.5\nhi\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "numeric-constrained kindvar admits integer and real, used in arithmetic body",
      """
        |def twice[T: Numeric](x: T): T = x + x
        |def main() =
        |  print(twice(21))
        |  print(twice(1.5))
      """.stripMargin,
      "42\n3.0\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "ord-constrained kindvar accepts ordered comparison in body",
      """
        |def min2[T: Ord](a: T, b: T): T = if a < b then a else b
        |def main() =
        |  print(min2(3, 7))
        |  print(min2(2.5, 1.5))
      """.stripMargin,
      "3\n1.5\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "eq-constrained kindvar accepts equality in body",
      """
        |def same[T: Eq](a: T, b: T): bool = a == b
        |def main() =
        |  print(same(1, 1))
        |  print(same(1, 2))
        |  print(same("hi", "hi"))
        |  print(same("hi", "bye"))
        |  print(same(true, true))
      """.stripMargin,
      "true\nfalse\ntrue\nfalse\ntrue\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "eq-constrained kindvar over complex args",
      """
        |def same[T: Eq](a: T, b: T): bool = a == b
        |def main() =
        |  print(same(1.0 + 2.0i, 1.0 + 2.0i))
        |  print(same(1.0 + 2.0i, 3.0 + 4.0i))
      """.stripMargin,
      "true\nfalse\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "concrete overload wins over generic when both apply",
      """
        |def f(x: integer): integer = 100
        |def f[T](x: T): integer = 200
        |def main() =
        |  print(f(1))
        |  print(f("hi"))
      """.stripMargin,
      "100\n200\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "concrete overload (with promotion) wins over generic when both apply",
      """
        |def f(x: real): integer = 1
        |def f[T](x: T): integer = 2
        |def main() =
        |  print(f(3.14))
        |  print(f(42))
        |  print(f("x"))
      """.stripMargin,
      "1\n1\n2\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "two type params: first[T, U] picks first arg",
      """
        |def first[T, U](a: T, b: U): T = a
        |def main() =
        |  print(first(1, "hi"))
        |  print(first("yes", 99))
        |  print(first(3.14, true))
      """.stripMargin,
      "1\nyes\n3.14\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "two type params: second[T, U] picks second arg",
      """
        |def second[T, U](a: T, b: U): U = b
        |def main() =
        |  print(second(1, "hi"))
        |  print(second("yes", 99))
      """.stripMargin,
      "hi\n99\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "same kind var across two params binds consistently",
      """
        |def pickMax[T: Ord](a: T, b: T): T = if a < b then b else a
        |def main() =
        |  print(pickMax(3, 7))
        |  print(pickMax(2.5, 1.5))
      """.stripMargin,
      "7\n2.5\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "generic function takes a lambda and applies it",
      """
        |def apply1[T, U](x: T, f: T -> U): U = f(x)
        |def main() =
        |  print(apply1(5, x -> x * 2))
        |  print(apply1(3.0, x -> x + 0.5))
      """.stripMargin,
      "10\n3.5\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "generic-calls-generic with type-param threading",
      """
        |def id[T](x: T): T = x
        |def applyTwice[T](x: T, f: T -> T): T = f(f(x))
        |def main() =
        |  print(applyTwice(5, y -> id(y) + 1))
        |  print(applyTwice("a", s -> id(s)))
      """.stripMargin,
      "7\na\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "numeric kindvar promotes when two args differ (integer + real)",
      """
        |def add[T: Numeric](a: T, b: T): T = a + b
        |def main() =
        |  print(add(1, 2))
        |  print(add(1.5, 2.5))
      """.stripMargin,
      "3\n4.0\n",
      mlir = true,
    ),
    Case(
      "user generics",
      "ord-constrained generic plus concrete bool overload",
      """
        |def show(x: bool): string = if x then "yes" else "no"
        |def show[T: Ord](x: T): string = "ord"
        |def main() =
        |  print(show(true))
        |  print(show(7))
        |  print(show(2.5))
      """.stripMargin,
      "yes\nord\nord\n",
      mlir = true,
    ),

    // ========================================================================
    // user-defined generics — Stage 2 (generic structs)
    // ========================================================================

    Case(
      "user generic structs",
      "single-param Box[T] construct + field access (integer)",
      """
        |struct Box[T]
        |  value: T
        |end Box
        |
        |def main() =
        |  val b = Box(42)
        |  print(b.value)
      """.stripMargin,
      "42\n",
      mlir = true,
    ),
    Case(
      "user generic structs",
      "single-param Box[T] specialized for string in the same program",
      """
        |struct Box[T]
        |  value: T
        |end Box
        |
        |def main() =
        |  val a = Box(42)
        |  val b = Box("hi")
        |  print(a.value)
        |  print(b.value)
      """.stripMargin,
      "42\nhi\n",
      mlir = true,
    ),
    Case(
      "user generic structs",
      "multi-param Pair[A, B] heterogeneous specialization",
      """
        |struct Pair[A, B]
        |  fst: A
        |  snd: B
        |end Pair
        |
        |def main() =
        |  val p = Pair(1, "hi")
        |  print(p.fst)
        |  print(p.snd)
      """.stripMargin,
      "1\nhi\n",
      mlir = true,
    ),
    Case(
      "user generic structs",
      "nested generic: Pair of Pairs",
      """
        |struct Pair[A, B]
        |  fst: A
        |  snd: B
        |end Pair
        |
        |def main() =
        |  val p = Pair(Pair(1, 2), Pair(3, 4))
        |  print(p.fst.fst)
        |  print(p.fst.snd)
        |  print(p.snd.fst)
        |  print(p.snd.snd)
      """.stripMargin,
      "1\n2\n3\n4\n",
      mlir = true,
    ),
    Case(
      "user generic structs",
      "generic struct passed to a generic function",
      """
        |struct Box[T]
        |  value: T
        |end Box
        |
        |def unbox[T](b: Box[T]): T = b.value
        |
        |def main() =
        |  print(unbox(Box(7)))
        |  print(unbox(Box("hi")))
      """.stripMargin,
      "7\nhi\n",
      mlir = true,
    ),
    Case(
      "user generic structs",
      "Pair specialized for integer reused across construction sites",
      """
        |struct Pair[A, B]
        |  fst: A
        |  snd: B
        |end Pair
        |
        |def main() =
        |  val p = Pair(1, 2)
        |  val q = Pair(3, 4)
        |  print(p.fst + q.snd)
      """.stripMargin,
      "5\n",
      mlir = true,
    ),
    Case(
      "user generic structs",
      "ord-constrained type param accepts ordered comparisons via field",
      """
        |struct OrdBox[T: Ord]
        |  value: T
        |end OrdBox
        |
        |def main() =
        |  val a = OrdBox(3)
        |  val b = OrdBox(7)
        |  print(if a.value < b.value then "less" else "geq")
      """.stripMargin,
      "less\n",
      mlir = true,
    ),
    Case(
      "user generic structs",
      "explicit type annotation Pair[integer, string]",
      """
        |struct Pair[A, B]
        |  fst: A
        |  snd: B
        |end Pair
        |
        |def main() =
        |  val p: Pair[integer, string] = Pair(1, "hi")
        |  print(p.fst)
        |  print(p.snd)
      """.stripMargin,
      "1\nhi\n",
      mlir = true,
    ),
    Case(
      "user generic structs",
      "field write through a specialized generic struct (var binding)",
      """
        |struct Box[T]
        |  value: T
        |end Box
        |
        |def main() =
        |  var b = Box(1)
        |  b.value = 99
        |  print(b.value)
      """.stripMargin,
      "99\n",
      mlir = true,
    ),

    // ========================================================================
    // user-defined generics — Stage 2 (generic enums)
    // ========================================================================

    Case(
      "user generic enums",
      "Opt[T] Some(integer) match",
      """
        |enum Opt[T] =
        |  Some(value: T)
        |  None
        |end Opt
        |
        |def main() =
        |  val x = Some(42)
        |  val msg: string = x match
        |    Some(v) -> "got"
        |    None    -> "none"
        |  print(msg)
      """.stripMargin,
      "got\n",
    ),
    Case(
      "user generic enums",
      "Opt[T] bound to bare variant via annotation",
      """
        |enum Opt[T] =
        |  Some(value: T)
        |  None
        |end Opt
        |
        |def main() =
        |  val x: Opt[integer] = None
        |  val msg: string = x match
        |    Some(_) -> "got"
        |    None    -> "nothing"
        |  print(msg)
      """.stripMargin,
      "nothing\n",
    ),
    Case(
      "user generic enums",
      "Opt[T] payload binding read in arm body",
      """
        |enum Opt[T] =
        |  Some(value: T)
        |  None
        |end Opt
        |
        |def main() =
        |  val x = Some(7)
        |  val v: integer = x match
        |    Some(n) -> n
        |    None    -> 0
        |  print(v)
      """.stripMargin,
      "7\n",
    ),
    Case(
      "user generic enums",
      "Opt[T] specialized for string + integer in the same program",
      """
        |enum Opt[T] =
        |  Some(value: T)
        |  None
        |end Opt
        |
        |def main() =
        |  val a = Some(1)
        |  val b = Some("hi")
        |  val sa: string = a match
        |    Some(_) -> "int"
        |    None    -> "none"
        |  val sb: string = b match
        |    Some(_) -> "str"
        |    None    -> "none"
        |  print(sa)
        |  print(sb)
      """.stripMargin,
      "int\nstr\n",
    ),
    Case(
      "user generic enums",
      "Result[T, E] heterogeneous match",
      """
        |enum Result[T, E] =
        |  Ok(value: T)
        |  Err(error: E)
        |end Result
        |
        |def main() =
        |  val r: Result[integer, string] = Ok(7)
        |  val v: integer = r match
        |    Ok(n)  -> n
        |    Err(_) -> -1
        |  print(v)
      """.stripMargin,
      "7\n",
    ),
    Case(
      "user generic enums",
      "Result[T, E] specialized differently across sites",
      """
        |enum Result[T, E] =
        |  Ok(value: T)
        |  Err(error: E)
        |end Result
        |
        |def main() =
        |  val r1: Result[integer, string] = Ok(7)
        |  val r2: Result[integer, string] = Err("oops")
        |  val a: integer = r1 match
        |    Ok(n)  -> n
        |    Err(_) -> -1
        |  val b: string = r2 match
        |    Ok(_)  -> "ok"
        |    Err(m) -> m
        |  print(a)
        |  print(b)
      """.stripMargin,
      "7\noops\n",
    ),
    Case(
      "user generic enums",
      "generic enum read through a generic function",
      """
        |enum Opt[T] =
        |  Some(value: T)
        |  None
        |end Opt
        |
        |def unwrap_or[T](o: Opt[T], d: T): T =
        |  o match
        |    Some(v) -> v
        |    None    -> d
        |
        |def main() =
        |  print(unwrap_or(Some(7), 0))
        |  print(unwrap_or(Some("hi"), "miss"))
      """.stripMargin,
      "7\nhi\n",
    ),
    Case(
      "user generic enums",
      "Ord-constrained enum payload comparison",
      """
        |enum Opt[T: Ord] =
        |  Some(value: T)
        |  None
        |end Opt
        |
        |def main() =
        |  val a = Some(3)
        |  val b = Some(7)
        |  val aOk: bool = a match
        |    Some(n) -> n < 5
        |    None    -> false
        |  val bOk: bool = b match
        |    Some(n) -> n < 5
        |    None    -> false
        |  print(aOk)
        |  print(bOk)
      """.stripMargin,
      "true\nfalse\n",
    ),
    Case(
      "user generic enums",
      "explicit Opt[integer] annotation drives spec",
      """
        |enum Opt[T] =
        |  Some(value: T)
        |  None
        |end Opt
        |
        |def main() =
        |  val x: Opt[integer] = Some(99)
        |  val v: integer = x match
        |    Some(n) -> n
        |    None    -> 0
        |  print(v)
      """.stripMargin,
      "99\n",
    ),
    Case(
      "user generic enums",
      "wildcard arm covers the rest",
      """
        |enum Result[T, E] =
        |  Ok(value: T)
        |  Err(error: E)
        |end Result
        |
        |def main() =
        |  val r: Result[integer, string] = Ok(1)
        |  val s: string = r match
        |    Ok(_) -> "ok"
        |    _     -> "other"
        |  print(s)
      """.stripMargin,
      "ok\n",
    ),
  )
