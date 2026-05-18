package io.github.edadma.nex

import java.io.ByteArrayOutputStream
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Reference-interpreter tests. Each test parses + elaborates + runs a
  * Nex source fragment, optionally captures stdout, and asserts on the
  * result value and/or the captured output.
  *
  * Spec sections most exercised here: §3.2 (promotion), §4 (operators
  * incl. element-wise and matmul), §10 (prelude). The interpreter design
  * lives in `project_nex_elaborator_handoff.md`.
  */
class NexInterpreterTests extends AnyWordSpec with Matchers:

  // --------------------------------------------------------------------------
  // Helpers
  // --------------------------------------------------------------------------

  /** Run a program, returning the captured stdout. Any nex trap is
    * surfaced as a `fail`.
    */
  private def runOut(src: String): String =
    val (out, _) = runOutAndValue(src)
    out

  private def runValue(src: String): Value =
    val (_, v) = runOutAndValue(src)
    v

  private def runOutAndValue(src: String): (String, Value) =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    val tp = new NexElaborator().elaborate(ast) match
      case Right(t)   => t
      case Left(errs) => fail(s"elab errors: ${errs.map(_.toString).mkString("; ")}")
    val buf  = new ByteArrayOutputStream
    val interp = new NexInterpreter
    val v    = Console.withOut(buf) { interp.runProgram(tp) }
    (buf.toString, v)

  private def shouldTrap(src: String): NexTrap =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    val tp = new NexElaborator().elaborate(ast) match
      case Right(t)   => t
      case Left(errs) => fail(s"elab errors: ${errs.map(_.toString).mkString("; ")}")
    val buf  = new ByteArrayOutputStream
    val interp = new NexInterpreter
    try
      Console.withOut(buf) { interp.runProgram(tp) }
      fail("expected a trap, but program completed")
    catch case t: NexTrap => t

  // ==========================================================================
  // Literals & top-level bindings
  // ==========================================================================

  "top-level bindings + print" should {

    "evaluate integer literal" in {
      runOut("""
        |val x = 42
        |def main() = print(x)
      """.stripMargin) shouldBe "42\n"
    }

    "evaluate real literal" in {
      runOut("""
        |val x = 3.5
        |def main() = print(x)
      """.stripMargin) shouldBe "3.5\n"
    }

    "evaluate bool literal" in {
      runOut("""
        |def main() = print(true)
      """.stripMargin) shouldBe "true\n"
    }

    "evaluate string literal" in {
      runOut("""
        |def main() = print("hello")
      """.stripMargin) shouldBe "hello\n"
    }
  }

  // ==========================================================================
  // Arithmetic (§3.2 promotion + §4.4 scalar ops)
  // ==========================================================================

  "scalar arithmetic" should {

    "add integers" in {
      runOut("""def main() = print(2 + 3)""") shouldBe "5\n"
    }

    "promote int + real to real" in {
      runOut("""def main() = print(2 + 0.5)""") shouldBe "2.5\n"
    }

    "use real division on `/`" in {
      runOut("""def main() = print(7 / 2)""") shouldBe "3.5\n"
    }

    "use integer division on `div`" in {
      runOut("""def main() = print(7 div 2)""") shouldBe "3\n"
    }

    "modulo on integers" in {
      runOut("""def main() = print(7 % 3)""") shouldBe "1\n"
    }

    "power (integer)" in {
      runOut("""def main() = print(2 ^ 10)""") shouldBe "1024\n"
    }

    "power (real)" in {
      runOut("""def main() = print(2.0 ^ 0.5)""") should startWith("1.41421")
    }

    "unary minus" in {
      runOut("""def main() = print(-7)""") shouldBe "-7\n"
    }

    "complex from `i`" in {
      runOut("""def main() = print(3 + 4i)""") shouldBe "3.0+4.0i\n"
    }

    "complex multiply" in {
      // (1+i)*(1-i) = 1 - i^2 = 2
      runOut("""
        |def main() =
        |  val a = 1 + 1i
        |  val b = 1 - 1i
        |  print(a * b)
      """.stripMargin) shouldBe "2.0+0.0i\n"
    }

    "subtraction is not juxtaposed multiplication (regression)" in {
      // Before the fix to juxtExpr's body, `5 - 2` parsed as `5 * (-2)`
      // because `numericLit ~ powExpr` let powExpr's unary `-` swallow the
      // binary subtraction operator.
      runOut("""def main() = print(5 - 2)""")           shouldBe "3\n"
      runOut("""def main() = print(10 - 3 - 4)""")      shouldBe "3\n"
      runOut("""def main() = print(3 - 4i)""")          shouldBe "3.0-4.0i\n"
      runOut("""
        |def main() =
        |  val x = 5
        |  print(2x - 3)
      """.stripMargin) shouldBe "7\n"
    }

    "negated juxtaposition coefficient" in {
      // `-4i` parses as `JuxtaposeExpr(-4, i)`, evaluating to 0 - 4i.
      runOut("""def main() = print(-4i)""")        shouldBe "0.0-4.0i\n"
      runOut("""def main() = print(-2.5 + 0i)""")  shouldBe "-2.5+0.0i\n"
      // Inside an expression: `7 + -3i` should give 7 - 3i.
      runOut("""def main() = print(7 + -3i)""")    shouldBe "7.0-3.0i\n"
      // `-3pi` is `-(3 * pi)`.
      runOut("""def main() = print(-1pi)""")       shouldBe "-3.141592653589793\n"
    }

    "unary minus binds tighter than power" in {
      // Spec §4.3: `-2^2` is `(-2)^2 = 4`, NOT `-(2^2) = -4`.
      runOut("""def main() = print(-2^2)""") shouldBe "4\n"
    }

    "division by zero traps" in {
      shouldTrap("""def main() = print(1 / 0)""").msg should include("division by zero")
    }
  }

  // ==========================================================================
  // Comparison and logical (§4.6, §4.7)
  // ==========================================================================

  "comparison + logical" should {
    "==" in { runOut("""def main() = print(3 == 3)""") shouldBe "true\n" }
    "!=" in { runOut("""def main() = print(3 != 4)""") shouldBe "true\n" }
    "<"  in { runOut("""def main() = print(3 < 4)""")  shouldBe "true\n" }
    "and short-circuits" in {
      // If `and` evaluated the RHS, `1 / 0` would trap.
      runOut("""def main() = print(false and (1 / 0 == 0))""") shouldBe "false\n"
    }
    "or short-circuits" in {
      runOut("""def main() = print(true or (1 / 0 == 0))""") shouldBe "true\n"
    }
    "not" in { runOut("""def main() = print(not false)""") shouldBe "true\n" }
  }

  // ==========================================================================
  // Control flow (§4.10 / §4.13)
  // ==========================================================================

  "control flow" should {

    "if/else" in {
      runOut("""
        |def main() =
        |  val x = 10
        |  if x > 5 then print("big") else print("small")
      """.stripMargin) shouldBe "big\n"
    }

    "if expression yields a value" in {
      runOut("""
        |def main() =
        |  val x = if 3 < 4 then 1 else 0
        |  print(x)
      """.stripMargin) shouldBe "1\n"
    }

    "while loop" in {
      runOut("""
        |def main() =
        |  var i = 0
        |  while i < 3 do
        |    print(i)
        |    i = i + 1
      """.stripMargin) shouldBe "0\n1\n2\n"
    }

    "for over array" in {
      runOut("""
        |def main() =
        |  for x in [10, 20, 30] do
        |    print(x)
      """.stripMargin) shouldBe "10\n20\n30\n"
    }

    "for over range" in {
      runOut("""
        |def main() =
        |  for i in 0..3 do
        |    print(i)
      """.stripMargin) shouldBe "0\n1\n2\n"
    }

    "for with tuple destructuring on enumerate(xs)" in {
      runOut("""
        |def main() =
        |  val xs = [10, 20, 30]
        |  for i, x in enumerate(xs) do
        |    print(s"$i: $x")
      """.stripMargin) shouldBe "0: 10\n1: 20\n2: 30\n"
    }

    "early return" in {
      runOut("""
        |def f(x: integer) =
        |  if x < 0 then return -1
        |  x * 2
        |
        |def main() =
        |  print(f(-5))
        |  print(f(4))
      """.stripMargin) shouldBe "-1\n8\n"
    }
  }

  // ==========================================================================
  // Functions & closures
  // ==========================================================================

  "functions" should {

    "plain function" in {
      runOut("""
        |def add(x: integer, y: integer) = x + y
        |def main() = print(add(3, 4))
      """.stripMargin) shouldBe "7\n"
    }

    "recursion (fact)" in {
      runOut("""
        |def fact(n: integer): integer =
        |  if n <= 1 then 1 else n * fact(n - 1)
        |def main() = print(fact(6))
      """.stripMargin) shouldBe "720\n"
    }

    "mutual recursion" in {
      runOut("""
        |def isEven(n: integer): bool =
        |  if n == 0 then true else isOdd(n - 1)
        |def isOdd(n: integer): bool =
        |  if n == 0 then false else isEven(n - 1)
        |def main() =
        |  print(isEven(10))
        |  print(isOdd(7))
      """.stripMargin) shouldBe "true\ntrue\n"
    }

    "lambda + capture" in {
      runOut("""
        |def main() =
        |  val k = 10
        |  val f = x -> x + k
        |  print(f(5))
      """.stripMargin) shouldBe "15\n"
    }

    "closure captures live cells" in {
      // var k is captured; mutating it changes what the closure sees
      runOut("""
        |def main() =
        |  var k = 1
        |  val f = x -> x + k
        |  print(f(10))
        |  k = 100
        |  print(f(10))
      """.stripMargin) shouldBe "11\n110\n"
    }

    "unannotated lambda passed to plain call infers param type and runs" in {
      // Bidirectional inference end-to-end: the lambda's `x` carries no
      // annotation but the callee declares `f: (integer -> integer)`,
      // so push-down gives `x: integer` and the runtime computes 6.
      runOut("""
        |def apply(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  print(apply(x -> x * 2, 3))
      """.stripMargin) shouldBe "6\n"
    }

    "unannotated lambda passed via method-call sugar runs" in {
      // Push-down for the method-call path: `n.apply(f)` desugars to
      // `apply(n, f)`. Stage 2 looks `apply` up by name and pushes its
      // declared second-param type into the lambda.
      runOut("""
        |def apply(n: integer, f: (integer -> integer)) = f(n)
        |def main() =
        |  print((3).apply(x -> x * 2))
      """.stripMargin) shouldBe "6\n"
    }

    "lambda bound to a val with a declared function type runs" in {
      runOut("""
        |def main() =
        |  val f: (integer -> integer) = x -> x * 2
        |  print(f(7))
      """.stripMargin) shouldBe "14\n"
    }

    "unannotated multi-arg lambda passed to plain call infers and runs" in {
      runOut("""
        |def apply2(f: ((integer, integer) -> integer), a: integer, b: integer) = f(a, b)
        |def main() =
        |  print(apply2((x, y) -> x + y, 4, 5))
      """.stripMargin) shouldBe "9\n"
    }

    "prelude HOF `map` with unannotated lambda infers and runs" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3]
        |  val ys = map(xs, x -> x * 2)
        |  print(sum(ys))
      """.stripMargin) shouldBe "12\n"
    }

    "prelude HOF `reduce` with unannotated lambda infers and runs" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(reduce(xs, 0, (a, x) -> a + x))
      """.stripMargin) shouldBe "10\n"
    }

    "prelude HOF `filter` with unannotated lambda infers and runs" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3, 4, 5]
        |  val ev = filter(xs, x -> x % 2 == 0)
        |  print(sum(ev))
      """.stripMargin) shouldBe "6\n"
    }

    "prelude HOF method-call sugar `xs.map(x -> ...)` runs" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(sum(xs.map(x -> x * 10)))
      """.stripMargin) shouldBe "60\n"
    }

    "bound-then-called lambda with no declared type runs after deferred resolve" in {
      // The lambda `f` is bound with no declared type; its param starts
      // at TyUnknown. The deferred-resolve pass refines it at the first
      // call site (`apply(f, 3)`), where `apply`'s declared
      // `f: (integer -> integer)` gives the expected shape.
      runOut("""
        |def apply(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  val f = x -> x * 2
        |  print(apply(f, 3))
      """.stripMargin) shouldBe "6\n"
    }

    "bound-then-called lambda runs through prelude HOF" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  val f  = x -> x * 10
        |  print(sum(map(xs, f)))
      """.stripMargin) shouldBe "100\n"
    }
  }

  // ==========================================================================
  // Arrays (§4.5 element-wise + §10 prelude)
  // ==========================================================================

  "arrays" should {

    "literal + length" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(xs.length())
      """.stripMargin) shouldBe "3\n"
    }

    "index access" in {
      runOut("""
        |def main() =
        |  val xs = [10, 20, 30]
        |  print(xs[1])
      """.stripMargin) shouldBe "20\n"
    }

    "sum" in {
      runOut("""
        |def main() = print(sum([1, 2, 3, 4]))
      """.stripMargin) shouldBe "10\n"
    }

    "element-wise add" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3]
        |  val ys = [10, 20, 30]
        |  print(xs + ys)
      """.stripMargin) shouldBe "[11, 22, 33]\n"
    }

    "broadcast scalar * array" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(2 * xs)
      """.stripMargin) shouldBe "[2, 4, 6]\n"
    }

    "juxtaposition broadcast" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(2xs)
      """.stripMargin) shouldBe "[2, 4, 6]\n"
    }

    "rank-2 literal + transpose" in {
      runOut("""
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m.transpose())
      """.stripMargin) shouldBe "[[1, 4], [2, 5], [3, 6]]\n"
    }

    "matrix * vector via @" in {
      // [[1, 0], [0, 1]] @ [3, 5] = [3, 5]
      runOut("""
        |def main() =
        |  val a = [[1, 0], [0, 1]]
        |  val b = [3, 5]
        |  print(a @ b)
      """.stripMargin) shouldBe "[3, 5]\n"
    }

    "dot product via @ on rank-1 / rank-1" in {
      // [1, 2, 3] @ [4, 5, 6] = 4 + 10 + 18 = 32
      runOut("""
        |def main() = print([1, 2, 3] @ [4, 5, 6])
      """.stripMargin) shouldBe "32\n"
    }

    "map + sum chain" in {
      runOut("""
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(xs.map(x -> x * x).sum())
      """.stripMargin) shouldBe "30\n"
    }

    "var array index assign" in {
      runOut("""
        |def main() =
        |  var xs = [1, 2, 3]
        |  xs[1] = 99
        |  print(xs)
      """.stripMargin) shouldBe "[1, 99, 3]\n"
    }

    "out-of-bounds traps" in {
      shouldTrap("""
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(xs[5])
      """.stripMargin).msg should include("index out of bounds")
    }
  }

  // ==========================================================================
  // Structs
  // ==========================================================================

  "structs" should {

    "construct + field access" in {
      runOut("""
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |def main() =
        |  val p = Point(3.0, 4.0)
        |  print(p.x)
        |  print(p.y)
      """.stripMargin) shouldBe "3.0\n4.0\n"
    }

    "var struct field assign" in {
      runOut("""
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |def main() =
        |  var p = Point(1.0, 2.0)
        |  p.x = 99.0
        |  print(p.x)
      """.stripMargin) shouldBe "99.0\n"
    }
  }

  // ==========================================================================
  // Prelude (math constants + functions)
  // ==========================================================================

  "prelude" should {

    "pi" in {
      runOut("""def main() = print(pi)""") should startWith("3.14159")
    }

    "sqrt of real" in {
      runOut("""def main() = print(sqrt(9.0))""") shouldBe "3.0\n"
    }

    "sqrt of negative → complex" in {
      runOut("""def main() = print(sqrt(-4.0))""") shouldBe "0.0+2.0i\n"
    }

    "abs" in {
      runOut("""def main() = print(abs(-7))""") shouldBe "7\n"
    }

    "range" in {
      runOut("""def main() = print(range(0, 5))""") shouldBe "[0, 1, 2, 3, 4]\n"
    }

    "linspace" in {
      runOut("""def main() = print(linspace(0.0, 1.0, 5))""") shouldBe "[0.0, 0.25, 0.5, 0.75, 1.0]\n"
    }

    "zeros + ones" in {
      runOut("""def main() =
        |  print(zeros(3))
        |  print(ones(3))
      """.stripMargin) shouldBe "[0, 0, 0]\n[1, 1, 1]\n"
    }

    "format returns a string" in {
      runOut("""
        |def main() =
        |  val s = format(2 + 3)
        |  print(s)
        |  print(s == "5")
      """.stripMargin) shouldBe "5\ntrue\n"
    }

    "assert_eq passes silently" in {
      runOut("""
        |def main() =
        |  assert_eq(2 + 2, 4)
        |  print("ok")
      """.stripMargin) shouldBe "ok\n"
    }

    "assert_eq traps on mismatch" in {
      shouldTrap("""def main() = assert_eq(2 + 2, 5)""").msg should include("assert_eq")
    }

    "assert_traps passes when the lambda traps" in {
      runOut("""
        |def main() =
        |  assert_traps(() -> assert(false, "boom"))
        |  print("ok")
      """.stripMargin) shouldBe "ok\n"
    }

    "assert_traps with substring match passes" in {
      runOut("""
        |def main() =
        |  assert_traps(() -> assert(false, "division by zero"), "division")
        |  print("ok")
      """.stripMargin) shouldBe "ok\n"
    }

    "assert_traps traps when the lambda returns normally" in {
      shouldTrap("""def main() = assert_traps(() -> 1 + 1)""").msg should include("expected trap")
    }

    "assert_traps with substring match traps on mismatch" in {
      shouldTrap("""def main() = assert_traps(() -> assert(false, "alpha"), "beta")""").msg should include("expected substring")
    }

    "assert_traps catches a runtime trap (division by zero)" in {
      runOut("""
        |def main() =
        |  assert_traps(() -> 1 / 0)
        |  print("ok")
      """.stripMargin) shouldBe "ok\n"
    }
  }

  // ==========================================================================
  // Blocks & scoping
  // ==========================================================================

  "blocks + scoping" should {

    "sequential vals are scoped to the function body" in {
      runOut("""
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(a + b)
      """.stripMargin) shouldBe "7\n"
    }

    "val RHS can be an indented block" in {
      runOut("""
        |def main() =
        |  val total =
        |    val a = 1
        |    val b = 2
        |    val c = 3
        |    a + b + c
        |  print(total)
      """.stripMargin) shouldBe "6\n"
    }

    "top-level val RHS can be an indented block" in {
      runOut("""
        |val greeting =
        |  val name = "world"
        |  s"hello, $name"
        |
        |def main() = print(greeting)
      """.stripMargin) shouldBe "hello, world\n"
    }

    "inner def can shadow outer val" in {
      runOut("""
        |def f(x: integer) =
        |  val x = x + 100
        |  x
        |
        |def main() =
        |  print(f(1))
      """.stripMargin) shouldBe "101\n"
    }
  }

  // ==========================================================================
  // Tuple destructuring (`val a, b = ...`)
  // ==========================================================================

  "tuple destructuring" should {

    "bind block-level names from a tuple literal" in {
      runOut("""
        |def main() =
        |  val a, b = (3, 4)
        |  print(a)
        |  print(b)
        |  print(a + b)
      """.stripMargin) shouldBe "3\n4\n7\n"
    }

    "bind from an existing tuple variable" in {
      runOut("""
        |def main() =
        |  val pair = (10, 20)
        |  val x, y = pair
        |  print(x - y)
      """.stripMargin) shouldBe "-10\n"
    }

    "handle 3-tuples with mixed types" in {
      runOut("""
        |def main() =
        |  val p, q, r = (1.5, 2.5, 3.5)
        |  print(p + q + r)
      """.stripMargin) shouldBe "7.5\n"
    }

    "bind names at the top level" in {
      runOut("""
        |val a, b = (10, 20)
        |def main() = print(a + b)
      """.stripMargin) shouldBe "30\n"
    }

    "wildcard component is allowed" in {
      runOut("""
        |def main() =
        |  val _, b = (99, 7)
        |  print(b)
      """.stripMargin) shouldBe "7\n"
    }

    "preserves heterogeneous element types" in {
      // After destructuring, each name carries its own static type — `n`
      // is integer, `s` is string. Verified by formatting both.
      runOut("""
        |def main() =
        |  val n, s = (42, "hi")
        |  print(s"n=$n s=$s")
      """.stripMargin) shouldBe "n=42 s=hi\n"
    }

    "rejects arity mismatch at elaboration time" in {
      val src =
        """def main() =
          |  val a, b, c = (1, 2)
          |  print(a)
        """.stripMargin
      val ast = new NexParser().parseProgram(src).fold(e => fail(e), identity)
      new NexElaborator().elaborate(ast) match
        case Right(_)   => fail("expected an elaboration error for arity mismatch")
        case Left(errs) =>
          errs.map(_.toString).mkString(";") should include("binds 3 names but the value has only 2 elements")
    }

    "rejects non-tuple value at elaboration time" in {
      val src =
        """def main() =
          |  val a, b = 5
          |  print(a)
        """.stripMargin
      val ast = new NexParser().parseProgram(src).fold(e => fail(e), identity)
      new NexElaborator().elaborate(ast) match
        case Right(_)   => fail("expected an elaboration error for non-tuple RHS")
        case Left(errs) =>
          errs.map(_.toString).mkString(";") should include("requires a tuple value")
    }
  }

  // ==========================================================================
  // Paren-less tuple construction (the spec convention)
  // ==========================================================================

  "paren-less tuple construction" should {

    "bind a tuple value with no parens at all" in {
      // `val a, b = 1, 2` — both sides paren-less.
      runOut("""
        |def main() =
        |  val a, b = 1, 2
        |  print(a)
        |  print(b)
        |  print(a + b)
      """.stripMargin) shouldBe "1\n2\n3\n"
    }

    "bind a single name to a paren-less tuple RHS" in {
      // `val t = 1, 2` parses as `val t = TupleExpr(1, 2)` — t holds a tuple.
      runOut("""
        |def main() =
        |  val t = 1, 2
        |  print(t)
      """.stripMargin) shouldBe "(1, 2)\n"
    }

    "bind a paren-less 3-tuple at the top level" in {
      runOut("""
        |val a, b, c = 10, 20, 30
        |def main() = print(a + b + c)
      """.stripMargin) shouldBe "60\n"
    }

    "bind a paren-less heterogeneous tuple" in {
      runOut("""
        |def main() =
        |  val n, s = 42, "hello"
        |  print(s"n=$n s=$s")
      """.stripMargin) shouldBe "n=42 s=hello\n"
    }

    "produce equivalent values whether parens are written or not" in {
      // `1, 2, 3` and `(1, 2, 3)` are the same tuple — parens are pure
      // grouping. Bind both forms to a name and print; outputs must match.
      // (Can't do `print(1, 2, 3)` because that's a 3-arg call to print,
      // not a single tuple argument — function-call commas bind tighter.)
      val withParens =
        runOut("""def main() = print((1, 2, 3))""")
      val viaBinding =
        runOut("""
          |def main() =
          |  val t = 1, 2, 3
          |  print(t)
        """.stripMargin)
      withParens shouldBe "(1, 2, 3)\n"
      viaBinding shouldBe withParens
    }
  }

  // ==========================================================================
  // Strings + interpolation
  // ==========================================================================

  "string interpolation" should {

    "embed variable" in {
      runOut("""
        |def main() =
        |  val x = 42
        |  print(s"x = $x")
      """.stripMargin) shouldBe "x = 42\n"
    }

    "embed multiple variables + literal text" in {
      runOut("""
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"$a + $b")
      """.stripMargin) shouldBe "3 + 4\n"
    }

    "evaluate a `${...}` arithmetic expression" in {
      runOut("""
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"sum = ${a + b}")
      """.stripMargin) shouldBe "sum = 7\n"
    }

    "evaluate multiple `${...}` expressions in one string" in {
      runOut("""
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"${a} * ${b} = ${a * b}")
      """.stripMargin) shouldBe "3 * 4 = 12\n"
    }

    "evaluate a function call inside `${...}`" in {
      runOut("""
        |def square(x: integer) = x * x
        |def main() =
        |  print(s"5^2 = ${square(5)}")
      """.stripMargin) shouldBe "5^2 = 25\n"
    }

    "evaluate an `if` expression inside `${...}`" in {
      runOut("""
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"max = ${if a < b then b else a}")
      """.stripMargin) shouldBe "max = 4\n"
    }

    "evaluate a `${...}` reading destructured names" in {
      // Sanity: re-parsing happens in the surrounding scope, so names
      // introduced by tuple destructuring are visible.
      runOut("""
        |def main() =
        |  val x, y = (10, 20)
        |  print(s"x+y = ${x + y}")
      """.stripMargin) shouldBe "x+y = 30\n"
    }

    "reject a `${...}` with a parse error at elaboration time" in {
      val src =
        """def main() =
          |  print(s"bad = ${1 + }")
        """.stripMargin
      val ast = new NexParser().parseProgram(src).fold(e => fail(e), identity)
      new NexElaborator().elaborate(ast) match
        case Right(_)   => fail("expected an elaboration error for the bad interpolation body")
        case Left(errs) =>
          errs.map(_.toString).mkString(";") should include("failed to parse interpolated expression")
    }
  }

  // ==========================================================================
  // Call-site mode validation (§6.4)
  // ==========================================================================

  private def elaborateExpectingErrors(src: String): List[String] =
    val ast = new NexParser().parseProgram(src).fold(e => fail(e), identity)
    new NexElaborator().elaborate(ast) match
      case Right(_)   => fail("expected an elaboration error")
      case Left(errs) => errs.map(_.toString)

  private def elaborateExpectingSuccess(src: String): Unit =
    val ast = new NexParser().parseProgram(src).fold(e => fail(e), identity)
    new NexElaborator().elaborate(ast) match
      case Right(_)   => ()
      case Left(errs) => fail(s"expected clean elaboration, got: ${errs.map(_.toString).mkString("; ")}")

  "call-site mode check" should {

    "accept a var passed to a `mut` parameter" in {
      elaborateExpectingSuccess("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def main() =
        |  var n = 5
        |  bump(n)
      """.stripMargin)
    }

    "reject a `val` passed to a `mut` parameter" in {
      val errs = elaborateExpectingErrors("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def main() =
        |  val n = 5
        |  bump(n)
      """.stripMargin)
      errs.mkString(";") should include("cannot pass `n` to a `mut`-mode parameter")
    }

    "reject a literal passed to a `mut` parameter" in {
      val errs = elaborateExpectingErrors("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def main() = bump(5)
      """.stripMargin)
      errs.mkString(";") should include("requires an l-value argument")
    }

    "reject the result of a call passed to a `mut` parameter" in {
      val errs = elaborateExpectingErrors("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def pure() = 7
        |def main() = bump(pure())
      """.stripMargin)
      errs.mkString(";") should include("requires an l-value argument")
    }

    "accept forwarding a `mut` parameter to another `mut` parameter" in {
      elaborateExpectingSuccess("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def forward(y: mut integer) =
        |  bump(y)
        |def main() =
        |  var n = 5
        |  forward(n)
      """.stripMargin)
    }

    "reject a read-mode parameter passed to a `mut` parameter" in {
      val errs = elaborateExpectingErrors("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def caller(y: integer) =
        |  bump(y)
        |def main() =
        |  var n = 5
        |  caller(n)
      """.stripMargin)
      errs.mkString(";") should include("cannot pass `y` to a `mut`-mode parameter")
    }

    "accept a `var`-rooted struct field passed to a `mut` parameter" in {
      elaborateExpectingSuccess("""
        |struct Box
        |  v: integer
        |def bump(x: mut integer) =
        |  x = x + 1
        |def main() =
        |  var b = Box(10)
        |  bump(b.v)
      """.stripMargin)
    }

    "stays silent when no mut parameters are declared" in {
      // Pure-read functions should never trip the call-site check, no
      // matter what shape the argument is.
      elaborateExpectingSuccess("""
        |def square(x: integer) = x * x
        |def main() = print(square(5))
      """.stripMargin)
    }
  }
