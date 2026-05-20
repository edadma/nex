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
  *
  * Note: parity-compatible programs (the simple `runOut(src) shouldBe expected`
  * shape) have been migrated to [[NexProgramCorpus]] so they automatically
  * get cross-platform interpreter coverage AND JVM interp-vs-AOT parity
  * coverage. What remains here is intentionally interp-only:
  *
  *   - `shouldTrap` cases (AOT trap surface is via `assert_traps`,
  *     exercised separately).
  *   - Tests that use `startWith` or other inexact matchers (the parity
  *     runner needs byte-for-byte agreement).
  *   - Tests that directly drive the parser or elaborator to inspect
  *     errors.
  *   - Tests that issue multiple `runOut` calls in one `in { }` block.
  *
  * When adding a NEW test that just runs a program and checks its stdout,
  * add it as a `NexProgramCorpus.Case` instead of here — that way it gets
  * parity coverage for free. This file should only grow when the test
  * genuinely cannot be expressed as `(src, expected)`.
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
  // Arithmetic (§3.2 promotion + §4.4 scalar ops)
  // ==========================================================================

  "scalar arithmetic" should {

    "power (real)" in {
      runOut("""def main() = print(2.0 ^ 0.5)""") should startWith("1.41421")
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

    "division by zero traps" in {
      shouldTrap("""def main() = print(1 / 0)""").msg should include("division by zero")
    }
  }

  // ==========================================================================
  // Arrays (§4.5 element-wise + §10 prelude)
  // ==========================================================================

  "arrays" should {

    "reshape traps on size mismatch" in {
      shouldTrap("""
        |def main() =
        |  val flat = [1, 2, 3, 4]
        |  print(reshape(flat, 2, 3))
      """.stripMargin).msg should include("size mismatch")
    }

    "rank-1 slice out-of-bounds traps" in {
      shouldTrap("""
        |def main() =
        |  val a = [1, 2, 3]
        |  print(a[0..10])
      """.stripMargin).msg should include("out of bounds")
    }

    "`:` outside an index list is a parse error" in {
      new NexParser().parseProgram("def main() = print(:)") match
        case Left(_)  => succeed
        case Right(_) => fail("expected the parser to reject `:` outside an index list")
    }

    "out-of-bounds traps" in {
      shouldTrap("""
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(xs[5])
      """.stripMargin).msg should include("index out of bounds")
    }

    "rank-1 slice-assign length mismatch traps" in {
      shouldTrap("""
        |def main() =
        |  var xs = [1, 2, 3, 4, 5]
        |  xs[1..4] = [99]
      """.stripMargin).msg should include("length mismatch")
    }

    "rank-1 slice-assign out-of-bounds traps" in {
      shouldTrap("""
        |def main() =
        |  var xs = [1, 2, 3]
        |  xs[0..10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      """.stripMargin).msg should include("out of bounds")
    }

    "rank-2 submatrix replace traps on shape mismatch" in {
      shouldTrap("""
        |def main() =
        |  var m = [[1, 2, 3], [4, 5, 6]]
        |  m[0..2, 0..2] = [[1, 2]]
      """.stripMargin).msg should include("shape mismatch")
    }
  }

  // ==========================================================================
  // Prelude (math constants + functions)
  // ==========================================================================

  "prelude" should {

    "pi" in {
      runOut("""def main() = print(pi)""") should startWith("3.14159")
    }

    // ----------------------------------------------------------------
    // Complex-argument scalar math (spec §10.2 line 32: sin / cos /
    // exp / log / sqrt apply to real AND complex). Regression for the
    // bug where `exp(2pi * i)` trapped with "expected numeric" — the
    // dispatch was routing all args through `asReal` which rejected
    // any non-zero imaginary part.
    // ----------------------------------------------------------------

    "exp(complex) — exp(2pi * i) ≈ 1 (Euler's identity, full cycle)" in {
      val out = runOut("""def main() = print(exp(2pi * i))""")
      // Real part rounds to 1.0 exactly; imaginary is FP noise ≈ −2.4e-16.
      out should startWith("1.0")
      out should include("i")
    }

    "exp(complex) — exp(pi * i) ≈ -1 (Euler's identity proper)" in {
      val out = runOut("""def main() = print(exp(pi * i))""")
      out should startWith("-1.0")
      out should include("i")
    }

    "log(complex) — log(e + 0i) = 1 + 0i" in {
      val out = runOut("""def main() = print(log(e + 0i))""")
      out should startWith("1.0")
    }

    "log(complex) — log(-1 + 0i) = 0 + π·i" in {
      val out = runOut("""def main() = print(log(-1.0 + 0i))""")
      out should startWith("0.0+3.14159")
    }

    "sin(complex) — sin(0 + i) = i·sinh(1) ≈ 1.175i" in {
      val out = runOut("""def main() = print(sin(0.0 + 1.0 * i))""")
      out should startWith("0.0+1.175")
    }

    "cos(complex) — cos(0 + i) = cosh(1) ≈ 1.543 + 0i" in {
      val out = runOut("""def main() = print(cos(0.0 + 1.0 * i))""")
      out should startWith("1.543")
    }

    "sin² + cos² = 1 for complex args (identity check)" in {
      // Picks a complex point off the real axis; rounds to 1 + 0i
      // modulo FP noise.
      val out = runOut("""
        |def main() =
        |  val z = 0.5 + 0.7 * i
        |  val s = sin(z)
        |  val c = cos(z)
        |  print(s * s + c * c)
      """.stripMargin)
      out should startWith("1.0")  // imag is ~1e-17
    }

    "tan(complex) of a small-imag value matches tan(real) closely" in {
      // tan(0.5 + 0i) should agree with tan(0.5) on the real component.
      val out = runOut("""def main() = print(tan(0.5 + 0i))""")
      out should startWith("0.546")
    }

    "assert_eq traps on mismatch" in {
      shouldTrap("""def main() = assert_eq(2 + 2, 5)""").msg should include("assert_eq")
    }

    "assert_traps traps when the lambda returns normally" in {
      shouldTrap("""def main() = assert_traps(() -> 1 + 1)""").msg should include("expected trap")
    }

    "assert_traps with substring match traps on mismatch" in {
      shouldTrap("""def main() = assert_traps(() -> assert(false, "alpha"), "beta")""").msg should include("expected substring")
    }
  }

  // ==========================================================================
  // Tuple destructuring (`val a, b = ...`)
  // ==========================================================================

  "tuple destructuring" should {

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

  // ==========================================================================
  // Sum types (enums) — interp-only until AOT codegen lands.
  // Once the LLVM backend supports them these can migrate to the
  // parity corpus.
  // ==========================================================================

  "sum types (enums)" should {

    "declare and print bare variants" in {
      runOut("""
        |enum Color =
        |  Red
        |  Green
        |  Blue
        |
        |def main() =
        |  print(Red)
        |  print(Green)
        |  print(Blue)
      """.stripMargin) shouldBe "Red\nGreen\nBlue\n"
    }

    "construct and print fielded variants" in {
      runOut("""
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |  MaxIters(iters: integer, last: real)
        |
        |def main() =
        |  print(Converged(3.14))
        |  print(Diverged)
        |  print(MaxIters(100, 0.5))
      """.stripMargin) shouldBe "Converged(3.14)\nDiverged\nMaxIters(100, 0.5)\n"
    }

    "use the enum name as a binding's type annotation" in {
      runOut("""
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |
        |def main() =
        |  val a: Solver = Converged(2.5)
        |  val b: Solver = Diverged
        |  print(a)
        |  print(b)
      """.stripMargin) shouldBe "Converged(2.5)\nDiverged\n"
    }

    "pass an enum value through a function and return it" in {
      runOut("""
        |enum Step =
        |  Continue(n: integer)
        |  Done
        |
        |def advance(s: Step): Step = s
        |
        |def main() =
        |  print(advance(Continue(7)))
        |  print(advance(Done))
      """.stripMargin) shouldBe "Continue(7)\nDone\n"
    }

    "reject a wrong-arity variant construction at compile time" in {
      // Fielded variants get a `TyFunc` signature; the elaborator's
      // normal arity check catches `Converged(1.0, 2.0)` before the
      // interpreter ever sees it.
      val errs = elaborateExpectingErrors("""
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |
        |def main() = print(Converged(1.0, 2.0))
      """.stripMargin)
      errs.mkString(";") should include("expects 1 args")
    }

    "match dispatches on a bare variant" in {
      runOut("""
        |enum Color =
        |  Red
        |  Green
        |  Blue
        |
        |def label(c: Color): string =
        |  match c
        |    case Red   => "stop"
        |    case Green => "go"
        |    case Blue  => "wait"
        |
        |def main() =
        |  print(label(Red))
        |  print(label(Green))
        |  print(label(Blue))
      """.stripMargin) shouldBe "stop\ngo\nwait\n"
    }

    "match binds variant fields into the arm body" in {
      runOut("""
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |  MaxIters(iters: integer, last: real)
        |
        |def describe(s: Solver): real =
        |  match s
        |    case Converged(x)       => x
        |    case Diverged           => -1.0
        |    case MaxIters(n, last)  => last
        |
        |def main() =
        |  print(describe(Converged(3.14)))
        |  print(describe(Diverged))
        |  print(describe(MaxIters(100, 2.5)))
      """.stripMargin) shouldBe "3.14\n-1.0\n2.5\n"
    }

    "match wildcard catches the remaining variants" in {
      runOut("""
        |enum Color =
        |  Red
        |  Green
        |  Blue
        |
        |def isRed(c: Color): bool =
        |  match c
        |    case Red => true
        |    case _   => false
        |
        |def main() =
        |  print(isRed(Red))
        |  print(isRed(Green))
        |  print(isRed(Blue))
      """.stripMargin) shouldBe "true\nfalse\nfalse\n"
    }

    "match ignores unused fields with `_`" in {
      runOut("""
        |enum Step =
        |  Continue(n: integer)
        |  Halt
        |
        |def kind(s: Step): string =
        |  match s
        |    case Continue(_) => "continue"
        |    case Halt        => "halt"
        |
        |def main() =
        |  print(kind(Continue(99)))
        |  print(kind(Halt))
      """.stripMargin) shouldBe "continue\nhalt\n"
    }

    "match arms unify to a single result type" in {
      runOut("""
        |enum Step =
        |  Continue(n: integer)
        |  Done(total: integer)
        |
        |def main() =
        |  val s: Step = Continue(7)
        |  val v: integer = match s
        |    case Continue(n) => n
        |    case Done(t)     => t
        |  print(v)
      """.stripMargin) shouldBe "7\n"
    }

    "non-exhaustive match without wildcard is a compile error" in {
      val errs = elaborateExpectingErrors("""
        |enum Color =
        |  Red
        |  Green
        |  Blue
        |
        |def main() =
        |  val c: Color = Red
        |  match c
        |    case Red => print("red")
      """.stripMargin)
      errs.mkString(";") should include("non-exhaustive")
    }

    "duplicate match arm is a compile error" in {
      val errs = elaborateExpectingErrors("""
        |enum Color =
        |  Red
        |  Green
        |
        |def main() =
        |  val c: Color = Red
        |  match c
        |    case Red   => print(1)
        |    case Red   => print(2)
        |    case Green => print(3)
      """.stripMargin)
      errs.mkString(";") should include("duplicate")
    }

    "wrong-arity variant pattern is a compile error" in {
      val errs = elaborateExpectingErrors("""
        |enum Solver =
        |  Converged(x: real)
        |  Diverged
        |
        |def main() =
        |  val s: Solver = Diverged
        |  match s
        |    case Converged(a, b) => print(a)
        |    case Diverged        => print(0)
      """.stripMargin)
      errs.mkString(";") should include("expects 1 field")
    }

    "matching on a non-enum is a compile error" in {
      val errs = elaborateExpectingErrors("""
        |def main() =
        |  val n: integer = 3
        |  match n
        |    case x => print(x)
      """.stripMargin)
      errs.mkString(";") should include("must be an enum")
    }
  }
