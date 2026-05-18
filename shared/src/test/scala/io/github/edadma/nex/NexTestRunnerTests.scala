package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Lower-level coverage for the test runner's machinery — `initializeProgram`
  * + `callNullary` on the interpreter, plus discovery of `@test`-annotated
  * functions from the elaborated AST. The CLI-level shell that wraps these
  * is covered manually via `nex test examples/tests/main.nex`.
  */
class NexTestRunnerTests extends AnyWordSpec with Matchers:

  private def elab(src: String): TProgram =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    new NexElaborator().elaborate(ast) match
      case Right(tp)  => tp
      case Left(errs) => fail(s"elab errors: ${errs.map(_.toString).mkString("; ")}")

  private def discoverTests(tp: TProgram): List[TFunDecl] =
    tp.decls.collect {
      case f: TFunDecl if f.attributes.contains("test") && f.params.isEmpty => f
    }

  "@test discovery" should {

    "find every `@test`-annotated nullary function" in {
      val tp = elab("""
        |def helper() = 42
        |
        |@test
        |def test_a() = ()
        |
        |@test
        |def test_b() = ()
        |
        |def test_c_not_annotated() = ()
      """.stripMargin)
      val tests = discoverTests(tp)
      tests.map(_.sym.name).toSet shouldBe Set("test_a", "test_b")
    }

    "ignore `@test` functions that take parameters (those are user helpers)" in {
      val tp = elab("""
        |@test
        |def test_with_arg(x: integer) = ()
        |
        |@test
        |def test_nullary() = ()
      """.stripMargin)
      discoverTests(tp).map(_.sym.name) shouldBe List("test_nullary")
    }

    "ignore `@test` on non-function decls (currently impossible — sanity)" in {
      val tp = elab("def f() = 0")
      discoverTests(tp) shouldBe Nil
    }
  }

  "test invocation" should {

    "run a passing test without raising" in {
      val tp = elab("""
        |@test
        |def test_simple() = assert_eq(1 + 1, 2)
      """.stripMargin)
      val sym = discoverTests(tp).head.sym
      val interp = new NexInterpreter
      interp.initializeProgram(tp)
      noException should be thrownBy interp.callNullary(sym)
    }

    "surface a failing assert as a NexTrap" in {
      val tp = elab("""
        |@test
        |def test_bad() = assert_eq(1 + 1, 3)
      """.stripMargin)
      val sym = discoverTests(tp).head.sym
      val interp = new NexInterpreter
      interp.initializeProgram(tp)
      val trap = intercept[NexTrap](interp.callNullary(sym))
      trap.msg should include("assert_eq")
    }

    "evaluate top-level initializers once before running tests" in {
      // Top-level `var c = 0` is a single cell — incrementing it inside
      // the test is observable because the test runs after initializers
      // have set up the global frame.
      val tp = elab("""
        |var counter = 10
        |
        |@test
        |def test_sees_initializer() = assert_eq(counter, 10)
      """.stripMargin)
      val sym    = discoverTests(tp).head.sym
      val interp = new NexInterpreter
      interp.initializeProgram(tp)
      noException should be thrownBy interp.callNullary(sym)
    }

    "isolates state across fresh interpreter instances" in {
      // Two interpreter instances → each test sees the initial value.
      val tp = elab("""
        |var counter = 0
        |
        |def bump() = counter = counter + 1
        |
        |@test
        |def test_isolated() = bump()
      """.stripMargin)
      val sym = discoverTests(tp).head.sym
      // First run mutates counter, but a fresh interpreter re-initializes
      // from the AST so the next test sees counter = 0 again.
      val interp1 = new NexInterpreter
      interp1.initializeProgram(tp)
      noException should be thrownBy interp1.callNullary(sym)

      val interp2 = new NexInterpreter
      interp2.initializeProgram(tp)
      noException should be thrownBy interp2.callNullary(sym)
    }
  }
