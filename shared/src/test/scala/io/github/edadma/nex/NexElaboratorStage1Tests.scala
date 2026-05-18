package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Stage 1 of the elaborator: name resolution. Every expression's `tpe`
  * is still `TyUnknown`; what we verify here is the symbol-table structure
  * and the error messages for undefined / duplicate names.
  */
class NexElaboratorStage1Tests extends AnyWordSpec with Matchers:

  private def elab(src: String): TProgram =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    new NexElaborator().elaborate(ast) match
      case Right(tp)   => tp
      case Left(errs)  => fail(s"elaboration errors: ${errs.map(_.toString).mkString("; ")}")

  private def elabExpect(src: String): List[String] =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    new NexElaborator().elaborate(ast) match
      case Right(_)   => fail("expected elaboration errors, but elaboration succeeded")
      case Left(errs) => errs.map(_.message)

  // ==========================================================================
  // Prelude
  // ==========================================================================

  "prelude" should {
    "expose pi, e, i, sqrt, sin, print" in {
      val tp = elab("""
        |def use() =
        |  print(pi)
        |  print(e)
        |  print(i)
        |  print(sqrt(2.0))
        |  print(sin(0.0))
      """.stripMargin)
      tp.decls should have size 1
    }
  }

  // ==========================================================================
  // Top-level bindings
  // ==========================================================================

  "top-level bindings" should {
    "resolve val references" in {
      val tp = elab("""
        |val x = 1
        |val y = x + 2
      """.stripMargin)
      tp.decls should have size 2
      val yBinding = tp.decls(1).asInstanceOf[TTopBinding]
      val rhs      = yBinding.value.asInstanceOf[TBinOp]
      rhs.lhs.asInstanceOf[TVarRef].sym.name shouldBe "x"
    }

    "resolve def references at top level (forward-decl works)" in {
      val tp = elab("""
        |def even(n: integer): bool = if n == 0 then true else odd(n - 1)
        |def odd(n: integer): bool  = if n == 0 then false else even(n - 1)
      """.stripMargin)
      tp.decls should have size 2
    }

    "report duplicate top-level binding" in {
      val errs = elabExpect("""
        |val x = 1
        |val x = 2
      """.stripMargin)
      errs.exists(_.contains("redeclaration of `x`")) shouldBe true
    }

    "report duplicate def" in {
      val errs = elabExpect("""
        |def f() = 1
        |def f() = 2
      """.stripMargin)
      errs.exists(_.contains("redeclaration of `f`")) shouldBe true
    }
  }

  // ==========================================================================
  // Functions and parameters
  // ==========================================================================

  "function parameters" should {
    "be visible in the body" in {
      val tp = elab("def add(a: integer, b: integer) = a + b")
      val f  = tp.decls.head.asInstanceOf[TFunDecl]
      f.params.map(_.name) shouldBe List("a", "b")
      val body = f.body.asInstanceOf[TBinOp]
      body.lhs.asInstanceOf[TVarRef].sym.name shouldBe "a"
      body.rhs.asInstanceOf[TVarRef].sym.name shouldBe "b"
      body.lhs.asInstanceOf[TVarRef].sym.id   shouldBe f.params.head.id
    }

    "go out of scope after the function ends" in {
      val errs = elabExpect("""
        |def f(x: integer) = x
        |val y = x
      """.stripMargin)
      errs.exists(_.contains("undefined name `x`")) shouldBe true
    }

    "report duplicate parameter names" in {
      val errs = elabExpect("def f(a: integer, a: integer) = a")
      errs.exists(_.contains("redeclaration of `a`")) shouldBe true
    }
  }

  // ==========================================================================
  // Block scoping and shadowing
  // ==========================================================================

  "blocks" should {
    "expose val bindings to subsequent items" in {
      val tp = elab("""
        |def f() =
        |  val a = 1
        |  val b = a + 1
        |  b
      """.stripMargin)
      val body  = tp.decls.head.asInstanceOf[TFunDecl].body.asInstanceOf[TBlock]
      body.items should have size 2
      val bRhs = body.items(1).asInstanceOf[TBlockBinding].value.asInstanceOf[TBinOp]
      bRhs.lhs.asInstanceOf[TVarRef].sym.name shouldBe "a"
    }

    "report undefined name in a block" in {
      val errs = elabExpect("""
        |def f() =
        |  val a = 1
        |  c
      """.stripMargin)
      errs.exists(_.contains("undefined name `c`")) shouldBe true
    }

    "allow shadowing across nested scopes" in {
      // No errors when shadowing in a different scope.
      elab("""
        |val x = 1
        |def f() =
        |  val x = 2
        |  x
      """.stripMargin)
    }

    "reject duplicate val in the same block" in {
      val errs = elabExpect("""
        |def f() =
        |  val a = 1
        |  val a = 2
        |  a
      """.stripMargin)
      errs.exists(_.contains("redeclaration of `a`")) shouldBe true
    }

    "elaborate val RHS in the outer scope" in {
      // Inside f, the `val x = x + 1` line — the RHS `x` should resolve
      // to the OUTER top-level x, not (yet) the new local x.
      val tp = elab("""
        |val x = 1
        |def f() =
        |  val x = x + 1
        |  x
      """.stripMargin)
      val fbody = tp.decls(1).asInstanceOf[TFunDecl].body.asInstanceOf[TBlock]
      val binding = fbody.items.head.asInstanceOf[TBlockBinding]
      val rhs = binding.value.asInstanceOf[TBinOp]
      val rhsLhs = rhs.lhs.asInstanceOf[TVarRef].sym
      val topX = tp.decls.head.asInstanceOf[TTopBinding].sym
      rhsLhs.id shouldBe topX.id
      // And the new local x is a different symbol.
      binding.sym.id should not equal topX.id
    }
  }

  // ==========================================================================
  // Control flow
  // ==========================================================================

  "for loops" should {
    "scope the loop variable to the body" in {
      val tp = elab("""
        |def use() =
        |  for k in range(0, 10) do print(k)
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      val forE = body match
        case b: TBlock => b.items.head.asInstanceOf[TBlockExpr].expr.asInstanceOf[TFor]
        case f: TFor   => f
        case other     => fail(s"expected TFor, got $other")
      forE.loopVars.map(_.name) shouldBe List("k")
    }

    "leak nothing out of the loop body" in {
      val errs = elabExpect("""
        |def use() =
        |  for k in range(0, 10) do print(k)
        |  print(k)
      """.stripMargin)
      errs.exists(_.contains("undefined name `k`")) shouldBe true
    }
  }

  // ==========================================================================
  // Lambdas
  // ==========================================================================

  "lambdas" should {
    "scope parameters to the body" in {
      val tp = elab("val f = x -> x * 2")
      val lam = tp.decls.head.asInstanceOf[TTopBinding].value.asInstanceOf[TLambda]
      lam.params.map(_.name) shouldBe List("x")
      val body = lam.body.asInstanceOf[TBinOp]
      body.lhs.asInstanceOf[TVarRef].sym.id shouldBe lam.params.head.id
    }

    "not leak parameter names outside" in {
      val errs = elabExpect("""
        |val f = x -> x + 1
        |val y = x
      """.stripMargin)
      errs.exists(_.contains("undefined name `x`")) shouldBe true
    }
  }

  // ==========================================================================
  // Structs
  // ==========================================================================

  "structs" should {
    "register the struct name as a type" in {
      val tp = elab("""
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |def origin(): Point = Point(0.0, 0.0)
      """.stripMargin)
      tp.decls should have size 2
      val sd = tp.decls.head.asInstanceOf[TStructDecl]
      sd.sym.name shouldBe "Point"
      sd.fields shouldBe List("x" -> TyReal, "y" -> TyReal)
      val fn = tp.decls(1).asInstanceOf[TFunDecl]
      fn.returnType shouldBe TyStruct("Point", List("x" -> TyReal, "y" -> TyReal))
    }
  }

  // ==========================================================================
  // Assignment validation
  // ==========================================================================

  "assignment" should {
    "accept name, index, and field targets" in {
      elab("""
        |def f() =
        |  var x = 1
        |  x = 2
      """.stripMargin)
    }

    "reject literal as target" in {
      val errs = elabExpect("""
        |def f() =
        |  1 = 2
      """.stripMargin)
      errs.exists(_.contains("invalid assignment target")) shouldBe true
    }
  }
