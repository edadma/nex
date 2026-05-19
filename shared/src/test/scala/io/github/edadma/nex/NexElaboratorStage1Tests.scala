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

    "accept rank-1 slice target (spec §4.14 lvalue form)" in {
      elab("""
        |def f() =
        |  var xs = [1, 2, 3, 4]
        |  xs[0..2] = [10, 20]
      """.stripMargin)
    }

    "accept rank-2 slice target with range axis" in {
      elab("""
        |def f() =
        |  var m = [[1, 2], [3, 4]]
        |  m[0..1, :] = [[10, 20]]
      """.stripMargin)
    }

    "accept rank-2 slice target with axis-all" in {
      elab("""
        |def f() =
        |  var m = [[1, 2], [3, 4]]
        |  m[:, 0] = [9, 9]
      """.stripMargin)
    }
  }

  // ==========================================================================
  // `${...}` Stage-1 re-parse — verifies parts shape, not types
  // ==========================================================================

  "${...} interpolation" should {

    "build a TInterpStringLit with a TInterpExpr part for `${expr}`" in {
      val tp = elab("""
        |def main() =
        |  val a = 1
        |  print(s"x = ${a + 1}")
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      val print = body.asInstanceOf[TBlock].result.asInstanceOf[TCall]
      val interp = print.args.head.asInstanceOf[TInterpStringLit]
      // Should contain: TInterpText("x = "), TInterpExpr(TBinOp("+", a, 1))
      interp.parts.size shouldBe 2
      interp.parts.head shouldBe a[TInterpText]
      val exprPart = interp.parts(1).asInstanceOf[TInterpExpr]
      exprPart.expr shouldBe a[TBinOp]
      exprPart.expr.asInstanceOf[TBinOp].op shouldBe "+"
    }

    "still build a TInterpRef for the legacy `$ident` form" in {
      val tp = elab("""
        |def main() =
        |  val n = 5
        |  print(s"n = $n")
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      val print = body.asInstanceOf[TBlock].result.asInstanceOf[TCall]
      val interp = print.args.head.asInstanceOf[TInterpStringLit]
      interp.parts.collect { case r: TInterpRef => r.sym.name } shouldBe List("n")
    }

    "report `failed to parse interpolated expression` for a malformed `${...}`" in {
      val errs = elabExpect("""
        |def main() =
        |  print(s"bad = ${1 + }")
      """.stripMargin)
      errs.exists(_.contains("failed to parse interpolated expression")) shouldBe true
    }
  }

  // ==========================================================================
  // Tuple destructuring — Stage-1 binding shape
  // ==========================================================================

  "tuple destructuring (Stage 1 shape)" should {

    "expand a top-level `val a, b = (1, 2)` to temp + 2 projection bindings" in {
      val tp = elab("val a, b = (1, 2)")
      // Should produce 3 TTopBindings: $tuple, a, b.
      tp.decls should have size 3
      val temp = tp.decls(0).asInstanceOf[TTopBinding]
      val aDecl = tp.decls(1).asInstanceOf[TTopBinding]
      val bDecl = tp.decls(2).asInstanceOf[TTopBinding]
      temp.sym.name shouldBe "$tuple"
      temp.value shouldBe a[TTuple]
      aDecl.sym.name shouldBe "a"
      aDecl.value shouldBe a[TTupleProj]
      aDecl.value.asInstanceOf[TTupleProj].idx shouldBe 0
      bDecl.value.asInstanceOf[TTupleProj].idx shouldBe 1
      // Both projections receive from the temp.
      aDecl.value.asInstanceOf[TTupleProj].receiver.asInstanceOf[TVarRef].sym.id shouldBe temp.sym.id
    }

    "expand a block-level `val a, b = pair` to TBlockBindings of the same shape" in {
      val tp = elab("""
        |def main() =
        |  val pair = (10, 20)
        |  val a, b = pair
        |  print(a)
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body.asInstanceOf[TBlock]
      // items: [pair=..., $tuple=pair, a=$tuple.0, b=$tuple.1]
      val items = body.items
      items should have size 4
      items.map {
        case TBlockBinding(s, _, _) => s.name
        case _                      => "(other)"
      } shouldBe List("pair", "$tuple", "a", "b")
    }

    "the wildcard `_` in a tuple pattern still introduces a binding" in {
      // The wildcard slot still produces a TTopBinding (for the temp it
      // can never be referenced) so projections stay one-to-one with the
      // tuple value's elements.
      val tp = elab("val _, b = (99, 7)")
      tp.decls should have size 3
      tp.decls(1).asInstanceOf[TTopBinding].sym.name shouldBe "_"
      tp.decls(2).asInstanceOf[TTopBinding].sym.name shouldBe "b"
    }
  }

  // ==========================================================================
  // Modules + imports
  // ==========================================================================

  "modules" should {

    "accept a leading module declaration and carry its path into TProgram" in {
      val tp = elab("""
        |module foo.bar
        |val x = 1
      """.stripMargin)
      tp.modulePath shouldBe List("foo", "bar")
    }

    "default to an empty modulePath when no module declaration is present" in {
      val tp = elab("val x = 1")
      tp.modulePath shouldBe Nil
    }

    "report `module declaration must come first` when out of place" in {
      val errs = elabExpect("""
        |val x = 1
        |module foo.bar
      """.stripMargin)
      errs.exists(_.contains("module declaration must come first")) shouldBe true
    }
  }

  "imports" should {

    "register each imported selector as a symbol in scope" in {
      val tp = elab("""
        |import some.lib.{a, b as bAlias}
        |val use = a
      """.stripMargin)
      val imp = tp.decls.collectFirst { case i: TImportDecl => i }.get
      imp.path shouldBe List("some", "lib")
      imp.selectors.map { case (s, alias) => (s.name, alias) } shouldBe List(
        "a" -> None,
        "bAlias" -> Some("bAlias"),
      )
    }

    "produce a TImportDecl with an empty selector list when no `.{...}` is given" in {
      val tp = elab("import some.lib")
      val imp = tp.decls.head.asInstanceOf[TImportDecl]
      imp.path shouldBe List("some", "lib")
      imp.selectors shouldBe Nil
    }
  }

  // ==========================================================================
  // Type-annotation diagnostics
  // ==========================================================================

  "type-annotation diagnostics" should {

    "reject an unknown type name" in {
      val errs = elabExpect("val x: Whatever = 1")
      errs.exists(_.contains("unknown type")) shouldBe true
    }

    "reject a non-tuple type annotation on a tuple-destructuring binding" in {
      // Spec §4.16 allows tuple-typed annotations on tuple-destructuring
      // bindings: `val (a, b): (integer, real) = 1, 2.0`. A scalar
      // annotation like `: integer` doesn't match the pattern's shape;
      // the elaborator should error with a tuple-type message rather
      // than silently accepting it.
      val errs = elabExpect("val a, b: integer = (1, 2)")
      errs.exists(_.contains("tuple type annotation")) shouldBe true
    }

    "accept a tuple type annotation matching the destructuring shape (spec §4.16)" in {
      val tp = elab("val (a, b): (integer, real) = (1, 2.0)")
      val aBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "a").get
      val bBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "b").get
      aBind.sym.tpe shouldBe TyInteger
      bBind.sym.tpe shouldBe TyReal
    }

    "reject a tuple type annotation whose arity doesn't match the pattern" in {
      val errs = elabExpect("val (a, b): (integer, real, integer) = (1, 2.0, 3)")
      errs.exists(_.contains("3 elements but the pattern binds 2")) shouldBe true
    }
  }

  // ==========================================================================
  // Stage 0 — `@intrinsic` declarations
  // ==========================================================================

  "intrinsic declarations" should {

    "produce a TFunDecl whose body is TIntrinsic carrying the opId" in {
      val tp = elab("""
        |@intrinsic("test.identity")
        |def identity(x: integer): integer
      """.stripMargin)
      val fn = tp.decls.head.asInstanceOf[TFunDecl]
      fn.sym.name shouldBe "identity"
      fn.params should have size 1
      fn.params.head.name shouldBe "x"
      fn.params.head.tpe shouldBe TyInteger
      fn.body shouldBe a [TIntrinsic]
      fn.body.asInstanceOf[TIntrinsic].opId shouldBe "test.identity"
    }

    "elaborate a real-typed intrinsic and preserve its return type" in {
      val tp = elab("""
        |@intrinsic("libm.cbrt")
        |def cbrt(x: real): real
      """.stripMargin)
      val fn = tp.decls.head.asInstanceOf[TFunDecl]
      fn.returnType shouldBe TyReal
      fn.body.asInstanceOf[TIntrinsic].opId shouldBe "libm.cbrt"
    }

    "reject a bodyless def without an @intrinsic attribute" in {
      val errs = elabExpect("def mystery(x: integer): integer")
      errs.exists(_.contains("requires @intrinsic")) shouldBe true
    }

    "reject an @intrinsic decl that also has a body" in {
      val errs = elabExpect("""
        |@intrinsic("test.id")
        |def id(x: integer): integer = x
      """.stripMargin)
      errs.exists(_.contains("must not have a body")) shouldBe true
    }

    "reject an @intrinsic with the wrong arity" in {
      val errs = elabExpect("""
        |@intrinsic()
        |def foo(x: integer): integer
      """.stripMargin)
      errs.exists(_.contains("exactly one string argument")) shouldBe true
    }
  }
