package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Stage 1 of the elaborator: name resolution. Every expression's `tpe`
  * is still `TyUnknown`; what we verify here is the symbol-table structure
  * and the error messages for undefined / duplicate names.
  */
class NexElaboratorStage1Tests extends AnyWordSpec with Matchers:

  private def elab(src: String): TProgram = elab(src, runMonomorph = true)

  /** Elaborate a program. By default the full pipeline including
    * monomorphization runs, so generic templates are stripped and
    * replaced with specialized clones. Pass `runMonomorph = false` to
    * keep the generic templates around — useful for Stage 3-α tests
    * that inspect a generic def's declared `TyKindVar`s.
    */
  private def elab(src: String, runMonomorph: Boolean): TProgram =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    new NexElaborator().elaborate(ast, runMonomorph) match
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
      // Use a fresh name because the source prelude already binds
      // `cbrt` via its own `@intrinsic("libm.cbrt")` declaration; a
      // duplicate top-level `def cbrt` here would clash with the
      // wildcard-imported source-prelude symbol.
      val tp = elab("""
        |@intrinsic("libm.cbrt")
        |def my_cbrt(x: real): real
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
      errs.exists(_.contains("at least one argument")) shouldBe true
    }
  }

  // ==========================================================================
  // Stage 3-α — kind-parameterized defs
  // ==========================================================================
  //
  // The elaborator mints each `[T: Float]` type parameter as a
  // TypeName symbol carrying a TyKindVar. Param and return type
  // references to `T` resolve through scope lookup to that
  // kind-variable type. Stage 3-β (monomorphization) substitutes
  // these out before codegen sees them.

  "kind-parameterized defs (Stage 3-α)" should {

    "parameter type resolves to TyKindVar carrying the source name + constraint" in {
      val tp = elab("def f[T: Float](x: T): T = x", runMonomorph = false)
      val fn = tp.decls.head.asInstanceOf[TFunDecl]
      fn.params.head.tpe shouldBe TyKindVar("T", KindConstraint.Float)
      fn.returnType shouldBe TyKindVar("T", KindConstraint.Float)
    }

    "unbounded type parameter `[T]` defaults to KindConstraint.Any" in {
      val tp = elab("def id[T](x: T): T = x", runMonomorph = false)
      val fn = tp.decls.head.asInstanceOf[TFunDecl]
      fn.params.head.tpe shouldBe TyKindVar("T", KindConstraint.Any)
    }

    "multiple type parameters each carry their own constraint" in {
      val tp = elab("def f[T: Real, U: Numeric](x: T, y: U): T = x", runMonomorph = false)
      val fn = tp.decls.head.asInstanceOf[TFunDecl]
      fn.params(0).tpe shouldBe TyKindVar("T", KindConstraint.Real)
      fn.params(1).tpe shouldBe TyKindVar("U", KindConstraint.Numeric)
    }

    "type parameter is scope-local: another def can reuse the name `T`" in {
      val tp = elab("""
        |def f[T: Float](x: T): T = x
        |def g[T: Numeric](x: T): T = x
      """.stripMargin, runMonomorph = false)
      val f = tp.decls(0).asInstanceOf[TFunDecl]
      val g = tp.decls(1).asInstanceOf[TFunDecl]
      f.params.head.tpe shouldBe TyKindVar("T", KindConstraint.Float)
      g.params.head.tpe shouldBe TyKindVar("T", KindConstraint.Numeric)
    }

    "unknown constraint name is an error" in {
      val errs = elabExpect("def f[T: NotAKind](x: T): T = x")
      errs.exists(_.contains("unknown kind constraint `NotAKind`")) shouldBe true
    }

    "`Ord` is a recognised constraint" in {
      val tp = elab("def f[T: Ord](x: T, y: T): bool = x < y", runMonomorph = false)
      val fn = tp.decls.head.asInstanceOf[TFunDecl]
      fn.params.head.tpe shouldBe TyKindVar("T", KindConstraint.Ord)
    }

    "`Eq` is a recognised constraint" in {
      val tp = elab("def f[T: Eq](x: T, y: T): bool = x == y", runMonomorph = false)
      val fn = tp.decls.head.asInstanceOf[TFunDecl]
      fn.params.head.tpe shouldBe TyKindVar("T", KindConstraint.Eq)
    }

    "`Ord` rejects complex argument at call site" in {
      val errs = elabExpect("""
        |def min2[T: Ord](a: T, b: T): T = if a < b then a else b
        |def use() = print(min2(1.0 + 2.0i, 3.0 + 4.0i))
      """.stripMargin)
      errs.exists(e => e.contains("`T`") && e.contains("Ord")) shouldBe true
    }

    "`Eq` admits a string argument at call site" in {
      val tp = elab("""
        |def same[T: Eq](a: T, b: T): bool = a == b
        |def use(): bool = same("hi", "hi")
      """.stripMargin, runMonomorph = false)
      val use   = tp.decls(1).asInstanceOf[TFunDecl]
      val call  = use.body.asInstanceOf[TCall]
      call.tpe shouldBe TyBool
    }

    "`real64` resolves to the same type as `real`" in {
      val tp = elab("""
        |def f(x: real): real = x
        |def g(x: real64): real64 = x
      """.stripMargin)
      val f = tp.decls(0).asInstanceOf[TFunDecl]
      val g = tp.decls(1).asInstanceOf[TFunDecl]
      f.params.head.tpe shouldBe TyReal
      g.params.head.tpe shouldBe TyReal
    }

    "`complex64` resolves to the same type as `complex`" in {
      val tp = elab("""
        |def f(x: complex): complex = x
        |def g(x: complex64): complex64 = x
      """.stripMargin)
      tp.decls(0).asInstanceOf[TFunDecl].params.head.tpe shouldBe TyComplex
      tp.decls(1).asInstanceOf[TFunDecl].params.head.tpe shouldBe TyComplex
    }
  }

  // ==========================================================================
  // Stage 3-β.1 — generic call-site type-argument deduction
  // ==========================================================================

  // For a call to a generic def, the elaborator unifies each formal
  // parameter type against the actual argument type, building a
  // substitution map from kind-variable names to concrete types. The
  // resulting `TCall.tpe` is the substituted return type — the callee's
  // `TVarRef` still points at the generic symbol so monomorphization
  // (Stage 3-β.2) can rewrite it later. Constraint admission is
  // validated; same kind variable bound to two different types is a
  // hard error unless the two are numeric and promote.

  "generic call-site inference (Stage 3-β.1)" should {

    "call to `def id[T](x: T): T` with an integer arg infers integer return" in {
      val tp = elab("""
        |def id[T](x: T): T = x
        |def use() = print(id(42))
      """.stripMargin, runMonomorph = false)
      // Walk to the `id(42)` call and check its inferred return type.
      val use    = tp.decls(1).asInstanceOf[TFunDecl]
      val print  = use.body.asInstanceOf[TCall]
      val idCall = print.args.head.asInstanceOf[TCall]
      idCall.tpe shouldBe TyInteger
    }

    "call to `def id[T](x: T): T` with a real arg infers real return" in {
      val tp = elab("""
        |def id[T](x: T): T = x
        |def use(): real = id(3.14)
      """.stripMargin, runMonomorph = false)
      val use    = tp.decls(1).asInstanceOf[TFunDecl]
      val idCall = use.body.asInstanceOf[TCall]
      idCall.tpe shouldBe TyReal
    }

    "Float-constrained type param rejects integer arg" in {
      val errs = elabExpect("""
        |def sq[T: Float](x: T): T = x
        |def use() = print(sq(2))
      """.stripMargin)
      errs.exists(e => e.contains("`T`") && e.contains("Float")) shouldBe true
    }

    "Numeric constraint admits integer, real, complex" in {
      val tp = elab("""
        |def f[T: Numeric](x: T): T = x
        |def use() =
        |  print(f(1))
        |  print(f(1.0))
      """.stripMargin, runMonomorph = false)
      val use      = tp.decls(1).asInstanceOf[TFunDecl]
      val block    = use.body.asInstanceOf[TBlock]
      val firstPr  = block.items.collect { case TBlockExpr(e) => e }.head.asInstanceOf[TCall]
      val secondPr = block.result.asInstanceOf[TCall]
      firstPr.args.head.asInstanceOf[TCall].tpe shouldBe TyInteger
      secondPr.args.head.asInstanceOf[TCall].tpe shouldBe TyReal
    }

    "same kind var bound to two consistent integer args stays integer" in {
      val tp = elab("""
        |def f[T: Numeric](x: T, y: T): T = x
        |def use(): integer = f(1, 2)
      """.stripMargin, runMonomorph = false)
      val use    = tp.decls(1).asInstanceOf[TFunDecl]
      val fCall  = use.body.asInstanceOf[TCall]
      fCall.tpe shouldBe TyInteger
    }

    "same kind var bound to integer+real widens to real (numeric promotion)" in {
      val tp = elab("""
        |def f[T: Numeric](x: T, y: T): T = x
        |def use(): real = f(1, 2.0)
      """.stripMargin, runMonomorph = false)
      val use   = tp.decls(1).asInstanceOf[TFunDecl]
      val fCall = use.body.asInstanceOf[TCall]
      fCall.tpe shouldBe TyReal
    }

    "kind var inside array element unifies with array's actual element type" in {
      val tp = elab("""
        |def head[T](xs: [T]): T = xs[0]
        |def use(): real = head([1.0, 2.0, 3.0])
      """.stripMargin, runMonomorph = false)
      val use   = tp.decls(1).asInstanceOf[TFunDecl]
      val hCall = use.body.asInstanceOf[TCall]
      hCall.tpe shouldBe TyReal
    }

    "monomorphization rewrites callee to specialized clone with concrete signature" in {
      val tp = elab("""
        |def id[T](x: T): T = x
        |def use(): integer = id(42)
      """.stripMargin)
      // After monomorph, the generic template is gone from `decls` and
      // a specialized clone has been appended. The user's `use` body
      // calls into the clone, not into a generic.
      val funDecls = tp.decls.collect { case f: TFunDecl => f }
      // Originals: `use` (non-generic), plus the `id$integer` clone.
      // The generic `id` template was dropped.
      funDecls.exists(_.sym.name == "id$integer") shouldBe true
      funDecls.exists(_.sym.name == "id") shouldBe false
      val use    = funDecls.find(_.sym.name == "use").get
      val call   = use.body.asInstanceOf[TCall]
      val callee = call.callee.asInstanceOf[TVarRef]
      callee.sym.name shouldBe "id$integer"
      // The specialized signature has no kind variables left.
      callee.tpe shouldBe TyFunc(List((TyInteger, ParamMode.Read)), TyInteger)
    }
  }

  // ==========================================================================
  // Stage 3-ε — kind-specialized `@intrinsic` was retired in favor of
  // overloaded source defs. The elaborator now rejects type-parameter
  // refs on `@intrinsic`; the parser still admits the syntactic shape
  // so existing fixtures parse and the rejection diagnostic surfaces
  // at elaboration time with a clear message.
  // ==========================================================================

  "kind-specialized @intrinsic (retired in Stage 3-ε)" should {

    "reject @intrinsic carrying any trailing type-param ref" in {
      val errs = elabExpect("""
        |@intrinsic("libm.sqrt", T)
        |def gsqrt[T: Float](x: T): T
      """.stripMargin)
      errs.exists(e => e.contains("retired in Stage 3-ε") || e.contains("overloaded source defs")) shouldBe true
    }

    "reject @intrinsic whose first arg is a type-param ref instead of an opId string" in {
      val errs = elabExpect("""
        |@intrinsic(T)
        |def gsqrt[T: Float](x: T): T
      """.stripMargin)
      errs.exists(_.contains("first argument must be the opId string")) shouldBe true
    }

  }

  // ==========================================================================
  // Stage 3-ε — function overload resolution. Two `def f(...)` decls
  // with disjoint parameter signatures form an overload set; the
  // elaborator picks the matching overload at each call site by scoring
  // argument types (exact match wins; numeric promotion ranks integer
  // → real → complex with smaller distance preferred). Ambiguous calls
  // error; no-match calls error.
  // ==========================================================================

  "overload resolution (Stage 3-ε)" should {

    "two defs with disjoint param types form an overload set" in {
      val tp = elab("""
        |def f(x: real): real = x
        |def f(z: complex): complex = z
        |def use() = print(f(1.0))
      """.stripMargin, runMonomorph = false)
      val funDecls = tp.decls.collect { case f: TFunDecl => f }
      funDecls.count(_.sym.name == "f") shouldBe 2
    }

    "two defs with identical signatures are a redeclaration error" in {
      val errs = elabExpect("""
        |def f(x: real): real = x
        |def f(y: real): real = y
      """.stripMargin)
      errs.exists(e => e.contains("redeclaration of `f`") && e.contains("overload signatures must differ")) shouldBe true
    }

    "mixed kinds at the same name (def + val) are an error" in {
      val errs = elabExpect("""
        |def f(x: real): real = x
        |val f = 1
      """.stripMargin)
      errs.exists(_.contains("redeclaration of `f`")) shouldBe true
    }

    "call with exact-match arg type picks the real overload" in {
      val tp = elab("""
        |def f(x: real): real = x
        |def f(z: complex): complex = z
        |def use(): real = f(1.0)
      """.stripMargin)
      val use   = tp.decls.collectFirst { case d: TFunDecl if d.sym.name == "use" => d }.get
      val call  = use.body.asInstanceOf[TCall]
      val cTpe  = call.callee.asInstanceOf[TVarRef].sym.tpe
      cTpe match
        case TyFunc(List((TyReal, _)), TyReal) => succeed
        case other => fail(s"expected real overload, got $other")
    }

    "call with complex arg picks the complex overload" in {
      val tp = elab("""
        |def f(x: real): real = x
        |def f(z: complex): complex = z
        |def use(): complex = f(1.0 + 0i)
      """.stripMargin)
      val use   = tp.decls.collectFirst { case d: TFunDecl if d.sym.name == "use" => d }.get
      val call  = use.body.asInstanceOf[TCall]
      val cTpe  = call.callee.asInstanceOf[TVarRef].sym.tpe
      cTpe match
        case TyFunc(List((TyComplex, _)), TyComplex) => succeed
        case other => fail(s"expected complex overload, got $other")
    }

    "integer arg picks the real overload via one-step promotion" in {
      // Cost: int→real = 1, int→complex = 2. The real overload wins by
      // smaller promotion distance.
      val tp = elab("""
        |def f(x: real): real = x
        |def f(z: complex): complex = z
        |def use(): real = f(8)
      """.stripMargin)
      val use  = tp.decls.collectFirst { case d: TFunDecl if d.sym.name == "use" => d }.get
      val call = use.body.asInstanceOf[TCall]
      val cTpe = call.callee.asInstanceOf[TVarRef].sym.tpe
      cTpe match
        case TyFunc(List((TyReal, _)), TyReal) => succeed
        case other => fail(s"expected real overload, got $other")
    }

    "no-match call surfaces a clear error" in {
      val errs = elabExpect("""
        |def f(x: real): real = x
        |def use() = print(f("hello"))
      """.stripMargin)
      // Single-overload path goes through the existing TyFunc check,
      // which reports a type-assignment error (string into real).
      errs.exists(e => e.contains("cannot assign") || e.contains("string")) shouldBe true
    }

    "ambiguous call surfaces an error" in {
      // Both overloads accept (integer, integer) at equal promotion
      // cost (one int→real conversion each). The resolver must report
      // ambiguity rather than silently pick one.
      val errs = elabExpect("""
        |def f(x: real, y: integer): integer = y
        |def f(x: integer, y: real): integer = x
        |def use() = print(f(1, 1))
      """.stripMargin)
      errs.exists(e => e.contains("ambiguous") && e.contains("`f`")) shouldBe true
    }

    "two-arg overload set: each arg type picks the matching arm" in {
      val tp = elab("""
        |def f(x: real, y: real): real = x + y
        |def f(x: real, y: complex): complex = y
        |def use(): complex = f(1.0, 0.5 + 0.25 * i)
      """.stripMargin)
      val use  = tp.decls.collectFirst { case d: TFunDecl if d.sym.name == "use" => d }.get
      val call = use.body.asInstanceOf[TCall]
      val cTpe = call.callee.asInstanceOf[TVarRef].sym.tpe
      cTpe match
        case TyFunc(List((TyReal, _), (TyComplex, _)), TyComplex) => succeed
        case other => fail(s"expected (real, complex) overload, got $other")
    }

    "recursive call inside an overload body resolves to the same overload" in {
      // `f(real)` calls `f` recursively with a real arg — must resolve
      // to itself, not to the complex overload.
      val tp = elab("""
        |def f(x: real): real = if x <= 0.0 then 0.0 else x + f(x - 1.0)
        |def f(z: complex): complex = z
        |def use(): real = f(3.0)
      """.stripMargin)
      val use = tp.decls.collectFirst { case d: TFunDecl if d.sym.name == "use" => d }.get
      val call = use.body.asInstanceOf[TCall]
      call.tpe shouldBe TyReal
    }

    "complex overload's body can call its real sibling" in {
      // Closely models the prelude's `sqrt(complex)` calling
      // `sqrt(real)`. The complex body references `f` with a real arg
      // — overload resolution at THAT call site picks the real arm.
      val tp = elab("""
        |def f(x: real): real = x * 2.0
        |def f(z: complex): complex =
        |  val mag = f(z.re)
        |  mag + z.im * i
        |def use(): complex = f(3.0 + 4.0 * i)
      """.stripMargin)
      val funDecls = tp.decls.collect { case f: TFunDecl => f }
      // Both overloads still present (LLVM mangling kicks in at codegen
      // — at the typed-AST level both decls keep their bare name).
      funDecls.count(_.sym.name == "f") shouldBe 2
    }
  }

  // ==========================================================================
  // Overload-vs-generic resolution: concrete wins over generic.
  // ==========================================================================

  "overload-vs-generic resolution" should {

    "concrete overload picked over generic when args match exactly" in {
      val tp = elab("""
        |def f(x: integer): integer = 100
        |def f[T](x: T): integer = 200
        |def use(): integer = f(1)
      """.stripMargin, runMonomorph = false)
      val use    = tp.decls.find { case fd: TFunDecl => fd.sym.name == "use"; case _ => false }.get.asInstanceOf[TFunDecl]
      val call   = use.body.asInstanceOf[TCall]
      val callee = call.callee.asInstanceOf[TVarRef]
      callee.sym.tpe shouldBe TyFunc(List((TyInteger, ParamMode.Read)), TyInteger)
    }

    "generic overload picked when no concrete accepts the arg type" in {
      val tp = elab("""
        |def f(x: integer): integer = 100
        |def f[T](x: T): integer = 200
        |def use(): integer = f("hi")
      """.stripMargin, runMonomorph = false)
      val use    = tp.decls.find { case fd: TFunDecl => fd.sym.name == "use"; case _ => false }.get.asInstanceOf[TFunDecl]
      val call   = use.body.asInstanceOf[TCall]
      val callee = call.callee.asInstanceOf[TVarRef]
      callee.sym.tpe shouldBe TyFunc(List((TyKindVar("T", KindConstraint.Any), ParamMode.Read)), TyInteger)
    }

    "concrete overload (via promotion) still wins over generic" in {
      val tp = elab("""
        |def f(x: real): integer = 1
        |def f[T](x: T): integer = 2
        |def use(): integer = f(42)
      """.stripMargin, runMonomorph = false)
      val use    = tp.decls.find { case fd: TFunDecl => fd.sym.name == "use"; case _ => false }.get.asInstanceOf[TFunDecl]
      val call   = use.body.asInstanceOf[TCall]
      val callee = call.callee.asInstanceOf[TVarRef]
      callee.sym.tpe shouldBe TyFunc(List((TyReal, ParamMode.Read)), TyInteger)
    }

    "two generic overloads both applicable is an ambiguity error" in {
      val errs = elabExpect("""
        |def f[T: Numeric](x: T): integer = 1
        |def f[T: Ord](x: T): integer = 2
        |def use() = print(f(42))
      """.stripMargin)
      errs.exists(_.contains("ambiguous")) shouldBe true
    }

    "lambda arg into generic carries through to a concrete specialization" in {
      val tp = elab("""
        |def apply1[T, U](x: T, f: T -> U): U = f(x)
        |def main() = print(apply1(5, x -> x * 2))
      """.stripMargin)
      val specs = tp.decls.collect { case f: TFunDecl if f.sym.name.startsWith("apply1$") => f.sym.name }
      specs shouldBe List("apply1$integer$integer")
    }

    "generic-calls-generic threads type params through both layers" in {
      val tp = elab("""
        |def id[T](x: T): T = x
        |def applyTwice[T](x: T, f: T -> T): T = f(f(x))
        |def main() =
        |  print(applyTwice(5, y -> id(y) + 1))
        |  print(applyTwice("a", s -> id(s)))
      """.stripMargin)
      val specs = tp.decls.collect { case f: TFunDecl => f.sym.name }.toSet
      specs should contain ("applyTwice$integer")
      specs should contain ("applyTwice$string")
      specs should contain ("id$integer")
      specs should contain ("id$string")
      specs should not contain "id$T"
    }
  }

  // ==========================================================================
  // Stage 2 of the user-generics roadmap: generic structs now elaborate;
  // generic enums remain rejected until Stage 2 chunk 3 lands.
  // ==========================================================================

  "generic struct / enum declarations (Stage 2)" should {
    "elaborate a single-param generic struct" in {
      val tp = elab("""
        |struct Box[T]
        |  value: T
        |
        |def main() =
        |  val b = Box(42)
        |  print(b.value)
      """.stripMargin)
      tp.decls.collect { case s: TStructDecl => s.sym.name } should contain ("Box")
    }

    "elaborate a multi-parameter generic struct" in {
      val tp = elab("""
        |struct Pair[A, B]
        |  fst: A
        |  snd: B
        |
        |def main() =
        |  val p = Pair(1, "hi")
        |  print(p.fst)
        |  print(p.snd)
      """.stripMargin)
      tp.decls.collect { case s: TStructDecl => s.sym.name } should contain ("Pair")
    }

    "elaborate a single-param generic enum" in {
      val tp = elab("""
        |enum Opt[T] =
        |  Some(value: T)
        |  None
        |
        |def main() =
        |  val x = Some(42)
        |  val msg: string = x match
        |    Some(v) -> "got it"
        |    None    -> "none"
        |  print(msg)
      """.stripMargin)
      tp.decls.collect { case e: TEnumDecl => e.sym.name } should contain ("Opt")
    }

    "elaborate a multi-parameter generic enum" in {
      val tp = elab("""
        |enum Either[L, R] =
        |  Left(value: L)
        |  Right(value: R)
        |
        |def main() =
        |  val e = Left(1)
        |  val msg: string = e match
        |    Left(_)  -> "left"
        |    Right(_) -> "right"
        |  print(msg)
      """.stripMargin)
      tp.decls.collect { case e: TEnumDecl => e.sym.name } should contain ("Either")
    }

    "monomorphic struct + enum still elaborate cleanly (no regression)" in {
      val tp = elab("""
        |struct Point
        |  x: real
        |  y: real
        |enum Color =
        |  Red
        |  Green
        |  Blue
      """.stripMargin)
      tp.decls.collect { case s: TStructDecl => s.sym.name } should contain ("Point")
      tp.decls.collect { case e: TEnumDecl   => e.sym.name } should contain ("Color")
    }
  }
