package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Stage 3 of the elaborator: sugar lowering + mode validation.
  *   - juxtaposition lowered to multiplication / broadcast
  *   - method-call resolved per §4.9 (field access vs function call sugar)
  *   - read-mode parameters validated for non-mutation
  */
class NexElaboratorStage3Tests extends AnyWordSpec with Matchers:

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
      case Right(_)    => fail("expected elaboration errors, but elaboration succeeded")
      case Left(errs)  => errs.map(_.message)

  private def rhsOf(tp: TProgram): TExpr =
    tp.decls.head.asInstanceOf[TTopBinding].value

  // ==========================================================================
  // __array placeholder lowering (lives in Stage 1, but tested here as
  // an integration concern)
  // ==========================================================================

  "array literals" should {
    "elaborate as TArrayLit" in {
      val tp = elab("val xs = [1, 2, 3]")
      val v  = rhsOf(tp)
      v shouldBe a[TArrayLit]
      v.tpe shouldBe TyArray(TyInteger, 1)
      val lit = v.asInstanceOf[TArrayLit]
      lit.elems should have size 3
    }

    "support rank-2 (nested) array literals" in {
      val tp = elab("val m = [[1.0, 2.0], [3.0, 4.0]]")
      val v  = rhsOf(tp)
      v shouldBe a[TArrayLit]
      v.tpe shouldBe TyArray(TyReal, 2)
    }
  }

  // ==========================================================================
  // Juxtaposition lowering
  // ==========================================================================

  "juxtaposition lowering" should {
    "lower `2x` to TBinOp(*) on scalar*scalar" in {
      val tp = elab("""
        |def f(x: real) = 2x
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body match
        case TBinOp("*", _, _, _, _) => succeed
        case TJuxtapose(_, _, _, _)  => fail("juxtaposition not lowered")
        case other                   => fail(s"unexpected body shape: $other")
    }

    "lower `2v` to TBroadcast when v is an array" in {
      val tp = elab("""
        |def f(v: [real]) = 2v
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TBroadcast]
      body.tpe shouldBe TyArray(TyReal, 1)
    }

    "lower complex construction `3 + 4i`" in {
      // Here 4i parses as JuxtaposeExpr(4, i). After lowering it becomes
      // TBinOp("*", 4, i). Then `3 + (4*i)` is TBinOp("+", 3, ...).
      val tp = elab("val z = 3 + 4i")
      rhsOf(tp).tpe shouldBe TyComplex
    }
  }

  // ==========================================================================
  // Method-call dispatch (§4.9)
  // ==========================================================================

  "method-call dispatch" should {
    "lower `arr.length()` to TCall(length, arr)" in {
      val tp = elab("""
        |def f(a: [real]) = a.length()
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TCall]
      val c = body.asInstanceOf[TCall]
      c.callee.asInstanceOf[TVarRef].sym.name shouldBe "length"
      c.args should have size 1
      c.args.head.asInstanceOf[TVarRef].sym.name shouldBe "a"
    }

    "lower `arr.map(f)` to TCall(map, arr, f)" in {
      val tp = elab("""
        |def f(a: [real]) = a.map(x -> x * 2.0)
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TCall]
      val c = body.asInstanceOf[TCall]
      c.callee.asInstanceOf[TVarRef].sym.name shouldBe "map"
      c.args should have size 2
    }

    "lower `point.x` as field access when x is a field" in {
      // Note: the parser produces this as FieldExpr (not MethodCall) when
      // there are no parens. But the lowering pass should respect it
      // either way.
      val tp = elab("""
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |def f(p: Point) = p.x
      """.stripMargin)
      val body = tp.decls(1).asInstanceOf[TFunDecl].body
      body shouldBe a[TField]
      body.tpe shouldBe TyReal
    }

    "lower `z.re` on a complex" in {
      val tp = elab("""
        |def f(z: complex) = z.re
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TField]
      body.tpe shouldBe TyReal
    }
  }

  // ==========================================================================
  // Mode validation (§6.4)
  // ==========================================================================

  "mode validation" should {
    "accept assignment to a `mut` parameter" in {
      elab("""
        |def setHead(v: mut [real], x: real) =
        |  v[0] = x
      """.stripMargin)
    }

    "reject assignment to a read-mode parameter" in {
      val errs = elabExpect("""
        |def bad(v: [real]) =
        |  v[0] = 1.0
      """.stripMargin)
      errs.exists(_.contains("cannot mutate read-mode parameter `v`")) shouldBe true
    }

    "reject mutation of a read-mode struct param's field" in {
      val errs = elabExpect("""
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |def shift(p: Point) =
        |  p.x = 0.0
      """.stripMargin)
      errs.exists(_.contains("cannot mutate read-mode parameter `p`")) shouldBe true
    }

    "allow assignment to a local val/var introduced inside the body" in {
      elab("""
        |def use() =
        |  var x = 1
        |  x = 2
      """.stripMargin)
    }
  }

  // ==========================================================================
  // Combinations
  // ==========================================================================

  "combined lowering" should {
    "handle `arr.map(f).sum()`" in {
      val tp = elab("""
        |def f(a: [real]) = a.map(x -> x * 2.0).sum()
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TCall]
      val outer = body.asInstanceOf[TCall]
      outer.callee.asInstanceOf[TVarRef].sym.name shouldBe "sum"
      outer.args.head shouldBe a[TCall]
      val inner = outer.args.head.asInstanceOf[TCall]
      inner.callee.asInstanceOf[TVarRef].sym.name shouldBe "map"
    }
  }

  // ==========================================================================
  // `${...}` sub-expression lowering (regression for bug found 2026-05-18)
  // ==========================================================================

  "interpolated `${...}` lowering" should {

    "lower juxtaposition inside `${...}` to a TBroadcast over an array" in {
      // Before the fix to lowerExpr's TInterpStringLit branch, the inner
      // TJuxtapose was never lowered — it survived as TJuxtapose at the
      // root of the printed string and the eventual codegen would choke.
      val tp = elab("""
        |def main() =
        |  val v = [1.0, 2.0]
        |  print(s"doubled = ${2v}")
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      val print = body.asInstanceOf[TBlock].result.asInstanceOf[TCall]
      val interp = print.args.head.asInstanceOf[TInterpStringLit]
      val exprPart = interp.parts.collect { case e: TInterpExpr => e }.head
      exprPart.expr shouldBe a[TBroadcast]
    }

    "lower method-call dispatch inside `${...}`" in {
      // `a.sum()` inside `${...}` should be lowered from TMethodCall to a
      // direct TCall (function-call sugar per §4.9).
      val tp = elab("""
        |def main() =
        |  val a = [1, 2, 3]
        |  print(s"total = ${a.sum()}")
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      val print = body.asInstanceOf[TBlock].result.asInstanceOf[TCall]
      val interp = print.args.head.asInstanceOf[TInterpStringLit]
      val exprPart = interp.parts.collect { case e: TInterpExpr => e }.head
      // After lowering, the inner method-call is a plain TCall, not a TMethodCall.
      exprPart.expr shouldBe a[TCall]
      exprPart.expr.asInstanceOf[TCall].callee.asInstanceOf[TVarRef].sym.name shouldBe "sum"
    }

    "catch `mut`-misuse inside `${...}` via walkForMutations" in {
      // Before the fix to walkForMutations's TInterpStringLit branch,
      // call-site mode validation never visited interpolated subtrees,
      // so a misuse like `${bump(read_param)}` would slip past.
      val errs = elabExpect("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def caller(y: integer) =
        |  print(s"after = ${bump(y)}")
      """.stripMargin)
      errs.mkString(";") should include("cannot pass `y` to a `mut`-mode parameter")
    }
  }

  // ==========================================================================
  // Call-site mode validation — Stage-3 unit coverage of checkCallSiteModes
  // (integration tests live in NexInterpreterTests::"call-site mode check")
  // ==========================================================================

  "checkCallSiteModes" should {

    "accept a `var`-rooted name as a `mut` argument" in {
      elab("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def main() =
        |  var n = 5
        |  bump(n)
      """.stripMargin)
    }

    "accept a `var`-rooted field access as a `mut` argument" in {
      elab("""
        |struct Box
        |  v: integer
        |def bump(x: mut integer) =
        |  x = x + 1
        |def main() =
        |  var b = Box(10)
        |  bump(b.v)
      """.stripMargin)
    }

    "reject a `val`-rooted name as a `mut` argument" in {
      val errs = elabExpect("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def main() =
        |  val n = 5
        |  bump(n)
      """.stripMargin)
      errs.exists(_.contains("cannot pass `n` to a `mut`-mode parameter")) shouldBe true
    }

    "reject an arbitrary expression (no root symbol) as a `mut` argument" in {
      val errs = elabExpect("""
        |def bump(x: mut integer) =
        |  x = x + 1
        |def main() = bump(2 + 3)
      """.stripMargin)
      errs.exists(_.contains("requires an l-value argument")) shouldBe true
    }

    "stay silent for callees with no `mut` parameters" in {
      // The mode check should not even fire when the callee declares no
      // mut params — otherwise the predicate guard would burn cycles
      // walking every TCall in every function body.
      elab("""
        |def square(x: integer) = x * x
        |def main() = print(square(5))
      """.stripMargin)
    }
  }

  // ==========================================================================
  // Tuple destructuring lowering (Stage 3 passes TTupleProj through)
  // ==========================================================================

  "tuple destructuring lowering" should {

    "leave TTupleProj nodes intact through lowering" in {
      // Stage 3's lowerExpr just recurses on TTupleProj's receiver.
      // Confirm the projection survives all the way to the final AST.
      val tp = elab("val a, b = (1, 2)")
      val aDecl = tp.decls(1).asInstanceOf[TTopBinding]
      aDecl.value shouldBe a[TTupleProj]
      aDecl.value.asInstanceOf[TTupleProj].idx shouldBe 0
    }

    "lower nested calls inside the projected expression" in {
      // The projection's `receiver` field is recursed into by lowerExpr,
      // so any method-call sugar etc. inside the temp's initializer
      // gets lowered too. Smoke test: the temp's value (a TTuple of
      // calls) survives unchanged because TTuple just elem-recurses.
      val tp = elab("""
        |def add(x: integer, y: integer) = x + y
        |val a, b = add(1, 2), add(3, 4)
      """.stripMargin)
      val temp = tp.decls.collectFirst {
        case b: TTopBinding if b.sym.name == "$tuple" => b
      }.getOrElse(fail("temp binding not found"))
      val tuple = temp.value.asInstanceOf[TTuple]
      tuple.elems should have size 2
      tuple.elems(0) shouldBe a[TCall]
      tuple.elems(1) shouldBe a[TCall]
    }
  }
