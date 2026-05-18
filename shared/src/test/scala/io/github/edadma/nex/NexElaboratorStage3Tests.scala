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
