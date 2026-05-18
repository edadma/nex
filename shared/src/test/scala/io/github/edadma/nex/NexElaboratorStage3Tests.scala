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

  // ==========================================================================
  // §4.11 — unique closure capture for var-array bindings
  // ==========================================================================

  "unique closure capture (§4.11)" should {

    "accept a single closure capturing a var-array binding" in {
      val tp = elab("""
        |def main() =
        |  var a = [1, 2, 3]
        |  val f = () -> a[0]
        |  print(f())
      """.stripMargin)
      tp.decls should not be empty
    }

    "reject two closures capturing the same var-array binding" in {
      val errs = elabExpect("""
        |def main() =
        |  var a = [1, 2, 3]
        |  val f = () -> a[0]
        |  val g = () -> a[1]
        |  print(f() + g())
      """.stripMargin)
      errs should have size 1
      errs.head should include("already captured by another closure")
      errs.head should include("§4.11")
    }

    "accept two closures capturing different var-array bindings" in {
      val tp = elab("""
        |def main() =
        |  var a = [1, 2, 3]
        |  var b = [4, 5, 6]
        |  val f = () -> a[0]
        |  val g = () -> b[0]
        |  print(f())
        |  print(g())
      """.stripMargin)
      tp.decls should not be empty
    }

    "accept multiple closures capturing a scalar var (not array)" in {
      // Only var-arrays are unique-owned per spec §8.2; scalar vars can
      // be freely shared across closures.
      val tp = elab("""
        |def main() =
        |  var k = 1
        |  val f = () -> k
        |  val g = () -> k + 1
        |  print(f())
        |  print(g())
      """.stripMargin)
      tp.decls should not be empty
    }

    "accept multiple closures capturing a val-array (not var)" in {
      // val-arrays aren't unique-owned (a val can't be moved), so they
      // can be captured by any number of closures.
      val tp = elab("""
        |def main() =
        |  val a = [1, 2, 3]
        |  val f = () -> a[0]
        |  val g = () -> a[1]
        |  print(f() + g())
      """.stripMargin)
      tp.decls should not be empty
    }

    "lambda's own param shadows an outer var-array with the same name" in {
      // The inner `a` is the lambda parameter, not the outer var, so
      // there's no capture and the outer-binding rule doesn't apply.
      val tp = elab("""
        |def main() =
        |  var a = [1, 2, 3]
        |  val f = a -> a[0]
        |  val g = () -> a[0]
        |  print(f([9, 9, 9]))
        |  print(g())
      """.stripMargin)
      tp.decls should not be empty
    }

    "nested var inside a lambda body is a local binding, not a capture" in {
      // `var inner` is bound INSIDE the lambda body, so it's not a
      // capture of any outer binding. Two outer closures with their own
      // local var-arrays should both elaborate cleanly.
      val tp = elab("""
        |def main() =
        |  val f = () ->
        |    var inner = [1, 2, 3]
        |    inner[0]
        |  val g = () ->
        |    var inner = [4, 5, 6]
        |    inner[0]
        |  print(f())
        |  print(g())
      """.stripMargin)
      tp.decls should not be empty
    }

    "rank-2 var-array uniqueness is enforced the same way" in {
      val errs = elabExpect("""
        |def main() =
        |  var m = [[1, 2], [3, 4]]
        |  val f = () -> m[0, 0]
        |  val g = () -> m[1, 1]
      """.stripMargin)
      errs should have size 1
      errs.head should include("already captured")
    }

    "closure inside a for loop body counts as a capture once per closure value" in {
      // The closure inside `for` is constructed once per iteration —
      // the SAME TLambda node is reachable in the AST exactly once, so
      // the uniqueness check sees a single capture. (A second TLambda
      // in the loop body capturing the same binding would still error,
      // which the next test verifies.)
      val tp = elab("""
        |def main() =
        |  var a = [1, 2, 3]
        |  for i in [1, 2, 3] do
        |    val f = () -> a[i]
        |    print(f())
      """.stripMargin)
      tp.decls should not be empty
    }

    "two captures in different control-flow branches still both count" in {
      val errs = elabExpect("""
        |def main() =
        |  var a = [1, 2, 3]
        |  val f = if 1 > 0 then () -> a[0] else () -> a[1]
        |  print(f())
      """.stripMargin)
      errs should have size 1
      errs.head should include("already captured")
    }
  }

  // ==========================================================================
  // §8.3 — verify TClone is inserted at the AST level
  // ==========================================================================

  "auto-clone AST shape (§8.3)" should {

    /** Find every TClone descendant under `e`. */
    def findClones(e: TExpr): List[TClone] =
      val out = scala.collection.mutable.ListBuffer.empty[TClone]
      def go(x: TExpr): Unit =
        x match
          case c: TClone => out += c
          case _         => ()
        x match
          case TBlock(items, r, _, _) =>
            items.foreach {
              case TBlockBinding(_, _, v) => go(v)
              case TBlockExpr(y)          => go(y)
            }
            go(r)
          case TCall(callee, args, _, _)   => go(callee); args.foreach(go)
          case TBinOp(_, l, r, _, _)       => go(l); go(r)
          case TUnaryOp(_, y, _, _)        => go(y)
          case TIf(c, t, el, _, _)         => go(c); go(t); el.foreach(go)
          case TFor(_, it, body, _, _)     => go(it); go(body)
          case TWhile(c, b, _, _)          => go(c); go(b)
          case TReturn(v, _, _)            => v.foreach(go)
          case TAssign(t, v, _, _)         => go(t); go(v)
          case TIndex(a, i, _, _)          => go(a); i.foreach(go)
          case TLambda(_, body, _, _)      => go(body)
          case TArrayLit(es, _, _)         => es.foreach(go)
          case TTuple(es, _, _)            => es.foreach(go)
          case TClone(a, _, _)             => go(a)
          case _                           => ()
      go(e)
      out.toList

    /** Pull a function's body out of a TProgram by name. */
    def funBody(tp: TProgram, name: String): TExpr =
      tp.decls.collectFirst {
        case f: TFunDecl if f.sym.name == name => f.body
      }.getOrElse(fail(s"function `$name` not found"))

    "var b = a inserts TClone(a) when a is reused later" in {
      val tp = elab("""
        |def main() =
        |  var a = [1, 2, 3]
        |  var b = a
        |  print(a[0])
        |  print(b[0])
      """.stripMargin)
      val clones = findClones(funBody(tp, "main"))
      clones should have size 1
      clones.head.arr shouldBe a[TVarRef]
      clones.head.arr.asInstanceOf[TVarRef].sym.name shouldBe "a"
    }

    "var b = a does NOT insert TClone when a has no later use" in {
      val tp = elab("""
        |def main() =
        |  var a = [1, 2, 3]
        |  var b = a
        |  print(b[0])
      """.stripMargin)
      val clones = findClones(funBody(tp, "main"))
      clones shouldBe empty
    }

    "mut-call with later var-use inserts TClone at the arg" in {
      val tp = elab("""
        |def zero_first(xs: mut [integer]) = xs[0] = 0
        |
        |def main() =
        |  var a = [10, 20]
        |  zero_first(a)
        |  print(a[0])
      """.stripMargin)
      val clones = findClones(funBody(tp, "main"))
      clones should have size 1
      clones.head.arr.asInstanceOf[TVarRef].sym.name shouldBe "a"
    }

    "scalar var is NEVER wrapped in TClone" in {
      val tp = elab("""
        |def main() =
        |  var x = 1
        |  var y = x
        |  print(x)
        |  print(y)
      """.stripMargin)
      val clones = findClones(funBody(tp, "main"))
      clones shouldBe empty
    }
  }
