package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Stage 2 of the elaborator: type inference. Asserts on the `tpe` field
  * of nodes produced by the elaborator and on the array-rewrite shapes
  * (TElementWise, TBroadcast, TMatMul) that arithmetic gets rewritten to.
  */
class NexElaboratorStage2Tests extends AnyWordSpec with Matchers:

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

  /** Grab the RHS expression of the only top-level val/var/const. */
  private def rhsOf(tp: TProgram): TExpr =
    tp.decls.head.asInstanceOf[TTopBinding].value

  // ==========================================================================
  // Literal types
  // ==========================================================================

  "literals" should {
    "give integer literals type TyInteger" in {
      rhsOf(elab("val x = 42")).tpe shouldBe TyInteger
    }
    "give real literals type TyReal" in {
      rhsOf(elab("val x = 3.14")).tpe shouldBe TyReal
    }
    "give boolean literals type TyBool" in {
      rhsOf(elab("val x = true")).tpe shouldBe TyBool
    }
    "give string literals type TyString" in {
      rhsOf(elab("""val x = "hi" """)).tpe shouldBe TyString
    }
    "give the unit literal type TyUnit" in {
      rhsOf(elab("val x = ()")).tpe shouldBe TyUnit
    }
  }

  // ==========================================================================
  // Binding type propagation
  // ==========================================================================

  "bindings" should {
    "carry the inferred type on the symbol" in {
      val tp  = elab("val x = 1\nval y = x")
      val ySym = tp.decls(1).asInstanceOf[TTopBinding].sym
      ySym.tpe shouldBe TyInteger
    }

    "honour an explicit type annotation" in {
      val tp = elab("val x: real = 1")
      tp.decls.head.asInstanceOf[TTopBinding].sym.tpe shouldBe TyReal
      rhsOf(tp).tpe shouldBe TyInteger
    }

    "complain when value can't be assigned to declared type" in {
      val errs = elabExpect("""val x: integer = "hi" """)
      errs.exists(_.contains("cannot assign")) shouldBe true
    }
  }

  // ==========================================================================
  // Numeric promotion
  // ==========================================================================

  "numeric promotion" should {
    "promote int + real to real" in {
      rhsOf(elab("val x = 1 + 2.0")).tpe shouldBe TyReal
    }
    "promote int + complex to complex" in {
      rhsOf(elab("val x = 1 + i")).tpe shouldBe TyComplex
    }
    "promote real + complex to complex" in {
      rhsOf(elab("val x = 1.0 + i")).tpe shouldBe TyComplex
    }
    "real division of two integers produces real" in {
      rhsOf(elab("val x = 1 / 2")).tpe shouldBe TyReal
    }
    "integer division (div) of two integers stays integer" in {
      rhsOf(elab("val x = 7 div 3")).tpe shouldBe TyInteger
    }
    "complain when div has a non-integer operand" in {
      val errs = elabExpect("val x = 7 div 3.0")
      errs.exists(_.contains("`div` requires integer")) shouldBe true
    }
  }

  // ==========================================================================
  // Comparisons
  // ==========================================================================

  "comparison operators" should {
    "produce bool" in {
      rhsOf(elab("val x = 1 == 2")).tpe shouldBe TyBool
      rhsOf(elab("val x = 1 < 2")).tpe shouldBe TyBool
      rhsOf(elab("val x = 1.0 <= 2")).tpe shouldBe TyBool
    }
    "reject ordered comparison on complex" in {
      val errs = elabExpect("val x = i < i")
      errs.exists(_.contains("ordered numeric")) shouldBe true
    }
    "allow equality on complex" in {
      rhsOf(elab("val x = i == i")).tpe shouldBe TyBool
    }
  }

  // ==========================================================================
  // Logical ops
  // ==========================================================================

  "logical operators" should {
    "produce bool from bool operands" in {
      rhsOf(elab("val x = true and false")).tpe shouldBe TyBool
      rhsOf(elab("val x = not true")).tpe shouldBe TyBool
    }
    "reject non-bool operand to and/or" in {
      val errs = elabExpect("val x = 1 and 2")
      errs.exists(_.contains("must be bool")) shouldBe true
    }
  }

  // ==========================================================================
  // Array operations
  // ==========================================================================

  "array literals" should {
    "have type TyArray(integer, 1) for [1,2,3]" in {
      rhsOf(elab("val a = [1, 2, 3]")).tpe shouldBe TyArray(TyInteger, 1)
    }
    "have type TyArray(real, 1) for [1.0, 2.0, 3.0]" in {
      rhsOf(elab("val a = [1.0, 2.0, 3.0]")).tpe shouldBe TyArray(TyReal, 1)
    }
    "promote mixed numeric array literals" in {
      rhsOf(elab("val a = [1, 2.0, 3]")).tpe shouldBe TyArray(TyReal, 1)
    }
    "have type TyArray(real, 2) for nested literal" in {
      rhsOf(elab("val m = [[1.0, 2.0], [3.0, 4.0]]")).tpe shouldBe TyArray(TyReal, 2)
    }
  }

  "array arithmetic" should {
    "rewrite scalar * array as TBroadcast" in {
      // Construct an array typed binding via an explicit annotation.
      val tp = elab("""
        |def f(v: [real]) = 2.0 * v
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TBroadcast]
      body.tpe shouldBe TyArray(TyReal, 1)
    }

    "rewrite array + array as TElementWise" in {
      val tp = elab("""
        |def f(a: [real], b: [real]) = a + b
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TElementWise]
      body.tpe shouldBe TyArray(TyReal, 1)
    }

    "rewrite rank-2 + rank-2 as TElementWise" in {
      val tp = elab("""
        |def f(a: [[real]], b: [[real]]) = a + b
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TElementWise]
      body.tpe shouldBe TyArray(TyReal, 2)
    }

    "rewrite @ as TMatMul (matrix × matrix → matrix)" in {
      val tp = elab("""
        |def f(a: [[real]], b: [[real]]) = a @ b
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TMatMul]
      body.tpe shouldBe TyArray(TyReal, 2)
    }

    "rewrite @ as TMatMul (matrix × vector → vector)" in {
      val tp = elab("""
        |def f(a: [[real]], v: [real]) = a @ v
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TMatMul]
      body.tpe shouldBe TyArray(TyReal, 1)
    }

    "rewrite @ as TMatMul (vector · vector → scalar)" in {
      val tp = elab("""
        |def f(a: [real], b: [real]) = a @ b
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TMatMul]
      body.tpe shouldBe TyReal
    }

    "element-wise comparison of arrays gives [bool]" in {
      val tp = elab("""
        |def f(a: [real], b: [real]) = a == b
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body shouldBe a[TElementWise]
      body.tpe shouldBe TyArray(TyBool, 1)
    }
  }

  // ==========================================================================
  // Function types
  // ==========================================================================

  "function declarations" should {
    "infer return type from body when not annotated" in {
      val tp = elab("def add(a: integer, b: integer) = a + b")
      val f  = tp.decls.head.asInstanceOf[TFunDecl]
      f.returnType shouldBe TyInteger
      f.sym.tpe shouldBe TyFunc(
        List((TyInteger, ParamMode.Read), (TyInteger, ParamMode.Read)),
        TyInteger,
      )
    }

    "respect an explicit return-type annotation" in {
      val tp = elab("def add(a: integer, b: integer): real = a + b")
      val f  = tp.decls.head.asInstanceOf[TFunDecl]
      f.returnType shouldBe TyReal
    }
  }

  // ==========================================================================
  // Function calls
  // ==========================================================================

  "function calls" should {
    "produce the function's return type" in {
      val tp = elab("""
        |def square(x: integer) = x * x
        |val y = square(5)
      """.stripMargin)
      tp.decls(1).asInstanceOf[TTopBinding].value.tpe shouldBe TyInteger
    }

    "complain when arg count is wrong" in {
      val errs = elabExpect("""
        |def f(a: integer) = a
        |val y = f(1, 2)
      """.stripMargin)
      errs.exists(_.contains("expects 1 args")) shouldBe true
    }
  }

  // ==========================================================================
  // Structs
  // ==========================================================================

  "struct construction" should {
    "return the struct type" in {
      val tp = elab("""
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |val p = Point(1.0, 2.0)
      """.stripMargin)
      val pv = tp.decls(1).asInstanceOf[TTopBinding].value
      pv.tpe shouldBe TyStruct("Point", List("x" -> TyReal, "y" -> TyReal))
    }

    "give field access the field's type" in {
      val tp = elab("""
        |struct Point
        |  x: real
        |  y: real
        |end Point
        |
        |val p = Point(1.0, 2.0)
        |val px = p.x
      """.stripMargin)
      tp.decls(2).asInstanceOf[TTopBinding].value.tpe shouldBe TyReal
    }
  }

  // ==========================================================================
  // Complex numbers
  // ==========================================================================

  "complex" should {
    "have .re and .im of type real" in {
      val tp = elab("""
        |val z = i
        |val r = z.re
        |val im = z.im
      """.stripMargin)
      tp.decls(1).asInstanceOf[TTopBinding].value.tpe shouldBe TyReal
      tp.decls(2).asInstanceOf[TTopBinding].value.tpe shouldBe TyReal
    }
  }

  // ==========================================================================
  // Conditionals
  // ==========================================================================

  "if expressions" should {
    "have the lub of the two branches' types" in {
      rhsOf(elab("val x = if true then 1 else 2.0")).tpe shouldBe TyReal
    }
    "be unit when there's no else" in {
      val tp = elab("""
        |def use() =
        |  if true then print(1)
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      body.tpe shouldBe TyUnit
    }
    "require bool condition" in {
      val errs = elabExpect("val x = if 1 then 2 else 3")
      errs.exists(_.contains("condition must be bool")) shouldBe true
    }
  }

  // ==========================================================================
  // Blocks
  // ==========================================================================

  "blocks" should {
    "have the result expression's type" in {
      val tp = elab("""
        |def f() =
        |  val a = 1
        |  val b = 2
        |  a + b
      """.stripMargin)
      tp.decls.head.asInstanceOf[TFunDecl].returnType shouldBe TyInteger
    }
  }

  // ==========================================================================
  // Indexing
  // ==========================================================================

  "array indexing" should {
    "give the element type for rank-1" in {
      val tp = elab("""
        |def f(v: [real]) = v[0]
      """.stripMargin)
      tp.decls.head.asInstanceOf[TFunDecl].returnType shouldBe TyReal
    }
    "give the scalar element type for rank-2 with two indices" in {
      val tp = elab("""
        |def f(m: [[real]]) = m[0, 1]
      """.stripMargin)
      tp.decls.head.asInstanceOf[TFunDecl].returnType shouldBe TyReal
    }
    "give a rank-1 slice for rank-2 with one index" in {
      val tp = elab("""
        |def f(m: [[real]]) = m[0]
      """.stripMargin)
      tp.decls.head.asInstanceOf[TFunDecl].returnType shouldBe TyArray(TyReal, 1)
    }
    "reject non-integer index" in {
      val errs = elabExpect("""
        |def f(v: [real]) = v[1.5]
      """.stripMargin)
      errs.exists(_.contains("array index must be integer")) shouldBe true
    }
  }

  // ==========================================================================
  // Ranges
  // ==========================================================================

  "ranges" should {
    "produce [integer]" in {
      val tp = elab("val r = 0..10")
      rhsOf(tp).tpe shouldBe TyArray(TyInteger, 1)
    }
    "produce [integer] for closed range" in {
      val tp = elab("val r = 0..=10")
      rhsOf(tp).tpe shouldBe TyArray(TyInteger, 1)
    }
  }

  // ==========================================================================
  // Interpolated-string subtree inference (regression for bug found 2026-05-18)
  // ==========================================================================

  "interpolated `${...}` subtrees" should {

    "type-infer the sub-expression of a `${...}` part" in {
      // Before the fix to infExpr's TInterpStringLit branch, the inner
      // TInterpExpr(a + b) kept TyUnknown — Stage 2 never walked into it.
      val tp = elab("""
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"sum = ${a + b}")
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      val print = body.asInstanceOf[TBlock].result.asInstanceOf[TCall]
      val interp = print.args.head.asInstanceOf[TInterpStringLit]
      val exprPart = interp.parts.collect { case e: TInterpExpr => e }.head
      exprPart.expr.tpe shouldBe TyInteger
    }

    "type-infer a complex sub-expression" in {
      // `${if a < b then b else a}` — the inner if-expression should
      // come back with TyInteger, having walked condition + both branches.
      val tp = elab("""
        |def main() =
        |  val a = 3
        |  val b = 4
        |  print(s"max = ${if a < b then b else a}")
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      val print = body.asInstanceOf[TBlock].result.asInstanceOf[TCall]
      val interp = print.args.head.asInstanceOf[TInterpStringLit]
      val ifExpr = interp.parts.collect { case e: TInterpExpr => e }.head.expr.asInstanceOf[TIf]
      ifExpr.tpe shouldBe TyInteger
      ifExpr.cond.tpe shouldBe TyBool
    }
  }

  // ==========================================================================
  // For-loop tuple destructuring (regression for bug found 2026-05-18)
  // ==========================================================================

  "for-loop tuple destructuring" should {

    "assign each loop var the corresponding tuple element type" in {
      // Before the fix, Stage 2 fell into the "tuple destructuring left
      // for Stage 3" branch and the loop vars kept TyUnknown — the
      // interpreter happened to dispatch correctly at runtime, but any
      // static analysis or AOT backend would have been wrong. We use a
      // directly-typed tuple-array literal (rather than the prelude's
      // `enumerate`, which still returns TyUnknown) so the test pins the
      // type-propagation path itself, not the prelude's typing.
      val tp = elab("""
        |def main() =
        |  for a, b in [(1, 1.5), (2, 2.5)] do
        |    print(a)
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      val forExpr = body match
        case f: TFor => f
        case b: TBlock => b.result.asInstanceOf[TFor]
        case other => fail(s"unexpected body shape: $other")
      forExpr.loopVars.size shouldBe 2
      forExpr.loopVars(0).tpe shouldBe TyInteger
      forExpr.loopVars(1).tpe shouldBe TyReal
    }

    "reject loop-var arity mismatch against the tuple element shape" in {
      val errs = elabExpect("""
        |def main() =
        |  for a, b, c in [(1, 2), (3, 4)] do
        |    print(a)
      """.stripMargin)
      errs.exists(_.contains("for-loop tuple destructuring binds 3 names but each element has 2")) shouldBe true
    }

    "reject multiple loop vars over an array of non-tuple elements" in {
      val errs = elabExpect("""
        |def main() =
        |  for a, b in [1, 2, 3] do
        |    print(a)
      """.stripMargin)
      errs.exists(_.contains("not a tuple")) shouldBe true
    }

    "type the loop var in the single-name case too (regression)" in {
      // Same stale-Symbol bug existed on the single-loop-var path —
      // `setSymType` updated the symbol table but the TFor field kept
      // its pre-inference Symbol. Just never caught because no test
      // looked at `forExpr.loopVars.head.tpe`.
      val tp = elab("""
        |def main() =
        |  for x in [1.5, 2.5, 3.5] do
        |    print(x)
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body
      val forExpr = body match
        case f: TFor   => f
        case b: TBlock => b.result.asInstanceOf[TFor]
        case other     => fail(s"unexpected body shape: $other")
      forExpr.loopVars.size shouldBe 1
      forExpr.loopVars.head.tpe shouldBe TyReal
    }
  }

  // ==========================================================================
  // TTupleProj inference (direct unit-test coverage for the projection node)
  // ==========================================================================

  "TTupleProj inference" should {

    "type each projection as the corresponding element type" in {
      // `val a, b = (1, 2.5)` lowers to:
      //   $tuple = (1, 2.5)            // TyTuple(TyInteger, TyReal)
      //   a      = $tuple.0            // TTupleProj idx=0 → TyInteger
      //   b      = $tuple.1            // TTupleProj idx=1 → TyReal
      val tp = elab("val a, b = (1, 2.5)")
      val aDecl = tp.decls(1).asInstanceOf[TTopBinding]
      val bDecl = tp.decls(2).asInstanceOf[TTopBinding]
      aDecl.value.tpe shouldBe TyInteger
      bDecl.value.tpe shouldBe TyReal
      // The owning symbols' types are updated too.
      aDecl.sym.tpe shouldBe TyInteger
      bDecl.sym.tpe shouldBe TyReal
    }

    "report arity mismatch via the projection type-check" in {
      val errs = elabExpect("val a, b, c = (1, 2)")
      errs.exists(_.contains("binds 3 names but the value has only 2 elements")) shouldBe true
    }

    "report a non-tuple RHS via the projection type-check" in {
      val errs = elabExpect("val a, b = 5")
      errs.exists(_.contains("requires a tuple value")) shouldBe true
    }
  }

  // ==========================================================================
  // Misc diagnostics — covers error sites that previously had no test.
  // ==========================================================================

  "diagnostics" should {

    "reject `@` on non-array operands" in {
      val errs = elabExpect("""
        |def f(x: integer, y: integer) = x @ y
      """.stripMargin)
      errs.exists(_.contains("`@` requires array operands")) shouldBe true
    }

    "reject unary `-` on a bool" in {
      val errs = elabExpect("""
        |def f() = -true
      """.stripMargin)
      errs.exists(_.contains("unary `-` requires numeric operand")) shouldBe true
    }

    "reject an if-without-else whose branch is not unit" in {
      val errs = elabExpect("""
        |def f() =
        |  if true then 42
      """.stripMargin)
      errs.exists(_.contains("if-without-else branch must be unit")) shouldBe true
    }

    "reject indexing a non-array value" in {
      val errs = elabExpect("""
        |def f() =
        |  val x = 5
        |  x[0]
      """.stripMargin)
      errs.exists(_.contains("cannot index value of type")) shouldBe true
    }

    "reject field access on a non-struct" in {
      val errs = elabExpect("""
        |def f() =
        |  val x = 5
        |  x.something
      """.stripMargin)
      errs.exists(_.contains("cannot access field")) shouldBe true
    }

    "reject access to a non-existent struct field" in {
      val errs = elabExpect("""
        |struct Point
        |  x: real
        |  y: real
        |def f(p: Point) = p.z
      """.stripMargin)
      errs.exists(_.contains("no field")) shouldBe true
    }

    "reject a non-existent method call" in {
      val errs = elabExpect("""
        |def f() =
        |  val xs = [1, 2, 3]
        |  xs.notAThing()
      """.stripMargin)
      errs.exists(_.contains("no method or function")) shouldBe true
    }

    "reject a range bound that isn't an integer" in {
      val errs = elabExpect("""
        |def f() =
        |  val r = 0.5..5
      """.stripMargin)
      errs.exists(_.contains("range bound must be integer")) shouldBe true
    }

    "reject `div` on non-integer operands" in {
      val errs = elabExpect("""
        |def f() =
        |  val x = 3.5 div 2
      """.stripMargin)
      errs.exists(_.contains("requires integer")) shouldBe true
    }

    "reject `<` on non-numeric operands" in {
      val errs = elabExpect("""
        |def f() =
        |  val b = "hi" < "ho"
      """.stripMargin)
      errs.exists(e => e.contains("ordered numeric") || e.contains("cannot compare")) shouldBe true
    }

    "TVarRef carries the latest Symbol, not a Stage-1 snapshot (regression)" in {
      // Same class of bug as the for-loop loopVars stale-Symbol issue:
      // `setSymType` only updates the symbol table; pre-existing TVarRef
      // nodes still hold the Stage-1 Symbol snapshot. Stage 2's TVarRef
      // case refreshes `.tpe` from `currentType(s)` but should also
      // refresh `.sym` so node walkers reading `ref.sym.tpe` see the
      // current type.
      val tp = elab("""
        |val x = 1 + 2
        |def f() = x
      """.stripMargin)
      val fn = tp.decls.collectFirst { case f: TFunDecl => f }.get
      val ref = fn.body.asInstanceOf[TVarRef]
      ref.tpe shouldBe TyInteger
      ref.sym.tpe shouldBe TyInteger
    }

    "TInterpRef carries the latest Symbol too" in {
      // Same staleness fix applied to the `$ident` interpolation form.
      val tp = elab("""
        |val n = 3 + 4
        |def main() = print(s"value=$n")
      """.stripMargin)
      val fn = tp.decls.collectFirst { case f: TFunDecl if f.sym.name == "main" => f }.get
      val print = fn.body.asInstanceOf[TCall]
      val interp = print.args.head.asInstanceOf[TInterpStringLit]
      val ref = interp.parts.collect { case r: TInterpRef => r }.head
      ref.sym.tpe shouldBe TyInteger
    }

    "TStructDecl carries the resolved struct type, not the Nil placeholder" in {
      // `elabStruct` mints the struct sym with a Nil-fields placeholder
      // type (so the name resolves during Pass A), then updates the
      // symbol table when the field types are known. The returned
      // TStructDecl previously kept the placeholder Symbol. Verify
      // the resolved type is now exposed on the decl's `sym`.
      val tp = elab("""
        |struct Point
        |  x: real
        |  y: real
      """.stripMargin)
      val sd = tp.decls.head.asInstanceOf[TStructDecl]
      sd.sym.tpe shouldBe TyStruct("Point", List("x" -> TyReal, "y" -> TyReal))
    }

    "annotated lambda params get the annotated type on the Symbol" in {
      // `(x: real) -> x * 2.0` — `x` is annotated, so the param sym
      // starts with TyReal at Stage 1. No inference required; just
      // verify the annotated type survives into the typed AST.
      val tp = elab("""
        |def main() =
        |  val f = (x: real) -> x * 2.0
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body.asInstanceOf[TBlock]
      val bind = body.items.collectFirst { case b: TBlockBinding => b }.get
      val lam = bind.value.asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyReal
    }
  }

  // ==========================================================================
  // Bidirectional inference (lambda params get pushed-down expected types)
  // ==========================================================================

  "bidirectional inference" should {
    "infer an unannotated lambda's param type from a plain-call callee's TyFunc" in {
      // `apply` declares `f: (integer -> integer)`. The lambda passed at
      // the call site carries no `: T` on `x`, so its param sym leaves
      // Stage 1 with TyUnknown. After inference push-down, the param
      // should be TyInteger and the lambda's outer type should be the
      // matching TyFunc.
      val tp = elab("""
        |def apply(f: (integer -> integer), x: integer) = f(x)
        |val r = apply(x -> x * 2, 3)
      """.stripMargin)
      val bind = tp.decls.collectFirst { case b: TTopBinding => b }.get
      val call = bind.value.asInstanceOf[TCall]
      val lam  = call.args.head.asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      lam.tpe shouldBe TyFunc(List((TyInteger, ParamMode.Read)), TyInteger)
    }

    "lambda body gets re-inferred with the refined param type" in {
      // `x * 2` inside the lambda must compute as integer × integer →
      // integer after push-down (not the TyUnknown × integer → TyUnknown
      // it would produce without it). The body's tpe is what proves the
      // body inference saw the refined type.
      val tp = elab("""
        |def apply(f: (integer -> integer), x: integer) = f(x)
        |val r = apply(x -> x * 2, 3)
      """.stripMargin)
      val bind = tp.decls.collectFirst { case b: TTopBinding => b }.get
      val lam  = bind.value.asInstanceOf[TCall].args.head.asInstanceOf[TLambda]
      lam.body.tpe shouldBe TyInteger
    }

    "infer a lambda's param type from a method-call callee resolving to a top-level function" in {
      // `(3).apply(x -> x * 2)` is a TMethodCall at Stage 2 (push-down
      // happens there). Stage 3 then lowers it to `apply(3, x -> x * 2)`
      // — by the time we read the final tree it's a TCall. Either way
      // the lambda's params got refined at Stage 2.
      val tp = elab("""
        |def apply(n: integer, f: (integer -> integer)) = f(n)
        |val r = (3).apply(x -> x * 2)
      """.stripMargin)
      val bind = tp.decls.collectFirst { case b: TTopBinding => b }.get
      val call = bind.value.asInstanceOf[TCall]
      // After Stage 3 lowering: TCall(apply, [3, lambda])
      val lam = call.args(1).asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      lam.body.tpe shouldBe TyInteger
    }

    "infer a lambda's param type from a declared val-binding type" in {
      // `val f: (integer -> integer) = x -> x * 2` — the declared type
      // on the binding gets pushed into the lambda before inference.
      val tp = elab("""
        |val f: (integer -> integer) = x -> x * 2
      """.stripMargin)
      val bind = tp.decls.head.asInstanceOf[TTopBinding]
      val lam  = bind.value.asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      lam.body.tpe shouldBe TyInteger
    }

    "infer a lambda's param type from a block-level val with declared type" in {
      // Same as the top-level case but for a TBlockBinding inside a def
      // body — exercises inferBlockItem's push-down branch.
      val tp = elab("""
        |def main() =
        |  val f: (integer -> integer) = x -> x * 2
        |  f(3)
      """.stripMargin)
      val body = tp.decls.head.asInstanceOf[TFunDecl].body.asInstanceOf[TBlock]
      val bind = body.items.collectFirst { case b: TBlockBinding => b }.get
      val lam  = bind.value.asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      lam.body.tpe shouldBe TyInteger
    }

    "push-down infers multi-arg lambdas" in {
      val tp = elab("""
        |def apply2(f: ((integer, integer) -> integer), a: integer, b: integer) = f(a, b)
        |val r = apply2((x, y) -> x + y, 1, 2)
      """.stripMargin)
      val bind = tp.decls.collectFirst { case b: TTopBinding => b }.get
      val lam  = bind.value.asInstanceOf[TCall].args.head.asInstanceOf[TLambda]
      lam.params.map(_.tpe) shouldBe List(TyInteger, TyInteger)
      lam.body.tpe shouldBe TyInteger
    }

    "annotated params are not silently overwritten by push-down" in {
      // The annotated `x: real` on the lambda is the source of truth.
      // Push-down only refines TyUnknown params; an explicit annotation
      // remains intact when expected and annotated agree.
      val tp = elab("""
        |def apply(f: (real -> real), x: real) = f(x)
        |val r = apply((x: real) -> x * 2.0, 3.0)
      """.stripMargin)
      val bind = tp.decls.collectFirst { case b: TTopBinding => b }.get
      val lam  = bind.value.asInstanceOf[TCall].args.head.asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyReal
    }

    "push-down works for prelude HOF `map` (element type into lambda's first param)" in {
      // `map` carries a TyUnknown signature (prelude funcs have no type
      // variables in v0), but the elaborator hand-rolls bidirectional
      // inference for the three HOFs that take lambdas: map, reduce,
      // filter. The lambda's param gets the array's element type.
      val tp = elab("""
        |val xs = [1, 2, 3]
        |val ys = map(xs, x -> x * 2)
      """.stripMargin)
      val ysBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "ys").get
      val call   = ysBind.value.asInstanceOf[TCall]
      val lam    = call.args(1).asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      lam.body.tpe shouldBe TyInteger
      // Result type of map(arr, f) should be [f.body.tpe]
      call.tpe shouldBe TyArray(TyInteger, 1)
    }

    "push-down works for prelude HOF `reduce` (acc, elem) into lambda params" in {
      val tp = elab("""
        |val xs = [1, 2, 3]
        |val s  = reduce(xs, 0, (a, x) -> a + x)
      """.stripMargin)
      val sBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "s").get
      val call  = sBind.value.asInstanceOf[TCall]
      val lam   = call.args(2).asInstanceOf[TLambda]
      lam.params.map(_.tpe) shouldBe List(TyInteger, TyInteger)
      lam.body.tpe shouldBe TyInteger
      call.tpe shouldBe TyInteger
    }

    "push-down works for prelude HOF `filter` (elem into lambda, expected bool result)" in {
      val tp = elab("""
        |val xs = [1, 2, 3, 4]
        |val ev = filter(xs, x -> x % 2 == 0)
      """.stripMargin)
      val evBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "ev").get
      val call   = evBind.value.asInstanceOf[TCall]
      val lam    = call.args(1).asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      lam.body.tpe shouldBe TyBool
      call.tpe shouldBe TyArray(TyInteger, 1)
    }

    "`filter` rejects rank-2 source (runtime only handles rank-1)" in {
      val errs = elabExpect("""
        |val m  = [[1, 2], [3, 4]]
        |val ev = filter(m, x -> x % 2 == 0)
      """.stripMargin)
      errs.exists(_.contains("filter requires a rank-1 array")) shouldBe true
    }

    "prelude `map` preserves source rank in its result type (rank-1)" in {
      val tp = elab("""
        |val xs = [1, 2, 3]
        |val ys = map(xs, x -> x * 2)
      """.stripMargin)
      val ysBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "ys").get
      ysBind.value.tpe shouldBe TyArray(TyInteger, 1)
    }

    "prelude `map` preserves source rank in its result type (rank-2)" in {
      val tp = elab("""
        |val m  = [[1, 2], [3, 4]]
        |val ys = map(m, x -> x * 2)
      """.stripMargin)
      val ysBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "ys").get
      ysBind.value.tpe shouldBe TyArray(TyInteger, 2)
    }

    "push-down works for `xs.map(x -> ...)` method-call sugar" in {
      // Method-call form goes through the TMethodCall branch in Stage 2.
      // The HOF dispatch synthesizes the equivalent TCall shape (receiver
      // as first arg) and re-wraps the typed args back into a TMethodCall
      // so Stage 3 can still lower it via lowerMethodCall.
      val tp = elab("""
        |val xs = [1, 2, 3]
        |val ys = xs.map(x -> x * 2)
      """.stripMargin)
      val ysBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "ys").get
      // After Stage 3 lowering: TCall(map, [xs, lambda]).
      val call = ysBind.value.asInstanceOf[TCall]
      val lam  = call.args(1).asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      lam.body.tpe shouldBe TyInteger
      call.tpe shouldBe TyArray(TyInteger, 1)
    }

    "deferred resolve refines a bound-then-called lambda at the first call site" in {
      // `val f = x -> x * 2` has no declared type, so the lambda's param
      // sym leaves the binding site at TyUnknown. The deferredLambdas
      // map remembers it; the first call site with a concrete TyFunc
      // expected type (here `apply`'s declared `f: (integer -> integer)`)
      // triggers in-place refinement. After elaboration:
      //   - f's TTopBinding.value points at the refined TLambda
      //   - the lambda's param sym is TyInteger
      //   - f's Symbol's tpe is the refined TyFunc
      val tp = elab("""
        |def apply(f: (integer -> integer), x: integer) = f(x)
        |val f = x -> x * 2
        |val r = apply(f, 3)
      """.stripMargin)
      val fBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "f").get
      val lam   = fBind.value.asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      lam.body.tpe shouldBe TyInteger
      lam.tpe shouldBe TyFunc(List((TyInteger, ParamMode.Read)), TyInteger)
      fBind.sym.tpe shouldBe TyFunc(List((TyInteger, ParamMode.Read)), TyInteger)
    }

    "deferred resolve works for a block-level bound-then-called lambda" in {
      val tp = elab("""
        |def apply(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  val f = x -> x * 2
        |  apply(f, 3)
      """.stripMargin)
      val main = tp.decls.collectFirst { case f: TFunDecl if f.sym.name == "main" => f }.get
      val body = main.body.asInstanceOf[TBlock]
      val bind = body.items.collectFirst { case b: TBlockBinding => b }.get
      val lam  = bind.value.asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      bind.sym.tpe shouldBe TyFunc(List((TyInteger, ParamMode.Read)), TyInteger)
    }

    "deferred resolve works through prelude HOF call sites" in {
      // `xs.map(f)` (or `map(xs, f)`) where f is a bound lambda — the
      // HOF push-down path constructs an expected TyFunc for the f
      // arg, which is then routed through inferArg's deferred branch.
      val tp = elab("""
        |val xs = [1, 2, 3]
        |val f  = x -> x * 2
        |val ys = map(xs, f)
      """.stripMargin)
      val fBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "f").get
      val lam   = fBind.value.asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyInteger
      lam.body.tpe shouldBe TyInteger
    }

    "deferred resolve is a no-op when there is no later call site" in {
      // Pure bind-and-never-call leaves the lambda at TyUnknown — the
      // map is populated but nothing reads it. Verifies the resolve
      // path doesn't fire spuriously.
      val tp = elab("""
        |val f = x -> x * 2
      """.stripMargin)
      val fBind = tp.decls.collect { case b: TTopBinding => b }.find(_.sym.name == "f").get
      val lam   = fBind.value.asInstanceOf[TLambda]
      lam.params.head.tpe shouldBe TyUnknown
    }

    "arity mismatch between lambda and expected falls back gracefully" in {
      // Lambda has 2 params but callee expects 1. The push-down path
      // skips (its `eparams.size == params.size` guard); inference
      // proceeds via the no-expected branch. The downstream type-check
      // then catches the mismatch. We assert the elaborator doesn't
      // crash and reports a type error.
      val errs = elabExpect("""
        |def apply(f: (integer -> integer), x: integer) = f(x)
        |val r = apply((x, y) -> x + y, 3)
      """.stripMargin)
      errs.exists(e =>
        e.contains("cannot assign") || e.contains("expects")
      ) shouldBe true
    }
  }
