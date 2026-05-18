package io.github.edadma.nex

import java.io.ByteArrayOutputStream
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Stage 4: array fusion. Asserts:
  *
  *   1. Shape: rank-1 [[TElementWise]] / [[TBroadcast]] get rewritten to a
  *      `TBlock` wrapping a [[TFusedLoop]].
  *   2. Equivalence: programs produce the same stdout with and without
  *      the fusion pass applied. The interpreter handles both shapes.
  *   3. Out-of-scope: rank-2 ops and TMatMul are left alone in this chunk.
  *
  * Fusion is opt-in (not wired into NexElaborator.elaborate), so these
  * tests run elaborate → fuse explicitly.
  */
class NexFusionTests extends AnyWordSpec with Matchers:

  // --------------------------------------------------------------------------
  // Helpers
  // --------------------------------------------------------------------------

  private def elabAndFuse(src: String): TProgram =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    val tp = new NexElaborator().elaborate(ast) match
      case Right(t)   => t
      case Left(errs) => fail(s"elab errors: ${errs.map(_.toString).mkString("; ")}")
    new NexFusion(tp.symbols).fuseProgram(tp)

  private def runFused(src: String): String =
    val tp     = elabAndFuse(src)
    val buf    = new ByteArrayOutputStream
    val interp = new NexInterpreter
    Console.withOut(buf) { interp.runProgram(tp) }
    buf.toString

  private def runUnfused(src: String): String =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    val tp = new NexElaborator().elaborate(ast) match
      case Right(t)   => t
      case Left(errs) => fail(s"elab errors: ${errs.map(_.toString).mkString("; ")}")
    val buf    = new ByteArrayOutputStream
    val interp = new NexInterpreter
    Console.withOut(buf) { interp.runProgram(tp) }
    buf.toString

  /** Find the body expression of `f` after fusion. */
  private def fBodyAfterFusion(src: String): TExpr =
    val tp = elabAndFuse(src)
    tp.decls.collectFirst { case fn: TFunDecl if fn.sym.name == "f" => fn.body }.get

  // ==========================================================================
  // AST shape after fusion
  // ==========================================================================

  "fusion shape" should {

    "rewrite rank-1 array + array into TBlock(TFusedLoop)" in {
      val body = fBodyAfterFusion("""
        |def f(a: [real], b: [real]) = a + b
      """.stripMargin)
      body shouldBe a[TBlock]
      val block = body.asInstanceOf[TBlock]
      block.items.size shouldBe 2
      block.result shouldBe a[TFusedLoop]
      val loop = block.result.asInstanceOf[TFusedLoop]
      loop.tpe shouldBe TyArray(TyReal, 1)
      loop.body shouldBe a[TBinOp]
    }

    "rewrite rank-1 scalar * array into TBlock(TFusedLoop)" in {
      val body = fBodyAfterFusion("""
        |def f(v: [real]) = 2.0 * v
      """.stripMargin)
      body shouldBe a[TBlock]
      val loop = body.asInstanceOf[TBlock].result.asInstanceOf[TFusedLoop]
      loop.tpe shouldBe TyArray(TyReal, 1)
      val bin = loop.body.asInstanceOf[TBinOp]
      bin.op shouldBe "*"
    }

    "rewrite rank-1 arr - scalar with the right operand order in the body" in {
      // The TBroadcast carries `scalarFirst=false` for `arr - scalar`;
      // the fused body must put the array element on the LEFT.
      val body = fBodyAfterFusion("""
        |def f(xs: [integer]) = xs - 1
      """.stripMargin)
      val loop = body.asInstanceOf[TBlock].result.asInstanceOf[TFusedLoop]
      val bin  = loop.body.asInstanceOf[TBinOp]
      bin.op shouldBe "-"
      bin.lhs shouldBe a[TIndex]   // array element on the left
      bin.rhs shouldBe a[TVarRef]  // scalar on the right
    }

    "rewrite rank-1 scalar - arr with the right operand order in the body" in {
      val body = fBodyAfterFusion("""
        |def f(xs: [integer]) = 100 - xs
      """.stripMargin)
      val loop = body.asInstanceOf[TBlock].result.asInstanceOf[TFusedLoop]
      val bin  = loop.body.asInstanceOf[TBinOp]
      bin.op shouldBe "-"
      bin.lhs shouldBe a[TVarRef]  // scalar on the left
      bin.rhs shouldBe a[TIndex]   // array element on the right
    }

    "leave rank-2 element-wise alone (chunk 1 is rank-1 only)" in {
      val body = fBodyAfterFusion("""
        |def f(a: [[real]], b: [[real]]) = a + b
      """.stripMargin)
      body shouldBe a[TElementWise]
    }

    "leave TMatMul alone (it's not a fusion target)" in {
      val body = fBodyAfterFusion("""
        |def f(a: [[real]], b: [[real]]) = a @ b
      """.stripMargin)
      body shouldBe a[TMatMul]
    }

    "leave scalar arithmetic alone" in {
      val body = fBodyAfterFusion("""
        |def f(x: integer, y: integer) = x + y
      """.stripMargin)
      body shouldBe a[TBinOp]
    }
  }

  // ==========================================================================
  // Equivalence — fused vs. un-fused programs produce identical output
  // ==========================================================================

  "fused output matches un-fused output" should {

    "element-wise add" in {
      val src = """
        |def main() =
        |  val a = [1, 2, 3]
        |  val b = [10, 20, 30]
        |  print(a + b)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[11, 22, 33]\n"
    }

    "broadcast scalar * array" in {
      val src = """
        |def main() =
        |  val xs = [1, 2, 3]
        |  print(2 * xs)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[2, 4, 6]\n"
    }

    "broadcast `arr - scalar` (non-commutative regression)" in {
      val src = """
        |def main() =
        |  val xs = [10, 20, 30]
        |  print(xs - 1)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[9, 19, 29]\n"
    }

    "chained element-wise and broadcast still runs" in {
      // Chunk 1 produces two separate fused loops with an intermediate
      // array — no inlining yet. Chunk 2 will inline them into one loop.
      // Either way, the output should match the un-fused interpreter.
      val src = """
        |def main() =
        |  val a = [1.0, 2.0, 3.0]
        |  val b = [10.0, 20.0, 30.0]
        |  print(2.0 * a + b)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[12.0, 24.0, 36.0]\n"
    }
  }
