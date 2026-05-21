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
      bin.lhs shouldBe a[TFlatIndex] // array element on the left
      bin.rhs shouldBe a[TVarRef]    // scalar on the right
    }

    "rewrite rank-1 scalar - arr with the right operand order in the body" in {
      val body = fBodyAfterFusion("""
        |def f(xs: [integer]) = 100 - xs
      """.stripMargin)
      val loop = body.asInstanceOf[TBlock].result.asInstanceOf[TFusedLoop]
      val bin  = loop.body.asInstanceOf[TBinOp]
      bin.op shouldBe "-"
      bin.lhs shouldBe a[TVarRef]    // scalar on the left
      bin.rhs shouldBe a[TFlatIndex] // array element on the right
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
      // Chunk 2 inlines the chain into a single fused loop (verified by
      // the shape test below); the interpreter output must match the
      // un-fused baseline either way.
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

  // ==========================================================================
  // Chain inlining — nested fused subtrees collapse into one loop
  // ==========================================================================

  "chain inlining" should {

    "collapse `2 * a + b` into a single TFusedLoop" in {
      // Stage 3 lowers to TElementWise("+", TBroadcast(2, a, "*", sf=true), b).
      // After fusion, the outer TElementWise should produce ONE TFusedLoop
      // — no nested TBlock holding another TFusedLoop. The fused-loop
      // body should reference both a's elements (via the inlined inner
      // body) and b's elements directly.
      val body = fBodyAfterFusion("""
        |def f(a: [real], b: [real]) = 2.0 * a + b
      """.stripMargin)
      body shouldBe a[TBlock]
      val block = body.asInstanceOf[TBlock]
      block.result shouldBe a[TFusedLoop]
      val loop = block.result.asInstanceOf[TFusedLoop]
      // Body must NOT contain a nested TFusedLoop — chain inlining
      // is responsible for collapsing them.
      countFusedLoops(loop.body) shouldBe 0
    }

    "collapse `(a + b) - c` into a single TFusedLoop" in {
      val body = fBodyAfterFusion("""
        |def f(a: [real], b: [real], c: [real]) = (a + b) - c
      """.stripMargin)
      val block = body.asInstanceOf[TBlock]
      val loop  = block.result.asInstanceOf[TFusedLoop]
      countFusedLoops(loop.body) shouldBe 0
    }

    "collapse `a - 1 + b` (broadcast + element-wise chain)" in {
      val body = fBodyAfterFusion("""
        |def f(a: [integer], b: [integer]) = a - 1 + b
      """.stripMargin)
      val block = body.asInstanceOf[TBlock]
      val loop  = block.result.asInstanceOf[TFusedLoop]
      countFusedLoops(loop.body) shouldBe 0
    }

    "inlined chain still produces the right values (regression)" in {
      // `2.0 * a + b - 1.0` → one loop in fusion; the operand-order
      // for `- 1.0` must be `(prev) - 1.0`, not `1.0 - (prev)`.
      val src = """
        |def main() =
        |  val a = [1.0, 2.0, 3.0]
        |  val b = [10.0, 20.0, 30.0]
        |  print(2.0 * a + b - 1.0)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[11.0, 23.0, 35.0]\n"
    }

    "inlined chain preserves operand order on the inner broadcast" in {
      // The inner `a - 1` (scalarFirst=false) must inline as
      // `tmp_a[i] - 1`, not `1 - tmp_a[i]`, when chained into an outer
      // element-wise.
      val src = """
        |def main() =
        |  val a = [10, 20, 30]
        |  val b = [1, 2, 3]
        |  print((a - 1) + b)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[10, 21, 32]\n"
    }
  }

  // ==========================================================================
  // Prelude map fusion — `map(arr, x -> body)` inlines lambda body
  // ==========================================================================

  "map fusion" should {

    "rewrite `map(xs, x -> x * 2)` into a TBlock(TFusedLoop)" in {
      val body = fBodyAfterFusion("""
        |def f(xs: [integer]) = map(xs, x -> x * 2)
      """.stripMargin)
      body shouldBe a[TBlock]
      val block = body.asInstanceOf[TBlock]
      block.result shouldBe a[TFusedLoop]
      val loop = block.result.asInstanceOf[TFusedLoop]
      countFusedLoops(loop.body) shouldBe 0
      // The lambda body `x * 2` should be inlined: body is a TBinOp
      // whose LHS is no longer a TVarRef to the lambda param (it's
      // been substituted with `tmp_a[i]`).
      loop.body shouldBe a[TBinOp]
    }

    "chain map with element-wise — `map(xs, x -> x * 2) + ys` is one loop" in {
      val body = fBodyAfterFusion("""
        |def f(xs: [integer], ys: [integer]) = map(xs, x -> x * 2) + ys
      """.stripMargin)
      val loop = body.asInstanceOf[TBlock].result.asInstanceOf[TFusedLoop]
      countFusedLoops(loop.body) shouldBe 0
    }

    "leave `map(xs, f)` alone when f is not a registered lambda" in {
      // Chunk 4's named-lambda chasing only fires for [[TVarRef]]s that
      // resolve to a binding whose value is a TLambda — and even then only
      // when the binding has been processed earlier in the walk. A bare
      // [[TVarRef]] to a function parameter (no recorded lambda) is left
      // alone, as is a TVarRef whose binding lives in another module.
      val body = fBodyAfterFusion("""
        |def f(xs: [integer], g: (integer -> integer)) = map(xs, g)
      """.stripMargin)
      val mapCall = body.asInstanceOf[TCall]
      mapCall.callee.asInstanceOf[TVarRef].sym.name shouldBe "map"
    }

    "fused map produces the same output as un-fused map (basic)" in {
      val src = """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(map(xs, x -> x * 10))
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[10, 20, 30, 40]\n"
    }

    "fused map.sum chain produces the right value" in {
      // `sum(map(xs, x -> x * 2))` — the map fuses to a TFusedLoop and
      // sum runs over the result. Interpreter must handle both shapes.
      val src = """
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(sum(map(xs, x -> x * 2)))
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "20\n"
    }

    "fused chain: map result feeds into element-wise" in {
      // The fused `map(xs, x -> x * 2)` collapses with the outer `+ ys`
      // into a single loop.
      val src = """
        |def main() =
        |  val xs = [1, 2, 3]
        |  val ys = [100, 200, 300]
        |  print(map(xs, x -> x * 2) + ys)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[102, 204, 306]\n"
    }
  }

  // ==========================================================================
  // Named-lambda chasing — `map(xs, f)` where `f` is a TVarRef that
  // resolves to a registered TLambda. Lookup is sequential, so the
  // binding must lexically precede the call site within the program.
  // ==========================================================================

  "named-lambda chasing" should {

    "fuse `map(xs, f)` where f is a top-level val lambda binding" in {
      val body = fBodyAfterFusion("""
        |val mul2 = x -> x * 2
        |def f(xs: [integer]) = map(xs, mul2)
      """.stripMargin)
      body shouldBe a[TBlock]
      val block = body.asInstanceOf[TBlock]
      block.result shouldBe a[TFusedLoop]
      val loop = block.result.asInstanceOf[TFusedLoop]
      // Lambda body inlined into the loop body; no nested fused loops.
      countFusedLoops(loop.body) shouldBe 0
      loop.body shouldBe a[TBinOp]
    }

    "fuse `map(xs, g)` where g is a block-level val lambda binding" in {
      val body = fBodyAfterFusion("""
        |def f(xs: [integer]) =
        |  val g = x -> x * 2
        |  map(xs, g)
      """.stripMargin)
      // The outer block has the `val g = ...` item; its result is the
      // fused map call. Drill in to assert the fused shape.
      val outer = body.asInstanceOf[TBlock]
      outer.result shouldBe a[TBlock]
      val fused = outer.result.asInstanceOf[TBlock]
      fused.result shouldBe a[TFusedLoop]
      countFusedLoops(fused.result.asInstanceOf[TFusedLoop].body) shouldBe 0
    }

    "leave `map(xs, f)` alone when f is a function parameter" in {
      // Function parameters aren't registered in lambdaBindings — no
      // lambda value is known at fuse time. The TCall stays.
      val body = fBodyAfterFusion("""
        |def f(xs: [integer], g: (integer -> integer)) = map(xs, g)
      """.stripMargin)
      val mapCall = body.asInstanceOf[TCall]
      mapCall.callee.asInstanceOf[TVarRef].sym.name shouldBe "map"
    }

    "chain `map(xs, f) + ys` collapses to one loop when f is named" in {
      val body = fBodyAfterFusion("""
        |val mul2 = x -> x * 2
        |def f(xs: [integer], ys: [integer]) = map(xs, mul2) + ys
      """.stripMargin)
      val outer = body.asInstanceOf[TBlock]
      val loop  = outer.result.asInstanceOf[TFusedLoop]
      countFusedLoops(loop.body) shouldBe 0
    }

    "fused top-level named lambda produces the same output as un-fused" in {
      val src = """
        |val mul2 = x -> x * 2
        |
        |def main() =
        |  val xs = [1, 2, 3, 4]
        |  print(map(xs, mul2))
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[2, 4, 6, 8]\n"
    }

    "fused block-level named lambda produces the same output as un-fused" in {
      val src = """
        |def main() =
        |  val g = x -> x + 100
        |  val xs = [1, 2, 3]
        |  print(map(xs, g))
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[101, 102, 103]\n"
    }

    "named lambda used by two call sites fuses both" in {
      // Each call site gets its own copy of the lambda body, so two
      // independent fused loops form.
      val src = """
        |val mul2 = x -> x * 2
        |
        |def main() =
        |  val xs = [1, 2, 3]
        |  val ys = [10, 20, 30]
        |  print(map(xs, mul2))
        |  print(map(ys, mul2))
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[2, 4, 6]\n[20, 40, 60]\n"
    }

    "named lambda + element-wise chain runs the same fused and un-fused" in {
      val src = """
        |val mul2 = x -> x * 2
        |
        |def main() =
        |  val xs = [1, 2, 3]
        |  val ys = [100, 200, 300]
        |  print(map(xs, mul2) + ys)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[102, 204, 306]\n"
    }
  }

  // ==========================================================================
  // Rank-2 fusion — flat-loop body via TFlatIndex; result wraps in a
  // rank-2 array via TFusedLoop.cols.
  // ==========================================================================

  "rank-2 fusion" should {

    "rewrite rank-2 array + array into TBlock(TFusedLoop) with cols set" in {
      val body = fBodyAfterFusion("""
        |def f(a: [[real]], b: [[real]]) = a + b
      """.stripMargin)
      body shouldBe a[TBlock]
      val block = body.asInstanceOf[TBlock]
      block.result shouldBe a[TFusedLoop]
      val loop = block.result.asInstanceOf[TFusedLoop]
      loop.tpe shouldBe TyArray(TyReal, 2)
      loop.cols should not be None
      // Loop body uses TFlatIndex (rank-agnostic flat access).
      val bin = loop.body.asInstanceOf[TBinOp]
      bin.lhs shouldBe a[TFlatIndex]
      bin.rhs shouldBe a[TFlatIndex]
    }

    "rewrite rank-2 scalar * array into TBlock(TFusedLoop)" in {
      val body = fBodyAfterFusion("""
        |def f(a: [[real]]) = 2.0 * a
      """.stripMargin)
      val loop = body.asInstanceOf[TBlock].result.asInstanceOf[TFusedLoop]
      loop.tpe shouldBe TyArray(TyReal, 2)
      loop.cols should not be None
    }

    "rewrite rank-2 arr - scalar with the right operand order in the body" in {
      val body = fBodyAfterFusion("""
        |def f(a: [[integer]]) = a - 1
      """.stripMargin)
      val loop = body.asInstanceOf[TBlock].result.asInstanceOf[TFusedLoop]
      val bin  = loop.body.asInstanceOf[TBinOp]
      bin.op shouldBe "-"
      bin.lhs shouldBe a[TFlatIndex] // element on the left
      bin.rhs shouldBe a[TVarRef]    // scalar on the right
    }

    "rewrite `map(m, x -> x * 2)` over rank-2 array" in {
      val body = fBodyAfterFusion("""
        |def f(m: [[integer]]) = map(m, x -> x * 2)
      """.stripMargin)
      val loop = body.asInstanceOf[TBlock].result.asInstanceOf[TFusedLoop]
      loop.cols should not be None
      loop.tpe shouldBe TyArray(TyInteger, 2)
      countFusedLoops(loop.body) shouldBe 0
    }

    "leave TMatMul alone (still not a fusion target)" in {
      val body = fBodyAfterFusion("""
        |def f(a: [[real]], b: [[real]]) = a @ b
      """.stripMargin)
      body shouldBe a[TMatMul]
    }

    "rank-2 element-wise add: fused matches un-fused" in {
      val src = """
        |def main() =
        |  val a = [[1, 2], [3, 4]]
        |  val b = [[10, 20], [30, 40]]
        |  print(a + b)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[[11, 22], [33, 44]]\n"
    }

    "rank-2 broadcast scalar * matrix: fused matches un-fused" in {
      val src = """
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(10 * m)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[[10, 20, 30], [40, 50, 60]]\n"
    }

    "rank-2 broadcast matrix - scalar (non-commutative): fused matches un-fused" in {
      val src = """
        |def main() =
        |  val m = [[10, 20], [30, 40]]
        |  print(m - 1)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[[9, 19], [29, 39]]\n"
    }

    "rank-2 map with inline lambda: fused matches un-fused" in {
      val src = """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  print(map(m, x -> x + 100))
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[[101, 102], [103, 104]]\n"
    }

    "rank-2 map result feeds into rank-2 element-wise: fused matches un-fused" in {
      // Chain inlining over rank-2: map producer collapses with the outer
      // rank-2 element-wise add into a single flat loop.
      val src = """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  val n = [[100, 200], [300, 400]]
        |  print(map(m, x -> x * 10) + n)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[[110, 220], [330, 440]]\n"
    }

    "rank-2 chain `2 * a + b` collapses to a single TFusedLoop" in {
      val body = fBodyAfterFusion("""
        |def f(a: [[integer]], b: [[integer]]) = 2 * a + b
      """.stripMargin)
      val block = body.asInstanceOf[TBlock]
      block.result shouldBe a[TFusedLoop]
      val loop = block.result.asInstanceOf[TFusedLoop]
      loop.cols should not be None
      countFusedLoops(loop.body) shouldBe 0
    }

    "rank-2 chain `2 * a + b` runs the same fused and un-fused" in {
      val src = """
        |def main() =
        |  val a = [[1, 2], [3, 4]]
        |  val b = [[100, 200], [300, 400]]
        |  print(2 * a + b)
      """.stripMargin
      runFused(src) shouldBe runUnfused(src)
      runFused(src) shouldBe "[[102, 204], [306, 408]]\n"
    }
  }

  /** Recursively count TFusedLoop nodes in an expression tree (for chain-
    * inlining assertions). Walks every TExpr variant that can contain
    * children.
    */
  private def countFusedLoops(e: TExpr): Int = e match
    case TFusedLoop(_, len, body, cols, _, _) => 1 + countFusedLoops(len) + countFusedLoops(body) + cols.map(countFusedLoops).getOrElse(0)
    case TFlatIndex(a, i, _, _)         => countFusedLoops(a) + countFusedLoops(i)
    case TBinOp(_, l, r, _, _)          => countFusedLoops(l) + countFusedLoops(r)
    case TUnaryOp(_, x, _, _)           => countFusedLoops(x)
    case TJuxtapose(c, b, _, _)         => countFusedLoops(c) + countFusedLoops(b)
    case TCall(c, args, _, _)           => countFusedLoops(c) + args.map(countFusedLoops).sum
    case TIndex(a, i, _, _)             => countFusedLoops(a) + i.map(countFusedLoops).sum
    case TSlice(a, lo, hi, _, st, _, _) => countFusedLoops(a) + lo.map(countFusedLoops).getOrElse(0) + hi.map(countFusedLoops).getOrElse(0) + st.map(countFusedLoops).getOrElse(0)
    case TField(r, _, _, _)             => countFusedLoops(r)
    case TTupleProj(r, _, _, _)         => countFusedLoops(r)
    case TMethodCall(r, _, args, _, _)  => countFusedLoops(r) + args.map(countFusedLoops).sum
    case TLambda(_, body, _, _)         => countFusedLoops(body)
    case TTuple(es, _, _)               => es.map(countFusedLoops).sum
    case TArrayLit(es, _, _)            => es.map(countFusedLoops).sum
    case TIf(c, th, el, _, _)           => countFusedLoops(c) + countFusedLoops(th) + el.map(countFusedLoops).getOrElse(0)
    case TFor(_, it, b, _, _)           => countFusedLoops(it) + countFusedLoops(b)
    case TWhile(c, b, _, _)             => countFusedLoops(c) + countFusedLoops(b)
    case TReturn(v, _, _)               => v.map(countFusedLoops).getOrElse(0)
    case TAssign(t, v, _, _)            => countFusedLoops(t) + countFusedLoops(v)
    case TBlock(items, r, _, _) =>
      items.map {
        case TBlockBinding(_, _, v) => countFusedLoops(v)
        case TBlockExpr(x)          => countFusedLoops(x)
      }.sum + countFusedLoops(r)
    case TElementWise(_, l, r, _, _)    => countFusedLoops(l) + countFusedLoops(r)
    case TBroadcast(s, a, _, _, _, _)   => countFusedLoops(s) + countFusedLoops(a)
    case TMap(a, f, _, _)               => countFusedLoops(a) + countFusedLoops(f)
    case TReduce(a, i, f, _, _)         => countFusedLoops(a) + countFusedLoops(i) + countFusedLoops(f)
    case TMatMul(l, r, _, _)            => countFusedLoops(l) + countFusedLoops(r)
    case _                              => 0
