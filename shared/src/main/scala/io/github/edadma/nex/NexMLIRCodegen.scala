package io.github.edadma.nex

/** Milestone-1 MLIR backend. Compiles exactly one shape of program:
  *
  *   def main() = print(sum([<integer literals>]))
  *
  * Anything outside that shape is rejected with `notYet`. The point of
  * the milestone is to land the plumbing — Scala-side emission, the
  * parity-harness lowering pipeline, runtime linkage — not to cover
  * features. Subsequent milestones expand the surface.
  *
  * Emission strategy is string concatenation against the
  * hand-validated `mlir-poc/sum5.mlir` shape; no Scala-side MLIR AST
  * yet. The output runs through
  *
  *   mlir-opt --one-shot-bufferize="bufferize-function-boundaries"
  *            --convert-linalg-to-loops --convert-scf-to-cf
  *            --finalize-memref-to-llvm --convert-arith-to-llvm
  *            --convert-func-to-llvm --convert-cf-to-llvm
  *            --reconcile-unrealized-casts
  *   mlir-translate --mlir-to-llvmir
  *   clang -O1 ... mlir_runtime.c
  *
  * and links against `nex_print_i64` from `mlir_runtime.c`.
  */
class NexMLIRCodegen:

  private val out = new StringBuilder

  def compile(tp: TProgram): String =
    val mainDecl = tp.decls.collectFirst {
      case f: TFunDecl if f.sym.name == "main" && f.params.isEmpty => f
    }.getOrElse(notYet("program has no `def main()`"))

    out.append("func.func private @nex_print_i64(i64)\n\n")
    out.append("func.func @main() -> i32 {\n")
    out.append("  %c0 = arith.constant 0 : i32\n")
    emitMainBody(mainDecl.body)
    out.append("  func.return %c0 : i32\n")
    out.append("}\n")
    out.toString

  /** The body of `main`. Only `print(sum([...]))` is recognised for
    * milestone 1. The print and sum sites are pattern-matched directly
    * so the emitted MLIR is the minimal hand-shape from the PoC. A
    * `def f() = expr` body sometimes elaborates as a `TBlock` whose
    * result is the call, so we unwrap a no-binding block.
    */
  private def emitMainBody(body: TExpr): Unit = unwrap(body) match
    case TCall(TVarRef(p, _, _), List(inner), _, _) if p.name == "print" =>
      val sumReg = emitIntScalar(inner)
      out.append(s"  func.call @nex_print_i64($sumReg) : (i64) -> ()\n")
    case _ =>
      notYet(s"main body shape: ${body.getClass.getSimpleName}")

  private def unwrap(e: TExpr): TExpr = e match
    case TBlock(Nil, r, _, _) => unwrap(r)
    case other                => other

  /** Emit MLIR producing an `i64` SSA value. For milestone 1 this is
    * always `sum(<rank-1 integer array literal>)`.
    */
  private def emitIntScalar(e: TExpr): String = e match
    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "sum" =>
      emitSumOfArrayLit(arr)
    case _ =>
      notYet(s"int-scalar expression: ${e.getClass.getSimpleName}")

  /** Emit `tensor.from_elements` + `linalg.reduce { arith.addi }` for a
    * rank-1 integer array literal, returning the SSA register holding
    * the extracted scalar sum.
    */
  private def emitSumOfArrayLit(arr: TExpr): String = arr match
    case TArrayLit(elems, _, _) if elems.nonEmpty && elems.forall(_.isInstanceOf[TIntLit]) =>
      val ints = elems.collect { case TIntLit(v, _, _) => v }
      val regs = ints.zipWithIndex.map { (v, i) =>
        val r = s"%c${i + 1}"
        out.append(s"  $r = arith.constant $v : i64\n")
        r
      }
      val n     = elems.size
      val arrR  = "%arr"
      out.append(s"  $arrR = tensor.from_elements ${regs.mkString(", ")} : tensor<${n}xi64>\n")
      out.append("  %init_e = arith.constant 0 : i64\n")
      out.append("  %init = tensor.from_elements %init_e : tensor<i64>\n")
      out.append(
        s"  %sum_t = linalg.reduce ins($arrR : tensor<${n}xi64>) outs(%init : tensor<i64>) dimensions = [0]\n",
      )
      out.append("    (%in: i64, %acc: i64) {\n")
      out.append("      %s = arith.addi %in, %acc : i64\n")
      out.append("      linalg.yield %s : i64\n")
      out.append("    }\n")
      out.append("  %sum = tensor.extract %sum_t[] : tensor<i64>\n")
      "%sum"
    case _ =>
      notYet(s"sum argument: ${arr.getClass.getSimpleName}")

  private def notYet(msg: String): Nothing =
    throw new UnsupportedOperationException(s"NexMLIRCodegen: $msg")
