package io.github.edadma.nex

/** MLIR backend. Compiles a tiny but growing surface of Nex programs to
  * MLIR text, which the parity harness lowers via `mlir-opt` and
  * `mlir-translate` and links with `mlir_runtime.c` (supplying
  * `nex_print_i64` / `nex_print_f64`) through `clang`.
  *
  * Current surface — milestone 2:
  *
  *   def main() = print(sum([<int literals>]))     // M1: i64 path
  *   def main() = print(sum([<real literals>]))    // M2: f64 path
  *
  * The element type of the array literal (taken from its elaborated
  * `tpe`) drives which arithmetic ops and print helper are emitted.
  * Anything outside the recognised shape throws `notYet`.
  */
class NexMLIRCodegen:

  private val out = new StringBuilder

  def compile(tp: TProgram): String =
    val mainDecl = tp.decls.collectFirst {
      case f: TFunDecl if f.sym.name == "main" && f.params.isEmpty => f
    }.getOrElse(notYet("program has no `def main()`"))

    out.append("func.func private @nex_print_i64(i64)\n")
    out.append("func.func private @nex_print_f64(f64)\n\n")
    out.append("func.func @main() -> i32 {\n")
    out.append("  %c0 = arith.constant 0 : i32\n")
    emitMainBody(mainDecl.body)
    out.append("  func.return %c0 : i32\n")
    out.append("}\n")
    out.toString

  /** The body of `main`. Only `print(sum([...]))` is recognised; the
    * argument's element type chooses between the i64 and f64 paths.
    * A `def f() = expr` body sometimes elaborates as a `TBlock` whose
    * result is the call, so we unwrap a no-binding block.
    */
  private def emitMainBody(body: TExpr): Unit = unwrap(body) match
    case TCall(TVarRef(p, _, _), List(inner), _, _) if p.name == "print" =>
      val (reg, elem) = emitScalar(inner)
      val helper = elem match
        case TyInteger => "nex_print_i64"
        case TyReal    => "nex_print_f64"
        case other     => notYet(s"print of element type $other")
      out.append(s"  func.call @$helper($reg) : (${mlirElem(elem)}) -> ()\n")
    case _ =>
      notYet(s"main body shape: ${body.getClass.getSimpleName}")

  private def unwrap(e: TExpr): TExpr = e match
    case TBlock(Nil, r, _, _) => unwrap(r)
    case other                => other

  /** Emit MLIR producing a scalar SSA value. Returns (register, elem
    * type). For milestones 1+2 the only recognised expression is
    * `sum(<rank-1 numeric array literal>)`.
    */
  private def emitScalar(e: TExpr): (String, Type) = e match
    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "sum" =>
      emitSumOfArrayLit(arr)
    case _ =>
      notYet(s"scalar expression: ${e.getClass.getSimpleName}")

  /** Emit `tensor.from_elements` + `linalg.reduce { arith.add* }` for a
    * rank-1 numeric array literal. The array's elaborated element type
    * picks between the i64 (`addi`, zero init) and f64 (`addf`, 0.0
    * init) lowerings; both end with `tensor.extract` to a scalar
    * register, which is returned alongside the element type.
    */
  private def emitSumOfArrayLit(arr: TExpr): (String, Type) = arr match
    case lit @ TArrayLit(elems, _, TyArray(elemT, 1)) if elems.nonEmpty =>
      val mlirT = mlirElem(elemT)
      val regs  = elems.zipWithIndex.map { (e, i) =>
        val r = s"%c${i + 1}"
        out.append(s"  $r = arith.constant ${constLit(e, elemT)} : $mlirT\n")
        r
      }
      val n    = elems.size
      val arrR = "%arr"
      out.append(s"  $arrR = tensor.from_elements ${regs.mkString(", ")} : tensor<${n}x$mlirT>\n")
      out.append(s"  %init_e = arith.constant ${zeroLit(elemT)} : $mlirT\n")
      out.append(s"  %init = tensor.from_elements %init_e : tensor<$mlirT>\n")
      out.append(
        s"  %sum_t = linalg.reduce ins($arrR : tensor<${n}x$mlirT>) outs(%init : tensor<$mlirT>) dimensions = [0]\n",
      )
      out.append(s"    (%in: $mlirT, %acc: $mlirT) {\n")
      out.append(s"      %s = ${addOp(elemT)} %in, %acc : $mlirT\n")
      out.append(s"      linalg.yield %s : $mlirT\n")
      out.append("    }\n")
      out.append(s"  %sum = tensor.extract %sum_t[] : tensor<$mlirT>\n")
      ("%sum", elemT)
    case _ =>
      notYet(s"sum argument: ${arr.getClass.getSimpleName}")

  /** MLIR scalar type for a Nex element type. */
  private def mlirElem(t: Type): String = t match
    case TyInteger => "i64"
    case TyReal    => "f64"
    case other     => notYet(s"element type $other")

  /** Constant-op literal text for a scalar literal. Integers print as
    * their decimal form; reals must include a decimal point so MLIR
    * parses them as `FloatAttr` rather than `IntegerAttr` (`6.0e0`
    * also works). MLIR rejects `arith.constant 6 : f64`.
    */
  private def constLit(e: TExpr, elemT: Type): String = (foldLiteral(e), elemT) match
    case (TIntLit(v, _, _), TyInteger)   => v.toString
    case (TIntLit(v, _, _), TyReal)      => f"$v%d.0"
    case (TRealLit(v, _, _), TyReal)     => formatReal(v)
    case _ =>
      notYet(s"constant literal: ${e.getClass.getSimpleName} as $elemT")

  /** Fold a literal-only expression to a single literal node — covers
    * unary `-` on a numeric literal, which the parser leaves as
    * `TUnaryOp("-", TIntLit/TRealLit)`. Constant folding past unary
    * minus elsewhere (e.g. `-x`) is out of scope.
    */
  private def foldLiteral(e: TExpr): TExpr = e match
    case TUnaryOp("-", TIntLit(v, p, t), _, _)  => TIntLit(-v, p, t)
    case TUnaryOp("-", TRealLit(v, p, t), _, _) => TRealLit(-v, p, t)
    case other                                   => other

  private def zeroLit(t: Type): String = t match
    case TyInteger => "0"
    case TyReal    => "0.0"
    case other     => notYet(s"zero literal for $other")

  private def addOp(t: Type): String = t match
    case TyInteger => "arith.addi"
    case TyReal    => "arith.addf"
    case other     => notYet(s"add op for $other")

  /** Render a `Double` so MLIR's FloatAttr parser accepts it. Whole
    * numbers get a trailing `.0`; everything else uses Scala's
    * shortest-round-trip `toString`, which already includes a `.` or
    * `e` for non-integral doubles. Special-case NaN / Inf — MLIR
    * accepts `0x7FF...` hex literals; we don't expect those from a
    * literal source program for milestone 2.
    */
  private def formatReal(v: Double): String =
    if v.isNaN || v.isInfinite then notYet(s"non-finite real literal: $v")
    else
      val s = v.toString
      if s.contains('.') || s.contains('e') || s.contains('E') then s
      else s + ".0"

  private def notYet(msg: String): Nothing =
    throw new UnsupportedOperationException(s"NexMLIRCodegen: $msg")
