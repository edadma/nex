package io.github.edadma.nex

import scala.collection.mutable

/** MLIR backend. Compiles a growing surface of Nex programs to MLIR
  * text, which the parity harness lowers via `mlir-opt` and
  * `mlir-translate` and links with `mlir_runtime.c` (supplying
  * `nex_print_i64` / `nex_print_f64`) through `clang`.
  *
  * Surface as of milestone 3:
  *
  *   - `def main() = print(sum([<num literals>]))`
  *   - `def main() = print(sum(a + b))` where `a + b` are rank-1
  *     numeric array literals, possibly bound via `val` inside a block.
  *
  * The codegen is now structured as an expression visitor —
  * [[emitExpr]] takes a typed expression and returns the SSA register
  * plus its MLIR type. A symbol environment (Symbol id → MlirVal)
  * carries val-bindings through nested blocks. Element-wise array
  * addition lowers to `linalg.map` over `tensor.empty()` outputs;
  * `sum` over a tensor lowers to `linalg.reduce` followed by
  * `tensor.extract`. Both still flow through the same one-shot
  * bufferize → linalg-to-loops pipeline from the milestone-1 PoC.
  *
  * Anything outside the recognised shape throws `notYet`.
  */
class NexMLIRCodegen:

  /** MLIR type of an emitted value. Either an LLVM-like scalar or a
    * static-shape tensor. We don't model rank-2 yet — every tensor in
    * milestone 3 is rank-1 with a known length.
    */
  private sealed trait MlirType:
    def text: String

  private case class MScalar(elem: Type) extends MlirType:
    def text: String = scalarText(elem)

  /** A `tensor<...x<elem>>`. `shape == Nil` denotes a 0-d
    * `tensor<elem>` (the shape of `linalg.reduce`'s output).
    */
  private case class MTensor(elem: Type, shape: List[Int]) extends MlirType:
    def text: String =
      val dims = if shape.isEmpty then "" else shape.mkString("", "x", "x")
      s"tensor<$dims${scalarText(elem)}>"

  private case class MlirVal(reg: String, ty: MlirType)

  private val out     = new StringBuilder
  private var nextReg = 0
  private val env     = mutable.Map.empty[Int, MlirVal]

  def compile(tp: TProgram): String =
    val mainDecl = tp.decls.collectFirst {
      case f: TFunDecl if f.sym.name == "main" && f.params.isEmpty => f
    }.getOrElse(notYet("program has no `def main()`"))

    out.append("func.func private @nex_print_i64(i64)\n")
    out.append("func.func private @nex_print_f64(f64)\n\n")
    out.append("func.func @main() -> i32 {\n")
    nextReg = 0
    env.clear()
    val c0 = fresh("c0")
    out.append(s"  $c0 = arith.constant 0 : i32\n")
    emitMainBody(mainDecl.body)
    out.append(s"  func.return $c0 : i32\n")
    out.append("}\n")
    out.toString

  /** Recognise `print(<scalar>)` as the only allowed top-level effect.
    * Anything else inside the body must be a no-binding `TBlock` whose
    * result is the print call.
    */
  private def emitMainBody(body: TExpr): Unit = unwrapEmptyBlock(body) match
    case TCall(TVarRef(p, _, _), List(arg), _, _) if p.name == "print" =>
      emitPrintCall(arg)
    case TBlock(items, TCall(TVarRef(p, _, _), List(arg), _, _), _, _) if p.name == "print" =>
      items.foreach(emitBlockItem)
      emitPrintCall(arg)
    case other =>
      notYet(s"main body shape: ${other.getClass.getSimpleName}")

  private def unwrapEmptyBlock(e: TExpr): TExpr = e match
    case TBlock(Nil, r, _, _) => unwrapEmptyBlock(r)
    case other                => other

  /** Emit `func.call @nex_print_{i64,f64}` for a scalar expression.
    * The argument is evaluated through the visitor; its MLIR scalar
    * type chooses the helper.
    */
  private def emitPrintCall(arg: TExpr): Unit =
    val v = emitExpr(arg)
    v.ty match
      case MScalar(TyInteger) =>
        out.append(s"  func.call @nex_print_i64(${v.reg}) : (i64) -> ()\n")
      case MScalar(TyReal) =>
        out.append(s"  func.call @nex_print_f64(${v.reg}) : (f64) -> ()\n")
      case other =>
        notYet(s"print of $other")

  /** The expression visitor. Every node that lowers must produce a
    * single SSA value with a known MLIR type; nodes that don't fit
    * (assignments, side effects, function calls other than the
    * recognised prelude ones) reject via `notYet`.
    */
  private def emitExpr(e: TExpr): MlirVal = e match
    case TIntLit(v, _, _) =>
      val r = fresh("ci")
      out.append(s"  $r = arith.constant $v : i64\n")
      MlirVal(r, MScalar(TyInteger))

    case TRealLit(v, _, _) =>
      val r = fresh("cr")
      out.append(s"  $r = arith.constant ${formatReal(v)} : f64\n")
      MlirVal(r, MScalar(TyReal))

    case TUnaryOp("-", inner, _, _) =>
      foldLiteralNeg(inner) match
        case Some(lit) => emitExpr(lit)
        case None      => notYet(s"unary minus on non-literal: ${inner.getClass.getSimpleName}")

    case TArrayLit(elems, _, TyArray(elemT, 1)) if elems.nonEmpty =>
      val vals = elems.map(emitExpr)
      vals.foreach { v =>
        v.ty match
          case MScalar(t) if t == elemT => ()
          case other                    => notYet(s"array element type mismatch: $other vs $elemT")
      }
      val ty = MTensor(elemT, List(elems.size))
      val r  = fresh("arr")
      out.append(s"  $r = tensor.from_elements ${vals.map(_.reg).mkString(", ")} : ${ty.text}\n")
      MlirVal(r, ty)

    case TVarRef(sym, _, _) =>
      env.getOrElse(sym.id, notYet(s"unbound symbol ${sym.name}#${sym.id}"))

    case TBlock(items, result, _, _) =>
      items.foreach(emitBlockItem)
      emitExpr(result)

    case TElementWise(op, lhs, rhs, _, _) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty) match
        case (lt: MTensor, rt: MTensor) if lt == rt =>
          emitElementWiseBinop(op, lv, rv, lt)
        case (lt, rt) =>
          notYet(s"element-wise $op on $lt and $rt")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "sum" =>
      val av = emitExpr(arr)
      av.ty match
        case t: MTensor => emitSumReduce(av.reg, t)
        case other      => notYet(s"sum over $other")

    case other =>
      notYet(s"expression: ${other.getClass.getSimpleName}")

  private def emitBlockItem(it: TBlockItem): Unit = it match
    case TBlockBinding(sym, BindingKind.Val, value) =>
      env(sym.id) = emitExpr(value)
    case TBlockBinding(sym, kind, _) =>
      notYet(s"$kind binding for ${sym.name}")
    case TBlockExpr(_) =>
      notYet("statement-position expressions in block")

  /** Element-wise binop via `linalg.map` over a fresh `tensor.empty()`
    * output. Both operands must already have the same tensor type.
    *
    * LLVM 22 quirk: the `linalg.map` block arity is `inputs + outputs`,
    * not `inputs` as the dialect docs suggest. The out argument is
    * unused — we discard its value and yield only the computed
    * element — but the verifier requires it.
    */
  private def emitElementWiseBinop(op: String, lv: MlirVal, rv: MlirVal, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val opName = scalarBinop(op, elemT)
    val initR  = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val outR = fresh("ew")
    out.append(
      s"  $outR = linalg.map ins(${lv.reg}, ${rv.reg} : ${ty.text}, ${ty.text}) outs($initR : ${ty.text})\n",
    )
    out.append(s"    (%a: $scalar, %b: $scalar, %_o: $scalar) {\n")
    out.append(s"      %s = $opName %a, %b : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Sum-reduce a rank-1 tensor to a 0-d tensor, then extract the
    * scalar. Init value is the element-type zero; reducer is the
    * element-type add.
    */
  private def emitSumReduce(srcReg: String, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val outTy  = MTensor(elemT, Nil)
    val initER = fresh("init_e")
    out.append(s"  $initER = arith.constant ${zeroLit(elemT)} : $scalar\n")
    val initR = fresh("init")
    out.append(s"  $initR = tensor.from_elements $initER : ${outTy.text}\n")
    val sumTR = fresh("sum_t")
    out.append(
      s"  $sumTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [0]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("+", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val sumR = fresh("sum")
    out.append(s"  $sumR = tensor.extract $sumTR[] : ${outTy.text}\n")
    MlirVal(sumR, MScalar(elemT))

  /** Fold a unary-minus over a numeric literal. Returns `None` for
    * non-literal operands so the caller can decide whether to reject.
    * Constant folding past unary minus on arbitrary expressions is
    * out of scope until we have proper `arith.subi` / `arith.negf`
    * emission.
    */
  private def foldLiteralNeg(e: TExpr): Option[TExpr] = e match
    case TIntLit(v, p, t)  => Some(TIntLit(-v, p, t))
    case TRealLit(v, p, t) => Some(TRealLit(-v, p, t))
    case _                 => None

  private def scalarText(t: Type): String = t match
    case TyInteger => "i64"
    case TyReal    => "f64"
    case other     => notYet(s"scalar text for $other")

  private def zeroLit(t: Type): String = t match
    case TyInteger => "0"
    case TyReal    => "0.0"
    case other     => notYet(s"zero literal for $other")

  /** Map a Nex binop + element type to the `arith.*` op that performs
    * it on a scalar. Used uniformly inside `linalg.reduce` and
    * `linalg.map` body regions, so both reductions and element-wise
    * ops share the dispatch table.
    */
  private def scalarBinop(op: String, t: Type): String = (op, t) match
    case ("+", TyInteger) => "arith.addi"
    case ("-", TyInteger) => "arith.subi"
    case ("*", TyInteger) => "arith.muli"
    case ("+", TyReal)    => "arith.addf"
    case ("-", TyReal)    => "arith.subf"
    case ("*", TyReal)    => "arith.mulf"
    case _                => notYet(s"scalar binop $op on $t")

  /** Render a `Double` so MLIR's FloatAttr parser accepts it. Whole
    * numbers get a trailing `.0`; everything else uses Scala's
    * shortest-round-trip `toString`. NaN / Inf intentionally rejected
    * — not reachable from a literal source program at this milestone.
    */
  private def formatReal(v: Double): String =
    if v.isNaN || v.isInfinite then notYet(s"non-finite real literal: $v")
    else
      val s = v.toString
      if s.contains('.') || s.contains('e') || s.contains('E') then s
      else s + ".0"

  private def fresh(prefix: String): String =
    val r = s"%${prefix}${nextReg}"
    nextReg += 1
    r

  private def notYet(msg: String): Nothing =
    throw new UnsupportedOperationException(s"NexMLIRCodegen: $msg")
