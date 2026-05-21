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

  private val out             = new StringBuilder
  private var nextReg         = 0
  private val env             = mutable.Map.empty[Int, MlirVal]
  /** `var` scalar bindings live in stack memrefs so that
    * load-modify-store patterns and loop-mutated counters compile to
    * the obvious code. Keyed by symbol id; the value carries the
    * memref SSA name and the element type. Registered when a
    * `var x = init` binding is emitted; consulted by both `TVarRef`
    * (which loads) and `TAssign` (which stores).
    */
  private val varSlots        = mutable.Map.empty[Int, (String, MScalar)]
  /** Per-program registry of `@intrinsic("libm.X")` function symbols.
    * Populated at the start of [[compile]] by scanning every
    * [[TFunDecl]] whose body is a [[TIntrinsic]]. At a [[TCall]] site
    * the codegen looks the callee's symbol id up here; on a hit the
    * call lowers to a direct `func.call @X(...)` instead of routing
    * through the generic call path (which still rejects everything
    * else as `notYet`).
    */
  private val libmIntrinsics  = mutable.Map.empty[Int, String]

  def compile(tp: TProgram): String =
    val mainDecl = tp.decls.collectFirst {
      case f: TFunDecl if f.sym.name == "main" && f.params.isEmpty => f
    }.getOrElse(notYet("program has no `def main()`"))

    libmIntrinsics.clear()
    tp.allDecls.foreach {
      case f: TFunDecl =>
        f.body match
          case TIntrinsic(opId, _, _) if opId.startsWith("libm.") =>
            libmIntrinsics(f.sym.id) = opId.stripPrefix("libm.")
          case _ => ()
      case _ => ()
    }

    out.append("func.func private @nex_print_i64(i64)\n")
    out.append("func.func private @nex_print_f64(f64)\n")
    out.append("func.func private @nex_print_bool(i1)\n")
    out.append("func.func private @nex_print_array_1d_i64(i64, i64)\n")
    out.append("func.func private @nex_print_array_1d_f64(i64, i64)\n")
    out.append("func.func private @nex_print_array_2d_i64(i64, i64, i64)\n")
    out.append("func.func private @nex_print_array_2d_f64(i64, i64, i64)\n")
    out.append("func.func private @nex_print_array_1d_bool(i64, i64)\n")
    out.append("func.func private @nex_print_array_2d_bool(i64, i64, i64)\n")
    out.append("func.func private @nex_ipow(i64, i64) -> i64\n")
    // libm bridges declared by the `@intrinsic` decls discovered above.
    // Two-argument libm fns (atan2, hypot, pow) get a (f64, f64) -> f64
    // signature; everything else is unary. `pow` is also declared
    // unconditionally because the `^` operator routes there.
    out.append("func.func private @pow(f64, f64) -> f64\n")
    libmIntrinsics.values.toSeq.sorted.distinct.foreach {
      case "pow"                  => ()  // already declared above
      case n @ ("atan2" | "hypot") =>
        out.append(s"func.func private @$n(f64, f64) -> f64\n")
      case n =>
        out.append(s"func.func private @$n(f64) -> f64\n")
    }
    out.append("\n")
    out.append("func.func @main() -> i32 {\n")
    nextReg = 0
    env.clear()
    val c0 = fresh("c0")
    out.append(s"  $c0 = arith.constant 0 : i32\n")
    emitTopBindings(tp)
    emitMainBody(mainDecl.body)
    out.append(s"  func.return $c0 : i32\n")
    out.append("}\n")
    out.toString

  /** Materialise every top-level `val` binding into the env, in
    * declaration order, by reusing the expression visitor. Emitted
    * inline at the top of `@main` — top-level vals semantically
    * execute once at program start, and `@main` is the only function
    * the backend emits today, so this is the natural place. `var` and
    * `const` top-level bindings are not yet supported and surface as
    * `notYet` if the program references them.
    */
  private def emitTopBindings(tp: TProgram): Unit =
    tp.decls.foreach {
      case TTopBinding(sym, BindingKind.Val, value, _) =>
        env(sym.id) = emitExpr(value)
      case TTopBinding(sym, kind, _, _) =>
        notYet(s"top-level $kind binding for ${sym.name}")
      case _ => ()
    }

  /** Recognise the shapes of `def main()` that the backend supports:
    *
    *   - a bare `print(arg)` expression
    *   - a `TBlock` whose result is a `print(arg)` (val bindings + one
    *     final print)
    *   - a `TBlock` whose result is `TUnitLit` (statement-position
    *     `print(...)` calls interleaved with val bindings)
    *
    * Anything else surfaces as `notYet`.
    */
  private def emitMainBody(body: TExpr): Unit = unwrapEmptyBlock(body) match
    case TCall(TVarRef(p, _, _), List(arg), _, _) if p.name == "print" =>
      emitPrintCall(arg)
    case TBlock(items, TCall(TVarRef(p, _, _), List(arg), _, _), _, _) if p.name == "print" =>
      items.foreach(emitBlockItem)
      emitPrintCall(arg)
    case TBlock(items, TUnitLit(_), _, _) =>
      items.foreach(emitBlockItem)
    case TBlock(items, last, _, _) if last.tpe == TyUnit =>
      items.foreach(emitBlockItem)
      emitBlockItem(TBlockExpr(last))
    case other if other.tpe == TyUnit =>
      emitBlockItem(TBlockExpr(other))
    case other =>
      notYet(s"main body shape: ${other.getClass.getSimpleName}")

  private def unwrapEmptyBlock(e: TExpr): TExpr = e match
    case TBlock(Nil, r, _, _) => unwrapEmptyBlock(r)
    case other                => other

  /** Emit a call into the runtime print surface for a typed value:
    *   - scalar i64/f64 → `nex_print_{i64,f64}`
    *   - rank-1 tensor  → bufferize to memref, extract data pointer as
    *     i64, pass with length to `nex_print_array_1d_{i64,f64}`
    *   - rank-2 tensor  → same plus row + column constants to
    *     `nex_print_array_2d_{i64,f64}`
    *
    * The argument is evaluated through the visitor; its MLIR type
    * chooses the helper. Format matches `NexInterpreter.formatValue`.
    */
  private def emitPrintCall(arg: TExpr): Unit =
    val v = emitExpr(arg)
    v.ty match
      case MScalar(TyInteger) =>
        out.append(s"  func.call @nex_print_i64(${v.reg}) : (i64) -> ()\n")
      case MScalar(TyReal) =>
        out.append(s"  func.call @nex_print_f64(${v.reg}) : (f64) -> ()\n")
      case MScalar(TyBool) =>
        out.append(s"  func.call @nex_print_bool(${v.reg}) : (i1) -> ()\n")
      case t @ MTensor(elemT, List(n)) =>
        val ptrReg = emitTensorPointer(v.reg, t)
        val lenReg = fresh("len")
        out.append(s"  $lenReg = arith.constant $n : i64\n")
        val helper = arrayPrintHelper(elemT, rank = 1)
        out.append(s"  func.call @$helper($ptrReg, $lenReg) : (i64, i64) -> ()\n")
      case t @ MTensor(elemT, List(rows, cols)) =>
        val ptrReg  = emitTensorPointer(v.reg, t)
        val rowsReg = fresh("rows")
        val colsReg = fresh("cols")
        out.append(s"  $rowsReg = arith.constant $rows : i64\n")
        out.append(s"  $colsReg = arith.constant $cols : i64\n")
        val helper = arrayPrintHelper(elemT, rank = 2)
        out.append(s"  func.call @$helper($ptrReg, $rowsReg, $colsReg) : (i64, i64, i64) -> ()\n")
      case other =>
        notYet(s"print of $other")

  /** Bufferize a tensor to a memref and extract its aligned data
    * pointer as a raw `i64`. `bufferization.to_buffer` is the LLVM 22
    * spelling (formerly `bufferization.to_memref`); the rest of the
    * pipeline already lowers memref ops to LLVM dialect, so the
    * resulting `i64` is the actual data address at runtime.
    */
  private def emitTensorPointer(srcReg: String, ty: MTensor): String =
    val mrefT = memrefText(ty)
    val mReg  = fresh("mref")
    out.append(s"  $mReg = bufferization.to_buffer $srcReg : ${ty.text} to $mrefT\n")
    val idxReg = fresh("pidx")
    out.append(s"  $idxReg = memref.extract_aligned_pointer_as_index $mReg : $mrefT -> index\n")
    val ptrReg = fresh("pi")
    out.append(s"  $ptrReg = arith.index_castui $idxReg : index to i64\n")
    ptrReg

  /** Memref text for a tensor: same element + shape, just `memref<...>`
    * instead of `tensor<...>`.
    */
  private def memrefText(ty: MTensor): String =
    val dims = if ty.shape.isEmpty then "" else ty.shape.mkString("", "x", "x")
    s"memref<$dims${scalarText(ty.elem)}>"

  private def arrayPrintHelper(elemT: Type, rank: Int): String = (elemT, rank) match
    case (TyInteger, 1) => "nex_print_array_1d_i64"
    case (TyReal,    1) => "nex_print_array_1d_f64"
    case (TyBool,    1) => "nex_print_array_1d_bool"
    case (TyInteger, 2) => "nex_print_array_2d_i64"
    case (TyReal,    2) => "nex_print_array_2d_f64"
    case (TyBool,    2) => "nex_print_array_2d_bool"
    case _              => notYet(s"array print helper for $elemT rank-$rank")

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

    case TBoolLit(v, _, _) =>
      val r = fresh("cb")
      out.append(s"  $r = arith.constant ${if v then 1 else 0} : i1\n")
      MlirVal(r, MScalar(TyBool))

    case TUnaryOp("-", inner, _, _) =>
      foldLiteralNeg(inner) match
        case Some(lit) => emitExpr(lit)
        case None =>
          val v = emitExpr(inner)
          v.ty match
            case MScalar(TyInteger) =>
              val zero = fresh("z")
              val r    = fresh("neg")
              out.append(s"  $zero = arith.constant 0 : i64\n")
              out.append(s"  $r = arith.subi $zero, ${v.reg} : i64\n")
              MlirVal(r, MScalar(TyInteger))
            case MScalar(TyReal) =>
              val r = fresh("neg")
              out.append(s"  $r = arith.negf ${v.reg} : f64\n")
              MlirVal(r, MScalar(TyReal))
            case other =>
              notYet(s"unary minus on $other")

    case TUnaryOp("not", inner, _, _) =>
      val v = emitExpr(inner)
      v.ty match
        case MScalar(TyBool) =>
          val one = fresh("one")
          val r   = fresh("not")
          out.append(s"  $one = arith.constant 1 : i1\n")
          out.append(s"  $r = arith.xori ${v.reg}, $one : i1\n")
          MlirVal(r, MScalar(TyBool))
        case other =>
          notYet(s"`not` on $other")

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

    case TArrayLit(rows, _, TyArray(elemT, 2)) if rows.nonEmpty =>
      // Each top-level element is itself a rank-1 array literal — the
      // elaborator enforces row uniformity, so the first row's length
      // is the column count. We flatten row-major into one
      // `tensor.from_elements` over `tensor<RxCx<elem>>`.
      val flatRows = rows.map {
        case TArrayLit(rowElems, _, TyArray(t, 1)) if t == elemT => rowElems
        case other => notYet(s"rank-2 row shape: ${other.getClass.getSimpleName}")
      }
      val cols  = flatRows.head.size
      flatRows.foreach { r =>
        if r.size != cols then notYet(s"ragged rank-2 literal — got ${r.size}, expected $cols")
      }
      val flat = flatRows.flatten
      val vals = flat.map(emitExpr)
      val ty   = MTensor(elemT, List(rows.size, cols))
      val r    = fresh("arr2")
      out.append(s"  $r = tensor.from_elements ${vals.map(_.reg).mkString(", ")} : ${ty.text}\n")
      MlirVal(r, ty)

    case TVarRef(s, _, _) if s.kind == SymKind.Prelude && s.name == "nan" =>
      // Compiler-built-in NaN constant. MLIR's `arith.constant` accepts
      // the IEEE 754 bit pattern as a hex literal on an `f64` attribute.
      val r = fresh("nan")
      out.append(s"  $r = arith.constant 0x7FF8000000000000 : f64\n")
      MlirVal(r, MScalar(TyReal))

    case TVarRef(s, _, _) if s.kind == SymKind.Prelude && s.name == "inf" =>
      val r = fresh("inf")
      out.append(s"  $r = arith.constant 0x7FF0000000000000 : f64\n")
      MlirVal(r, MScalar(TyReal))

    case TVarRef(sym, _, _) if varSlots.contains(sym.id) =>
      emitVarLoad(sym.id)

    case TVarRef(sym, _, _) =>
      env.getOrElse(sym.id, notYet(s"unbound symbol ${sym.name}#${sym.id}"))

    case TBlock(items, result, _, _) =>
      items.foreach(emitBlockItem)
      emitExpr(result)

    case TElementWise(op, lhs, rhs, _, _) if isComparisonOp(op) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty) match
        case (lt: MTensor, rt: MTensor) if lt.shape == rt.shape =>
          emitElementWiseComparison(op, lv, rv, lt, rt)
        case (lt, rt) =>
          notYet(s"element-wise comparison $op on $lt and $rt")

    case TElementWise(op, lhs, rhs, _, _) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty) match
        case (lt: MTensor, rt: MTensor) if lt == rt =>
          emitElementWiseBinop(op, lv, rv, lt)
        case (lt: MTensor, rt: MTensor) if lt.shape == rt.shape =>
          emitElementWiseMixed(op, lv, rv, lt, rt)
        case (lt, rt) =>
          notYet(s"element-wise $op on $lt and $rt")

    case TBroadcast(scalar, arr, op, scalarFirst, _, _) if isComparisonOp(op) =>
      val sv = emitExpr(scalar)
      val av = emitExpr(arr)
      (sv.ty, av.ty) match
        case (MScalar(st), t @ MTensor(et, _)) =>
          emitBroadcastComparison(op, sv, av, t, scalarFirst, st, et)
        case (sty, aty) =>
          notYet(s"broadcast comparison $op on $sty and $aty")

    case TBroadcast(scalar, arr, op, scalarFirst, _, _) =>
      val sv = emitExpr(scalar)
      val av = emitExpr(arr)
      (sv.ty, av.ty) match
        case (MScalar(st), t @ MTensor(et, _)) if st == et =>
          emitBroadcast(op, sv, av, t, scalarFirst)
        case (MScalar(st), t @ MTensor(et, _)) =>
          emitBroadcastMixed(op, sv, av, t, scalarFirst, st, et)
        case (sty, aty) =>
          notYet(s"broadcast $op on $sty and $aty")

    case TBinOp("..", TIntLit(lo, _, _), TIntLit(hi, _, _), _, _) =>
      emitRangeCall(lo, hi)

    case TBinOp("..=", TIntLit(lo, _, _), TIntLit(hi, _, _), _, _) =>
      emitRangeCall(lo, hi + 1)

    case TBinOp("^", lhs, rhs, _, resultTy) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      emitScalarPower(lv, rv, resultTy)

    case TBinOp("and", lhs, rhs, _, _) => emitShortCircuit(lhs, rhs, isAnd = true)
    case TBinOp("or",  lhs, rhs, _, _) => emitShortCircuit(lhs, rhs, isAnd = false)

    case TBinOp(op, lhs, rhs, _, _) if isComparisonOp(op) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      emitComparison(op, lv, rv)

    case TBinOp(op, lhs, rhs, _, resultTy) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty, resultTy) match
        case (MScalar(TyInteger), MScalar(TyInteger), TyReal) =>
          // `int op int` whose elaborated result is real — e.g. `7 / 2`.
          // Lift both operands to f64 before the float op.
          emitScalarBinop(op, promoteIntToReal(lv), promoteIntToReal(rv), MScalar(TyReal))
        case (MScalar(lt), MScalar(rt), _) if lt == rt =>
          emitScalarBinop(op, lv, rv, MScalar(lt))
        case (MScalar(TyInteger), MScalar(TyReal), _) =>
          emitScalarBinop(op, promoteIntToReal(lv), rv, MScalar(TyReal))
        case (MScalar(TyReal), MScalar(TyInteger), _) =>
          emitScalarBinop(op, lv, promoteIntToReal(rv), MScalar(TyReal))
        case (lt, rt, _) =>
          notYet(s"scalar binop $op on $lt and $rt")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "sum" =>
      val av = emitExpr(arr)
      av.ty match
        case t: MTensor => emitSumReduce(av.reg, t)
        case other      => notYet(s"sum over $other")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "product" =>
      val av = emitExpr(arr)
      av.ty match
        case t: MTensor => emitProductReduce(av.reg, t)
        case other      => notYet(s"product over $other")

    case TCall(TVarRef(s, _, _), List(arr, lam: TLambda), _, tpe)
        if s.kind == SymKind.Prelude && s.name == "map" && lam.params.size == 1 =>
      val av = emitExpr(arr)
      val outElem = tpe match
        case TyArray(e, _) => e
        case other         => notYet(s"map returns non-array $other")
      av.ty match
        case t: MTensor if t.shape.nonEmpty => emitMapInlineLambda(av, t, lam, outElem)
        case other                          => notYet(s"map over $other")

    case TCall(TVarRef(s, _, _), List(arr, init, lam: TLambda), _, _)
        if s.kind == SymKind.Prelude && s.name == "reduce" && lam.params.size == 2 =>
      val av = emitExpr(arr)
      val iv = emitExpr(init)
      av.ty match
        case t: MTensor if t.shape.nonEmpty => emitReduceInlineLambda(av, t, iv, lam)
        case other                          => notYet(s"reduce over $other")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && (s.name == "min" || s.name == "max") =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) => emitMinMaxReduce(s.name, av.reg, t)
        case other                   => notYet(s"${s.name} over $other")

    case TCall(TVarRef(s, _, _), List(a, b), _, _)
        if s.kind == SymKind.Prelude && (s.name == "min" || s.name == "max") =>
      emitScalarMinMax(s.name, emitExpr(a), emitExpr(b))

    case TCall(TVarRef(s, _, _), List(x), _, _)
        if s.kind == SymKind.Prelude && s.name == "abs" =>
      emitScalarAbs(emitExpr(x))

    case TCall(TVarRef(s, _, _), List(TIntLit(lo, _, _), TIntLit(hi, _, _)), _, _)
        if s.kind == SymKind.Prelude && s.name == "range" =>
      emitRangeCall(lo, hi)

    case TCall(TVarRef(s, _, _), List(TIntLit(n, _, _)), _, _)
        if s.kind == SymKind.Prelude && (s.name == "zeros" || s.name == "ones") =>
      emitConstFill(s.name, n.toInt)

    case TCall(TVarRef(s, _, _), List(loLit, hiLit, TIntLit(n, _, _)), _, _)
        if s.kind == SymKind.Prelude && s.name == "linspace" =>
      val lo = realLitValue(loLit)
      val hi = realLitValue(hiLit)
      emitLinspaceCall(lo, hi, n.toInt)

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "transpose" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(rows, cols)) => emitTranspose(av.reg, t, rows, cols)
        case other                            => notYet(s"transpose on $other")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "flatten" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_))           => emitFlatten1D(av)
        case t @ MTensor(_, List(rows, cols))  => emitFlatten2D(av, t, rows, cols)
        case other                             => notYet(s"flatten on $other")

    case TCall(TVarRef(s, _, _), List(arr, TIntLit(rows, _, _), TIntLit(cols, _, _)), _, _)
        if s.kind == SymKind.Prelude && s.name == "reshape" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) => emitReshape(av, t, rows.toInt, cols.toInt)
        case other                   => notYet(s"reshape on $other")

    case TCall(TVarRef(s, _, _), List(arr, TIntLit(axis, _, _)), _, _)
        if s.kind == SymKind.Prelude && s.name == "sum_axis" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_, _)) => emitSumAxis(av, t, axis.toInt)
        case other                      => notYet(s"sum_axis on $other")

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "diag" =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(n)) => emitDiag(av, t, n)
        case other                   => notYet(s"diag on $other")

    case TCall(TVarRef(s, _, _), List(TIntLit(n, _, _)), _, _)
        if s.kind == SymKind.Prelude && s.name == "identity" =>
      emitIdentity(n.toInt)

    case TCall(TVarRef(s, _, _), args, _, _) if libmIntrinsics.contains(s.id) =>
      emitLibmCall(libmIntrinsics(s.id), args.map(emitExpr))

    case TMatMul(lhs, rhs, _, _) =>
      emitMatMul(emitExpr(lhs), emitExpr(rhs))

    case TCall(TVarRef(s, _, _), List(lhs, rhs), _, _)
        if s.kind == SymKind.Prelude && s.name == "matmul" =>
      emitMatMul(emitExpr(lhs), emitExpr(rhs))

    case TCall(TVarRef(s, _, _), List(arr), _, _)
        if s.kind == SymKind.Prelude && s.name == "length" =>
      // `arr.length()` desugars to `length(arr)` in elaboration.
      // Result is the size of the outermost dimension — for rank-1
      // that's the element count, for rank-2 the row count. The
      // shape is statically known at codegen, so this lowers to a
      // single integer constant. The receiver is still evaluated
      // for side-effect parity; clang's DCE removes the unused
      // tensor at -O1.
      val av = emitExpr(arr)
      av.ty match
        case MTensor(_, shape) if shape.nonEmpty =>
          val r = fresh("len")
          out.append(s"  $r = arith.constant ${shape.head} : i64\n")
          MlirVal(r, MScalar(TyInteger))
        case other =>
          notYet(s"length on $other")

    case TIndex(arr, List(idx), _, _) =>
      val av = emitExpr(arr)
      val iv = emitExpr(idx)
      (av.ty, iv.ty) match
        case (t @ MTensor(et, List(_)), MScalar(TyInteger)) =>
          val idxR = fresh("idx")
          out.append(s"  $idxR = arith.index_cast ${iv.reg} : i64 to index\n")
          val r = fresh("elt")
          out.append(s"  $r = tensor.extract ${av.reg}[$idxR] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (t @ MTensor(et, List(_, cols)), MScalar(TyInteger)) =>
          // `m[i]` on a rank-2 receiver: return the i-th row as a fresh
          // rank-1 tensor. The row axis collapses; the column axis is
          // copied wholesale. `tensor.extract_slice`'s rank-reducing form
          // handles the rank drop when a static size-1 axis is present.
          val idxR = fresh("ridx")
          out.append(s"  $idxR = arith.index_cast ${iv.reg} : i64 to index\n")
          val outTy = MTensor(et, List(cols))
          val r     = fresh("row")
          out.append(
            s"  $r = tensor.extract_slice ${av.reg}[$idxR, 0] [1, $cols] [1, 1] : ${t.text} to ${outTy.text}\n",
          )
          MlirVal(r, outTy)
        case (aty, ity) =>
          notYet(s"single-index on $aty with $ity")

    case TIndex(arr, List(rowIdx, colIdx), _, _) =>
      val av = emitExpr(arr)
      val rv = emitExpr(rowIdx)
      val cv = emitExpr(colIdx)
      (av.ty, rv.ty, cv.ty) match
        case (t @ MTensor(et, List(_, _)), MScalar(TyInteger), MScalar(TyInteger)) =>
          val rR = fresh("ridx")
          out.append(s"  $rR = arith.index_cast ${rv.reg} : i64 to index\n")
          val cR = fresh("cidx")
          out.append(s"  $cR = arith.index_cast ${cv.reg} : i64 to index\n")
          val r = fresh("elt")
          out.append(s"  $r = tensor.extract ${av.reg}[$rR, $cR] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (aty, rty, cty) =>
          notYet(s"rank-2 index on $aty with $rty, $cty")

    case TIf(cond, thenB, Some(elseB), _, tpe) if isMlirScalarType(tpe) =>
      emitIfExpr(cond, thenB, elseB, MScalar(tpe))

    case TFusedLoop(loopVar, length, body, None, _, tpe) =>
      val n = staticLength(length).getOrElse(notYet(s"fused loop with non-static length"))
      val elemT = tpe match
        case TyArray(e, _) => e
        case other         => notYet(s"fused loop returns non-array type $other")
      emitFusedLoop1D(loopVar, n, elemT, body)

    case TFlatIndex(arr, idx, _, _) =>
      val av = emitExpr(arr)
      val iv = emitExpr(idx)
      (av.ty, iv.ty) match
        case (t @ MTensor(et, List(_)), MScalar(TyInteger)) =>
          val idxR = fresh("fidx")
          out.append(s"  $idxR = arith.index_cast ${iv.reg} : i64 to index\n")
          val r = fresh("felt")
          out.append(s"  $r = tensor.extract ${av.reg}[$idxR] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (t @ MTensor(et, List(_, cols)), MScalar(TyInteger)) =>
          // Rank-2 source with a flat index: convert flat → (i, j) via
          // `i = flat / cols`, `j = flat % cols`, then tensor.extract.
          val flatI = fresh("fidx")
          out.append(s"  $flatI = arith.index_cast ${iv.reg} : i64 to index\n")
          val colsI = fresh("fcols")
          out.append(s"  $colsI = arith.constant $cols : index\n")
          val iI    = fresh("fi")
          out.append(s"  $iI = arith.divui $flatI, $colsI : index\n")
          val jI    = fresh("fj")
          out.append(s"  $jI = arith.remui $flatI, $colsI : index\n")
          val r = fresh("felt")
          out.append(s"  $r = tensor.extract ${av.reg}[$iI, $jI] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (aty, ity) =>
          notYet(s"flat-index on $aty with $ity")

    case TSlice(arr, TIntLit(lo, _, _), TIntLit(hi, _, _), inclusive, _, _) =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) =>
          val end      = if inclusive then hi.toInt + 1 else hi.toInt
          val sliceLen = math.max(0, end - lo.toInt)
          emitRank1Slice(av, t, lo.toInt, sliceLen)
        case other =>
          notYet(s"rank-1 slice on $other")

    case TSlice2(arr, rowAx, colAx, _, _) =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(rows, cols)) =>
          val rowSpec = axisToSlice(rowAx, rows)
          val colSpec = axisToSlice(colAx, cols)
          emitRank2Slice(av, t, rowSpec, colSpec)
        case other =>
          notYet(s"rank-2 slice on $other")

    case TIntrinsic(opId, _, _) =>
      notYet(s"intrinsic `$opId` (MLIR backend has no Stage-0 intrinsic dispatch yet)")

    case other =>
      notYet(s"expression: ${other.getClass.getSimpleName}")

  private def emitBlockItem(it: TBlockItem): Unit = it match
    case TBlockBinding(sym, BindingKind.Val, value) =>
      env(sym.id) = emitExpr(value)
    case TBlockBinding(sym, BindingKind.Var, value) =>
      val v = emitExpr(value)
      v.ty match
        case s: MScalar => allocVarSlot(sym, v.reg, s)
        case other      => notYet(s"var binding for ${sym.name} of type $other (only scalars supported)")
    case TBlockBinding(sym, kind, _) =>
      notYet(s"$kind binding for ${sym.name}")
    case TBlockExpr(TCall(TVarRef(p, _, _), List(arg), _, _)) if p.name == "print" =>
      emitPrintCall(arg)
    case TBlockExpr(TFor(loopVars, TBinOp("..", lo, hi, _, _), body, _, _)) =>
      emitForRange(loopVars, lo, hi, inclusive = false, body)
    case TBlockExpr(TFor(loopVars, TBinOp("..=", lo, hi, _, _), body, _, _)) =>
      emitForRange(loopVars, lo, hi, inclusive = true, body)
    case TBlockExpr(TFor(loopVars, iter, body, _, _)) =>
      val av = emitExpr(iter)
      av.ty match
        case t @ MTensor(_, List(_)) => emitForArray(loopVars, av, t, body)
        case other                   => notYet(s"for over $other")
    case TBlockExpr(TWhile(cond, body, _, _)) =>
      emitWhile(cond, body)
    case TBlockExpr(TAssign(TVarRef(sym, _, _), value, _, _)) if varSlots.contains(sym.id) =>
      emitVarStore(sym.id, emitExpr(value))
    case TBlockExpr(other) =>
      notYet(s"statement-position expression: ${other.getClass.getSimpleName}")

  /** Statement-form `for i in lo..hi do body` over an integer range.
    * Lowers to `scf.for %iv = %lo to %hi step %c1 { … }`. The loop
    * var is provided as MLIR `index` type; we cast to i64 and bind
    * to the symbol id so the body's references resolve as scalars.
    * Inclusive ranges (`..=`) bump the upper bound by one via
    * `arith.addi` (scf.for is exclusive on its upper bound).
    *
    * Bounds are arbitrary `TyInteger` expressions — literals, folded
    * unary minus, var refs, anything that emits to i64. We index-cast
    * each side to `index` after emission.
    *
    * Body emission delegates to [[emitForBody]], which handles a
    * `TBlock` body (multiple statements) the same way it handles
    * `def main()`'s top-level block — items go through
    * `emitBlockItem`, so nested prints / nested for-loops compose
    * naturally.
    */
  private def emitForRange(loopVars: List[Symbol], lo: TExpr, hi: TExpr, inclusive: Boolean, body: TExpr): Unit =
    if loopVars.size != 1 then
      notYet(s"for over range with ${loopVars.size}-way destructuring")
      return
    val loopVar = loopVars.head
    val loV     = emitExpr(lo)
    val hiV     = emitExpr(hi)
    val loC     = fresh("flo")
    out.append(s"  $loC = arith.index_cast ${loV.reg} : i64 to index\n")
    val hiIdx   = fresh("fhi_idx")
    out.append(s"  $hiIdx = arith.index_cast ${hiV.reg} : i64 to index\n")
    val hiC     =
      if inclusive then
        val one  = fresh("fone")
        val bump = fresh("fhi")
        out.append(s"  $one = arith.constant 1 : index\n")
        out.append(s"  $bump = arith.addi $hiIdx, $one : index\n")
        bump
      else hiIdx
    val stepC   = fresh("fst")
    out.append(s"  $stepC = arith.constant 1 : index\n")
    val ivName  = fresh("iv")
    out.append(s"  scf.for $ivName = $loC to $hiC step $stepC {\n")
    val ivI64   = fresh("ivi")
    out.append(s"    $ivI64 = arith.index_cast $ivName : index to i64\n")
    val prev    = env.get(loopVar.id)
    env(loopVar.id) = MlirVal(ivI64, MScalar(TyInteger))
    emitForBody(body)
    prev match
      case Some(v) => env(loopVar.id) = v
      case None    => env.remove(loopVar.id)
    out.append("  }\n")

  /** Resolved spec for one axis of a rank-2 slice. `offset` and `size`
    * are the corresponding entries in the `tensor.extract_slice`
    * offsets/sizes lists; `collapsed` is true when this axis was a
    * single integer index (in which case the result tensor drops one
    * rank — MLIR's `extract_slice` handles this via its
    * rank-reducing form when the static size is 1).
    */
  private case class AxisSlice(offset: Int, size: Int, collapsed: Boolean)

  /** Resolve a `TAxisSpec` to its concrete (offset, size, collapsed)
    * triple given the corresponding source dimension extent. Only
    * literal-bound axes are accepted — anything else surfaces as
    * `notYet`.
    */
  private def axisToSlice(spec: TAxisSpec, dim: Int): AxisSlice = spec match
    case TAxisAll =>
      AxisSlice(offset = 0, size = dim, collapsed = false)
    case TAxisIndex(TIntLit(v, _, _)) =>
      AxisSlice(offset = v.toInt, size = 1, collapsed = true)
    case TAxisRange(TIntLit(lo, _, _), TIntLit(hi, _, _), inclusive) =>
      val end = if inclusive then hi.toInt + 1 else hi.toInt
      AxisSlice(offset = lo.toInt, size = math.max(0, end - lo.toInt), collapsed = false)
    case other =>
      notYet(s"rank-2 slice axis spec: ${other.getClass.getSimpleName} with non-literal bound")

  /** Rank-2 `tensor.extract_slice` with literal offset/size on both
    * axes. Output rank is `2 - (number of collapsed axes)`. When
    * both axes are collapsed the result is a scalar — but that
    * shape isn't reachable here because the elaborator emits a
    * `TIndex` (not a `TSlice2`) for the two-integer-index form.
    */
  private def emitRank2Slice(av: MlirVal, srcTy: MTensor, rowSpec: AxisSlice, colSpec: AxisSlice): MlirVal =
    val outShape = List(rowSpec, colSpec).collect {
      case s if !s.collapsed => s.size
    }
    val outTy = MTensor(srcTy.elem, outShape)
    if outShape.contains(0) then
      val r = fresh("emp")
      out.append(s"  $r = tensor.empty() : ${outTy.text}\n")
      return MlirVal(r, outTy)
    val r = fresh("sl2")
    out.append(
      s"  $r = tensor.extract_slice ${av.reg}[${rowSpec.offset}, ${colSpec.offset}] [${rowSpec.size}, ${colSpec.size}] [1, 1] : ${srcTy.text} to ${outTy.text}\n",
    )
    MlirVal(r, outTy)

  /** Rank-1 slice `a[lo..hi]` / `a[lo..=hi]` with literal bounds.
    * Lowers to `tensor.extract_slice` with a static offset / size /
    * unit stride, which produces a freshly-allocated tensor of the
    * sliced length. Empty slices (computed length <= 0) collapse to
    * `tensor.empty() : tensor<0xT>` — printing walks zero elements
    * and emits `[]\n`.
    */
  private def emitRank1Slice(av: MlirVal, srcTy: MTensor, offset: Int, len: Int): MlirVal =
    val outTy = MTensor(srcTy.elem, List(len))
    if len == 0 then
      val r = fresh("emp")
      out.append(s"  $r = tensor.empty() : ${outTy.text}\n")
      return MlirVal(r, outTy)
    val r = fresh("sl")
    out.append(s"  $r = tensor.extract_slice ${av.reg}[$offset] [$len] [1] : ${srcTy.text} to ${outTy.text}\n")
    MlirVal(r, outTy)

  /** Allocate a stack slot for a `var <sym>` scalar binding and store
    * the initial value. The slot lives in `varSlots` keyed by symbol
    * id so later reads/writes can find it. Uses `memref.alloca` for
    * stack-local lifetime — the kernel-stack region is large enough
    * for any plausible number of var counters, and the slot is
    * automatically reclaimed on function exit.
    */
  private def allocVarSlot(sym: Symbol, initReg: String, sty: MScalar): Unit =
    val mrefT = s"memref<${sty.text}>"
    val slot  = fresh(s"var_${sym.name}")
    out.append(s"  $slot = memref.alloca() : $mrefT\n")
    out.append(s"  memref.store $initReg, $slot[] : $mrefT\n")
    varSlots(sym.id) = (slot, sty)

  /** Read a `var` slot. Mirrors the existing `env`-lookup MlirVal
    * shape so the rest of the visitor doesn't have to know that
    * vars are different from vals.
    */
  private def emitVarLoad(symId: Int): MlirVal =
    val (slot, sty) = varSlots(symId)
    val r           = fresh("vr")
    out.append(s"  $r = memref.load $slot[] : memref<${sty.text}>\n")
    MlirVal(r, sty)

  /** Store a new value into a `var` slot. The value's type must match
    * the slot's element type — Nex's type system already guarantees
    * this at TAssign sites, so no promotion is needed here.
    */
  private def emitVarStore(symId: Int, v: MlirVal): Unit =
    val (slot, sty) = varSlots(symId)
    out.append(s"  memref.store ${v.reg}, $slot[] : memref<${sty.text}>\n")

  /** Statement-form `while cond do body` via `scf.while` with no
    * iter_args. The cond region computes the predicate and yields
    * it through `scf.condition`; the body region runs (reading and
    * writing var slots as needed) and ends with a bare `scf.yield`.
    * Mutable counters live in `var` memref slots — the load/store
    * pattern makes the SSA story trivial since the data isn't
    * threaded through region results.
    */
  private def emitWhile(cond: TExpr, body: TExpr): Unit =
    out.append("  scf.while : () -> () {\n")
    val cv = emitExpr(cond)
    out.append(s"    scf.condition(${cv.reg})\n")
    out.append("  } do {\n")
    emitForBody(body)
    out.append("    scf.yield\n")
    out.append("  }\n")

  /** Statement-form `for x in arr do body` over a rank-1 array. The
    * array's length is static (recorded in the `MTensor` shape), so
    * we walk `0..length` via `scf.for` and `tensor.extract` each
    * element into the loop var slot. The same body-emission path as
    * range-based `for` is reused — `emitForBody` handles `TBlock`s
    * and bare statements identically.
    */
  private def emitForArray(loopVars: List[Symbol], av: MlirVal, ty: MTensor, body: TExpr): Unit =
    if loopVars.size != 1 then
      notYet(s"for over array with ${loopVars.size}-way destructuring")
      return
    val loopVar = loopVars.head
    val len     = ty.shape.head
    val loC     = fresh("flo")
    out.append(s"  $loC = arith.constant 0 : index\n")
    val hiC     = fresh("fhi")
    out.append(s"  $hiC = arith.constant $len : index\n")
    val stepC   = fresh("fst")
    out.append(s"  $stepC = arith.constant 1 : index\n")
    val ivName  = fresh("iv")
    out.append(s"  scf.for $ivName = $loC to $hiC step $stepC {\n")
    val eltR    = fresh("elt")
    out.append(s"    $eltR = tensor.extract ${av.reg}[$ivName] : ${ty.text}\n")
    val prev    = env.get(loopVar.id)
    env(loopVar.id) = MlirVal(eltR, MScalar(ty.elem))
    emitForBody(body)
    prev match
      case Some(v) => env(loopVar.id) = v
      case None    => env.remove(loopVar.id)
    out.append("  }\n")

  /** Body of a loop. Accepts a bare `TBlock` whose result is `Unit`
    * (the common shape `for i do … do print(i)` produces, since the
    * `do` clause introduces a block), a `TBlock` that ends with a
    * value-producing expression (the result is discarded), or a
    * single non-block statement.
    */
  private def emitForBody(body: TExpr): Unit = body match
    case TBlock(items, TUnitLit(_), _, _) =>
      items.foreach(emitBlockItem)
    case TBlock(items, last, _, _) =>
      items.foreach(emitBlockItem)
      emitBlockItem(TBlockExpr(last))
    case other =>
      emitBlockItem(TBlockExpr(other))

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

  /** Scalar `+ - *` on matching int/int or real/real operands. The
    * dispatcher in [[scalarBinop]] is the gate for which operators
    * land; everything else (`/`, `div`, `%`, `^`) bubbles up as
    * `notYet` from there before any IR is emitted.
    */
  private def emitScalarBinop(op: String, lv: MlirVal, rv: MlirVal, ty: MScalar): MlirVal =
    val opName = scalarBinop(op, ty.elem)
    val r      = fresh("sb")
    out.append(s"  $r = $opName ${lv.reg}, ${rv.reg} : ${ty.text}\n")
    MlirVal(r, ty)

  /** Sign-extend-to-float promotion of an `i64` SSA value into `f64`,
    * matching the interpreter's `asReal` promotion at mixed-type
    * binop call sites. Used to handle `int + real`-style expressions
    * where the elaborator leaves operand types mismatched.
    */
  private def promoteIntToReal(v: MlirVal): MlirVal =
    val r = fresh("pr")
    out.append(s"  $r = arith.sitofp ${v.reg} : i64 to f64\n")
    MlirVal(r, MScalar(TyReal))

  private def isComparisonOp(op: String): Boolean =
    op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">="

  /** Scalar comparison `< <= > >= == !=` on matching int/int or
    * real/real operands. Integer ops use `arith.cmpi` with signed
    * predicates. Real ops use `arith.cmpf` with **ordered** predicates
    * (`oeq` / `olt` / etc.) — these return false whenever NaN is on
    * either side, matching the spec §10.2 / NexInterpreter semantics
    * where every relational op against NaN is false. `!=` is the
    * exception: it uses the **unordered** predicate `une`, so
    * `nan != nan` correctly returns true.
    */
  private def emitComparison(op: String, lv: MlirVal, rv: MlirVal): MlirVal =
    val (lp, rp) = (lv.ty, rv.ty) match
      case (MScalar(TyInteger), MScalar(TyReal))    => (promoteIntToReal(lv), rv)
      case (MScalar(TyReal),    MScalar(TyInteger)) => (lv, promoteIntToReal(rv))
      case _                                        => (lv, rv)
    (lp.ty, rp.ty) match
      case (MScalar(TyInteger), MScalar(TyInteger)) =>
        val pred = op match
          case "==" => "eq"
          case "!=" => "ne"
          case "<"  => "slt"
          case "<=" => "sle"
          case ">"  => "sgt"
          case ">=" => "sge"
        val r = fresh("cmp")
        out.append(s"  $r = arith.cmpi $pred, ${lp.reg}, ${rp.reg} : i64\n")
        MlirVal(r, MScalar(TyBool))
      case (MScalar(TyReal), MScalar(TyReal)) =>
        val pred = op match
          case "==" => "oeq"
          case "!=" => "une"
          case "<"  => "olt"
          case "<=" => "ole"
          case ">"  => "ogt"
          case ">=" => "oge"
        val r = fresh("cmp")
        out.append(s"  $r = arith.cmpf $pred, ${lp.reg}, ${rp.reg} : f64\n")
        MlirVal(r, MScalar(TyBool))
      case (MScalar(TyBool), MScalar(TyBool)) =>
        val pred = op match
          case "==" => "eq"
          case "!=" => "ne"
          case _    => notYet(s"ordering $op on bool")
        val r = fresh("cmp")
        out.append(s"  $r = arith.cmpi $pred, ${lp.reg}, ${rp.reg} : i1\n")
        MlirVal(r, MScalar(TyBool))
      case (lt, rt) =>
        notYet(s"comparison $op on $lt and $rt")

  /** Short-circuit `and` / `or` via `scf.if`. The lhs is evaluated
    * eagerly; the rhs is emitted *inside* one branch of the if so it
    * is only executed when the lhs doesn't already pin the result.
    * `--convert-scf-to-cf` (already in the pass pipeline) lowers the
    * `scf.if` to plain branches afterwards.
    *
    * Layout:
    *   `a and b` → `scf.if a then yield b else yield false`
    *   `a or  b` → `scf.if a then yield true else yield b`
    *
    * MLIR text is bracket-delimited, so the rhs's ops textually live
    * inside the region region even though `out` is a single
    * StringBuilder shared with the parent block. MLIR's dominance
    * rules let region bodies reference parent-scope SSA values, so
    * any vals captured by the rhs continue to work.
    */
  /** `map(arr, lambda)` with an inline single-param lambda. The body
    * inlines into a `linalg.map` region: the input operand becomes
    * the lambda's parameter (registered in env), the body is emitted
    * via `emitExpr`, and the result is yielded. Output tensor type
    * derives from the elaborator's result element type — supports
    * `[int] map → [real]` since the body can promote internally.
    * Works for any rank: `linalg.map` walks the shape regardless,
    * and `srcTy.text`/`outTy.text` encode the full shape.
    */
  private def emitMapInlineLambda(av: MlirVal, srcTy: MTensor, lam: TLambda, outElem: Type): MlirVal =
    val inElem = srcTy.elem
    val outTy  = MTensor(outElem, srcTy.shape)
    val inS    = scalarText(inElem)
    val outS   = scalarText(outElem)
    val initR  = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR   = fresh("hofmap")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${srcTy.text}) outs($initR : ${outTy.text})\n",
    )
    val paramName = fresh("p")
    out.append(s"    ($paramName: $inS, %_o: $outS) {\n")
    val paramSym  = lam.params.head
    val prev      = env.get(paramSym.id)
    env(paramSym.id) = MlirVal(paramName, MScalar(inElem))
    val bv = emitExpr(lam.body)
    out.append(s"      linalg.yield ${bv.reg} : $outS\n")
    out.append("    }\n")
    prev match
      case Some(v) => env(paramSym.id) = v
      case None    => env.remove(paramSym.id)
    MlirVal(outR, outTy)

  /** `reduce(arr, init, lambda)` with an inline two-param lambda.
    * Nex spec §10.4: the lambda is `(acc, x) -> body`. Lowers to
    * `linalg.reduce` over every axis of the input, with `init` seeded
    * into a 0-d output via `tensor.from_elements`. The lambda's `acc`
    * binds to the reduce body's accumulator and `x` to the input
    * element; the body emits and yields. All-dims reduce means the
    * lambda fires once per element regardless of rank — same flat
    * left-to-right traversal the interpreter and LLVM backend use.
    */
  private def emitReduceInlineLambda(av: MlirVal, srcTy: MTensor, iv: MlirVal, lam: TLambda): MlirVal =
    val accSym  = lam.params(0)
    val elemSym = lam.params(1)
    val accTy   = iv.ty match
      case s: MScalar => s
      case other      => notYet(s"reduce with non-scalar init $other")
    val outTy   = MTensor(accTy.elem, Nil)
    val initR   = fresh("init")
    out.append(s"  $initR = tensor.from_elements ${iv.reg} : ${outTy.text}\n")
    val outTR   = fresh("hofred_t")
    val inS     = scalarText(srcTy.elem)
    val accS    = scalarText(accTy.elem)
    val dims    = srcTy.shape.indices.mkString(", ")
    out.append(
      s"  $outTR = linalg.reduce ins(${av.reg} : ${srcTy.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    val inName  = fresh("p")
    val accName = fresh("a")
    out.append(s"    ($inName: $inS, $accName: $accS) {\n")
    val prevAcc  = env.get(accSym.id)
    val prevElem = env.get(elemSym.id)
    env(accSym.id)  = MlirVal(accName, accTy)
    env(elemSym.id) = MlirVal(inName, MScalar(srcTy.elem))
    val bv = emitExpr(lam.body)
    out.append(s"      linalg.yield ${bv.reg} : $accS\n")
    out.append("    }\n")
    prevAcc match { case Some(v) => env(accSym.id) = v; case None => env.remove(accSym.id) }
    prevElem match { case Some(v) => env(elemSym.id) = v; case None => env.remove(elemSym.id) }
    val outR = fresh("hofred")
    out.append(s"  $outR = tensor.extract $outTR[] : ${outTy.text}\n")
    MlirVal(outR, accTy)

  /** Resolve an Nex length expression to a compile-time integer when
    * possible. Recognises:
    *   - `TIntLit(n)` directly.
    *   - `length(arr)` where `arr` evaluates (via the env or a fresh
    *     emit, free of side effects) to a tensor with known static
    *     shape — the outer dim is the length.
    *   - `a * b` where both sides are static integers (used by the
    *     fusion pass for `rows * cols` on rank-2 sources).
    * Returns `None` otherwise so callers can fall back to `notYet`.
    */
  private def staticLength(e: TExpr): Option[Int] = e match
    case TIntLit(n, _, _) => Some(n.toInt)
    case TCall(TVarRef(s, _, _), List(arr), _, _) if s.kind == SymKind.Prelude && s.name == "length" =>
      staticTensorOf(arr).map(_.shape.head)
    case TBinOp("*", l, r, _, _) =>
      for li <- staticLength(l); ri <- staticLength(r) yield li * ri
    case _ => None

  /** Find the static `MTensor` type of an expression that's already
    * sitting in `env` (a `TVarRef` to a let-bound or var-bound array).
    * Used by [[staticLength]] to read the source shape without emitting
    * any side-effecting ops.
    */
  private def staticTensorOf(e: TExpr): Option[MTensor] = e match
    case TVarRef(s, _, _) =>
      env.get(s.id).map(_.ty).collect { case t: MTensor => t }
    case _ => None

  /** Rank-1 fused loop. The fusion pass already inlined the lambda body
    * with the param substituted to a `TFlatIndex(src, loopVar)` shape;
    * here we emit a `linalg.map` over a fresh `tensor<NxT_out>`, bind
    * the loop var symbol to the iteration index (cast to i64), and
    * `emitExpr(body)` inside the region. The body's `TFlatIndex` will
    * tensor.extract from the source tensor as needed.
    */
  private def emitFusedLoop1D(loopVar: Symbol, n: Int, elemT: Type, body: TExpr): MlirVal =
    val outTy = MTensor(elemT, List(n))
    val s     = scalarText(elemT)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("fl")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val ivR   = fresh("ivi")
    out.append(s"      $ivR = arith.index_castui $idxR : index to i64\n")
    val prev  = env.get(loopVar.id)
    env(loopVar.id) = MlirVal(ivR, MScalar(TyInteger))
    val bv    = emitExpr(body)
    out.append(s"      linalg.yield ${bv.reg} : $s\n")
    out.append("    }\n")
    prev match
      case Some(v) => env(loopVar.id) = v
      case None    => env.remove(loopVar.id)
    MlirVal(outR, outTy)

  /** Scalar types the backend can carry through an `scf.if -> (T)`
    * result slot. Tensor-returning `if` would need shape inference
    * (both branches must materialise the same static tensor type)
    * and isn't yet supported.
    */
  private def isMlirScalarType(t: Type): Boolean = t match
    case TyInteger | TyReal | TyBool => true
    case _                           => false

  /** Lower an `if cond then thenB else elseB` expression with a
    * scalar result. Models on [[emitShortCircuit]] — the cond is
    * evaluated eagerly, then both branches live inside an
    * `scf.if -> (T)` whose regions yield through `scf.yield`. The
    * pass pipeline already runs `--convert-scf-to-cf`, so this
    * lowers to plain branches before LLVM IR is emitted. Both
    * branches recurse through [[emitExpr]] so nested blocks /
    * arithmetic / calls / nested ifs are all handled by the same
    * machinery — MLIR is whitespace-insensitive, so the textual
    * indentation of region bodies doesn't have to match.
    */
  private def emitIfExpr(cond: TExpr, thenB: TExpr, elseB: TExpr, outTy: MlirType): MlirVal =
    val cv = emitExpr(cond)
    val r  = fresh("if")
    out.append(s"  $r = scf.if ${cv.reg} -> (${outTy.text}) {\n")
    val tv = emitExpr(thenB)
    out.append(s"    scf.yield ${tv.reg} : ${outTy.text}\n")
    out.append("  } else {\n")
    val ev = emitExpr(elseB)
    out.append(s"    scf.yield ${ev.reg} : ${outTy.text}\n")
    out.append("  }\n")
    MlirVal(r, outTy)

  private def emitShortCircuit(lhs: TExpr, rhs: TExpr, isAnd: Boolean): MlirVal =
    val lv = emitExpr(lhs)
    val r  = fresh(if isAnd then "and" else "or")
    out.append(s"  $r = scf.if ${lv.reg} -> (i1) {\n")
    if isAnd then
      val rv = emitExpr(rhs)
      out.append(s"    scf.yield ${rv.reg} : i1\n")
    else
      val tConst = fresh("scTrue")
      out.append(s"    $tConst = arith.constant 1 : i1\n")
      out.append(s"    scf.yield $tConst : i1\n")
    out.append("  } else {\n")
    if isAnd then
      val fConst = fresh("scFalse")
      out.append(s"    $fConst = arith.constant 0 : i1\n")
      out.append(s"    scf.yield $fConst : i1\n")
    else
      val rv = emitExpr(rhs)
      out.append(s"    scf.yield ${rv.reg} : i1\n")
    out.append("  }\n")
    MlirVal(r, MScalar(TyBool))

  /** Call a libm bridge declared in the prologue. All arguments are
    * promoted to f64 (sign-extending integer operands as needed) and
    * the result is f64. Used by every `@intrinsic("libm.X")` prelude
    * function — `sqrt(9.0)`, `hypot(3, 4)`, etc. Returns NaN for
    * domain errors (libm semantics), which matches NexInterpreter's
    * spec §10.2 behavior — e.g. `sqrt(-4.0)` prints `nan`.
    */
  private def emitLibmCall(name: String, args: List[MlirVal]): MlirVal =
    val argsF = args.map { v =>
      v.ty match
        case MScalar(TyReal)    => v
        case MScalar(TyInteger) => promoteIntToReal(v)
        case other              => notYet(s"libm `$name` arg of type $other")
    }
    val argTypes = argsF.map(_ => "f64").mkString(", ")
    val argRegs  = argsF.map(_.reg).mkString(", ")
    val r        = fresh(name)
    out.append(s"  $r = func.call @$name($argRegs) : ($argTypes) -> f64\n")
    MlirVal(r, MScalar(TyReal))

  /** Scalar `^` (power). `int ^ int` dispatches to the C runtime's
    * `nex_ipow` (exponentiation by squaring), matching the LLVM
    * backend's `@__nex_ipow`. Anything else routes through libm
    * `pow(f64, f64)` after lifting integer operands to f64. The
    * elaborated `resultTy` is the source of truth for which path
    * to take — `int ^ int` only stays integer-typed when the
    * exponent is provably non-negative.
    */
  private def emitScalarPower(lv: MlirVal, rv: MlirVal, resultTy: Type): MlirVal =
    (lv.ty, rv.ty, resultTy) match
      case (MScalar(TyInteger), MScalar(TyInteger), TyInteger) =>
        val r = fresh("ipow")
        out.append(s"  $r = func.call @nex_ipow(${lv.reg}, ${rv.reg}) : (i64, i64) -> i64\n")
        MlirVal(r, MScalar(TyInteger))
      case _ =>
        val lf = lv.ty match
          case MScalar(TyInteger) => promoteIntToReal(lv)
          case MScalar(TyReal)    => lv
          case other              => notYet(s"power lhs $other")
        val rf = rv.ty match
          case MScalar(TyInteger) => promoteIntToReal(rv)
          case MScalar(TyReal)    => rv
          case other              => notYet(s"power rhs $other")
        val r = fresh("rpow")
        out.append(s"  $r = func.call @pow(${lf.reg}, ${rf.reg}) : (f64, f64) -> f64\n")
        MlirVal(r, MScalar(TyReal))

  /** Generalised `@` operator covering the three rank combinations
    * `NexInterpreter` recognises:
    *
    *   - rank-2 × rank-2: `linalg.matmul` → rank-2 result
    *   - rank-2 × rank-1: `linalg.matvec` → rank-1 result
    *   - rank-1 × rank-1: `linalg.dot`    → 0-d result, then extract
    *
    * Element types must match and the contracted dimension must agree.
    * Every variant uses the same zero-fill init pattern since the
    * linalg op accumulates into its output.
    */
  private def emitMatMul(lv: MlirVal, rv: MlirVal): MlirVal =
    (lv.ty, rv.ty) match
      case (MTensor(elemL, List(n, k)), MTensor(elemR, List(k2, m))) if elemL == elemR && k == k2 =>
        val outTy = MTensor(elemL, List(n, m))
        val initR = emitZeroInit(outTy)
        val mmR   = fresh("mm")
        out.append(
          s"  $mmR = linalg.matmul ins(${lv.reg}, ${rv.reg} : ${lv.ty.text}, ${rv.ty.text}) outs($initR : ${outTy.text}) -> ${outTy.text}\n",
        )
        MlirVal(mmR, outTy)
      case (MTensor(elemL, List(n, k)), MTensor(elemR, List(k2))) if elemL == elemR && k == k2 =>
        val outTy = MTensor(elemL, List(n))
        val initR = emitZeroInit(outTy)
        val mvR   = fresh("mv")
        out.append(
          s"  $mvR = linalg.matvec ins(${lv.reg}, ${rv.reg} : ${lv.ty.text}, ${rv.ty.text}) outs($initR : ${outTy.text}) -> ${outTy.text}\n",
        )
        MlirVal(mvR, outTy)
      case (MTensor(elemL, List(k)), MTensor(elemR, List(k2))) if elemL == elemR && k == k2 =>
        val outTy = MTensor(elemL, Nil)
        val initR = emitZeroInit(outTy)
        val dotR  = fresh("dot")
        out.append(
          s"  $dotR = linalg.dot ins(${lv.reg}, ${rv.reg} : ${lv.ty.text}, ${rv.ty.text}) outs($initR : ${outTy.text}) -> ${outTy.text}\n",
        )
        val scalarR = fresh("dot_s")
        out.append(s"  $scalarR = tensor.extract $dotR[] : ${outTy.text}\n")
        MlirVal(scalarR, MScalar(elemL))
      case (lt, rt) =>
        notYet(s"matmul shape: $lt @ $rt")

  /** Zero-filled output tensor for the linalg.{matmul, matvec, dot}
    * family. `linalg.fill` over a `tensor.empty()` is the canonical
    * way to materialise the accumulator they reduce into.
    */
  private def emitZeroInit(ty: MTensor): String =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val zeroR  = fresh("zero")
    out.append(s"  $zeroR = arith.constant ${zeroLit(elemT)} : $scalar\n")
    val emptyR = fresh("empty")
    out.append(s"  $emptyR = tensor.empty() : ${ty.text}\n")
    val initR  = fresh("init")
    out.append(s"  $initR = linalg.fill ins($zeroR : $scalar) outs($emptyR : ${ty.text}) -> ${ty.text}\n")
    initR

  /** Sum-reduce a tensor of any rank to a 0-d tensor, then extract
    * the scalar. Matches `NexInterpreter`'s rule that `sum` walks
    * every element regardless of rank — for rank-N input we reduce
    * along all N dimensions in one `linalg.reduce` and the output
    * is 0-d. Init value is the element-type zero; reducer is the
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
    val dims  = ty.shape.indices.mkString(", ")
    val sumTR = fresh("sum_t")
    out.append(
      s"  $sumTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("+", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val sumR = fresh("sum")
    out.append(s"  $sumR = tensor.extract $sumTR[] : ${outTy.text}\n")
    MlirVal(sumR, MScalar(elemT))

  /** Scalar-against-tensor broadcast for arithmetic ops. Emits one
    * `linalg.map` over the tensor; the body closes over the scalar
    * SSA value from the parent scope (MLIR's dominance rules let
    * region bodies reference parent-scope values). `scalarFirst`
    * matters for non-commutative ops: `100 - xs` and `xs - 100`
    * differ.
    */
  private def emitBroadcast(op: String, sv: MlirVal, av: MlirVal, ty: MTensor, scalarFirst: Boolean): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val opName = scalarBinop(op, elemT)
    val initR  = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val outR = fresh("bc")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${ty.text})\n",
    )
    out.append(s"    (%a: $scalar, %_o: $scalar) {\n")
    if scalarFirst then
      out.append(s"      %s = $opName ${sv.reg}, %a : $scalar\n")
    else
      out.append(s"      %s = $opName %a, ${sv.reg} : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Element-wise arithmetic on tensors of matching shape but mismatched
    * element types (e.g. `[int] + [real]`). The wider numeric type is
    * the result element type; each loaded element is promoted to that
    * type via `arith.sitofp` inside the region body before the binop
    * runs. Mirrors `emitElementWiseComparison` but yields a numeric
    * tensor rather than a bool tensor.
    */
  private def emitElementWiseMixed(op: String, lv: MlirVal, rv: MlirVal, lt: MTensor, rt: MTensor): MlirVal =
    val lElem    = lt.elem
    val rElem    = rt.elem
    val commonT  = if lElem == TyReal || rElem == TyReal then TyReal else TyInteger
    val commonS  = scalarText(commonT)
    val outTy    = MTensor(commonT, lt.shape)
    val opName   = scalarBinop(op, commonT)
    val initR    = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR = fresh("ewm")
    out.append(
      s"  $outR = linalg.map ins(${lv.reg}, ${rv.reg} : ${lt.text}, ${rt.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(lElem)}, %b: ${scalarText(rElem)}, %_o: $commonS) {\n")
    val aName = promoteInRegion("%a", lElem, commonT)
    val bName = promoteInRegion("%b", rElem, commonT)
    out.append(s"      %s = $opName $aName, $bName : $commonS\n")
    out.append(s"      linalg.yield %s : $commonS\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** Scalar-against-tensor arithmetic broadcast when the scalar's
    * element type doesn't match the tensor's (e.g. `2 + [1.0, 2.0]`).
    * The scalar is promoted once outside the map body if needed; the
    * per-element promotion of the loaded tensor element happens
    * inside the region body. Output element type is the wider common
    * type. `scalarFirst` controls operand order for non-commutative
    * ops.
    */
  private def emitBroadcastMixed(
      op: String,
      sv: MlirVal,
      av: MlirVal,
      ty: MTensor,
      scalarFirst: Boolean,
      scalarElem: Type,
      arrElem: Type,
  ): MlirVal =
    val commonT = if scalarElem == TyReal || arrElem == TyReal then TyReal else TyInteger
    val commonS = scalarText(commonT)
    val opName  = scalarBinop(op, commonT)
    val svPromoted =
      if scalarElem == commonT then sv.reg
      else
        val r = fresh("ps")
        out.append(s"  $r = arith.sitofp ${sv.reg} : ${scalarText(scalarElem)} to $commonS\n")
        r
    val outTy = MTensor(commonT, ty.shape)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR = fresh("bcm")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(arrElem)}, %_o: $commonS) {\n")
    val elemName = promoteInRegion("%a", arrElem, commonT)
    val (lhs, rhs) = if scalarFirst then (svPromoted, elemName) else (elemName, svPromoted)
    out.append(s"      %s = $opName $lhs, $rhs : $commonS\n")
    out.append(s"      linalg.yield %s : $commonS\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** Element-wise comparison: `xs < ys`, `xs == ys`, etc. Output element
    * type is always `i1`. Mixed-element-type inputs (`[int] < [real]`)
    * are handled by promoting each loaded element up to the wider numeric
    * type inside the region body, then dispatching to `arith.cmpi` /
    * `arith.cmpf` with the same predicate scheme as scalar comparisons
    * (ordered for everything except `!=`, which uses `une` so NaN-vs-NaN
    * is correctly true).
    */
  private def emitElementWiseComparison(op: String, lv: MlirVal, rv: MlirVal, lt: MTensor, rt: MTensor): MlirVal =
    val lElem    = lt.elem
    val rElem    = rt.elem
    val commonT  = if lElem == TyReal || rElem == TyReal then TyReal else TyInteger
    val commonS  = scalarText(commonT)
    val outTy    = MTensor(TyBool, lt.shape)
    val initR    = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR = fresh("ewcmp")
    out.append(
      s"  $outR = linalg.map ins(${lv.reg}, ${rv.reg} : ${lt.text}, ${rt.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(lElem)}, %b: ${scalarText(rElem)}, %_o: i1) {\n")
    val aName = promoteInRegion("%a", lElem, commonT)
    val bName = promoteInRegion("%b", rElem, commonT)
    val pred  = comparisonPredicate(op, commonT)
    val cmp   = if commonT == TyInteger then "arith.cmpi" else "arith.cmpf"
    out.append(s"      %s = $cmp $pred, $aName, $bName : $commonS\n")
    out.append(s"      linalg.yield %s : i1\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** Scalar-against-tensor comparison broadcast: `xs < 5`, `2 < xs`,
    * etc. Output is a `tensor<...xi1>` with the tensor's shape. The
    * scalar is promoted once outside the map body if its type doesn't
    * match the common comparison type; the per-element promotion of
    * the loaded tensor element happens inside the region body.
    * `scalarFirst` carries through to the operand order of the cmp op
    * — matters for non-symmetric predicates (`<`, `<=`, `>`, `>=`).
    */
  private def emitBroadcastComparison(
      op: String,
      sv: MlirVal,
      av: MlirVal,
      ty: MTensor,
      scalarFirst: Boolean,
      scalarElem: Type,
      arrElem: Type,
  ): MlirVal =
    val commonT = if scalarElem == TyReal || arrElem == TyReal then TyReal else TyInteger
    val commonS = scalarText(commonT)
    val svPromoted =
      if scalarElem == commonT then sv.reg
      else
        val r = fresh("ps")
        out.append(s"  $r = arith.sitofp ${sv.reg} : ${scalarText(scalarElem)} to $commonS\n")
        r
    val outTy = MTensor(TyBool, ty.shape)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR = fresh("bccmp")
    out.append(
      s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${outTy.text})\n",
    )
    out.append(s"    (%a: ${scalarText(arrElem)}, %_o: i1) {\n")
    val elemName = promoteInRegion("%a", arrElem, commonT)
    val pred     = comparisonPredicate(op, commonT)
    val cmp      = if commonT == TyInteger then "arith.cmpi" else "arith.cmpf"
    val (lhs, rhs) = if scalarFirst then (svPromoted, elemName) else (elemName, svPromoted)
    out.append(s"      %s = $cmp $pred, $lhs, $rhs : $commonS\n")
    out.append(s"      linalg.yield %s : i1\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `product(arr)` (spec §10.4): rank-N multiplicative reduction.
    * Same shape as `emitSumReduce` but seeds the accumulator with 1
    * and uses the type-specific multiply op (`muli` for int, `mulf`
    * for real). Walks every element regardless of rank.
    */
  private def emitProductReduce(srcReg: String, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val outTy  = MTensor(elemT, Nil)
    val oneR   = fresh("init_e")
    val oneLit = elemT match
      case TyInteger => "1"
      case TyReal    => "1.0"
      case other     => notYet(s"one literal for $other")
    out.append(s"  $oneR = arith.constant $oneLit : $scalar\n")
    val initR = fresh("init")
    out.append(s"  $initR = tensor.from_elements $oneR : ${outTy.text}\n")
    val dims  = ty.shape.indices.mkString(", ")
    val prodTR = fresh("prod_t")
    out.append(
      s"  $prodTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("*", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val prodR = fresh("prod")
    out.append(s"  $prodR = tensor.extract $prodTR[] : ${outTy.text}\n")
    MlirVal(prodR, MScalar(elemT))

  /** `diag(arr)` (rank-1 → rank-2): build an n×n matrix with `arr` on
    * the diagonal and zeros elsewhere. Lowers to `linalg.fill` of 0
    * over a fresh n×n tensor, then a `linalg.map` over a rank-1
    * sequence of [0..n) that scatters `arr[i]` to position `(i, i)`
    * via `tensor.insert`. Output is a fresh tensor.
    *
    * Implemented as a `linalg.map` over the n×n output that uses
    * `linalg.index` to recover (i, j) and `arith.cmpi eq` plus
    * `arith.select` to either pick `arr[i]` or zero.
    */
  private def emitDiag(av: MlirVal, srcTy: MTensor, n: Int): MlirVal =
    val elemT = srcTy.elem
    val s     = scalarText(elemT)
    val zero  = zeroLit(elemT)
    val outTy = MTensor(elemT, List(n, n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("diag")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val iR    = fresh("i")
    out.append(s"      $iR = linalg.index 0 : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = linalg.index 1 : index\n")
    val cmpR  = fresh("eq")
    out.append(s"      $cmpR = arith.cmpi eq, $iR, $jR : index\n")
    val zR    = fresh("z")
    out.append(s"      $zR = arith.constant $zero : $s\n")
    val eltR  = fresh("elt")
    out.append(s"      $eltR = tensor.extract ${av.reg}[$iR] : ${srcTy.text}\n")
    val selR  = fresh("sel")
    out.append(s"      $selR = arith.select $cmpR, $eltR, $zR : $s\n")
    out.append(s"      linalg.yield $selR : $s\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `identity(n)`: n×n integer identity matrix. Same shape as `diag`
    * but the diagonal value is the constant 1. Element type is
    * integer (matches the interpreter — the spec says real-typed but
    * v0 has a known divergence).
    */
  private def emitIdentity(n: Int): MlirVal =
    val outTy = MTensor(TyInteger, List(n, n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("id")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: i64) {\n")
    val iR    = fresh("i")
    out.append(s"      $iR = linalg.index 0 : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = linalg.index 1 : index\n")
    val cmpR  = fresh("eq")
    out.append(s"      $cmpR = arith.cmpi eq, $iR, $jR : index\n")
    val oneR  = fresh("one")
    out.append(s"      $oneR = arith.constant 1 : i64\n")
    val zR    = fresh("z")
    out.append(s"      $zR = arith.constant 0 : i64\n")
    val selR  = fresh("sel")
    out.append(s"      $selR = arith.select $cmpR, $oneR, $zR : i64\n")
    out.append(s"      linalg.yield $selR : i64\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `sum_axis(m, axis)` (spec §10.4): rank-2 reduction along one axis,
    * producing a rank-1 result. axis=0 sums down each column (output
    * shape = cols); axis=1 sums across each row (output shape = rows).
    * Lowers to `linalg.reduce ... dimensions = [axis]` with a `+`
    * accumulator body — same kernel as the all-dims `emitSumReduce`,
    * just with a non-empty output shape.
    */
  private def emitSumAxis(av: MlirVal, srcTy: MTensor, axis: Int): MlirVal =
    val elemT     = srcTy.elem
    val scalar    = scalarText(elemT)
    val outShape  = srcTy.shape.zipWithIndex.collect { case (d, i) if i != axis => d }
    val outTy     = MTensor(elemT, outShape)
    val initER    = fresh("init_e")
    out.append(s"  $initER = arith.constant ${zeroLit(elemT)} : $scalar\n")
    val initR     = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val filledR   = fresh("filled")
    out.append(s"  $filledR = linalg.fill ins($initER : $scalar) outs($initR : ${outTy.text}) -> ${outTy.text}\n")
    val outR      = fresh("axis")
    out.append(
      s"  $outR = linalg.reduce ins(${av.reg} : ${srcTy.text}) outs($filledR : ${outTy.text}) dimensions = [$axis]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = ${scalarBinop("+", elemT)} %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `flatten(xs)` on a rank-1 array: identity, but materialise a fresh
    * tensor to match the interpreter's "flatten always copies"
    * semantics. A `linalg.copy`-style identity map does the job.
    */
  private def emitFlatten1D(av: MlirVal): MlirVal =
    val ty    = av.ty.asInstanceOf[MTensor]
    val elemT = ty.elem
    val s     = scalarText(elemT)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val outR  = fresh("flat")
    out.append(s"  $outR = linalg.map ins(${av.reg} : ${ty.text}) outs($initR : ${ty.text})\n")
    out.append(s"    (%a: $s, %_o: $s) {\n")
    out.append(s"      linalg.yield %a : $s\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** `flatten(m)` on a rank-2 array — Nex spec §10.4 says this is
    * **column-major**: `flat[i + r*j] = m[i, j]` for an r×c matrix.
    * We materialise a fresh rank-1 of length r·c via `linalg.map`
    * whose body uses `linalg.index 0` plus a divmod against `r` to
    * recover the source `(i, j)`, then `tensor.extract %m[i, j]`.
    */
  private def emitFlatten2D(av: MlirVal, srcTy: MTensor, rows: Int, cols: Int): MlirVal =
    val elemT = srcTy.elem
    val s     = scalarText(elemT)
    val total = rows * cols
    val outTy = MTensor(elemT, List(total))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("flat")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val rowsC = fresh("rows")
    out.append(s"      $rowsC = arith.constant $rows : index\n")
    val iR    = fresh("i")
    out.append(s"      $iR = arith.remui $idxR, $rowsC : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = arith.divui $idxR, $rowsC : index\n")
    val eltR  = fresh("elt")
    out.append(s"      $eltR = tensor.extract ${av.reg}[$iR, $jR] : ${srcTy.text}\n")
    out.append(s"      linalg.yield $eltR : $s\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** `reshape(flat, r, c)` — column-major inverse of `flatten`. The
    * input is rank-1; the output is rank-2 with `m[i, j] = flat[i + r*j]`.
    * Same shape as `emitFlatten2D` but iterates the (rows, cols) output
    * grid via two `linalg.index` calls and computes the flat source
    * index by hand. The elaborator rejects shape mismatches before
    * we get here, so `length(flat)` is guaranteed to equal `r*c`.
    */
  private def emitReshape(av: MlirVal, srcTy: MTensor, rows: Int, cols: Int): MlirVal =
    val elemT = srcTy.elem
    val s     = scalarText(elemT)
    val outTy = MTensor(elemT, List(rows, cols))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("rs")
    out.append(s"  $outR = linalg.map outs($initR : ${outTy.text})\n")
    out.append(s"    (%_o: $s) {\n")
    val iR    = fresh("i")
    out.append(s"      $iR = linalg.index 0 : index\n")
    val jR    = fresh("j")
    out.append(s"      $jR = linalg.index 1 : index\n")
    val rowsC = fresh("rows")
    out.append(s"      $rowsC = arith.constant $rows : index\n")
    val mul   = fresh("mul")
    out.append(s"      $mul = arith.muli $rowsC, $jR : index\n")
    val flat  = fresh("flat")
    out.append(s"      $flat = arith.addi $iR, $mul : index\n")
    val eltR  = fresh("elt")
    out.append(s"      $eltR = tensor.extract ${av.reg}[$flat] : ${srcTy.text}\n")
    out.append(s"      linalg.yield $eltR : $s\n")
    out.append("    }\n")
    MlirVal(outR, outTy)

  /** Rank-2 `.transpose()` / `transpose(m)`. Output shape swaps the
    * row and column dimensions; element type is unchanged. Lowers to
    * a single `linalg.transpose` with permutation `[1, 0]`, which
    * `--convert-linalg-to-loops` reduces to a nested counting loop
    * that does `out[j, i] = in[i, j]`.
    */
  private def emitTranspose(srcReg: String, ty: MTensor, rows: Int, cols: Int): MlirVal =
    val outTy = MTensor(ty.elem, List(cols, rows))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${outTy.text}\n")
    val outR  = fresh("tr")
    out.append(
      s"  $outR = linalg.transpose ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) permutation = [1, 0]\n",
    )
    MlirVal(outR, outTy)

  /** Literal `range(lo, hi)`: integer half-open range with statically
    * known length. Length zero (when `hi <= lo`) lowers to an empty
    * `tensor<0xi64>`. Otherwise we walk the iteration domain via
    * `linalg.index` and add the loaded position to the literal `lo`.
    */
  private def emitRangeCall(lo: Long, hi: Long): MlirVal =
    val len = math.max(0L, hi - lo).toInt
    val ty  = MTensor(TyInteger, List(len))
    if len == 0 then
      val r = fresh("rng")
      out.append(s"  $r = tensor.empty() : ${ty.text}\n")
      return MlirVal(r, ty)
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val loConst = fresh("lo")
    out.append(s"  $loConst = arith.constant $lo : i64\n")
    val outR = fresh("rng")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: i64) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI  = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val sumR  = fresh("sum")
    out.append(s"      $sumR = arith.addi $loConst, $idxI : i64\n")
    out.append(s"      linalg.yield $sumR : i64\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Literal `zeros(n)` / `ones(n)`. Always integer-typed per the
    * elaborator (matches the interpreter; the spec's real-typed
    * signature is a v0 divergence shared across backends). Emits a
    * `linalg.fill` over a fresh empty tensor.
    */
  private def emitConstFill(name: String, n: Int): MlirVal =
    val ty    = MTensor(TyInteger, List(n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val v     = if name == "zeros" then 0 else 1
    val cR    = fresh("c")
    out.append(s"  $cR = arith.constant $v : i64\n")
    val outR  = fresh(name)
    out.append(s"  $outR = linalg.fill ins($cR : i64) outs($initR : ${ty.text}) -> ${ty.text}\n")
    MlirVal(outR, ty)

  /** Literal `linspace(lo, hi, n)`: real array of length n with values
    * `lo + i * (hi - lo) / (n - 1)`. We compute the step at codegen
    * time and emit a `linalg.map` that yields `lo + step * i`. When
    * `n == 1`, every element collapses to `lo` (`step` is a 0/0 NaN
    * otherwise); special-case to avoid emitting NaN in the IR.
    */
  private def emitLinspaceCall(lo: Double, hi: Double, n: Int): MlirVal =
    val ty    = MTensor(TyReal, List(n))
    val initR = fresh("init")
    out.append(s"  $initR = tensor.empty() : ${ty.text}\n")
    val loC   = fresh("lo")
    out.append(s"  $loC = arith.constant ${formatReal(lo)} : f64\n")
    val step  = if n <= 1 then 0.0 else (hi - lo) / (n - 1)
    val stepC = fresh("step")
    out.append(s"  $stepC = arith.constant ${formatReal(step)} : f64\n")
    val outR  = fresh("ls")
    out.append(s"  $outR = linalg.map outs($initR : ${ty.text})\n")
    out.append(s"    (%_o: f64) {\n")
    val idxR  = fresh("idx")
    out.append(s"      $idxR = linalg.index 0 : index\n")
    val idxI  = fresh("idxi")
    out.append(s"      $idxI = arith.index_castui $idxR : index to i64\n")
    val idxF  = fresh("idxf")
    out.append(s"      $idxF = arith.sitofp $idxI : i64 to f64\n")
    val mulR  = fresh("mul")
    out.append(s"      $mulR = arith.mulf $stepC, $idxF : f64\n")
    val addR  = fresh("add")
    out.append(s"      $addR = arith.addf $loC, $mulR : f64\n")
    out.append(s"      linalg.yield $addR : f64\n")
    out.append("    }\n")
    MlirVal(outR, ty)

  /** Pull a `Double` out of an `TIntLit` or `TRealLit`. Used by the
    * `linspace` dispatch where the elaborator may leave `0` (int) or
    * `0.0` (real) untouched on the lo/hi arguments — both meanings
    * are valid sources.
    */
  private def realLitValue(e: TExpr): Double = e match
    case TIntLit(v, _, _)  => v.toDouble
    case TRealLit(v, _, _) => v
    case other             => notYet(s"non-literal linspace bound: ${other.getClass.getSimpleName}")

  /** Predicate string for `arith.cmpi` / `arith.cmpf`. Integers use
    * signed predicates; reals use ordered (`oeq`, `olt`, …) for every
    * op except `!=`, where `une` makes `nan != nan` true — matching
    * IEEE 754 and the spec §10.2.
    */
  private def comparisonPredicate(op: String, t: Type): String = (op, t) match
    case ("==", TyInteger) => "eq"
    case ("!=", TyInteger) => "ne"
    case ("<",  TyInteger) => "slt"
    case ("<=", TyInteger) => "sle"
    case (">",  TyInteger) => "sgt"
    case (">=", TyInteger) => "sge"
    case ("==", TyReal)    => "oeq"
    case ("!=", TyReal)    => "une"
    case ("<",  TyReal)    => "olt"
    case ("<=", TyReal)    => "ole"
    case (">",  TyReal)    => "ogt"
    case (">=", TyReal)    => "oge"
    case _                 => notYet(s"comparison predicate $op on $t")

  /** Emit a `sitofp` promotion inside a linalg.map / linalg.reduce
    * region body (indented at the region depth) and return the new
    * SSA name. Returns `srcName` unchanged when no promotion is
    * needed.
    */
  private def promoteInRegion(srcName: String, fromT: Type, toT: Type): String =
    if fromT == toT then srcName
    else (fromT, toT) match
      case (TyInteger, TyReal) =>
        val r = fresh("pr")
        out.append(s"      $r = arith.sitofp $srcName : i64 to f64\n")
        r
      case _ => notYet(s"in-region promote $fromT to $toT")

  /** Scalar binary `min` / `max`. Promotes mixed `int × real` operands
    * to real before dispatching to the matching `arith.{minsi, maxsi,
    * minimumf, maximumf}` op. Same NaN-propagating spelling we use
    * for array min/max.
    */
  private def emitScalarMinMax(name: String, lv: MlirVal, rv: MlirVal): MlirVal =
    val (lp, rp) = (lv.ty, rv.ty) match
      case (MScalar(TyInteger), MScalar(TyReal))    => (promoteIntToReal(lv), rv)
      case (MScalar(TyReal), MScalar(TyInteger))    => (lv, promoteIntToReal(rv))
      case _                                        => (lv, rv)
    (lp.ty, rp.ty) match
      case (MScalar(TyInteger), MScalar(TyInteger)) =>
        val opName = if name == "min" then "arith.minsi" else "arith.maxsi"
        val r      = fresh(name)
        out.append(s"  $r = $opName ${lp.reg}, ${rp.reg} : i64\n")
        MlirVal(r, MScalar(TyInteger))
      case (MScalar(TyReal), MScalar(TyReal)) =>
        val opName = if name == "min" then "arith.minimumf" else "arith.maximumf"
        val r      = fresh(name)
        out.append(s"  $r = $opName ${lp.reg}, ${rp.reg} : f64\n")
        MlirVal(r, MScalar(TyReal))
      case (lt, rt) =>
        notYet(s"$name on $lt and $rt")

  /** Scalar `abs`: branchless via `cmp + select`. Avoids depending on
    * the math dialect (which would force `--convert-math-to-llvm` into
    * the pass pipeline). Integer abs uses signed less-than against zero
    * and subtracts; real abs flips the sign with `arith.negf` when
    * `x < 0.0`. Note: `arith.cmpf olt(-0.0, 0.0)` is false (signed-zero
    * pair is equal in IEEE), so this preserves `-0.0` for the negative
    * zero — a known divergence the corpus does not currently test for
    * `abs(real)`.
    */
  private def emitScalarAbs(v: MlirVal): MlirVal = v.ty match
    case MScalar(TyInteger) =>
      val zero = fresh("z")
      out.append(s"  $zero = arith.constant 0 : i64\n")
      val neg = fresh("neg")
      out.append(s"  $neg = arith.subi $zero, ${v.reg} : i64\n")
      val cmp = fresh("cmp")
      out.append(s"  $cmp = arith.cmpi slt, ${v.reg}, $zero : i64\n")
      val r = fresh("abs")
      out.append(s"  $r = arith.select $cmp, $neg, ${v.reg} : i64\n")
      MlirVal(r, MScalar(TyInteger))
    case MScalar(TyReal) =>
      val zero = fresh("z")
      out.append(s"  $zero = arith.constant 0.0 : f64\n")
      val neg = fresh("neg")
      out.append(s"  $neg = arith.negf ${v.reg} : f64\n")
      val cmp = fresh("cmp")
      out.append(s"  $cmp = arith.cmpf olt, ${v.reg}, $zero : f64\n")
      val r = fresh("abs")
      out.append(s"  $r = arith.select $cmp, $neg, ${v.reg} : f64\n")
      MlirVal(r, MScalar(TyReal))
    case other =>
      notYet(s"abs of $other")

  /** Reduce a rank-1 tensor with the `min` or `max` prelude. Mirrors
    * `emitSumReduce` but seeds the accumulator with `arr[0]` rather
    * than a zero sentinel — matches `NexInterpreter`'s
    * `b.tail.foldLeft(b.head)` and dodges the need for an
    * IEEE-defined +∞ / -∞ constant in MLIR text.
    *
    * Reducer ops:
    *   - int  → `arith.minsi` / `arith.maxsi`
    *   - real → `arith.minimumf` / `arith.maximumf`
    *     (IEEE 754-2019 spelling, NaN-propagating like the
    *      `arith.cmpf` predicates we use elsewhere)
    *
    * Empty-array case is the caller's problem — the elaborator either
    * rejects empty literals or accepts them with a trap at runtime;
    * `tensor.extract` on an empty rank-1 tensor is UB by MLIR rules,
    * which matches NexInterpreter's `trap("min: empty array", ...)`.
    */
  private def emitMinMaxReduce(name: String, srcReg: String, ty: MTensor): MlirVal =
    val elemT  = ty.elem
    val scalar = scalarText(elemT)
    val outTy  = MTensor(elemT, Nil)
    val zeroIdx = fresh("z_idx")
    out.append(s"  $zeroIdx = arith.constant 0 : index\n")
    val initER = fresh("init_e")
    out.append(s"  $initER = tensor.extract $srcReg[$zeroIdx] : ${ty.text}\n")
    val initR = fresh("init")
    out.append(s"  $initR = tensor.from_elements $initER : ${outTy.text}\n")
    val opName = (name, elemT) match
      case ("min", TyInteger) => "arith.minsi"
      case ("max", TyInteger) => "arith.maxsi"
      case ("min", TyReal)    => "arith.minimumf"
      case ("max", TyReal)    => "arith.maximumf"
      case _                  => notYet(s"$name reducer on $elemT")
    val dims  = ty.shape.indices.mkString(", ")
    val redTR = fresh(s"${name}_t")
    out.append(
      s"  $redTR = linalg.reduce ins($srcReg : ${ty.text}) outs($initR : ${outTy.text}) dimensions = [$dims]\n",
    )
    out.append(s"    (%in: $scalar, %acc: $scalar) {\n")
    out.append(s"      %s = $opName %in, %acc : $scalar\n")
    out.append(s"      linalg.yield %s : $scalar\n")
    out.append("    }\n")
    val redR = fresh(name)
    out.append(s"  $redR = tensor.extract $redTR[] : ${outTy.text}\n")
    MlirVal(redR, MScalar(elemT))

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
    case TyBool    => "i1"
    case other     => notYet(s"scalar text for $other")

  private def zeroLit(t: Type): String = t match
    case TyInteger => "0"
    case TyReal    => "0.0"
    case other     => notYet(s"zero literal for $other")

  /** Map a Nex binop + element type to the `arith.*` op that performs
    * it on a scalar. Used uniformly inside `linalg.reduce` and
    * `linalg.map` body regions, so both reductions and element-wise
    * ops share the dispatch table.
    *
    * Nex's `/` is always real division — the elaborator promotes
    * `int / int` to real-typed before this dispatcher sees it, so
    * `("/", TyInteger)` is never expected and isn't listed. `div`
    * (integer division) and `%` (integer modulo) stay on `i64`.
    */
  private def scalarBinop(op: String, t: Type): String = (op, t) match
    case ("+",   TyInteger) => "arith.addi"
    case ("-",   TyInteger) => "arith.subi"
    case ("*",   TyInteger) => "arith.muli"
    case ("div", TyInteger) => "arith.divsi"
    case ("%",   TyInteger) => "arith.remsi"
    case ("+",   TyReal)    => "arith.addf"
    case ("-",   TyReal)    => "arith.subf"
    case ("*",   TyReal)    => "arith.mulf"
    case ("/",   TyReal)    => "arith.divf"
    case _                  => notYet(s"scalar binop $op on $t")

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
