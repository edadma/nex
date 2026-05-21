package io.github.edadma.nex

import scala.collection.mutable

/** Closure subsystem for [[NexMLIRCodegen]].
  *
  * A lambda that escapes its declaring scope (returned from a `def`,
  * bound to a `val` and called later, etc.) is lowered to:
  *
  *   - a top-level synthetic `llvm.func @nex_lambda_<id>(%env: i64, args...)`
  *     whose body reads its free variables out of the env;
  *   - a heap-allocated env blob whose layout is `i64` per capture slot
  *     (ByVal captures store the value, ByRef captures store a pointer
  *     to a separately heap-allocated 8-byte "var box");
  *   - an `nex_closure` descriptor on the heap holding `(fn_ptr, env)`
  *     — the closure value is the descriptor's address (`i64`).
  *
  * A `var` referenced inside any lambda is "boxed": its declaration
  * heap-allocates a box (instead of `memref.alloca`); reads and writes
  * route through `nex_env_load_i64` / `nex_env_store_i64`. This makes
  * mutations visible across the parent scope and any closures sharing
  * the same box, and lets the box survive the parent frame's return.
  *
  * Captures are restricted to `i64`-shaped types (integer, bool, string)
  * in the first slice; real / array captures land later. Lambda bodies
  * are emitted as `llvm.func` so their address can be taken via
  * `llvm.mlir.addressof` for the closure descriptor.
  *
  * Refcounting is NOT implemented yet — envs, boxes, and descriptors
  * leak. Test programs are short-running; refcount lands when the
  * surface expands.
  */
trait NexMLIRLambdas:
  self: NexMLIRCodegen =>

  protected enum CaptureMode:
    case ByVal, ByRef

  protected case class LambdaInfo(
      id:       Int,
      mlirName: String,
      params:   List[Symbol],
      body:     TExpr,
      retTyOpt: Option[MlirType],
      captures: List[(Symbol, MlirType, CaptureMode)],
  )

  protected val lambdaTable      = new java.util.IdentityHashMap[TLambda, LambdaInfo]
  protected val varBindings      = mutable.Set.empty[Int]
  protected val boxedVarSet      = mutable.Set.empty[Int]
  protected val boxedVarBoxes    = mutable.Map.empty[Int, String]
  protected var currentLambdaCaptures: Map[Int, (Int, MlirType, CaptureMode)] = Map.empty

  /** Synthetic-thunk registry for top-level user defs reached as
    * function-values (passed as args, bound to vals, returned, etc).
    * Keyed by the user-def's symbol id; value is the MLIR symbol name
    * of the thunk `llvm.func`. Populated by [[collectDefThunks]] from
    * any [[TVarRef]] to a `SymKind.Function` symbol that appears
    * outside the immediate-callee position of a [[TCall]]. The thunk
    * itself is written by [[emitDefThunkFunctions]] before `@main`;
    * the construction site at the reference produces the closure
    * descriptor `{thunk_addr, null_env}` via `nex_closure_make`.
    */
  protected val defThunks = mutable.Map.empty[Int, String]

  /** First pre-pass: collect every `var` binding's Symbol id. Drives the
    * later by-ref/by-val classification — a free variable referenced
    * inside a lambda is captured ByRef iff its symbol is here.
    */
  protected def collectVarBindings(tp: TProgram): Unit =
    def walk(e: TExpr): Unit = e match
      case TBlock(items, r, _, _) =>
        items.foreach {
          case TBlockBinding(s, BindingKind.Var, v) => varBindings += s.id; walk(v)
          case TBlockBinding(_, _, v)               => walk(v)
          case TBlockExpr(x)                        => walk(x)
        }
        walk(r)
      case _ => walkChildren(e, walk)
    tp.allDecls.foreach {
      case b: TTopBinding if b.kind == BindingKind.Var => varBindings += b.sym.id; walk(b.value)
      case b: TTopBinding                              => walk(b.value)
      case f: TFunDecl                                 => walk(f.body)
      case _                                           => ()
    }

  /** Second pre-pass: walk every function body, register each TLambda
    * with its capture layout. Captured-var classification: ByRef when
    * the symbol's id is in [[varBindings]], ByVal otherwise. Any
    * ByRef capture also marks the source var in [[boxedVarSet]] so
    * its declaration switches from `memref.alloca` to a heap box.
    *
    * Captures whose type isn't expressible at the MLIR level (rank-2
    * arrays into closures, tensor params, …) cause the lambda to be
    * skipped — the construction site will fall through to `notYet`.
    * For the first slice this leaves only i64-shaped captures
    * (integer, bool, string) in play.
    */
  protected def collectLambdas(tp: TProgram): Unit =
    def walk(e: TExpr, bound: Set[Int]): Unit = e match
      case lam @ TLambda(params, body, _, _) =>
        val paramIds = params.map(_.id).toSet
        val frees    = mutable.LinkedHashMap.empty[Int, (Symbol, Type)]
        collectFrees(body, paramIds, frees)
        val captureOpts = frees.values.toList.map { case (s, t) =>
          val mty  = mlirTypeOf(t)
          val mode = if varBindings.contains(s.id) then CaptureMode.ByRef else CaptureMode.ByVal
          mty.map(m => (s, m, mode))
        }
        val paramMTyOpts = params.map(p => mlirTypeOf(p.tpe))
        val retOpt: Option[Option[MlirType]] = lam.tpe match
          case TyFunc(_, r) => r match
            case TyUnit => Some(None)
            case other  => mlirTypeOf(other).map(Some(_))
          case _ => mlirTypeOf(body.tpe).map(Some(_))
        val signatureOk =
          paramMTyOpts.forall(_.exists(isLlvmFuncSignatureType)) &&
          retOpt.exists(_.forall(isLlvmFuncSignatureType))
        if signatureOk
          && captureOpts.forall(_.isDefined)
          && captureOpts.forall { c =>
            val (_, m, _) = c.get
            isClosureSlotType(m)
          }
        then
          val captures = captureOpts.map(_.get)
          captures.foreach { case (s, _, mode) =>
            if mode == CaptureMode.ByRef then boxedVarSet += s.id
          }
          val id       = lambdaTable.size
          val mlirName = s"@nex_lambda_$id"
          lambdaTable.put(lam, LambdaInfo(id, mlirName, params, body, retOpt.get, captures))
        walk(body, bound ++ paramIds)
      case TBlock(items, r, _, _) =>
        var sub = bound
        items.foreach {
          case TBlockBinding(s, _, v) => walk(v, sub); sub = sub + s.id
          case TBlockExpr(x)          => walk(x, sub)
        }
        walk(r, sub)
      case TFor(loopVars, iter, body, _, _) =>
        walk(iter, bound)
        walk(body, bound ++ loopVars.map(_.id))
      case _ => walkChildren(e, walk(_, bound))
    tp.allDecls.foreach {
      case f: TFunDecl    => walk(f.body, f.params.map(_.id).toSet)
      case b: TTopBinding => walk(b.value, Set.empty)
      case _              => ()
    }

  /** Third pre-pass: walk every program node and register a thunk for
    * each top-level user-def `TVarRef` that appears in a value
    * position (i.e. anywhere except as the immediate callee of a
    * [[TCall]]). A registered thunk name is `@nex_def_thunk_<id>` keyed
    * on the def's symbol id, so multiple references share one thunk.
    */
  protected def collectDefThunks(tp: TProgram): Unit =
    def walk(e: TExpr): Unit = e match
      case TCall(callee @ TVarRef(_, _, _), args, _, _) =>
        args.foreach(walk)
        // Intentionally do NOT recurse into `callee` — its TVarRef is
        // in callee position, handled by the user-def TCall arm.
        ()
      case TVarRef(s, _, _) if s.kind == SymKind.Function && userDefs.contains(s.id) =>
        // A thunk lives inside an `llvm.func`, which only admits
        // LLVM-dialect-compatible scalars in its signature. Reject
        // tensor params / returns up front so reference-as-value
        // surfaces at `notYet` rather than producing un-translatable
        // MLIR. Tensor-shaped function-values are a separate piece
        // of work (see roadmap §3 "Tensor captures + tensor closure
        // params/returns").
        val (_, paramTys, retTyOpt) = userDefs(s.id)
        val sigOk = paramTys.forall(isLlvmFuncSignatureType) &&
          retTyOpt.forall(isLlvmFuncSignatureType)
        if sigOk && !defThunks.contains(s.id) then defThunks(s.id) = s"@nex_def_thunk_${s.id}"
      case _ => walkChildren(e, walk)
    tp.allDecls.foreach {
      case f: TFunDecl    => walk(f.body)
      case b: TTopBinding => walk(b.value)
      case _              => ()
    }

  /** Slot type predicate: which MLIR types can sit in an env slot. The
    * first slice limits this to i64-shaped values (integer, bool,
    * string — and any MFunc, which is also i64). Real, tensor, etc.
    * fall through, causing the lambda to be skipped during
    * registration.
    */
  protected def isClosureSlotType(m: MlirType): Boolean = m match
    case MScalar(TyInteger) | MScalar(TyBool) => true
    case MString | _: MFunc                    => true
    case _                                      => false

  /** Lambda signature constraint: every param + return type must be an
    * LLVM-dialect-compatible scalar (i64 / f64 / i1) or i64-shaped
    * pointer (string / closure). Lambdas with tensor params or returns
    * stay unregistered — they only ever surface as inline arguments
    * to HOFs like `map` / `filter` / `reduce` / `flatMap`, which
    * inline their body directly and never need to take the lambda's
    * address.
    */
  protected def isLlvmFuncSignatureType(m: MlirType): Boolean = m match
    case MScalar(TyInteger) | MScalar(TyReal) | MScalar(TyBool) => true
    case MString | _: MFunc                                      => true
    case _                                                        => false

  /** Collect free variable references in `e` whose ids are not in
    * `bound`. Restricted to capturable [[SymKind]]s so prelude /
    * type / function symbols never enter env layout.
    */
  protected def collectFrees(
      e:     TExpr,
      bound: Set[Int],
      acc:   mutable.LinkedHashMap[Int, (Symbol, Type)],
  ): Unit = e match
    case TVarRef(s, _, t) if !bound.contains(s.id) && isCapturableSym(s) =>
      if !acc.contains(s.id) then acc(s.id) = (s, t)
    case TLambda(ps, body, _, _) =>
      collectFrees(body, bound ++ ps.map(_.id), acc)
    case TBlock(items, r, _, _) =>
      var sub = bound
      items.foreach {
        case TBlockBinding(s, _, v) => collectFrees(v, sub, acc); sub = sub + s.id
        case TBlockExpr(x)          => collectFrees(x, sub, acc)
      }
      collectFrees(r, sub, acc)
    case TFor(vs, it, body, _, _) =>
      collectFrees(it, bound, acc)
      collectFrees(body, bound ++ vs.map(_.id), acc)
    case _ =>
      walkChildren(e, x => collectFrees(x, bound, acc))

  protected def isCapturableSym(s: Symbol): Boolean = s.kind match
    case SymKind.Local | SymKind.Param | SymKind.TopLevel => true
    case _                                                 => false

  /** Walk an expression's children, invoking `f` on each. Identical in
    * shape to the LLVM backend's walker; kept local so the MLIR
    * subsystem is self-contained.
    */
  protected def walkChildren(e: TExpr, f: TExpr => Unit): Unit = e match
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit
       | _: TUnitLit | _: TVarRef | _: TAxisAllMark | _: TOpenSliceMark | _: TIntrinsic => ()
    case TInterpStringLit(parts, _, _) =>
      parts.foreach {
        case TInterpExpr(x, _) => f(x)
        case _                 => ()
      }
    case TBinOp(_, l, r, _, _)             => f(l); f(r)
    case TUnaryOp(_, x, _, _)              => f(x)
    case TJuxtapose(c, b, _, _)            => f(c); f(b)
    case TElementWise(_, l, r, _, _)       => f(l); f(r)
    case TBroadcast(s, a, _, _, _, _)      => f(s); f(a)
    case TMap(a, fn, _, _)                 => f(a); f(fn)
    case TReduce(a, i, fn, _, _)           => f(a); f(i); f(fn)
    case TMatMul(l, r, _, _)               => f(l); f(r)
    case TFusedLoop(_, len, b, cols, _, _) => f(len); f(b); cols.foreach(f)
    case TFlatIndex(a, i, _, _)            => f(a); f(i)
    case TClone(a, _, _)                   => f(a)
    case TCall(c, args, _, _)              => f(c); args.foreach(f)
    case TIndex(a, idx, _, _)              => f(a); idx.foreach(f)
    case TSlice(a, lo, hi, _, st, _, _) =>
      f(a); lo.foreach(f); hi.foreach(f); st.foreach(f)
    case TSlice2(a, rAx, cAx, _, _) =>
      f(a)
      def goAx(ax: TAxisSpec): Unit = ax match
        case TAxisAll                  => ()
        case TAxisIndex(e2)            => f(e2)
        case TAxisRange(lo, hi, _, st) => lo.foreach(f); hi.foreach(f); st.foreach(f)
      goAx(rAx); goAx(cAx)
    case TField(r, _, _, _)             => f(r)
    case TTupleProj(r, _, _, _)         => f(r)
    case TMethodCall(r, _, args, _, _)  => f(r); args.foreach(f)
    case TLambda(_, body, _, _)         => f(body)
    case TTuple(es, _, _)               => es.foreach(f)
    case TArrayLit(es, _, _)            => es.foreach(f)
    case TIf(c, t, e2, _, _)            => f(c); f(t); e2.foreach(f)
    case TFor(_, it, b, _, _)           => f(it); f(b)
    case TWhile(c, b, _, _)             => f(c); f(b)
    case TMatch(s, cs, _, _)            => f(s); cs.foreach(c => f(c.body))
    case TReturn(v, _, _)               => v.foreach(f)
    case TAssign(t, v, _, _)            => f(t); f(v)
    case TBlock(items, r, _, _) =>
      items.foreach {
        case TBlockBinding(_, _, v) => f(v)
        case TBlockExpr(x)          => f(x)
      }
      f(r)

  /** Emit one synthetic `llvm.func @nex_def_thunk_<id>` per registered
    * top-level def reached as a function-value. The thunk accepts an
    * `%env: i64` it ignores and forwards its remaining arguments to
    * the user's `func.call @nex_user_<name>_<id>` op. Returning the
    * underlying def's return type lets a closure literal `{thunk_addr,
    * null_env}` be dispatched through the regular indirect-call path.
    *
    * Must be emitted after the user-def `func.func` definitions and
    * before the closure-construction sites in `@main` — the addressof
    * resolution is by symbol name, so order is purely for readability.
    */
  protected def emitDefThunkFunctions(): Unit =
    defThunks.toSeq.sortBy(_._1).foreach { case (id, thunkName) =>
      val (userName, paramTys, retTyOpt) = userDefs(id)
      val paramSig =
        ("%env: i64" :: paramTys.zipWithIndex.map { case (t, i) => s"%arg$i: ${t.text}" }).mkString(", ")
      val retStr = retTyOpt.fold("")(t => s" -> ${t.text}")
      out.append(s"llvm.func $thunkName($paramSig)$retStr {\n")
      val callArgs = paramTys.indices.map(i => s"%arg$i").mkString(", ")
      val paramTyText = paramTys.map(_.text).mkString(", ")
      retTyOpt match
        case Some(retTy) =>
          out.append(s"  %r = func.call $userName($callArgs) : ($paramTyText) -> ${retTy.text}\n")
          out.append(s"  llvm.return %r : ${retTy.text}\n")
        case None =>
          out.append(s"  func.call $userName($callArgs) : ($paramTyText) -> ()\n")
          out.append("  llvm.return\n")
      out.append("}\n")
    }

  /** Build the closure value at a `TVarRef` site that names a
    * top-level user def. Allocates no env (env_ptr is `0`); takes the
    * thunk's address via `llvm.mlir.addressof` + `llvm.ptrtoint`; calls
    * `nex_closure_make` for the `{fn_ptr, env_ptr=0}` descriptor.
    */
  protected def emitDefThunkClosure(symId: Int, symName: String): MlirVal =
    val thunkName = defThunks.getOrElse(
      symId,
      notYet(s"def-thunk for `$symName` not registered (pre-pass missed it)"),
    )
    val (_, paramTys, retTyOpt) = userDefs(symId)
    val fnPtr = fresh("dthp")
    out.append(s"  $fnPtr = llvm.mlir.addressof $thunkName : !llvm.ptr\n")
    val fnInt = fresh("dthi")
    out.append(s"  $fnInt = llvm.ptrtoint $fnPtr : !llvm.ptr to i64\n")
    val envZ = fresh("dthe")
    out.append(s"  $envZ = arith.constant 0 : i64\n")
    val cl = fresh("dthcl")
    out.append(s"  $cl = func.call @nex_closure_make($fnInt, $envZ) : (i64, i64) -> i64\n")
    MlirVal(cl, MFunc(paramTys, retTyOpt))

  /** Emit every registered lambda's synthetic top-level `llvm.func` into
    * [[out]]. Must run before `@main` and before any user def that
    * constructs a closure — both reference the lambda by name. The
    * lambda body re-enters the visitor (`emitExpr`) with the local
    * env state cleared and `currentLambdaCaptures` installed so
    * captured-var reads route through the env.
    */
  protected def emitLambdaFunctions(): Unit =
    val it = lambdaTable.values().iterator()
    val infos = mutable.ArrayBuffer.empty[LambdaInfo]
    while it.hasNext do infos += it.next()
    infos.sortBy(_.id).foreach(emitOneLambda)

  private def emitOneLambda(info: LambdaInfo): Unit =
    val paramSig =
      ("%env: i64" :: info.params.zipWithIndex.map { case (p, i) =>
        val ty = mlirTypeOf(p.tpe).getOrElse(notYet(s"lambda param type ${p.tpe}"))
        s"%arg$i: ${ty.text}"
      }).mkString(", ")
    val retStr = info.retTyOpt.fold("") { t => s" -> ${t.text}" }

    out.append(s"llvm.func ${info.mlirName}($paramSig)$retStr {\n")

    val savedReg         = nextReg
    val savedEnv         = env.toMap
    val savedVarSlots    = varSlots.toMap
    val savedVarTensors  = varTensors.toMap
    val savedBoxes       = boxedVarBoxes.toMap
    val savedCaptures    = currentLambdaCaptures
    nextReg = 0
    env.clear()
    varSlots.clear()
    varTensors.clear()
    boxedVarBoxes.clear()

    info.params.zipWithIndex.foreach { case (p, i) =>
      val mty = mlirTypeOf(p.tpe).get
      env(p.id) = MlirVal(s"%arg$i", mty)
    }
    currentLambdaCaptures = info.captures.zipWithIndex.map { case ((s, m, mode), i) =>
      s.id -> (i, m, mode)
    }.toMap

    info.retTyOpt match
      case Some(retTy) =>
        val v = emitExpr(info.body)
        val r = coerceToType(v, retTy)
        out.append(s"  llvm.return $r : ${retTy.text}\n")
      case None =>
        emitForBody(info.body)
        out.append("  llvm.return\n")

    out.append("}\n")

    nextReg = savedReg
    env.clear(); savedEnv.foreach { case (k, v) => env(k) = v }
    varSlots.clear(); savedVarSlots.foreach { case (k, v) => varSlots(k) = v }
    varTensors.clear(); savedVarTensors.foreach { case (k, v) => varTensors(k) = v }
    boxedVarBoxes.clear(); savedBoxes.foreach { case (k, v) => boxedVarBoxes(k) = v }
    currentLambdaCaptures = savedCaptures

  /** Build the closure value for a TLambda at its construction site.
    * Allocates the env, stores each capture (ByVal = direct value,
    * ByRef = box pointer), takes the synthetic function's address via
    * `llvm.mlir.addressof` + `llvm.ptrtoint`, and hands both to
    * `nex_closure_make`. Returns the descriptor pointer as `i64`.
    */
  protected def emitLambdaConstruct(lam: TLambda): MlirVal =
    val info = Option(lambdaTable.get(lam)).getOrElse(notYet("lambda not registered (codegen bug)"))

    val nSlots  = info.captures.size
    val envReg  = fresh("lenv")
    val envSize = fresh("lenvsz")
    out.append(s"  $envSize = arith.constant ${math.max(nSlots, 1) * 8} : i64\n")
    out.append(s"  $envReg = func.call @nex_env_alloc($envSize) : (i64) -> i64\n")

    info.captures.zipWithIndex.foreach { case ((s, _, mode), i) =>
      val offReg = fresh("loff")
      out.append(s"  $offReg = arith.constant ${i * 8} : i64\n")
      val srcReg = mode match
        case CaptureMode.ByRef =>
          boxedVarBoxes.getOrElse(
            s.id,
            currentLambdaCaptures.get(s.id) match
              case Some((idx, _, CaptureMode.ByRef)) =>
                val pslot = fresh("psl")
                out.append(s"  $pslot = arith.constant ${idx * 8} : i64\n")
                val pp = fresh("pp")
                out.append(s"  $pp = func.call @nex_env_load_i64(%env, $pslot) : (i64, i64) -> i64\n")
                pp
              case _ => notYet(s"ByRef capture of unboxed `${s.name}`"),
          )
        case CaptureMode.ByVal =>
          readCaptureValueAtConstruct(s)
      out.append(s"  func.call @nex_env_store_i64($envReg, $offReg, $srcReg) : (i64, i64, i64) -> ()\n")
    }

    val fnPtr = fresh("fnp")
    out.append(s"  $fnPtr = llvm.mlir.addressof ${info.mlirName} : !llvm.ptr\n")
    val fnInt = fresh("fni")
    out.append(s"  $fnInt = llvm.ptrtoint $fnPtr : !llvm.ptr to i64\n")
    val cl = fresh("cl")
    out.append(s"  $cl = func.call @nex_closure_make($fnInt, $envReg) : (i64, i64) -> i64\n")
    val paramMTys = info.params.map(p => mlirTypeOf(p.tpe).getOrElse(notYet(s"lambda param type ${p.tpe}")))
    MlirVal(cl, MFunc(paramMTys, info.retTyOpt))

  /** Read a capturable symbol's current value at the closure
    * construction site. Used for ByVal captures only — these stash
    * the source's snapshot value into the env. ByRef captures need
    * the box pointer instead and go through a separate path.
    */
  private def readCaptureValueAtConstruct(s: Symbol): String =
    if currentLambdaCaptures.contains(s.id) then
      val (idx, m, mode) = currentLambdaCaptures(s.id)
      mode match
        case CaptureMode.ByVal =>
          val off = fresh("rcoff")
          out.append(s"  $off = arith.constant ${idx * 8} : i64\n")
          val v = fresh("rcv")
          out.append(s"  $v = func.call @nex_env_load_i64(%env, $off) : (i64, i64) -> i64\n")
          v
        case CaptureMode.ByRef =>
          val off = fresh("rcoff")
          out.append(s"  $off = arith.constant ${idx * 8} : i64\n")
          val p = fresh("rcp")
          out.append(s"  $p = func.call @nex_env_load_i64(%env, $off) : (i64, i64) -> i64\n")
          val v = fresh("rcv")
          val z = fresh("rcz")
          out.append(s"  $z = arith.constant 0 : i64\n")
          out.append(s"  $v = func.call @nex_env_load_i64($p, $z) : (i64, i64) -> i64\n")
          v
    else env.get(s.id) match
      case Some(MlirVal(reg, _: MScalar)) => reg
      case Some(MlirVal(reg, MString))    => reg
      case Some(MlirVal(reg, _: MFunc))   => reg
      case _ => varSlots.get(s.id) match
        case Some(_) => emitVarLoad(s.id).reg
        case None    => notYet(s"ByVal capture of unbound `${s.name}`")

  /** Read a captured ByRef var's value through env+box (inside a lambda
    * body) or directly through the box (inside the parent scope). The
    * returned MlirVal carries the captured type — TyInteger for the
    * first slice.
    */
  protected def emitCapturedRead(symId: Int, name: String): MlirVal =
    currentLambdaCaptures.get(symId) match
      case Some((idx, m, mode)) =>
        val off = fresh("coff")
        out.append(s"  $off = arith.constant ${idx * 8} : i64\n")
        mode match
          case CaptureMode.ByRef =>
            val box = fresh("cbox")
            out.append(s"  $box = func.call @nex_env_load_i64(%env, $off) : (i64, i64) -> i64\n")
            val z = fresh("cz")
            out.append(s"  $z = arith.constant 0 : i64\n")
            val v = fresh("cv")
            out.append(s"  $v = func.call @nex_env_load_i64($box, $z) : (i64, i64) -> i64\n")
            tagAsType(v, m)
          case CaptureMode.ByVal =>
            val v = fresh("cv")
            out.append(s"  $v = func.call @nex_env_load_i64(%env, $off) : (i64, i64) -> i64\n")
            tagAsType(v, m)
      case None => boxedVarBoxes.get(symId) match
        case Some(box) =>
          val z = fresh("bz")
          out.append(s"  $z = arith.constant 0 : i64\n")
          val v = fresh("bv")
          out.append(s"  $v = func.call @nex_env_load_i64($box, $z) : (i64, i64) -> i64\n")
          MlirVal(v, MScalar(TyInteger))
        case None => notYet(s"captured read of unbound `$name`")

  /** Write to a captured ByRef var's underlying box. Mirrors
    * [[emitCapturedRead]] but writes — used by [[TAssign]] arms.
    */
  protected def emitCapturedWrite(symId: Int, value: MlirVal): Unit =
    val v8 = narrowToI64(value)
    currentLambdaCaptures.get(symId) match
      case Some((idx, _, CaptureMode.ByRef)) =>
        val off = fresh("woff")
        out.append(s"  $off = arith.constant ${idx * 8} : i64\n")
        val box = fresh("wbox")
        out.append(s"  $box = func.call @nex_env_load_i64(%env, $off) : (i64, i64) -> i64\n")
        val z = fresh("wz")
        out.append(s"  $z = arith.constant 0 : i64\n")
        out.append(s"  func.call @nex_env_store_i64($box, $z, $v8) : (i64, i64, i64) -> ()\n")
      case Some(_) => notYet(s"write to ByVal captured `$symId`")
      case None => boxedVarBoxes.get(symId) match
        case Some(box) =>
          val z = fresh("bwz")
          out.append(s"  $z = arith.constant 0 : i64\n")
          out.append(s"  func.call @nex_env_store_i64($box, $z, $v8) : (i64, i64, i64) -> ()\n")
        case None => notYet(s"captured write to unbound symbol $symId")

  /** Tag an i64 reg with its captured MlirType. For the first slice
    * all closure slots are i64-shaped so this is mostly an identity
    * tagging; bool widens (env stores i64 but TyBool is i1 at the
    * SSA level) so we add a `trunci` step on read.
    */
  private def tagAsType(reg: String, m: MlirType): MlirVal = m match
    case MScalar(TyBool) =>
      val r = fresh("ctrunc")
      out.append(s"  $r = arith.trunci $reg : i64 to i1\n")
      MlirVal(r, MScalar(TyBool))
    case _ => MlirVal(reg, m)

  /** Widen the SSA value to i64 for closure-slot storage. TyBool's i1
    * extends via `arith.extui`; integer/string/MFunc are already i64.
    */
  private def narrowToI64(v: MlirVal): String = v.ty match
    case MScalar(TyBool) =>
      val r = fresh("ext64")
      out.append(s"  $r = arith.extui ${v.reg} : i1 to i64\n")
      r
    case _ => v.reg

  /** Allocate a heap box for a boxed `var`. Registers the box in
    * [[boxedVarBoxes]]. Caller is responsible for writing the initial
    * value via [[emitBoxedVarStore]].
    */
  protected def emitBoxedVarAlloc(symId: Int): String =
    val sz = fresh("bsz")
    out.append(s"  $sz = arith.constant 8 : i64\n")
    val box = fresh("box")
    out.append(s"  $box = func.call @nex_env_alloc($sz) : (i64) -> i64\n")
    boxedVarBoxes(symId) = box
    box

  /** Store an i64-shaped value into a boxed `var`'s box at offset 0. */
  protected def emitBoxedVarStore(symId: Int, v: MlirVal): Unit =
    val box = boxedVarBoxes.getOrElse(symId, notYet(s"store to unboxed boxed-var symbol $symId"))
    val v8  = narrowToI64(v)
    val z   = fresh("bsz")
    out.append(s"  $z = arith.constant 0 : i64\n")
    out.append(s"  func.call @nex_env_store_i64($box, $z, $v8) : (i64, i64, i64) -> ()\n")

  /** Dispatch a call whose callee is closure-typed. Mirrors the LLVM
    * backend's `emitClosureCall` — extracts fn / env, casts the fn
    * back to a pointer, issues an `llvm.call` indirectly.
    */
  protected def emitIndirectCall(callee: TExpr, args: List[TExpr], retTyOpt: Option[MlirType]): MlirVal =
    val cv = emitExpr(callee)
    cv.ty match
      case MFunc(_, _) => ()
      case other => notYet(s"indirect call on $other")
    val fn = fresh("ifn")
    out.append(s"  $fn = func.call @nex_closure_fn(${cv.reg}) : (i64) -> i64\n")
    val ev = fresh("ienv")
    out.append(s"  $ev = func.call @nex_closure_env(${cv.reg}) : (i64) -> i64\n")
    val argVals = args.map(emitExpr)
    val fnp = fresh("ifnp")
    out.append(s"  $fnp = llvm.inttoptr $fn : i64 to !llvm.ptr\n")

    val argRegs = (ev :: argVals.map(_.reg)).mkString(", ")
    val argTys  = ("i64" :: argVals.map(_.ty.text)).mkString(", ")
    retTyOpt match
      case Some(retTy) =>
        val r = fresh("ires")
        out.append(s"  $r = llvm.call $fnp($argRegs) : !llvm.ptr, ($argTys) -> ${retTy.text}\n")
        MlirVal(r, retTy)
      case None =>
        out.append(s"  llvm.call $fnp($argRegs) : !llvm.ptr, ($argTys) -> ()\n")
        MlirVal("%unused", MScalar(TyInteger))
