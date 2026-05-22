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
class NexMLIRCodegen extends NexMLIRStrings, NexMLIRArrays, NexMLIRHOFs, NexMLIRScalarControl, NexMLIRLambdas:

  /** MLIR type of an emitted value. Either an LLVM-like scalar or a
    * static-shape tensor. We don't model rank-2 yet — every tensor in
    * milestone 3 is rank-1 with a known length.
    */
  protected sealed trait MlirType:
    def text: String

  protected case class MScalar(elem: Type) extends MlirType:
    def text: String = scalarText(elem)

  /** A `tensor<...x<elem>>`. `shape == Nil` denotes a 0-d
    * `tensor<elem>` (the shape of `linalg.reduce`'s output).
    * A `-1` in `shape` denotes a dynamic dimension (prints as `?` in
    * the MLIR type spelling). Each dynamic dim's runtime size is
    * carried as an SSA `index` value by whatever op originally
    * produced the tensor — downstream code recovers it lazily via
    * `tensor.dim`.
    */
  protected case class MTensor(elem: Type, shape: List[Int]) extends MlirType:
    def text: String =
      val dims =
        if shape.isEmpty then ""
        else shape.map(d => if d < 0 then "?" else d.toString).mkString("", "x", "x")
      s"tensor<$dims${scalarText(elem)}>"
    def isDynamic: Boolean = shape.exists(_ < 0)

  /** A Nex `string` value at the MLIR level: opaque `i64` carrying the
    * pointer to a C-side `nex_str` descriptor (`{ refcount, length,
    * data }`). All string operations route through the C runtime in
    * `mlir_runtime.c`; this codegen never derefs the descriptor
    * directly, so `i64` is enough.
    */
  protected case object MString extends MlirType:
    def text: String = "i64"

  /** A first-class function value at the MLIR level. Carries an opaque
    * `i64` pointer to a heap-allocated closure descriptor (`nex_closure`
    * — see `mlir_runtime.c`) holding the function pointer and env. The
    * structural shape (param/return types) is tracked at the codegen
    * level for indirect-call signature inference; the MLIR type itself
    * is always `i64`.
    */
  protected case class MFunc(paramTys: List[MlirType], retTyOpt: Option[MlirType]) extends MlirType:
    def text: String = "i64"

  /** A by-reference parameter slot. At the MLIR level this is a
    * `memref<T>` carrying the caller's scalar var slot — the callee
    * loads through it for reads and stores through it for writes, so
    * mutations flow back to the caller's binding. Only scalar element
    * types are supported (the tensor `mut [T]` case needs a separate
    * memref-backed var-tensor design).
    */
  protected case class MMemref(scalar: MScalar) extends MlirType:
    def text: String = s"memref<${scalar.text}>"

  /** A nominal struct value. Lowered to `!llvm.struct<(t0, t1, ...)>`
    * from the LLVM dialect — the pass pipeline runs
    * `convert-func-to-llvm` so LLVM-dialect types flow through
    * func-boundary signatures unchanged. Construction emits
    * `llvm.mlir.undef` + a chain of `llvm.insertvalue`; field access
    * emits `llvm.extractvalue`. Field types are themselves `MlirType`
    * so nested structs work as long as each inner field is a type
    * the codegen can express.
    */
  protected case class MStruct(name: String, fields: List[(String, MlirType)]) extends MlirType:
    def text: String =
      val fieldText = fields.map(_._2.text).mkString(", ")
      s"!llvm.struct<($fieldText)>"

  /** A tuple value. Same `!llvm.struct<(...)>` lowering as [[MStruct]]
    * but anonymous (no field names) and slots are addressed by
    * position. Construction via `llvm.mlir.undef` + `llvm.insertvalue`
    * chain; projection via `llvm.extractvalue`.
    */
  protected case class MTuple(elems: List[MlirType]) extends MlirType:
    def text: String =
      val elemText = elems.map(_.text).mkString(", ")
      s"!llvm.struct<($elemText)>"

  /** A complex value `{ real, imag }` lowered to
    * `!llvm.struct<(f64, f64)>`. Construction packs an `re`/`im` pair
    * via `llvm.mlir.undef` + two `llvm.insertvalue`; arithmetic runs
    * per-component (see [[emitComplexArith]]); `.re` / `.im` field
    * reads emit `llvm.extractvalue`. Integer / real operands promote
    * to a complex pair `{ x, 0.0 }` at the boundary, matching the
    * LLVM backend's `toComplex` lift.
    */
  protected case object MComplex extends MlirType:
    def text: String = "!llvm.struct<(f64, f64)>"

  protected case class MlirVal(reg: String, ty: MlirType)

  protected val out             = new StringBuilder
  /** Module-level declarations (memref.global byte arrays for string
    * literals, etc.) flushed before [[out]] when [[compile]] returns
    * the final module text. Kept in a separate buffer so emitters deep
    * in the visitor can lazily contribute module-scope ops.
    */
  protected val globalDecls     = new StringBuilder
  /** Deduplicates string literal globals. Keys are the literal text;
    * values are the `@nex_strlit_N` symbol name. The byte-array global
    * has been emitted into [[globalDecls]] when the entry is first
    * created.
    */
  protected val stringLitPool   = mutable.Map.empty[String, String]
  protected var nextReg         = 0
  protected val env             = mutable.Map.empty[Int, MlirVal]
  /** `var` scalar bindings live in stack memrefs so that
    * load-modify-store patterns and loop-mutated counters compile to
    * the obvious code. Keyed by symbol id; the value carries the
    * memref SSA name and the element type. Registered when a
    * `var x = init` binding is emitted; consulted by both `TVarRef`
    * (which loads) and `TAssign` (which stores).
    */
  protected val varSlots        = mutable.Map.empty[Int, (String, MScalar)]
  /** `var` tensor bindings live as SSA-rebinding entries: each
    * mutation produces a new tensor value (via `tensor.insert`,
    * `tensor.insert_slice`, etc.) and re-binds the entry to the new
    * SSA register. Reads return the current register. Keyed by symbol
    * id. This works because Nex's array values are by-value at the
    * source level and we are not yet handling `mut by-ref` arg passing
    * to MLIR-emitted user defs.
    */
  protected val varTensors      = mutable.Map.empty[Int, (String, MTensor)]
  /** `var` struct bindings live as SSA-rebinding entries: each
    * mutation produces a new struct value (via `llvm.insertvalue`)
    * and re-binds the entry to the new SSA register. Reads return
    * the current register. Keyed by symbol id. Same pattern as
    * [[varTensors]] — Nex's struct values are by-value at the source
    * level, so this matches the source semantics naturally.
    */
  protected val varStructs      = mutable.Map.empty[Int, (String, MStruct)]
  /** `var` tuple bindings. SSA-rebinding entries, exactly mirroring
    * [[varStructs]] — `llvm.insertvalue` produces a new SSA value at
    * each mutation and we re-bind the entry. Keyed by symbol id.
    */
  protected val varTuples       = mutable.Map.empty[Int, (String, MTuple)]
  /** Per-program registry of `@intrinsic("libm.X")` function symbols.
    * Populated at the start of [[compile]] by scanning every
    * [[TFunDecl]] whose body is a [[TIntrinsic]]. At a [[TCall]] site
    * the codegen looks the callee's symbol id up here; on a hit the
    * call lowers to a direct `func.call @X(...)` instead of routing
    * through the generic call path (which still rejects everything
    * else as `notYet`).
    */
  protected val libmIntrinsics  = mutable.Map.empty[Int, String]

  /** Per-program registry of user-defined `def`s lowered to
    * `func.func`. Keyed by the declaration symbol id; value carries
    * the mangled MLIR symbol name, the param types, and the return
    * type (`None` for unit-returning defs — not yet supported in this
    * phase). A non-`main` `TFunDecl` is registered here if and only
    * if every param type and the return type are emittable
    * ([[mlirTypeOf]] returns `Some`). Calls whose callee is in this
    * map lower to `func.call @<name>` ; otherwise the existing
    * notYet path stays in place.
    */
  protected val userDefs        = mutable.Map.empty[Int, (String, List[MlirType], Option[MlirType])]

  /** Top-level `const` / `val` bindings whose initialiser is a literal
    * scalar (real / int / bool). Stored as the original `TExpr`. At
    * any `TVarRef(sym)` site whose `sym.id` is registered here, the
    * codegen re-emits the literal expression — that gives user-def
    * bodies access to prelude constants like `pi`/`e` (which live in
    * `auxDecls`) and to user top-level constants without needing
    * module-scope globals. Non-literal initialisers (e.g.
    * `val s = square(7)`) are left for env-based lookup inside the
    * main function and notYet inside user-def regions.
    */
  protected val topLevelLiteralInits = mutable.Map.empty[Int, TExpr]

  def compile(tp: TProgram): String =
    val mainDecl = tp.decls.collectFirst {
      case f: TFunDecl if f.sym.name == "main" && f.params.isEmpty => f
    }.getOrElse(notYet("program has no `def main()`"))

    libmIntrinsics.clear()
    stringLitPool.clear()
    globalDecls.clear()
    userDefs.clear()
    topLevelLiteralInits.clear()
    lambdaTable.clear()
    varBindings.clear()
    boxedVarSet.clear()
    boxedVarBoxes.clear()
    defThunks.clear()
    varSlots.clear()
    varTensors.clear()
    varStructs.clear()
    varTuples.clear()
    currentLambdaCaptures = Map.empty
    tp.allDecls.foreach {
      case TTopBinding(sym, BindingKind.Val | BindingKind.Const, value, _)
          if isLiteralScalarExpr(value) =>
        topLevelLiteralInits(sym.id) = value
      case _ => ()
    }
    tp.allDecls.foreach {
      case f: TFunDecl =>
        f.body match
          case TIntrinsic(opId, _, _) if opId.startsWith("libm.") =>
            libmIntrinsics(f.sym.id) = opId.stripPrefix("libm.")
          case _ => ()
      case _ => ()
    }
    // Registry of user-defined non-main, non-intrinsic `def`s whose
    // signatures the MLIR backend can express. Filtering here means
    // the dispatch site in `emitExpr` can stay narrow and any
    // call to an un-emittable def naturally falls through to notYet.
    tp.allDecls.foreach {
      case f: TFunDecl
          if f.sym.name != "main" && !libmIntrinsics.contains(f.sym.id) =>
        // Read the per-param mode list off the function's TyFunc. Mut
        // scalars lower to `memref<T>` ref slots so callee writes flow
        // back to the caller's var. Mut tensors are passed by-value
        // (`tensor<...>`) and rely on the elaborator's auto-clone pass
        // (NexLifetime) to wrap any caller-aliased var in TClone before
        // the call — that's enough to match the observable output for
        // every mut-tensor corpus case where the caller doesn't share
        // storage with the val source. Closures/strings as mut stay
        // un-registered.
        val modes: List[ParamMode] = f.sym.tpe match
          case TyFunc(ps, _) if ps.size == f.params.size => ps.map(_._2)
          case _ => List.fill(f.params.size)(ParamMode.Read)
        val paramTys = f.params.zip(modes).map { case (p, mode) =>
          (mlirTypeOf(p.tpe), mode) match
            case (Some(s: MScalar), ParamMode.Mut) => Some(MMemref(s))
            case (Some(t: MTensor), ParamMode.Mut) => Some(t)
            case (other, ParamMode.Read)           => other
            case _                                  => None
        }
        // Outer Option tracks "supported at all", inner Option tracks
        // "value-returning vs unit-returning". TyUnit registers with
        // an inner None.
        val retSlot: Option[Option[MlirType]] = f.returnType match
          case TyUnit => Some(None)
          case other  => mlirTypeOf(other).map(Some(_))
        if paramTys.forall(_.isDefined) && retSlot.isDefined then
          userDefs(f.sym.id) = (mangleUserDef(f.sym), paramTys.map(_.get), retSlot.get)
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
    out.append("func.func private @nex_trap_slice_oob()\n")
    out.append("func.func private @nex_trap_axis_oob()\n")
    out.append("func.func private @nex_trap_complex_div_zero()\n")
    out.append("func.func private @nex_str_lit_from_cstr(i64, i64) -> i64\n")
    out.append("func.func private @nex_str_inc(i64)\n")
    out.append("func.func private @nex_str_dec(i64)\n")
    out.append("func.func private @nex_print_str(i64)\n")
    out.append("func.func private @nex_str_concat(i64, i64) -> i64\n")
    out.append("func.func private @nex_str_eq(i64, i64) -> i8\n")
    out.append("func.func private @nex_str_from_i64(i64) -> i64\n")
    out.append("func.func private @nex_str_from_bool(i8) -> i64\n")
    out.append("func.func private @nex_str_from_f64(f64) -> i64\n")
    out.append("func.func private @nex_str_format_i64(i64, i64, i64) -> i64\n")
    out.append("func.func private @nex_str_format_f64(i64, i64, f64) -> i64\n")
    out.append("func.func private @nex_str_format_bin(i64, i64, i8, i8) -> i64\n")
    out.append("func.func private @nex_env_alloc(i64) -> i64\n")
    out.append("func.func private @nex_env_load_i64(i64, i64) -> i64\n")
    out.append("func.func private @nex_env_store_i64(i64, i64, i64)\n")
    out.append("func.func private @nex_closure_make(i64, i64) -> i64\n")
    out.append("func.func private @nex_closure_fn(i64) -> i64\n")
    out.append("func.func private @nex_closure_env(i64) -> i64\n")
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
    // Closure pre-passes: record every `var` binding, then walk the
    // program a second time to register each TLambda + its capture
    // layout (which also marks the captured vars as needing heap
    // boxes via `boxedVarSet`). Both run before any code is emitted
    // so user-def and main bodies see consistent state.
    collectVarBindings(tp)
    collectLambdas(tp)
    // Third closure pre-pass: spot every top-level user def reached
    // as a function-value (passed as an HOF arg, bound to a `val`,
    // returned, etc) so [[emitDefThunkFunctions]] can write a
    // synthetic `llvm.func @nex_def_thunk_<id>` wrapper for each.
    // Must run after [[userDefs]] is populated.
    collectDefThunks(tp)
    // Emit user-defined `def`s as `func.func` ops at module level
    // BEFORE `@main`, so they're visible to call sites inside main
    // (and to each other for mutual recursion). MLIR module ops are
    // order-independent, but emitting in declaration order helps when
    // skimming the lowered text.
    tp.allDecls.foreach {
      case f: TFunDecl if userDefs.contains(f.sym.id) =>
        emitUserDef(f)
      case _ => ()
    }
    // Synthetic `llvm.func` for each registered lambda. Emitted
    // before `@main` so its address (taken via `llvm.mlir.addressof`)
    // resolves at closure construction sites.
    emitLambdaFunctions()
    // Synthetic `llvm.func @nex_def_thunk_<id>` per top-level def
    // referenced as a function-value. The thunk ignores its env_ptr
    // and forwards to the user's `func.call @nex_user_<name>_<id>`.
    emitDefThunkFunctions()
    out.append("func.func @main() -> i32 {\n")
    nextReg = 0
    env.clear()
    val c0 = fresh("c0")
    out.append(s"  $c0 = arith.constant 0 : i32\n")
    emitTopBindings(tp)
    emitMainBody(mainDecl.body)
    out.append(s"  func.return $c0 : i32\n")
    out.append("}\n")
    // Module-level globals (string literal byte arrays, etc.) come
    // first; MLIR's module is order-independent but the conventional
    // ordering puts globals before functions.
    globalDecls.toString + out.toString

  /** Materialise every top-level `val` and `const` binding into the
    * env, in declaration order, by reusing the expression visitor.
    * Iterates `tp.allDecls` so that source-prelude consts (`pi`,
    * `e`) live in env alongside user-declared top-level vals.
    * Emitted inline at the top of `@main` — top-level bindings
    * semantically execute once at program start, and `@main` is the
    * only function-scope region the backend emits today, so this is
    * the natural place. `var` top-level bindings remain `notYet`.
    */
  protected def emitTopBindings(tp: TProgram): Unit =
    tp.allDecls.foreach {
      case TTopBinding(sym, BindingKind.Val | BindingKind.Const, value, _) =>
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
  protected def emitMainBody(body: TExpr): Unit = unwrapEmptyBlock(body) match
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

  protected def unwrapEmptyBlock(e: TExpr): TExpr = e match
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
  protected def emitPrintCall(arg: TExpr): Unit =
    val v = emitExpr(arg)
    v.ty match
      case MScalar(TyInteger) =>
        out.append(s"  func.call @nex_print_i64(${v.reg}) : (i64) -> ()\n")
      case MScalar(TyReal) =>
        out.append(s"  func.call @nex_print_f64(${v.reg}) : (f64) -> ()\n")
      case MScalar(TyBool) =>
        out.append(s"  func.call @nex_print_bool(${v.reg}) : (i1) -> ()\n")
      case MString =>
        out.append(s"  func.call @nex_print_str(${v.reg}) : (i64) -> ()\n")
        out.append(s"  func.call @nex_str_dec(${v.reg}) : (i64) -> ()\n")
      case t @ MTensor(elemT, List(_)) =>
        val lenReg = tensorDimAsI64(v.reg, t, 0)
        val ptrReg = emitTensorPointer(v.reg, t)
        val helper = arrayPrintHelper(elemT, rank = 1)
        out.append(s"  func.call @$helper($ptrReg, $lenReg) : (i64, i64) -> ()\n")
      case t @ MTensor(elemT, List(_, _)) =>
        val rowsReg = tensorDimAsI64(v.reg, t, 0)
        val colsReg = tensorDimAsI64(v.reg, t, 1)
        val ptrReg  = emitTensorPointer(v.reg, t)
        val helper = arrayPrintHelper(elemT, rank = 2)
        out.append(s"  func.call @$helper($ptrReg, $rowsReg, $colsReg) : (i64, i64, i64) -> ()\n")
      case mt: MTuple =>
        val descR = emitTupleToString(v.reg, mt)
        out.append(s"  func.call @nex_print_str($descR) : (i64) -> ()\n")
        out.append(s"  func.call @nex_str_dec($descR) : (i64) -> ()\n")
      case MComplex =>
        val descR = emitComplexToString(v.reg)
        out.append(s"  func.call @nex_print_str($descR) : (i64) -> ()\n")
        out.append(s"  func.call @nex_str_dec($descR) : (i64) -> ()\n")
      case other =>
        notYet(s"print of $other")

  /** Walk a `TField` chain to its innermost TVarRef. Returns the var
    * symbol plus the field names from outer to inner. For
    * `o.i.v` (parsed left-to-right) the AST is
    * `TField(TField(TVarRef(o), "i"), "v")` — outermost is the
    * leaf field, so we accumulate by prepending and reverse at the
    * end. Returns None if the chain doesn't bottom out at a var-
    * bound struct symbol.
    */
  protected def collectFieldPath(target: TField): (Symbol, List[String]) =
    def loop(e: TExpr, acc: List[String]): (Symbol, List[String]) = e match
      case TField(inner, name, _, _) => loop(inner, name :: acc)
      case TVarRef(sym, _, _)        => (sym, acc)
      case other                     => notYet(s"non-var receiver in field write: ${other.getClass.getSimpleName}")
    loop(target, Nil)

  /** Some(rootVarSym) when `target` chains through TFields down to a
    * TVarRef whose symbol is in `varStructs`. Otherwise None.
    */
  protected def rootVarStructSym(target: TField): Option[Symbol] =
    def loop(e: TExpr): Option[Symbol] = e match
      case TField(inner, _, _, _) => loop(inner)
      case TVarRef(sym, _, _) if varStructs.contains(sym.id) => Some(sym)
      case _                       => None
    loop(target)

  /** Build the new outer-struct SSA value after assigning `rhs` to
    * the field path. Recursive: at each level, extract the current
    * field, dive into the rest of the path, then `insertvalue` the
    * updated sub-struct back. The leaf insertion promotes
    * `int → real` if the field type demands it (matches
    * [[emitStructConstruct]]).
    */
  protected def writeFieldPath(
      curReg: String,
      curMs:  MStruct,
      path:   List[String],
      rhs:    MlirVal,
  ): String =
    path match
      case Nil =>
        notYet("empty field path in struct field write")
      case List(leaf) =>
        val idx = curMs.fields.indexWhere(_._1 == leaf)
        if idx < 0 then notYet(s"field `$leaf` not found on struct ${curMs.name}")
        val fieldTy = curMs.fields(idx)._2
        val rhsReg = (rhs.ty, fieldTy) match
          case (lt, rt) if lt == rt                  => rhs.reg
          case (MScalar(TyInteger), MScalar(TyReal)) => promoteIntToReal(rhs).reg
          case (lt, rt) =>
            notYet(s"struct field write: $lt -> $rt mismatch")
        val next = fresh(s"st_${curMs.name}")
        out.append(s"  $next = llvm.insertvalue $rhsReg, $curReg[$idx] : ${curMs.text}\n")
        next
      case head :: rest =>
        val idx = curMs.fields.indexWhere(_._1 == head)
        if idx < 0 then notYet(s"field `$head` not found on struct ${curMs.name}")
        val fieldTy = curMs.fields(idx)._2 match
          case s: MStruct => s
          case other      => notYet(s"nested field write through non-struct field type $other")
        val innerReg = fresh(s"f_${head}")
        out.append(s"  $innerReg = llvm.extractvalue $curReg[$idx] : ${curMs.text}\n")
        val newInner = writeFieldPath(innerReg, fieldTy, rest, rhs)
        val next = fresh(s"st_${curMs.name}")
        out.append(s"  $next = llvm.insertvalue $newInner, $curReg[$idx] : ${curMs.text}\n")
        next

  /** Lower a struct construction call. Builds an
    * `llvm.mlir.undef : !llvm.struct<...>` and folds each arg into
    * the corresponding slot with `llvm.insertvalue`. Args are emitted
    * in source order; each one's MLIR value type must match the
    * field's declared MLIR type. Promotes `int → real` at the slot
    * boundary so `Point(3, 4)` with declared `x: real, y: real` works
    * without surface coercion.
    */
  protected def emitStructConstruct(ms: MStruct, args: List[TExpr]): MlirVal =
    if args.size != ms.fields.size then
      notYet(s"struct ${ms.name}: arity mismatch (${args.size} args vs ${ms.fields.size} fields)")
    val argVals = args.zip(ms.fields).map { case (a, (_, fieldTy)) =>
      val av = emitExpr(a)
      (av.ty, fieldTy) match
        case (lt, rt) if lt == rt                  => av
        case (MScalar(TyInteger), MScalar(TyReal)) => promoteIntToReal(av)
        case (lt, rt) =>
          notYet(s"struct ${ms.name}: arg type $lt does not match field type $rt")
    }
    val undef = fresh(s"st_${ms.name}_undef")
    out.append(s"  $undef = llvm.mlir.undef : ${ms.text}\n")
    var acc = undef
    for ((v, idx) <- argVals.zipWithIndex) do
      val next = fresh(s"st_${ms.name}")
      out.append(s"  $next = llvm.insertvalue ${v.reg}, $acc[$idx] : ${ms.text}\n")
      acc = next
    MlirVal(acc, ms)

  /** Lower a tuple literal `(a, b, ...)`. Same `llvm.mlir.undef` +
    * `llvm.insertvalue` chain as a struct constructor, but slots are
    * positional and the element types come from each arg's inferred
    * type (collected into [[MTuple]] up front so the LLVM-struct text
    * is consistent across all the insertvalue ops). Element-position
    * `int → real` promotion is applied when the [[MTuple]] slot type
    * is `MScalar(TyReal)` and the arg evaluates to `MScalar(TyInteger)`.
    */
  protected def emitTupleConstruct(mt: MTuple, args: List[TExpr]): MlirVal =
    if args.size != mt.elems.size then
      notYet(s"tuple: arity mismatch (${args.size} args vs ${mt.elems.size} slots)")
    val argVals = args.zip(mt.elems).map { case (a, slotTy) =>
      val av = emitExpr(a)
      (av.ty, slotTy) match
        case (lt, rt) if lt == rt                  => av
        case (MScalar(TyInteger), MScalar(TyReal)) => promoteIntToReal(av)
        case (lt, rt) =>
          notYet(s"tuple: arg type $lt does not match slot type $rt")
    }
    val undef = fresh("tup_undef")
    out.append(s"  $undef = llvm.mlir.undef : ${mt.text}\n")
    var acc = undef
    for ((v, idx) <- argVals.zipWithIndex) do
      val next = fresh("tup")
      out.append(s"  $next = llvm.insertvalue ${v.reg}, $acc[$idx] : ${mt.text}\n")
      acc = next
    MlirVal(acc, mt)

  /** The expression visitor. Every node that lowers must produce a
    * single SSA value with a known MLIR type; nodes that don't fit
    * (assignments, side effects, function calls other than the
    * recognised prelude ones) reject via `notYet`.
    */
  protected def emitExpr(e: TExpr): MlirVal = e match
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

    case TStringLit(s, _, _) =>
      MlirVal(emitStringLiteral(s), MString)

    case TInterpStringLit(parts, _, _) =>
      emitInterpString(parts)

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
            case MComplex =>
              val (re, im) = emitUnpackComplex(v.reg)
              val nre = fresh("neg_re")
              val nim = fresh("neg_im")
              out.append(s"  $nre = arith.negf $re : f64\n")
              out.append(s"  $nim = arith.negf $im : f64\n")
              emitPackComplex(nre, nim)
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
          case _: MFunc if elemT.isInstanceOf[TyFunc] => ()
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

    case TVarRef(s, _, _) if s.kind == SymKind.Prelude && s.name == "i" =>
      val zero = fresh("zero")
      val one  = fresh("one")
      out.append(s"  $zero = arith.constant 0.000000e+00 : f64\n")
      out.append(s"  $one = arith.constant 1.000000e+00 : f64\n")
      emitPackComplex(zero, one)

    case TVarRef(sym, _, _) if currentLambdaCaptures.contains(sym.id) =>
      // Reading a captured value from inside a lambda body — go
      // through the env (and box for ByRef). Takes precedence over
      // varSlots / env lookups because the captured shadow is the
      // only legitimate view of the var from inside the lambda.
      emitCapturedRead(sym.id, sym.name)

    case TVarRef(sym, _, _) if boxedVarBoxes.contains(sym.id) =>
      // Parent-scope read of a boxed var (one that's captured by
      // some lambda). The actual value lives in the heap box.
      emitCapturedRead(sym.id, sym.name)

    case TVarRef(sym, _, _) if varSlots.contains(sym.id) =>
      emitVarLoad(sym.id)

    case TVarRef(sym, _, _) if varTensors.contains(sym.id) =>
      val (reg, ty) = varTensors(sym.id)
      MlirVal(reg, ty)

    case TVarRef(sym, _, _) if varStructs.contains(sym.id) =>
      val (reg, ty) = varStructs(sym.id)
      MlirVal(reg, ty)

    case TVarRef(sym, _, _) if varTuples.contains(sym.id) =>
      val (reg, ty) = varTuples(sym.id)
      MlirVal(reg, ty)

    case TClone(inner, _, _) =>
      // Auto-clone (§8.3) elaborates `var b = a` (with later use of a)
      // into `var b = clone(a)`. In MLIR's SSA semantics this is a
      // no-op: every `tensor.insert` / `tensor.insert_slice` produces
      // a fresh tensor value, so two var bindings pointing at the
      // same SSA register diverge naturally on the first mutation —
      // the un-mutated binding still references the original. No
      // explicit copy needed.
      emitExpr(inner)

    case TVarRef(sym, _, _) if topLevelLiteralInits.contains(sym.id) =>
      // Re-emit the literal initialiser. Safe because we only
      // register literal-scalar bindings here, so each call produces
      // a fresh `arith.constant` op in the current region — which is
      // exactly what user-def bodies (executing outside `@main`)
      // need for prelude constants like `pi` / `e`.
      emitExpr(topLevelLiteralInits(sym.id))

    case TVarRef(sym, _, _) if defThunks.contains(sym.id) =>
      // Top-level def reached as a function-value (HOF arg, bound to
      // a val, etc). Build the closure descriptor on the spot via
      // `nex_closure_make(thunk_addr, 0)`. Multiple references to
      // the same def share one thunk (memoized by symbol id) but
      // each reference builds its own closure value — the env_ptr
      // is null anyway, so the cost is just one `closure_make` call.
      emitDefThunkClosure(sym.id, sym.name)

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

    case TBinOp("..", loE, hiE, _, ty) if isRangeArrayType(ty) =>
      emitRangeDispatch(loE, hiE, inclusive = false)

    case TBinOp("..=", loE, hiE, _, ty) if isRangeArrayType(ty) =>
      emitRangeDispatch(loE, hiE, inclusive = true)

    case TBinOp("^", lhs, rhs, _, resultTy) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      emitScalarPower(lv, rv, resultTy)

    case TBinOp("and", lhs, rhs, _, _) => emitShortCircuit(lhs, rhs, isAnd = true)
    case TBinOp("or",  lhs, rhs, _, _) => emitShortCircuit(lhs, rhs, isAnd = false)

    case TBinOp("+", lhs, rhs, _, TyString) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty) match
        case (MString, MString) => emitStringConcat(lv.reg, rv.reg)
        case (lt, rt)           => notYet(s"`+` on string with $lt / $rt")

    case TBinOp(op @ ("==" | "!="), lhs, rhs, _, _)
        if lhs.tpe == TyComplex || rhs.tpe == TyComplex =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      val (lre, lim) = liftToComplex(lv)
      val (rre, rim) = liftToComplex(rv)
      val eR = fresh("ce_re")
      val eI = fresh("ce_im")
      out.append(s"  $eR = arith.cmpf oeq, $lre, $rre : f64\n")
      out.append(s"  $eI = arith.cmpf oeq, $lim, $rim : f64\n")
      val eq = fresh("ceq")
      out.append(s"  $eq = arith.andi $eR, $eI : i1\n")
      if op == "==" then MlirVal(eq, MScalar(TyBool))
      else
        val one = fresh("one")
        val neg = fresh("cne")
        out.append(s"  $one = arith.constant 1 : i1\n")
        out.append(s"  $neg = arith.xori $eq, $one : i1\n")
        MlirVal(neg, MScalar(TyBool))

    case TBinOp(op, lhs, rhs, _, _) if isComparisonOp(op) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      (lv.ty, rv.ty) match
        case (MString, MString) => emitStringCompare(op, lv.reg, rv.reg)
        case _                  => emitComparison(op, lv, rv)

    case TBinOp(op, lhs, rhs, _, TyComplex) =>
      val lv = emitExpr(lhs)
      val rv = emitExpr(rhs)
      val (lre, lim) = liftToComplex(lv)
      val (rre, rim) = liftToComplex(rv)
      emitComplexArith(op, lre, lim, rre, rim)

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

    case TCall(TVarRef(s, _, _), List(arg), _, _)
        if s.kind == SymKind.Prelude && s.name == "to_real" =>
      val v = emitExpr(arg)
      v.ty match
        case MScalar(TyReal)    => v
        case MScalar(TyInteger) => promoteIntToReal(v)
        case other              => notYet(s"to_real on $other")

    case TCall(TVarRef(s, _, _), List(arg), _, _)
        if s.kind == SymKind.Prelude && s.name == "to_integer" =>
      val v = emitExpr(arg)
      v.ty match
        case MScalar(TyInteger) => v
        case MScalar(TyReal) =>
          val r = fresh("toi")
          out.append(s"  $r = arith.fptosi ${v.reg} : f64 to i64\n")
          MlirVal(r, MScalar(TyInteger))
        case other =>
          notYet(s"to_integer on $other")

    case TCall(TVarRef(s, _, _), List(arg), _, _)
        if s.kind == SymKind.Prelude && s.name == "to_complex" =>
      val v = emitExpr(arg)
      v.ty match
        case MComplex            => v
        case MScalar(TyReal)     =>
          val z = fresh("c_im0")
          out.append(s"  $z = arith.constant 0.000000e+00 : f64\n")
          emitPackComplex(v.reg, z)
        case MScalar(TyInteger)  =>
          val r = promoteIntToReal(v)
          val z = fresh("c_im0")
          out.append(s"  $z = arith.constant 0.000000e+00 : f64\n")
          emitPackComplex(r.reg, z)
        case other =>
          notYet(s"to_complex on $other")

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

    case TCall(TVarRef(s, _, _), List(arr, fnExpr), _, tpe)
        if s.kind == SymKind.Prelude && s.name == "map"
          && mlirTypeOf(fnExpr.tpe).exists(_.isInstanceOf[MFunc]) =>
      val av = emitExpr(arr)
      val fv = emitExpr(fnExpr)
      val outElem = tpe match
        case TyArray(e, _) => e
        case other         => notYet(s"map returns non-array $other")
      av.ty match
        case t @ MTensor(_, List(_)) => emitMapClosure(av, t, fv, outElem)
        case other                   => notYet(s"closure-arg map over $other")

    case TCall(TVarRef(s, _, _), List(arr, init, lam: TLambda), _, _)
        if s.kind == SymKind.Prelude && s.name == "reduce" && lam.params.size == 2 =>
      val av = emitExpr(arr)
      val iv = emitExpr(init)
      av.ty match
        case t: MTensor if t.shape.nonEmpty => emitReduceInlineLambda(av, t, iv, lam)
        case other                          => notYet(s"reduce over $other")

    case TCall(TVarRef(s, _, _), List(arr, lam: TLambda), _, _)
        if s.kind == SymKind.Prelude && s.name == "filter" && lam.params.size == 1 =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) => emitFilterInlineLambda(av, t, lam)
        case other                   => notYet(s"filter over $other")

    case TCall(TVarRef(s, _, _), List(arr, lam: TLambda), _, tpe)
        if s.kind == SymKind.Prelude && s.name == "flatMap" && lam.params.size == 1 =>
      val av = emitExpr(arr)
      val outElem = tpe match
        case TyArray(e, _) => e
        case other         => notYet(s"flatMap returns non-array $other")
      av.ty match
        case t @ MTensor(_, List(_)) => emitFlatMapInlineLambda(av, t, lam, outElem)
        case other                   => notYet(s"flatMap over $other")

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

    case TCall(TVarRef(s, _, _), List(x), _, _)
        if s.kind == SymKind.Prelude && s.name == "sign" =>
      emitScalarSign(emitExpr(x))

    case TCall(TVarRef(s, _, _), List(loE, hiE), _, _)
        if s.kind == SymKind.Prelude && s.name == "range" =>
      emitRangeDispatch(loE, hiE, inclusive = false)

    case TCall(TVarRef(s, _, _), List(nE), _, _)
        if s.kind == SymKind.Prelude && (s.name == "zeros" || s.name == "ones") =>
      emitConstFillDispatch(s.name, nE)

    case TCall(TVarRef(s, _, _), List(loE, hiE, nE), _, _)
        if s.kind == SymKind.Prelude && s.name == "linspace" =>
      emitLinspaceDispatch(loE, hiE, nE)

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

    case TCall(TVarRef(s, _, _), args, _, tpe) if s.kind == SymKind.TypeName =>
      // Struct constructor: `Point(x, y)` lowers like a tuple literal —
      // `llvm.mlir.undef` of the struct's LLVM type, then a chain of
      // `llvm.insertvalue` placing each arg into its declared slot.
      // Field order is the source-declaration order, fixed by the
      // TyStruct's field list.
      mlirTypeOf(tpe) match
        case Some(ms: MStruct) => emitStructConstruct(ms, args)
        case _                  => notYet(s"unsupported struct constructor result type $tpe")

    case TTuple(elems, _, tpe) =>
      mlirTypeOf(tpe) match
        case Some(mt: MTuple) => emitTupleConstruct(mt, elems)
        case _                  => notYet(s"unsupported tuple literal element types $tpe")

    case TTupleProj(receiver, idx, _, _) =>
      val rv = emitExpr(receiver)
      rv.ty match
        case mt: MTuple =>
          if idx < 0 || idx >= mt.elems.size then
            notYet(s"tuple projection index $idx out of range 0..${mt.elems.size - 1}")
          val elemTy = mt.elems(idx)
          val r = fresh(s"tp_$idx")
          out.append(s"  $r = llvm.extractvalue ${rv.reg}[$idx] : ${mt.text}\n")
          MlirVal(r, elemTy)
        case other =>
          notYet(s"tuple projection on non-tuple type $other")

    case TCall(TVarRef(s, _, _), args, _, _) if userDefs.contains(s.id) =>
      val (name, paramTys, retTyOpt) = userDefs(s.id)
      retTyOpt match
        case Some(retTy) =>
          emitUserDefCall(name, paramTys, retTyOpt, args, s.name)
        case None =>
          // Unit-returning user defs surface only at statement
          // position (see [[emitBlockItem]]); reaching here means
          // someone is trying to use a `def foo() = print(...)` as
          // a value.
          notYet(s"unit-returning user def `${s.name}` reached value position")

    case lam: TLambda if lambdaTable.containsKey(lam) =>
      // Closure construction at the lambda's source location. The
      // synthetic top-level function was emitted in the pre-pass;
      // here we build the env (one i64 slot per capture), populate
      // it from the parent scope, and hand it to `nex_closure_make`
      // alongside the function pointer.
      emitLambdaConstruct(lam)

    case call @ TCall(callee, args, _, _)
        if mlirTypeOf(callee.tpe).exists(_.isInstanceOf[MFunc]) =>
      // Indirect call: callee evaluates to a closure (i64 descriptor
      // address). Extract `(fn_ptr, env_ptr)` and dispatch via
      // `llvm.call`. The callee's `TyFunc` carries the param/return
      // types we need.
      val retTyOpt = callee.tpe match
        case TyFunc(_, TyUnit) => None
        case TyFunc(_, other)  => mlirTypeOf(other)
        case _                 => mlirTypeOf(call.tpe)
      emitIndirectCall(callee, args, retTyOpt)

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
        case t @ MTensor(_, shape) if shape.nonEmpty =>
          val r = tensorDimAsI64(av.reg, t, 0)
          MlirVal(r, MScalar(TyInteger))
        case other =>
          notYet(s"length on $other")

    case TIndex(arr, List(idx), _, _) =>
      val av = emitExpr(arr)
      val iv = emitExpr(idx)
      (av.ty, iv.ty) match
        case (t @ MTensor(et, List(_)), MScalar(TyInteger)) =>
          val len = tensorDimAsIndex(av.reg, t, 0)
          val rawR = fresh("idxraw")
          out.append(s"  $rawR = arith.index_cast ${iv.reg} : i64 to index\n")
          val idxR = wrapNegBound(rawR, len)
          val c0Idx = fresh("c0")
          out.append(s"  $c0Idx = arith.constant 0 : index\n")
          emitAxisIndexTrap(idxR, len, c0Idx)
          val r = fresh("elt")
          out.append(s"  $r = tensor.extract ${av.reg}[$idxR] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (t @ MTensor(et, List(_, cols)), MScalar(TyInteger)) =>
          // `m[i]` on a rank-2 receiver: return the i-th row as a fresh
          // rank-1 tensor. The row axis collapses; the column axis is
          // copied wholesale. `tensor.extract_slice`'s rank-reducing form
          // handles the rank drop when a static size-1 axis is present.
          val rows = tensorDimAsIndex(av.reg, t, 0)
          val rawR = fresh("ridxraw")
          out.append(s"  $rawR = arith.index_cast ${iv.reg} : i64 to index\n")
          val idxR = wrapNegBound(rawR, rows)
          val c0Idx = fresh("c0")
          out.append(s"  $c0Idx = arith.constant 0 : index\n")
          emitAxisIndexTrap(idxR, rows, c0Idx)
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
          val rows = tensorDimAsIndex(av.reg, t, 0)
          val cols = tensorDimAsIndex(av.reg, t, 1)
          val c0Idx = fresh("c0")
          out.append(s"  $c0Idx = arith.constant 0 : index\n")
          val rRaw = fresh("rraw")
          out.append(s"  $rRaw = arith.index_cast ${rv.reg} : i64 to index\n")
          val rR = wrapNegBound(rRaw, rows)
          emitAxisIndexTrap(rR, rows, c0Idx)
          val cRaw = fresh("craw")
          out.append(s"  $cRaw = arith.index_cast ${cv.reg} : i64 to index\n")
          val cR = wrapNegBound(cRaw, cols)
          emitAxisIndexTrap(cR, cols, c0Idx)
          val r = fresh("elt")
          out.append(s"  $r = tensor.extract ${av.reg}[$rR, $cR] : ${t.text}\n")
          MlirVal(r, MScalar(et))
        case (aty, rty, cty) =>
          notYet(s"rank-2 index on $aty with $rty, $cty")

    case TField(receiver, fieldName, _, _) =>
      val rv = emitExpr(receiver)
      rv.ty match
        case ms: MStruct =>
          val idx = ms.fields.indexWhere(_._1 == fieldName)
          if idx < 0 then notYet(s"field `$fieldName` not found on struct ${ms.name}")
          val fieldTy = ms.fields(idx)._2
          val r       = fresh(s"f_${fieldName}")
          out.append(s"  $r = llvm.extractvalue ${rv.reg}[$idx] : ${ms.text}\n")
          MlirVal(r, fieldTy)
        case MComplex =>
          // `.re` / `.im` extract the f64 components of a complex value.
          val idx = fieldName match
            case "re" => 0
            case "im" => 1
            case other => notYet(s"field `$other` on complex (expected `re` or `im`)"); 0
          val r = fresh(s"c_${fieldName}")
          out.append(s"  $r = llvm.extractvalue ${rv.reg}[$idx] : !llvm.struct<(f64, f64)>\n")
          MlirVal(r, MScalar(TyReal))
        case other =>
          notYet(s"field access on non-struct type $other")

    case TIf(cond, thenB, Some(elseB), _, tpe) if isMlirScalarType(tpe) =>
      emitIfExpr(cond, thenB, elseB, MScalar(tpe))

    case TIf(cond, thenB, Some(elseB), _, TyString) =>
      emitIfExpr(cond, thenB, elseB, MString)

    case TIf(cond, thenB, Some(elseB), _, tpe) if mlirTypeOf(tpe).exists(_.isInstanceOf[MTensor]) =>
      // Tensor-returning if-expression. Both branches must produce
      // the same MLIR tensor type for the `scf.if -> (T)` result
      // slot to verify. We declare the result as the param-boundary
      // dynamic-shape tensor (`tensor<?xT>` / `tensor<?x?xT>`) and
      // cast each branch's emit to that type at the yield, which
      // accepts statically-shaped values as well.
      val outTy = mlirTypeOf(tpe).get.asInstanceOf[MTensor]
      emitIfExpr(cond, thenB, elseB, outTy)

    case TIf(cond, thenB, Some(elseB), _, tpe) if mlirTypeOf(tpe).exists(_.isInstanceOf[MTuple]) =>
      val outTy = mlirTypeOf(tpe).get.asInstanceOf[MTuple]
      emitIfExpr(cond, thenB, elseB, outTy)

    case TIf(cond, thenB, Some(elseB), _, TyComplex) =>
      emitIfExpr(cond, thenB, elseB, MComplex)

    case TIf(cond, thenB, Some(elseB), _, tpe) if mlirTypeOf(tpe).exists(_.isInstanceOf[MStruct]) =>
      val outTy = mlirTypeOf(tpe).get.asInstanceOf[MStruct]
      emitIfExpr(cond, thenB, elseB, outTy)

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

    case TSlice(arr, Some(TIntLit(lo, _, _)), Some(TIntLit(hi, _, _)), inclusive, None, _, _) =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) =>
          val end      = if inclusive then hi.toInt + 1 else hi.toInt
          val sliceLen = math.max(0, end - lo.toInt)
          emitRank1Slice(av, t, lo.toInt, sliceLen)
        case other =>
          notYet(s"rank-1 slice on $other")

    case TSlice(arr, loE, hiE, inclusive, strideE, _, _) =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(_)) =>
          emitRank1SliceDynamic(av, t, loE, hiE, inclusive, strideE)
        case other =>
          notYet(s"rank-1 slice on $other")

    case TSlice2(arr, rowAx, colAx, _, _) =>
      val av = emitExpr(arr)
      av.ty match
        case t @ MTensor(_, List(rows, cols)) =>
          if rows >= 0 && cols >= 0 && isStaticAxis(rowAx) && isStaticAxis(colAx) then
            val rowSpec = axisToSlice(rowAx, rows)
            val colSpec = axisToSlice(colAx, cols)
            emitRank2Slice(av, t, rowSpec, colSpec)
          else
            emitRank2SliceDynamic(av, t, rowAx, colAx)
        case other =>
          notYet(s"rank-2 slice on $other")

    case TIntrinsic(opId, _, _) =>
      notYet(s"intrinsic `$opId` (MLIR backend has no Stage-0 intrinsic dispatch yet)")

    case other =>
      notYet(s"expression: ${other.getClass.getSimpleName}")

  protected def emitBlockItem(it: TBlockItem): Unit = it match
    case TBlockBinding(sym, BindingKind.Val, value) =>
      env(sym.id) = emitExpr(value)
    case TBlockBinding(sym, BindingKind.Var, value) if boxedVarSet.contains(sym.id) =>
      // Captured by some lambda — promote to a heap-allocated box so
      // its lifetime can outlive the parent frame and closures can
      // share writes through the box pointer. Reads / writes in the
      // parent scope route through [[boxedVarBoxes]] (see TVarRef /
      // TAssign arms above).
      val v = emitExpr(value)
      v.ty match
        case _: MScalar | MString | _: MFunc =>
          emitBoxedVarAlloc(sym.id)
          emitBoxedVarStore(sym.id, v)
        case other => notYet(s"boxed var ${sym.name} of type $other")
    case TBlockBinding(sym, BindingKind.Var, value) =>
      val v = emitExpr(value)
      v.ty match
        case s: MScalar => allocVarSlot(sym, v.reg, s)
        case t: MTensor => varTensors(sym.id) = (v.reg, t)
        case s: MStruct => varStructs(sym.id) = (v.reg, s)
        case t: MTuple  => varTuples(sym.id) = (v.reg, t)
        case other      => notYet(s"var binding for ${sym.name} of type $other")
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
    case TBlockExpr(TAssign(TVarRef(sym, _, _), value, _, _))
        if currentLambdaCaptures.contains(sym.id) =>
      // Mutation through a captured ByRef var (inside a lambda body)
      // routes through env+box. See [[emitCapturedWrite]].
      emitCapturedWrite(sym.id, emitExpr(value))
    case TBlockExpr(TAssign(TVarRef(sym, _, _), value, _, _)) if boxedVarBoxes.contains(sym.id) =>
      // Parent-scope mutation of a boxed var — write directly into
      // the heap box so any closure sharing the box sees the new
      // value.
      emitBoxedVarStore(sym.id, emitExpr(value))
    case TBlockExpr(TAssign(TVarRef(sym, _, _), value, _, _)) if varSlots.contains(sym.id) =>
      emitVarStore(sym.id, emitExpr(value))
    case TBlockExpr(TAssign(TVarRef(sym, _, _), value, _, _)) if varTensors.contains(sym.id) =>
      val v = emitExpr(value)
      v.ty match
        case t: MTensor => varTensors(sym.id) = (v.reg, t)
        case other      => notYet(s"var-tensor reassign with type $other")
    case TBlockExpr(TAssign(TVarRef(sym, _, _), value, _, _)) if varStructs.contains(sym.id) =>
      val v = emitExpr(value)
      v.ty match
        case s: MStruct => varStructs(sym.id) = (v.reg, s)
        case other      => notYet(s"var-struct reassign with type $other")
    case TBlockExpr(TAssign(TVarRef(sym, _, _), value, _, _)) if varTuples.contains(sym.id) =>
      val v = emitExpr(value)
      v.ty match
        case t: MTuple => varTuples(sym.id) = (v.reg, t)
        case other     => notYet(s"var-tuple reassign with type $other")
    case TBlockExpr(TAssign(target: TField, value, _, _))
        if rootVarStructSym(target).isDefined =>
      // Field write on a var-bound struct, possibly through a chain
      // of nested struct fields (`o.i.v = 99`). We collect the
      // outer-to-inner field path, recursively extract along the
      // chain, perform the leaf insertvalue, then insertvalue back
      // out to the outer struct, finally rebinding the var slot.
      val (rootSym, fieldPath) = collectFieldPath(target)
      val (curReg, ms) = varStructs(rootSym.id)
      val rhs = emitExpr(value)
      val newRootReg = writeFieldPath(curReg, ms, fieldPath, rhs)
      varStructs(rootSym.id) = (newRootReg, ms)
    case TBlockExpr(TAssign(TIndex(TVarRef(sym, _, _), List(idx), _, _), value, _, _))
        if varTensors.contains(sym.id) =>
      emitVarTensorIndexAssign(sym.id, List(idx), value)
    case TBlockExpr(TAssign(TIndex(TVarRef(sym, _, _), List(r, c), _, _), value, _, _))
        if varTensors.contains(sym.id) =>
      emitVarTensorIndexAssign(sym.id, List(r, c), value)
    case TBlockExpr(TAssign(TSlice(TVarRef(sym, _, _), lo, hi, incl, stride, _, _), rhs, _, _))
        if varTensors.contains(sym.id) =>
      emitVarTensorSliceAssign(sym.id, lo, hi, incl, stride, rhs)
    case TBlockExpr(TAssign(TSlice2(TVarRef(sym, _, _), rowAx, colAx, _, _), rhs, _, _))
        if varTensors.contains(sym.id) =>
      emitVarTensorRank2SliceAssign(sym.id, rowAx, colAx, rhs)
    case TBlockExpr(TCall(TVarRef(s, _, _), args, _, _)) if userDefs.contains(s.id) =>
      val (name, paramTys, retTyOpt) = userDefs(s.id)
      val _ = emitUserDefCall(name, paramTys, retTyOpt, args, s.name)
    case TBlockExpr(TIf(cond, thenB, elseB, _, _)) =>
      emitIfStatement(cond, thenB, elseB)
    case TBlockExpr(other) =>
      notYet(s"statement-position expression: ${other.getClass.getSimpleName}")

  /** Fold a unary-minus over a numeric literal. Returns `None` for
    * non-literal operands so the caller can decide whether to reject.
    * Constant folding past unary minus on arbitrary expressions is
    * out of scope until we have proper `arith.subi` / `arith.negf`
    * emission.
    */
  protected def foldLiteralNeg(e: TExpr): Option[TExpr] = e match
    case TIntLit(v, p, t)  => Some(TIntLit(-v, p, t))
    case TRealLit(v, p, t) => Some(TRealLit(-v, p, t))
    case _                 => None

  protected def scalarText(t: Type): String = t match
    case TyInteger    => "i64"
    case TyReal       => "f64"
    case TyBool       => "i1"
    case TyString     => "i64"  // opaque pointer to a C-side nex_str descriptor
    case _: TyFunc    => "i64"  // opaque pointer to a C-side nex_closure descriptor
    case other        => notYet(s"scalar text for $other")


  protected def zeroLit(t: Type): String = t match
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
  protected def scalarBinop(op: String, t: Type): String = (op, t) match
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
  protected def formatReal(v: Double): String =
    if v.isNaN || v.isInfinite then notYet(s"non-finite real literal: $v")
    else
      val s = v.toString
      if s.contains('.') || s.contains('e') || s.contains('E') then s
      else s + ".0"

  protected def fresh(prefix: String): String =
    val r = s"%${prefix}${nextReg}"
    nextReg += 1
    r

  protected def notYet(msg: String): Nothing =
    throw new UnsupportedOperationException(s"NexMLIRCodegen: $msg")
