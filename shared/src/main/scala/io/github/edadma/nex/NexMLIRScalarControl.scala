package io.github.edadma.nex

/** Scalar arithmetic + control flow + user-def surface for
  * [[NexMLIRCodegen]]. Three logical sub-groups, all
  * scalar-shaped enough to share one file:
  *
  *   - Control statements: statement-position `if`, `for i in
  *     lo..hi`, `for x in arr`, `while`, var-slot read/write,
  *     loop-body shape adapter.
  *   - Scalar dispatchers: binop, `^` power, libm bridge, comparison
  *     predicate, comparison, short-circuit `and`/`or`, scalar
  *     `if`-expression, `min`/`max`, `abs`, `isMlirScalarType`,
  *     `promoteIntToReal`, `isComparisonOp`.
  *   - User `def` functions: type-of conversion, name mangling, call-
  *     site arg promotion, full `func.func` body emission, the
  *     `isLiteralScalarExpr` predicate used by the top-level-const
  *     hoist.
  *
  * Self-typed on `NexMLIRCodegen` for the shared state and the small
  * set of host-side utilities ([[scalarText]], [[scalarBinop]],
  * [[zeroLit]], [[foldLiteralNeg]], [[fresh]], [[notYet]]).
  */
trait NexMLIRScalarControl:
  self: NexMLIRCodegen =>


  /** Lower a statement-position `if cond then thenB else? elseB` to a
    * no-result `scf.if`. Each branch is emitted via [[emitForBody]]
    * (which already handles `TBlock` + value-discard semantics) and
    * terminated with a bare `scf.yield`. If the else branch is absent,
    * we emit `scf.if %c { ... }` (no else region), which is the
    * dialect's "single-side" form.
    */
  protected def emitIfStatement(cond: TExpr, thenB: TExpr, elseB: Option[TExpr]): Unit =
    val cv = emitExpr(cond)
    cv.ty match
      case MScalar(TyBool) =>
      case other          => notYet(s"if-statement condition of type $other")
    elseB match
      case Some(eB) =>
        out.append(s"  scf.if ${cv.reg} {\n")
        emitForBody(thenB)
        out.append("    scf.yield\n")
        out.append("  } else {\n")
        emitForBody(eB)
        out.append("    scf.yield\n")
        out.append("  }\n")
      case None =>
        out.append(s"  scf.if ${cv.reg} {\n")
        emitForBody(thenB)
        out.append("    scf.yield\n")
        out.append("  }\n")

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
  protected def emitForRange(loopVars: List[Symbol], lo: TExpr, hi: TExpr, inclusive: Boolean, body: TExpr): Unit =
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

  /** Allocate a stack slot for a `var <sym>` scalar binding and store
    * the initial value. The slot lives in `varSlots` keyed by symbol
    * id so later reads/writes can find it. Uses `memref.alloca` for
    * stack-local lifetime — the kernel-stack region is large enough
    * for any plausible number of var counters, and the slot is
    * automatically reclaimed on function exit.
    */
  protected def allocVarSlot(sym: Symbol, initReg: String, sty: MScalar): Unit =
    val mrefT = s"memref<${sty.text}>"
    val slot  = fresh(s"var_${sym.name}")
    out.append(s"  $slot = memref.alloca() : $mrefT\n")
    out.append(s"  memref.store $initReg, $slot[] : $mrefT\n")
    varSlots(sym.id) = (slot, sty)

  /** Read a `var` slot. Mirrors the existing `env`-lookup MlirVal
    * shape so the rest of the visitor doesn't have to know that
    * vars are different from vals.
    */
  protected def emitVarLoad(symId: Int): MlirVal =
    val (slot, sty) = varSlots(symId)
    val r           = fresh("vr")
    out.append(s"  $r = memref.load $slot[] : memref<${sty.text}>\n")
    MlirVal(r, sty)

  /** Store a new value into a `var` slot. The value's type must match
    * the slot's element type — Nex's type system already guarantees
    * this at TAssign sites, so no promotion is needed here.
    */
  protected def emitVarStore(symId: Int, v: MlirVal): Unit =
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
  protected def emitWhile(cond: TExpr, body: TExpr): Unit =
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
  protected def emitForArray(loopVars: List[Symbol], av: MlirVal, ty: MTensor, body: TExpr): Unit =
    if loopVars.size != 1 then
      notYet(s"for over array with ${loopVars.size}-way destructuring")
      return
    val loopVar = loopVars.head
    val len     = ty.shape.head
    val loC     = fresh("flo")
    out.append(s"  $loC = arith.constant 0 : index\n")
    val hiC     = fresh("fhi")
    if len >= 0 then
      out.append(s"  $hiC = arith.constant $len : index\n")
    else
      val axisR = fresh("axis")
      out.append(s"  $axisR = arith.constant 0 : index\n")
      out.append(s"  $hiC = tensor.dim ${av.reg}, $axisR : ${ty.text}\n")
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
  protected def emitForBody(body: TExpr): Unit = body match
    case TBlock(items, TUnitLit(_), _, _) =>
      items.foreach(emitBlockItem)
    case TBlock(items, last, _, _) =>
      items.foreach(emitBlockItem)
      emitBlockItem(TBlockExpr(last))
    case other =>
      emitBlockItem(TBlockExpr(other))
  /** Scalar `+ - *` on matching int/int or real/real operands. The
    * dispatcher in [[scalarBinop]] is the gate for which operators
    * land; everything else (`/`, `div`, `%`, `^`) bubbles up as
    * `notYet` from there before any IR is emitted.
    */
  protected def emitScalarBinop(op: String, lv: MlirVal, rv: MlirVal, ty: MScalar): MlirVal =
    val opName = scalarBinop(op, ty.elem)
    val r      = fresh("sb")
    out.append(s"  $r = $opName ${lv.reg}, ${rv.reg} : ${ty.text}\n")
    MlirVal(r, ty)

  /** Sign-extend-to-float promotion of an `i64` SSA value into `f64`,
    * matching the interpreter's `asReal` promotion at mixed-type
    * binop call sites. Used to handle `int + real`-style expressions
    * where the elaborator leaves operand types mismatched.
    */
  protected def promoteIntToReal(v: MlirVal): MlirVal =
    val r = fresh("pr")
    out.append(s"  $r = arith.sitofp ${v.reg} : i64 to f64\n")
    MlirVal(r, MScalar(TyReal))

  protected def isComparisonOp(op: String): Boolean =
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
  protected def emitComparison(op: String, lv: MlirVal, rv: MlirVal): MlirVal =
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

  /** Scalar types the backend can carry through an `scf.if -> (T)`
    * result slot. Tensor-returning `if` would need shape inference
    * (both branches must materialise the same static tensor type)
    * and isn't yet supported.
    */
  protected def isMlirScalarType(t: Type): Boolean = t match
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
  protected def emitIfExpr(cond: TExpr, thenB: TExpr, elseB: TExpr, outTy: MlirType): MlirVal =
    val cv = emitExpr(cond)
    val r  = fresh("if")
    out.append(s"  $r = scf.if ${cv.reg} -> (${outTy.text}) {\n")
    val tv = emitExpr(thenB)
    val tReg = coerceToType(tv, outTy)
    out.append(s"    scf.yield $tReg : ${outTy.text}\n")
    out.append("  } else {\n")
    val ev = emitExpr(elseB)
    val eReg = coerceToType(ev, outTy)
    out.append(s"    scf.yield $eReg : ${outTy.text}\n")
    out.append("  }\n")
    MlirVal(r, outTy)

  /** Adapt a branch result to the expected if-expression result type.
    * For matching types it's an identity pass-through; for tensor
    * branches whose static shape mismatches the boundary-form (which
    * is always dynamic-shape on the `if -> (T)` result type), insert
    * a `tensor.cast`. Other type mismatches are an elaborator bug to
    * surface here.
    */
  protected def coerceToType(v: MlirVal, expected: MlirType): String =
    (v.ty, expected) match
      case (lt, rt) if lt == rt => v.reg
      case (lt: MTensor, rt: MTensor) if lt.elem == rt.elem && lt.shape.length == rt.shape.length =>
        val r = fresh("tcast")
        out.append(s"    $r = tensor.cast ${v.reg} : ${lt.text} to ${rt.text}\n")
        r
      case (lt, rt) =>
        notYet(s"if branch result $lt cannot coerce to $rt")

  protected def emitShortCircuit(lhs: TExpr, rhs: TExpr, isAnd: Boolean): MlirVal =
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
  protected def emitLibmCall(name: String, args: List[MlirVal]): MlirVal =
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
  protected def emitScalarPower(lv: MlirVal, rv: MlirVal, resultTy: Type): MlirVal =
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






























  /** Scalar binary `min` / `max`. Promotes mixed `int × real` operands
    * to real before dispatching to the matching `arith.{minsi, maxsi,
    * minimumf, maximumf}` op. Same NaN-propagating spelling we use
    * for array min/max.
    */
  protected def emitScalarMinMax(name: String, lv: MlirVal, rv: MlirVal): MlirVal =
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
  protected def emitScalarAbs(v: MlirVal): MlirVal = v.ty match
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


  /** Convert a Nex elaborator-level `Type` to the codegen's
    * [[MlirType]] when the backend can express it as a function
    * parameter or return type. Returns `None` for types this phase
    * doesn't yet handle (complex, tuples, structs, enums, function
    * types, `TyUnit` — units are unit-returning defs which need a
    * different `func.return` shape).
    *
    * Rank-1 and rank-2 array types with scalar (int/real/bool)
    * elements lower to dynamic-shape tensors at the function-boundary
    * level (`tensor<?xT>` / `tensor<?x?xT>`); call sites bridge
    * statically-shaped args via `tensor.cast`. Static element-type
    * resolution lets the body still recover lengths via `tensor.dim`.
    */
  protected def mlirTypeOf(t: Type): Option[MlirType] = t match
    case TyInteger                                                => Some(MScalar(TyInteger))
    case TyReal                                                   => Some(MScalar(TyReal))
    case TyBool                                                   => Some(MScalar(TyBool))
    case TyString                                                 => Some(MString)
    case TyArray(elem, 1) if isMlirScalarType(elem)               => Some(MTensor(elem, List(-1)))
    case TyArray(elem, 2) if isMlirScalarType(elem)               => Some(MTensor(elem, List(-1, -1)))
    case _                                                        => None

  /** True when `e` is a scalar literal (after literal-fold of unary
    * minus) — i.e. an expression the codegen can re-emit at every
    * use site without changing semantics. Used to decide whether a
    * top-level binding is safe to inline at reference sites in user
    * `def` bodies (which run before `@main` and therefore can't
    * read the env-bound copy emitted at main's entry).
    */
  protected def isLiteralScalarExpr(e: TExpr): Boolean = e match
    case _: TIntLit | _: TRealLit | _: TBoolLit | _: TStringLit       => true
    case TUnaryOp("-", inner, _, _)                                    => isLiteralScalarExpr(inner)
    case _                                                             => false

  /** Mangle a user `def`'s name into a unique MLIR symbol. The id
    * suffix prevents collisions with the runtime print/format
    * helpers, libm bridges, and any two user defs that happen to
    * share a name across scopes — uncommon, but cheap insurance.
    */
  protected def mangleUserDef(sym: Symbol): String =
    s"@nex_user_${sym.name}_${sym.id}"

  /** Emit a call to a registered user def. Evaluates the args,
    * applies cheap numeric promotions to match the declared param
    * types, then writes a single `func.call`. Returns a fresh
    * `MlirVal` for value-returning calls; for unit-returning calls
    * the result has dummy register `%`-placeholder and type
    * `MScalar(TyInteger)` — the caller in [[emitBlockItem]] ignores
    * the result and uses the call purely for its side-effect.
    */
  protected def emitUserDefCall(
      name:     String,
      paramTys: List[MlirType],
      retTyOpt: Option[MlirType],
      args:     List[TExpr],
      symName:  String,
  ): MlirVal =
    if args.size != paramTys.size then
      notYet(s"arity mismatch on user def `$symName` — ${args.size} args vs ${paramTys.size} params")
    val argVals = args.zip(paramTys).map { case (a, expectedTy) =>
      val av = emitExpr(a)
      (av.ty, expectedTy) match
        case (lt, rt) if lt == rt                  => av
        case (MScalar(TyInteger), MScalar(TyReal)) => promoteIntToReal(av)
        case (lt: MTensor, rt: MTensor)
            if lt.elem == rt.elem
              && lt.shape.length == rt.shape.length
              && rt.shape.forall(_ < 0) =>
          // Caller has a tensor whose element type and rank match the
          // declared dynamic-shape param. Bridge via `tensor.cast`
          // so the call's argument type matches the callee's
          // signature byte-for-byte.
          val r = fresh("tcast")
          out.append(s"  $r = tensor.cast ${av.reg} : ${lt.text} to ${rt.text}\n")
          MlirVal(r, rt)
        case (lt, rt) =>
          notYet(s"user def `$symName` arg type $lt does not match param type $rt")
    }
    val paramTyText = paramTys.map(_.text).mkString(", ")
    retTyOpt match
      case Some(retTy) =>
        val r = fresh("ucall")
        out.append(
          s"  $r = func.call $name(${argVals.map(_.reg).mkString(", ")}) : ($paramTyText) -> ${retTy.text}\n",
        )
        MlirVal(r, retTy)
      case None =>
        out.append(
          s"  func.call $name(${argVals.map(_.reg).mkString(", ")}) : ($paramTyText) -> ()\n",
        )
        MlirVal("%unused", MScalar(TyInteger))

  /** Emit a `func.func` for one registered user def, writing the
    * signature, the body, and the closing terminator into `out`.
    * The current `nextReg` and `env` are saved + reset so the body's
    * SSA names don't collide with the caller's region. Parameter
    * symbols bind to MLIR block args (`%arg0`, `%arg1`, …) in the
    * fresh env. Value-returning bodies are emitted via `emitExpr`
    * and yielded through `func.return %r : T`; unit-returning
    * bodies are emitted as a statement sequence ([[emitForBody]] is
    * already the right shape for that) and end with a bare
    * `func.return`.
    */
  protected def emitUserDef(f: TFunDecl): Unit =
    val (name, paramTys, retTyOpt) = userDefs(f.sym.id)
    val sigParts = f.params.zip(paramTys).zipWithIndex.map { case ((_, ty), i) =>
      s"%arg$i: ${ty.text}"
    }
    val retStr = retTyOpt.fold("")(t => s" -> ${t.text}")
    out.append(s"func.func $name(${sigParts.mkString(", ")})$retStr {\n")
    val savedReg = nextReg
    val savedEnv = env.toMap
    nextReg = 0
    env.clear()
    f.params.zip(paramTys).zipWithIndex.foreach { case ((p, ty), i) =>
      env(p.id) = MlirVal(s"%arg$i", ty)
    }
    retTyOpt match
      case Some(retTy) =>
        val v = emitExpr(f.body)
        if v.ty != retTy then
          notYet(s"user def `${f.sym.name}` body type ${v.ty} does not match declared return ${retTy}")
        out.append(s"  func.return ${v.reg} : ${retTy.text}\n")
      case None =>
        emitForBody(f.body)
        out.append("  func.return\n")
    out.append("}\n")
    nextReg = savedReg
    env.clear()
    savedEnv.foreach { case (k, v) => env(k) = v }
