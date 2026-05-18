package io.github.edadma.nex

import scala.collection.mutable

/** Shared state, infrastructure helpers, type/size mapping, and ARC tracking
  * for the LLVM IR codegen. Split out of [[NexLLVMCodegen]] so the other
  * trait-shaped concerns (preamble, lambdas, arrays, printing, prelude) can
  * mix this in without each owning its own copy of the bookkeeping state.
  *
  * All members are `protected` so the implementing class and sibling traits
  * can call them, but nothing outside the codegen surface is widened to
  * public.
  */
protected trait NexLLVMState:

  protected val out          = new StringBuilder
  protected var regCounter   = 0
  protected var labelCounter = 0
  protected val locals       = mutable.Map.empty[Int, String] // symbol id → alloca register

  /** The name of the basic block currently accepting instructions.
    *
    * `Some(label)` while the block is open and instructions append to it.
    * `None` after a terminator (`br`, `ret`, `unreachable`) has been
    * emitted — further instruction emits are silently dropped until
    * [[startBlock]] opens a new block.
    *
    * Tracked so that early returns inside `if` / `while` branches don't
    * produce stray instructions after a terminator (which LLVM rejects).
    */
  protected var currentBlock: Option[String] = None

  /** Currently-emitting function's declared return type. Needed by
    * [[TReturn]] to choose `ret <T> <v>` vs `ret void` when the spec'd
    * return type differs from the value's inferred type (e.g., the
    * elaborator left it `TyUnknown`).
    */
  protected var currentReturnType: Type = TyUnit
  protected var currentIsMain:     Boolean = false

  /** Symbol ids of top-level `val` / `var` / `const` bindings emitted as
    * LLVM `@<name> = global ...`. Looked up by `emitExpr` when a
    * [[TVarRef]] resolves neither to a local alloca nor a function — so
    * we know to emit a `load ... @<name>` (or, on the assign side, a
    * `store ... ptr @<name>`).
    */
  protected val globalBindings = mutable.Set.empty[Int]

  /** Pool of string literals → `@.str.<N>` global names. Each unique
    * literal value gets a single global so duplicate literal text isn't
    * stored twice in the resulting binary.
    */
  protected val stringPool = mutable.LinkedHashMap.empty[String, String]

  /** Function-level array slots — currently only array-typed parameters.
    * Block-level `val` / `var` bindings live in [[blockArrayScopes]] and
    * are dec'd at block end, not function end. Cleared per function.
    */
  protected val arrayLocalSlots = mutable.LinkedHashMap.empty[Int, (String, Type)]

  /** A stack of array-slot maps, one per currently-open `TBlock`. Top of
    * the stack is the innermost block. Each `TBlockBinding(sym, _, …)`
    * whose `sym` is array-typed pushes onto the top. At block end we
    * dec every entry in the popped scope so the binding's owning share
    * is released. Early `return` consults the whole stack via
    * [[decAllLocalArrays]] so nothing leaks regardless of nesting.
    *
    * Maintained as a `List` of mutable maps so `push` / `pop` is cheap
    * and the natural iteration order matches innermost-first.
    */
  protected var blockArrayScopes: List[mutable.LinkedHashMap[Int, (String, Type)]] = Nil

  // ---------------------------------------------------------------------------
  // Closure / lambda support (chunk 9).
  //
  // Closure value layout: `{ ptr fn, ptr env }` (16 bytes). The fn pointer
  // targets a synthetic top-level function `__nex_lambda_<N>` with signature
  // `(ptr env, T0 arg0, ...)`; the env points at a heap-allocated capture
  // struct (or null if the lambda captures nothing).
  //
  // Captures:
  //   - val / param / const: by-value (the capture stores the value).
  //   - var:                 by-reference (the capture stores a ptr to the
  //                          parent's alloca, so mutations on either side
  //                          are visible to the other — see the interpreter
  //                          test "closure captures live cells").
  //
  // Limitations (acknowledged, not fixed in this chunk):
  //   - Captured arrays / closures (recursive): the env free path doesn't
  //     dec captured array refs, so capturing an array leaks. Diagnosed
  //     with `notYet`.
  //   - Closure escape: a closure returned from a fn whose stack still
  //     contains a captured `var` alloca would dangle. v0 test surface
  //     doesn't exercise this; documented in roadmap.
  //   - No closure-ARC: the env malloc is never freed. Long-running loops
  //     that build many closures will leak. Acceptable for v0 tests.
  // ---------------------------------------------------------------------------

  protected enum CaptureMode:
    case ByVal, ByRef

  protected case class LambdaInfo(
      id:       Int,
      llvmName: String,
      params:   List[Symbol],
      body:     TExpr,
      retType:  Type,
      captures: List[(Symbol, Type, CaptureMode)],
      envTy:    String, // inline LLVM struct literal, e.g. "{ i64, ptr }" or "" if empty
      envSize:  Int,    // bytes; 0 if no captures
  )

  /** Identity-keyed map from each TLambda node to its synthesized
    * lambda info. Populated in a single pre-pass over `compile` so
    * later expression emission can look up `__nex_lambda_<N>` names
    * and capture maps without re-walking the AST.
    */
  protected val lambdaTable = new java.util.IdentityHashMap[TLambda, LambdaInfo]

  /** While emitting a synthetic lambda function body, this maps each
    * captured Symbol id to its env field index. Cleared between
    * emit-functions; consulted by `emitExpr` when resolving
    * [[TVarRef]] hits to a captured name (looked up before
    * [[locals]]).
    */
  protected var lambdaCaptures: Map[Int, (Int, Type, CaptureMode)] = Map.empty
  protected var lambdaEnvTy:     String                              = ""

  /** Set of all Symbol ids that bind a `var` (i.e., need by-ref capture
    * when referenced inside a lambda).
    */
  protected val varBindings = mutable.Set.empty[Int]

  // ---------------------------------------------------------------------------
  // Cross-trait abstract methods. Concrete definitions live in
  // [[NexLLVMCodegen]] (compile / emitExpr / control flow / tuples / structs)
  // or in sibling traits (printing, prelude, lambdas, arrays).
  // ---------------------------------------------------------------------------

  protected def emitExpr(e: TExpr): String

  protected def emitPrintCall(arg: TExpr): Unit
  protected def emitPrintValue(arg: TExpr): Unit
  protected def emitPrintArray(arr: TExpr): Unit
  protected def emitPrintArrayElem(elem: Type, v: String): Unit
  protected def emitPrintTuple(arg: TExpr): Unit
  protected def emitPrintTupleValue(v: String, t: Type, es: List[Type]): Unit
  protected def emitPrintStruct(arg: TExpr): Unit
  protected def emitPrintStructValue(v: String, t: Type, name: String, fields: List[(String, Type)]): Unit
  protected def emitPrintArr1Inline(arrV: String, elem: Type, esz: Int, stT: String, langT: String): Unit

  protected def emitPreludeCall(name: String, args: List[TExpr], resultT: Type): String

  protected def emitLambdaConstruct(lam: TLambda): String
  protected def emitClosureCall(callee: TExpr, args: List[TExpr], retT: Type): String
  protected def emitLambdaFunctions(): Unit
  protected def collectVarBindings(tp: TProgram): Unit
  protected def collectLambdas(tp: TProgram): Unit

  protected def emitArrayLit(elems: List[TExpr], t: Type): String
  protected def emitIndex(arr: TExpr, indices: List[TExpr], resultT: Type): String
  protected def emitElementWise(op: String, lhs: TExpr, rhs: TExpr, resultT: Type): String
  protected def emitBroadcast(scalar: TExpr, arr: TExpr, op: String, scalarFirst: Boolean, resultT: Type): String
  protected def emitSlice(arr: TExpr, lo: TExpr, hi: TExpr, inclusive: Boolean, resultT: Type): String
  protected def emitSlice2(arr: TExpr, rowAx: TAxisSpec, colAx: TAxisSpec, resultT: Type): String
  protected def emitClone(arr: TExpr, resultT: Type): String
  protected def emitFlatIndex(arr: TExpr, idx: TExpr, resultT: Type): String
  protected def emitFusedLoop(loopVar: Symbol, length: TExpr, body: TExpr, cols: Option[TExpr], resultT: Type): String
  protected def storeElem(storageT: String, value: String, slot: String): Unit
  protected def loadElem(storageT: String, slot: String, langT: String): String
  protected def emitCountingLoop(len: String, prefix: String)(genBody: String => Unit): Unit
  protected def bufPtr(desc: String, t: Type): String

  protected def emitPreamble(): Unit
  protected def emitInitFunction(bindings: List[TTopBinding]): Unit

  // ---------------------------------------------------------------------------
  // Infrastructure helpers.
  // ---------------------------------------------------------------------------

  protected def newReg(): String =
    regCounter += 1
    s"%t$regCounter"

  protected def freshLabel(prefix: String): String =
    labelCounter += 1
    s"$prefix.$labelCounter"

  /** Emit a single line of LLVM IR into the current basic block. Silently
    * dropped if no block is currently open (i.e., after a terminator).
    */
  protected def emitLine(s: String): Unit =
    if currentBlock.isDefined then out.append(s)

  /** Emit a terminator instruction (br, ret, unreachable). Closes the
    * current block — subsequent [[emitLine]] calls become no-ops until
    * [[startBlock]] opens a new one. Silently dropped if already
    * terminated.
    */
  protected def emitTerminator(s: String): Unit =
    if currentBlock.isDefined then
      out.append(s)
      currentBlock = None

  /** Open a new basic block with the given label. If the prior block is
    * still open (no terminator emitted), an implicit `br label %<label>`
    * fall-through is added before opening the new one — this keeps the
    * block-structure obligation invariant without requiring callers to
    * remember to close every dangling block.
    */
  protected def startBlock(label: String): Unit =
    if currentBlock.isDefined then
      out.append(s"  br label %$label\n")
    out.append(s"$label:\n")
    currentBlock = Some(label)

  protected def formatReal(d: Double): String =
    // LLVM IR requires double constants to be in hex form (`0x...`) for
    // exact bit reproduction. Java's Double.doubleToLongBits + %016x
    // gives the bit pattern; LLVM parses it with the `0x` prefix.
    f"0x${java.lang.Double.doubleToLongBits(d)}%016X"

  /** Record a "not yet supported" feature inline in the output so the
    * resulting `.ll` is still readable; the CLI surfaces a friendlier
    * top-level error. We don't throw because the test surface wants to
    * see partial output for diagnosis.
    */
  protected def notYet(what: String): Unit =
    out.append(s"  ; TODO: $what not yet supported by NexLLVMCodegen\n")

  /** Add `s` to the literal pool if not already present, returning the
    * `@.str.<N>` global name as an SSA-usable pointer token. The actual
    * `@.str.<N> = constant [<len> x i8] c"<escaped>\00"` definitions are
    * flushed by [[flushStringPool]] right before the user code so they're
    * defined before any function references them.
    */
  protected def internStringLiteral(s: String): String =
    stringPool.getOrElseUpdate(s, s"@.str.${stringPool.size}")

  /** Emit all pooled string-literal globals. Called once between the
    * preamble and the @-bindings/init-function/user-functions section so
    * every later reference can resolve.
    */
  protected def flushStringPool(): Unit =
    if stringPool.nonEmpty then
      for (text, name) <- stringPool do
        val encoded = encodeIRString(text)
        val len     = encoded._2
        out.append(s"$name = private unnamed_addr constant [$len x i8] c\"${encoded._1}\"\n")
      out.append("\n")

  /** Encode a String into LLVM IR's c"..." form. Returns the encoded
    * string text (without surrounding quotes) and the total byte length
    * including the implicit trailing NUL. Non-ASCII / control bytes are
    * `\xx` hex-escaped; the trailing NUL is added as `\00`.
    */
  protected def encodeIRString(s: String): (String, Int) =
    val sb = new StringBuilder
    var bytes = 0
    for c <- s.getBytes("UTF-8") do
      val b = c & 0xff
      if b == '"' || b == '\\' || b < 0x20 || b > 0x7e then
        sb.append(f"\\$b%02X")
      else
        sb.append(b.toChar)
      bytes += 1
    sb.append("\\00")
    bytes += 1
    (sb.toString, bytes)

  /** Picks the appropriate `zeroinitializer` token for an LLVM type. */
  protected def zeroInitFor(ty: String): String = ty match
    case "double" => "0.0"
    case "i1"     => "false"
    case "ptr"    => "null"
    case _        => "0"

  // ---------------------------------------------------------------------------
  // Type and binop tables.
  // ---------------------------------------------------------------------------

  /** Map a Nex type to its LLVM type. Tuples lower to anonymous LLVM
    * struct types literal-style (`{ T0, T1, ... }`); no separate `%name
    * = type ...` registration is needed because LLVM accepts the
    * literal at every use site.
    */
  protected def llvmType(t: Type): String = t match
    case TyInteger      => "i64"
    case TyReal         => "double"
    case TyBool         => "i1"
    case TyUnit         => "void"
    case TyString       => "ptr"
    case TyArray(_,_)   => "ptr"
    case TyTuple(elems)        => elems.map(llvmType).mkString("{ ", ", ", " }")
    case TyStruct(_, fields)   => fields.map(f => llvmType(f._2)).mkString("{ ", ", ", " }")
    case TyFunc(_, _)   => "{ ptr, ptr }"
    case TyUnknown      => "i64" // best-effort placeholder for missing inference
    case other          => notYet(s"type `$other`"); "i64"

  /** Storage type for an array element. `i1` (bool) is stored as `i8` so the
    * buffer's stride is one byte per element rather than packed bits.
    * Everything else uses its natural LLVM type.
    */
  protected def storageType(elem: Type): String = elem match
    case TyBool => "i8"
    case other  => llvmType(other)

  /** Size in bytes of one element in an array buffer (matches
    * [[storageType]]). For aggregates (tuples, structs) the size is
    * the sum of field sizes — this assumes every field is naturally
    * 8-byte aligned, which holds for v0's mix of integer/real/ptr/bool
    * fields (bool fields are stored as i8 inside arrays but as i1
    * inside tuples/structs; we round up to 8 for aggregates to keep
    * the alignment story simple).
    */
  protected def elemSize(elem: Type): Int = elem match
    case TyInteger        => 8
    case TyReal           => 8
    case TyBool           => 1
    case TyString         => 8
    case TyArray(_, _)    => 8 // ptr to descriptor
    case TyTuple(es)      => es.map(aggregateFieldSize).sum
    case TyStruct(_, fs)  => fs.map(f => aggregateFieldSize(f._2)).sum
    case _                => 8

  /** Field-of-aggregate size: scalars and pointers are 8 bytes; bools
    * inside aggregates are 1 byte rounded to 8 for alignment; nested
    * aggregates contribute their own elemSize.
    */
  protected def aggregateFieldSize(t: Type): Int = t match
    case TyBool => 8 // padded
    case other  => elemSize(other)

  /** Element type of an array Type; emits a diag and returns TyInteger when
    * the given type isn't a TyArray (which would mean the elaborator left the
    * type stage-1 — should be rare by Stage 3).
    */
  protected def arrayElem(t: Type): Type = t match
    case TyArray(e, _) => e
    case _             => notYet(s"expected array type, got $t"); TyInteger

  protected def arrayRank(t: Type): Int = t match
    case TyArray(_, r) => r
    case _             => 1

  protected def isArrayType(t: Type): Boolean = t match
    case TyArray(_, _) => true
    case _             => false

  /** Pick the right ARC inc helper based on the array's static rank. */
  protected def arrIncFor(t: Type): String = arrayRank(t) match
    case 1 => "@__nex_arr1_inc"
    case 2 => "@__nex_arr2_inc"
    case _ => "@__nex_arr1_inc"

  /** Pick the right ARC dec helper based on the array's static rank. */
  protected def arrDecFor(t: Type): String = arrayRank(t) match
    case 1 => "@__nex_arr1_dec"
    case 2 => "@__nex_arr2_dec"
    case _ => "@__nex_arr1_dec"

  /** Emit `call void @__nex_arr*_inc(ptr value)`. No-op (silently skipped)
    * if no block is open or if [[t]] isn't an array type.
    */
  protected def emitArrInc(value: String, t: Type): Unit =
    if isArrayType(t) then
      emitLine(s"  call void ${arrIncFor(t)}(ptr $value)\n")

  /** Emit `call void @__nex_arr*_dec(ptr value)`. */
  protected def emitArrDec(value: String, t: Type): Unit =
    if isArrayType(t) then
      emitLine(s"  call void ${arrDecFor(t)}(ptr $value)\n")

  /** Decrement-ref every slot registered as an array-typed local in the
    * current function — innermost block first, then the function-level
    * (param) slots last. Called right before each function-exit `ret`
    * (including early `return`s) so that nothing leaks regardless of
    * how deeply nested the return site is.
    */
  protected def decAllLocalArrays(): Unit =
    if currentBlock.isDefined then
      for scope <- blockArrayScopes do decBlockScope(scope)
      for (id, (slot, t)) <- arrayLocalSlots do
        val v = newReg()
        emitLine(s"  $v = load ptr, ptr $slot\n")
        emitArrDec(v, t)

  /** Dec every array slot recorded in a single block scope. Used at
    * block end (after popping) and as a building block for
    * [[decAllLocalArrays]].
    */
  protected def decBlockScope(scope: mutable.LinkedHashMap[Int, (String, Type)]): Unit =
    if currentBlock.isDefined then
      for (id, (slot, t)) <- scope do
        val v = newReg()
        emitLine(s"  $v = load ptr, ptr $slot\n")
        emitArrDec(v, t)

  /** Push a fresh block scope; subsequent array bindings emit into it. */
  protected def pushBlockScope(): Unit =
    blockArrayScopes = mutable.LinkedHashMap.empty[Int, (String, Type)] :: blockArrayScopes

  /** Pop the innermost block scope and return it so the caller can dec
    * its entries at the block-exit position.
    */
  protected def popBlockScope(): mutable.LinkedHashMap[Int, (String, Type)] =
    val top = blockArrayScopes.head
    blockArrayScopes = blockArrayScopes.tail
    top

  /** Register an array-typed binding's slot under the appropriate scope:
    * the innermost block if we're inside one, otherwise the function-
    * level [[arrayLocalSlots]] (which is the right home for params).
    */
  protected def registerArraySlot(id: Int, slot: String, t: Type): Unit =
    blockArrayScopes match
      case head :: _ => head(id) = (slot, t)
      case Nil       => arrayLocalSlots(id) = (slot, t)

  /** Map a Nex binary operator + operand type to (LLVM instruction,
    * result LLVM type). Integer / real overloads are picked here.
    */
  protected def binOpInst(op: String, opT: Type): (String, String) =
    (op, opT) match
      case ("+", TyInteger) => ("add",  "i64")
      case ("-", TyInteger) => ("sub",  "i64")
      case ("*", TyInteger) => ("mul",  "i64")
      case ("/", TyInteger) => ("sdiv", "i64") // unreachable on scalars; / promotes
      case ("div", TyInteger) => ("sdiv", "i64") // truncation toward zero (≠ floor)
      case ("%", TyInteger) => ("srem", "i64")
      case ("+", TyReal)    => ("fadd", "double")
      case ("-", TyReal)    => ("fsub", "double")
      case ("*", TyReal)    => ("fmul", "double")
      case ("/", TyReal)    => ("fdiv", "double")
      case ("==", TyInteger) => ("icmp eq",  "i1")
      case ("!=", TyInteger) => ("icmp ne",  "i1")
      case ("<",  TyInteger) => ("icmp slt", "i1")
      case ("<=", TyInteger) => ("icmp sle", "i1")
      case (">",  TyInteger) => ("icmp sgt", "i1")
      case (">=", TyInteger) => ("icmp sge", "i1")
      case ("==", TyReal) => ("fcmp oeq", "i1")
      case ("!=", TyReal) => ("fcmp one", "i1")
      case ("<",  TyReal) => ("fcmp olt", "i1")
      case ("<=", TyReal) => ("fcmp ole", "i1")
      case (">",  TyReal) => ("fcmp ogt", "i1")
      case (">=", TyReal) => ("fcmp oge", "i1")
      case _                => notYet(s"binop `$op` on `$opT`"); ("add", "i64")
