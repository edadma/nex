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

  /** Pool of literals that need a user-facing %nex_str descriptor.
    * Maps literal text → (descriptor global name, byte length excluding
    * the NUL terminator). Each entry emits a static descriptor with
    * refcount=-1 (the immortal sentinel) wrapping the matching
    * stringPool byte array. Populated by [[internStringDescriptor]]
    * and flushed by [[flushStringPool]] right after the byte-array
    * globals it references.
    */
  protected val stringDescPool = mutable.LinkedHashMap.empty[String, (String, Int)]

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
  // Closure / lambda support.
  //
  // Closure value layout: `{ ptr fn, ptr env }` (16 bytes). The fn pointer
  // targets a synthetic top-level function `__nex_lambda_<N>` with signature
  // `(ptr env, T0 arg0, ...)`; the env points at a heap-allocated capture
  // struct (or null if the lambda captures nothing).
  //
  // The env carries a negative-offset header `[ rc(i64) | dtor(ptr) ]`. The
  // env pointer is participates in the same scope-based ARC machinery as
  // arrays and strings: function-param slots, val/var/assign bindings, and
  // map/reduce/filter HOFs all route through `__nex_env_inc/dec`, and the
  // per-lambda dtor walks ByVal captures to dec strings, arrays (deep-dec
  // when the element is refcounted), and nested closures before freeing.
  //
  // Captures:
  //   - val / param / const: by-value (the capture stores the value;
  //                          refcounted values are inc'd at capture time
  //                          and dec'd by the env dtor).
  //   - var:                 by-reference (the capture stores a ptr to the
  //                          parent's alloca, so mutations on either side
  //                          are visible to the other).
  //
  // Closure escape: a closure returned from a fn whose stack still contains
  // a ByRef-captured `var` alloca would dangle. The v0 test surface doesn't
  // exercise this; lifting the captured var to the heap (or always boxing
  // closure-escaping vars) is the v1+ fix.
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

  /** Symbol ids of `var` bindings that are captured by reference from
    * any lambda. These vars live in a heap-allocated box (one i64/double/
    * etc per box, refcount-headed via __nex_env_alloc) instead of on the
    * parent stack — so a returned closure that holds a ByRef ref to the
    * var continues to see live storage after the parent frame is gone.
    *
    * Populated in the lambda pre-pass before any function body is emitted.
    * Consulted by [[emitLocalBinding]] (to allocate the box), the TVarRef
    * read path (to double-load through the box), the TAssign write path
    * (to store through the box), and `lvalueOfBinding` (to hand the env
    * the box pointer instead of the dead alloca).
    */
  protected val boxedVarTypes = mutable.Map.empty[Int, Type]

  /** Function-level boxed-var slots: symbol id → alloca register that
    * stores the box pointer. Function exit dec's each box. Used for
    * boxed vars that live in the function's top scope (not nested in
    * a block).
    */
  protected val boxedFunctionSlots = mutable.LinkedHashMap.empty[Int, String]

  /** Parallel stack to [[blockArrayScopes]] for boxed-var bindings inside
    * a TBlock. Each scope holds id → alloca-register. Pushed / popped /
    * dec'd in sync with the array scope at the same depth.
    */
  protected var blockBoxedScopes: List[mutable.LinkedHashMap[Int, String]] = Nil

  /** Symbol id → opId for every `@intrinsic` top-level function. These
    * functions have no LLVM wrapper definition emitted (a wrapper would
    * either collide with the libm symbol or recurse into itself). Instead
    * the call site looks up the opId in this table and emits the
    * intrinsic body inline.
    */
  protected val intrinsicFunctionOpIds = mutable.Map.empty[Int, String]

  /** Symbol-id → emitted LLVM function name. Populated at codegen init
    * for any function that participates in an overload set whose name
    * would otherwise collide in the flat LLVM symbol namespace. The
    * fallback is the symbol's plain name; only overload-set members
    * (with a libm bridge or another overload sharing the name) get a
    * mangled `<name>$<param-mangle>` entry here.
    */
  protected val llvmFuncNames = mutable.Map.empty[Int, String]

  /** Top-level `def`s that are used as function VALUES — passed to a
    * higher-order function, stored in a binding, etc. The LLVM closure
    * shape is `{ fn-ptr, env-ptr }` and the indirect-call path prepends
    * an `env` argument; bare top-level defs don't take an env so we
    * synthesize a thin **thunk** wrapper per referenced def that takes
    * `(env_ptr, args...)`, ignores `env`, and tail-calls the real def.
    *
    * Symbol id → thunk LLVM name (also the IR symbol the closure literal
    * stores). [[defThunkPending]] tracks which thunks still need
    * emission; [[defThunkEmitted]] tracks which have been emitted.
    * [[flushDefThunks]] drains pending after the main function loop.
    */
  protected val defThunkNames    = mutable.Map.empty[Int, String]
  protected val defThunkPending  = mutable.Set.empty[Int]
  protected val defThunkEmitted  = mutable.Set.empty[Int]
  protected val defThunkSymbols  = mutable.Map.empty[Int, Symbol]

  /** Convert a list of param types into a stable, ASCII-only suffix for
    * use in overload-disambiguating LLVM function names. Mirrors the
    * shape NexMonomorphize already uses for kind-specialized clones.
    */
  protected def mangleParamTypes(ts: List[Type]): String =
    ts.map(mangleTypeForLLVM).mkString("_")

  protected def mangleTypeForLLVM(t: Type): String = t match
    case TyInteger       => "integer"
    case TyReal          => "real"
    case TyComplex       => "complex"
    case TyBool          => "bool"
    case TyString        => "string"
    case TyUnit          => "unit"
    case TyArray(e, 1)   => s"array_${mangleTypeForLLVM(e)}"
    case TyArray(e, r)   => s"array${r}_${mangleTypeForLLVM(e)}"
    case TyTuple(es)     => es.map(mangleTypeForLLVM).mkString("tup_", "_", "")
    case TyStruct(n, _)  => n
    case TyFunc(ps, r)   => ps.map((pt, _) => mangleTypeForLLVM(pt)).mkString("fn_", "_", s"_to_${mangleTypeForLLVM(r)}")
    case TyKindVar(n, _) => n
    case TyUnknown       => "unknown"

  /** Resolved LLVM function name for `sym` — mangled when sym is part of
    * an overload set, plain otherwise.
    */
  protected def llvmFuncNameOf(sym: Symbol): String =
    llvmFuncNames.getOrElse(sym.id, sym.name)

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

  /** Lower an arbitrary-typed expression to a %nex_str descriptor.
    * Concrete impl lives in [[NexLLVMCodegen]]; declared here so other
    * traits (Prelude / Print) can route values through it without a
    * trait-dependency cycle.
    */
  protected def emitValueToString(e: TExpr): String

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
    * resulting `.ll` is still readable. Some call sites use this as a
    * soft fallback (the surrounding code returns a placeholder that
    * happens to compile for the cases the test corpus exercises);
    * others use it as a hard barrier. A hard-fail variant ([[notImpl]])
    * exists for the latter so future regressions surface at codegen
    * time instead of as silent miscompiles.
    */
  protected def notYet(what: String): Unit =
    out.append(s"  ; TODO: $what not yet supported by NexLLVMCodegen\n")

  /** Abort codegen with a clear error when we hit a feature the LLVM
    * backend doesn't implement and there is no sensible placeholder.
    * Caught by the CLI and the test harness so the diagnostic surfaces
    * loudly. Use this in preference to [[notYet]] when a silent
    * placeholder would produce a miscompiled binary.
    */
  protected def notImpl(what: String): Nothing =
    throw NexCodegenError(s"$what not yet supported by NexLLVMCodegen")

  /** Add `s` to the literal pool if not already present, returning the
    * `@.str.<N>` global name as an SSA-usable pointer token. The actual
    * `@.str.<N> = constant [<len> x i8] c"<escaped>\00"` definitions are
    * flushed by [[flushStringPool]] right before the user code so they're
    * defined before any function references them.
    */
  protected def internStringLiteral(s: String): String =
    stringPool.getOrElseUpdate(s, s"@.str.${stringPool.size}")

  /** Intern a literal that's used as a user-facing Nex string value.
    * Returns the descriptor global name (a `ptr`). The byte-array
    * global is interned alongside; both are emitted by
    * [[flushStringPool]] (the byte array first so the descriptor's
    * `ptr` field can reference it).
    */
  protected def internStringDescriptor(s: String): String =
    internStringLiteral(s)
    stringDescPool.getOrElseUpdate(
      s,
      (s"@.strd.${stringDescPool.size}", s.getBytes("UTF-8").length),
    )._1

  /** Emit all pooled string-literal globals. Called once between the
    * preamble and the @-bindings/init-function/user-functions section so
    * every later reference can resolve. Byte-array globals come first,
    * then any descriptor globals that wrap them.
    */
  protected def flushStringPool(): Unit =
    if stringPool.nonEmpty then
      for (text, name) <- stringPool do
        val encoded = encodeIRString(text)
        val len     = encoded._2
        out.append(s"$name = private unnamed_addr constant [$len x i8] c\"${encoded._1}\"\n")
      out.append("\n")
    if stringDescPool.nonEmpty then
      for (text, (descName, byteLen)) <- stringDescPool do
        // Look up the matching byte-array global; it was interned by
        // [[internStringDescriptor]] so it's guaranteed to exist.
        val bytesName = stringPool(text)
        // Immortal sentinel: refcount = -1, so inc/dec no-op. The byte
        // count we store is the user-visible length (no NUL).
        out.append(
          s"$descName = private unnamed_addr constant %nex_str { i64 -1, i64 $byteLen, ptr $bytesName }\n",
        )
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

  /** Picks the appropriate zero-value literal for an LLVM type at module
    * global scope. Aggregates (`{ ... }` struct literals) require
    * `zeroinitializer` rather than `0`. Scalars get their natural zero
    * token: `0` for integers, `0.0` for doubles, `false` for i1,
    * `null` for pointers.
    */
  protected def zeroInitFor(ty: String): String = ty match
    case "double" => "0.0"
    case "i1"     => "false"
    case "ptr"    => "null"
    case "void"   => "0"
    case s if s.startsWith("{") => "zeroinitializer"
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
    case TyComplex      => "{ double, double }"
    case TyArray(_,_)   => "ptr"
    case TyTuple(elems)        => elems.map(llvmType).mkString("{ ", ", ", " }")
    case TyStruct(_, fields)   => fields.map(f => llvmType(f._2)).mkString("{ ", ", ", " }")
    case TyFunc(_, _)   => "{ ptr, ptr }"
    case TyUnknown      => "i64" // best-effort placeholder for missing inference
    case TyKindVar(n, _) =>
      // Reaching codegen with an un-monomorphized kind variable is a
      // compiler bug — the monomorphization pass should have produced
      // a concrete specialization before codegen ran.
      throw new RuntimeException(
        s"llvmType: encountered un-substituted kind variable `$n` — monomorphization pass did not run or missed this site",
      )

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
    case TyComplex        => 16 // { double, double }
    case TyArray(_, _)    => 8  // ptr to descriptor
    case TyFunc(_, _)     => 16 // closure value is { fn_ptr, env_ptr }
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

  /** A closure value is `{ ptr fn, ptr env }`. The `env` pointer (when
    * non-null) targets a refcounted heap allocation; inc/dec route
    * through `__nex_env_inc/dec` after extracting it from the value.
    */
  protected def isClosureType(t: Type): Boolean = t match
    case TyFunc(_, _) => true
    case _            => false

  /** Types that participate in scope-based refcounting. Array, closure,
    * and string are tracked uniformly through [[arrayLocalSlots]] /
    * [[blockArrayScopes]]; the load shape and dec helper differ but
    * the registration / decrement timing is identical. String literals
    * carry the immortal sentinel (rc=-1), so inc/dec on them is a
    * no-op — only heap-allocated descriptors actually free.
    *
    * Tuples and structs are SSA values, not heap allocations, so they
    * have no direct refcount; however when an aggregate carries any
    * refcounted field, the aggregate's slot still needs scope-end inc /
    * dec so that the per-field shares are released — see
    * [[aggregateContainsRefCounted]]. The inc / dec routes through
    * per-aggregate-type helpers generated on demand.
    */
  protected def isRefCountedType(t: Type): Boolean =
    isArrayType(t) || isClosureType(t) || t == TyString || aggregateContainsRefCounted(t)

  /** True when `t` is a tuple or struct (transitively) carrying at
    * least one refcounted leaf. Memoized because the same type shows
    * up across many call sites (var-decl, projection, capture, etc.).
    */
  protected def aggregateContainsRefCounted(t: Type): Boolean =
    aggContainsMemo.get(t) match
      case Some(b) => b
      case None =>
        val r = t match
          case TyTuple(es)     => es.exists(isRefCountedType)
          case TyStruct(_, fs) => fs.exists(f => isRefCountedType(f._2))
          case _               => false
        aggContainsMemo(t) = r
        r

  private val aggContainsMemo = mutable.Map.empty[Type, Boolean]

  /** Pick the right ARC inc helper based on the array's static rank. */
  protected def arrIncFor(t: Type): String = arrayRank(t) match
    case 1 => "@__nex_arr1_inc"
    case 2 => "@__nex_arr2_inc"
    case _ => "@__nex_arr1_inc"

  /** Pick the right ARC dec helper based on the array's rank AND element
    * type. For arrays whose element type is itself refcounted (strings,
    * nested arrays), this returns a per-element-type deep-dec variant
    * and registers it for emission. The deep variant walks the buffer
    * and dec's each element before freeing — without it, releasing the
    * outer descriptor would leak every inner refcounted descriptor.
    */
  protected def arrDecFor(t: Type): String =
    val rank = arrayRank(t)
    val elem = arrayElem(t)
    val flat = rank match
      case 1 => "@__nex_arr1_dec"
      case 2 => "@__nex_arr2_dec"
      case _ => "@__nex_arr1_dec"
    if !deepDecEligible(elem) then flat
    else
      val name = s"@__nex_arr${rank}_dec_${typeMangle(elem)}"
      val key  = (rank, elem)
      if !deepDecEmitted.contains(key) then deepDecPending += key
      name

  /** Element types for which we generate per-element-type deep-dec
    * helpers. Strings and nested arrays carry refcounts and are stored
    * as plain ptr slots, so a single load + matching dec call works
    * uniformly. Aggregates carrying refcounted leaves use the
    * aggregate's storage type for the slot load and route through the
    * per-aggregate drop helper. Closure values are `{ ptr, ptr }` (16
    * bytes) — they don't fit the v0 by-pointer element pattern and are
    * not yet supported as array elements.
    */
  protected def deepDecEligible(elem: Type): Boolean = elem match
    case TyString                                          => true
    case TyArray(_, _)                                     => true
    case t if aggregateContainsRefCounted(t)               => true
    case _                                                 => false

  /** Stable mangling of a Nex type for use in generated symbol names.
    * Strings and primitive scalars get short tags; nested arrays
    * recurse so `[[String]]` mangles to `arr1_str`. Aggregates encode
    * their field shape so distinct tuple / struct layouts map to
    * distinct helper names: `tup_<f0>_<f1>...`,
    * `struct_<name>_<f0>_<f1>...`. Anything outside the helper-
    * generating set falls through to `any` (unused — guarded by
    * [[deepDecEligible]] / [[aggregateContainsRefCounted]]).
    */
  protected def typeMangle(t: Type): String = t match
    case TyString        => "str"
    case TyInteger       => "i64"
    case TyReal          => "f64"
    case TyBool          => "i1"
    case TyComplex       => "complex"
    case TyArray(e, r)   => s"arr${r}_${typeMangle(e)}"
    case TyTuple(es)     => "tup_" + es.map(typeMangle).mkString("_")
    case TyStruct(n, fs) => s"struct_${n}_" + fs.map(f => typeMangle(f._2)).mkString("_")
    case _               => "any"

  /** Per-element-type deep-dec helpers that still need an emitted
    * definition. Populated by [[arrDecFor]] each time a previously
    * unseen `(rank, elem)` pair is requested; drained by
    * `flushDeepDecs` at end-of-module.
    */
  protected val deepDecPending = mutable.LinkedHashSet.empty[(Int, Type)]
  protected val deepDecEmitted = mutable.Set.empty[(Int, Type)]

  /** Per-aggregate-type inc / drop helpers (`__nex_inc_<mangle>` /
    * `__nex_drop_<mangle>`) that still need an emitted definition.
    * Populated by [[emitArrInc]] / [[emitArrDec]] each time they see a
    * previously unseen aggregate type; drained by `flushAggHelpers` at
    * end-of-module. Helper bodies recursively register nested
    * aggregates / array helpers, so the flush loop runs to fixed point.
    */
  protected val aggHelperPending = mutable.LinkedHashSet.empty[Type]
  protected val aggHelperEmitted = mutable.Set.empty[Type]

  protected def aggIncHelperName(t: Type): String = s"@__nex_inc_${typeMangle(t)}"
  protected def aggDropHelperName(t: Type): String = s"@__nex_drop_${typeMangle(t)}"

  /** Mark an aggregate type as needing inc / drop helpers if not
    * already emitted. Idempotent. Called from [[emitArrInc]] /
    * [[emitArrDec]] and from helper bodies that recurse into nested
    * aggregates.
    */
  protected def requestAggHelper(t: Type): Unit =
    if !aggHelperEmitted.contains(t) then aggHelperPending += t

  /** Emit an inc-refcount call appropriate for [[t]]. For arrays the
    * SSA `value` is the array descriptor ptr; for closures it's the
    * `{ ptr, ptr }` value, from which we extract env_ptr before
    * routing to `__nex_env_inc`. Aggregates (tuples / structs)
    * carrying refcounted leaves route through a per-aggregate-type
    * `__nex_inc_<mangle>` helper that walks fields and inc's each
    * refcounted share — value is the aggregate SSA, passed by value
    * to the helper. Other types are silently skipped.
    */
  protected def emitArrInc(value: String, t: Type): Unit =
    if isArrayType(t) then
      emitLine(s"  call void ${arrIncFor(t)}(ptr $value)\n")
    else if isClosureType(t) then
      val env = newReg()
      emitLine(s"  $env = extractvalue { ptr, ptr } $value, 1\n")
      emitLine(s"  call void @__nex_env_inc(ptr $env)\n")
    else if t == TyString then
      emitLine(s"  call void @__nex_str_inc(ptr $value)\n")
    else if aggregateContainsRefCounted(t) then
      requestAggHelper(t)
      emitLine(s"  call void ${aggIncHelperName(t)}(${llvmType(t)} $value)\n")

  /** Symmetric dec — see [[emitArrInc]] for shape semantics. */
  protected def emitArrDec(value: String, t: Type): Unit =
    if isArrayType(t) then
      emitLine(s"  call void ${arrDecFor(t)}(ptr $value)\n")
    else if isClosureType(t) then
      val env = newReg()
      emitLine(s"  $env = extractvalue { ptr, ptr } $value, 1\n")
      emitLine(s"  call void @__nex_env_dec(ptr $env)\n")
    else if t == TyString then
      emitLine(s"  call void @__nex_str_dec(ptr $value)\n")
    else if aggregateContainsRefCounted(t) then
      requestAggHelper(t)
      emitLine(s"  call void ${aggDropHelperName(t)}(${llvmType(t)} $value)\n")

  /** Decrement-ref every slot registered as an array-typed local in the
    * current function — innermost block first, then the function-level
    * (param) slots last. Called right before each function-exit `ret`
    * (including early `return`s) so that nothing leaks regardless of
    * how deeply nested the return site is. Also walks every boxed-var
    * scope so the heap box backing each escape-captured `var` is
    * released when the parent function exits.
    */
  protected def decAllLocalArrays(): Unit =
    if currentBlock.isDefined then
      for scope <- blockArrayScopes do decBlockScope(scope)
      for boxScope <- blockBoxedScopes do decBoxedScope(boxScope)
      for (id, (slot, t)) <- arrayLocalSlots do
        val v = newReg()
        emitLine(s"  $v = load ${llvmType(t)}, ptr $slot\n")
        emitArrDec(v, t)
      for (_, slot) <- boxedFunctionSlots do decBoxedSlot(slot)

  /** Dec every refcounted slot recorded in a single block scope. Used at
    * block end (after popping) and as a building block for
    * [[decAllLocalArrays]]. The load type follows `llvmType(t)` so
    * arrays load as ptr and closures load as `{ ptr, ptr }`.
    */
  protected def decBlockScope(scope: mutable.LinkedHashMap[Int, (String, Type)]): Unit =
    if currentBlock.isDefined then
      for (id, (slot, t)) <- scope do
        val v = newReg()
        emitLine(s"  $v = load ${llvmType(t)}, ptr $slot\n")
        emitArrDec(v, t)

  /** Dec every boxed-var slot in a block scope. Each slot's alloca holds
    * a box pointer; loading it gives the heap address to release via
    * `__nex_env_dec` (the box was allocated by `__nex_env_alloc`).
    */
  protected def decBoxedScope(scope: mutable.LinkedHashMap[Int, String]): Unit =
    if currentBlock.isDefined then
      for (_, slot) <- scope do decBoxedSlot(slot)

  /** Dec a single boxed-var slot — load the box pointer from the alloca
    * and release the heap allocation.
    */
  protected def decBoxedSlot(slot: String): Unit =
    if currentBlock.isDefined then
      val bp = newReg()
      emitLine(s"  $bp = load ptr, ptr $slot\n")
      emitLine(s"  call void @__nex_env_dec(ptr $bp)\n")

  /** Push a fresh block scope; subsequent array bindings emit into it.
    * Pushes a matching boxed-var scope so escape-captured vars declared
    * inside the block are released at block exit.
    */
  protected def pushBlockScope(): Unit =
    blockArrayScopes = mutable.LinkedHashMap.empty[Int, (String, Type)] :: blockArrayScopes
    blockBoxedScopes = mutable.LinkedHashMap.empty[Int, String] :: blockBoxedScopes

  /** Pop the innermost block scope and return it so the caller can dec
    * its entries at the block-exit position. Also pops the parallel
    * boxed-var scope and dec's each boxed slot before returning.
    */
  protected def popBlockScope(): mutable.LinkedHashMap[Int, (String, Type)] =
    val topArr = blockArrayScopes.head
    blockArrayScopes = blockArrayScopes.tail
    val topBox = blockBoxedScopes.head
    blockBoxedScopes = blockBoxedScopes.tail
    decBoxedScope(topBox)
    topArr

  /** Register a refcounted (array or closure) binding's slot under the
    * appropriate scope: the innermost block if we're inside one,
    * otherwise the function-level [[arrayLocalSlots]] (which is the
    * right home for params). Despite the legacy name, the map tracks
    * any refcounted type — see [[isRefCountedType]].
    */
  protected def registerArraySlot(id: Int, slot: String, t: Type): Unit =
    blockArrayScopes match
      case head :: _ => head(id) = (slot, t)
      case Nil       => arrayLocalSlots(id) = (slot, t)

  /** Register a boxed-var binding's alloca (which holds the box pointer)
    * under the innermost block, or function-level when not inside a
    * block. Mirrors [[registerArraySlot]] but uses the parallel
    * box-tracking maps.
    */
  protected def registerBoxedSlot(id: Int, slot: String, t: Type): Unit =
    boxedVarTypes(id) = t
    blockBoxedScopes match
      case head :: _ => head(id) = slot
      case Nil       => boxedFunctionSlots(id) = slot

  /** Size in bytes of the box backing a captured-by-ref var. Mirrors
    * [[captureSlotSize]] in NexLLVMLambdas — kept consistent with how
    * an env field would size the same value.
    */
  protected def boxSizeBytes(t: Type): Int = t match
    case TyBool       => 1
    case TyInteger    => 8
    case TyReal       => 8
    case TyString     => 8
    case TyFunc(_, _) => 16
    case _            => elemSize(t)

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
      case ("!=", TyReal) => ("fcmp une", "i1") // IEEE: NaN != NaN is true; `une` = unordered OR not-equal
      case ("<",  TyReal) => ("fcmp olt", "i1")
      case ("<=", TyReal) => ("fcmp ole", "i1")
      case (">",  TyReal) => ("fcmp ogt", "i1")
      case (">=", TyReal) => ("fcmp oge", "i1")
      case _                => notYet(s"binop `$op` on `$opT`"); ("add", "i64")

/** Surfaces an unsupported codegen path from `notImpl`. Caught by the
  * CLI's compile driver so users see a clean diagnostic instead of a
  * silent miscompile, and by the test harness so any path that hits
  * `notImpl` fails loudly during the test run.
  */
class NexCodegenError(msg: String) extends RuntimeException(msg)
