package io.github.edadma.nex

import scala.collection.mutable

/** Expression evaluator and function-call mechanics for the tree-walking
  * interpreter. Hosts the big [[evalExpr]] dispatch (every `TExpr` shape
  * the elaborator can produce), the user-function-call path with `mut`-
  * parameter aliasing ([[callUserFunctionWithModes]]), the
  * `@intrinsic` body dispatcher ([[evalUserBody]] +
  * [[intrinsicDispatch]]), the L-value assignment walker
  * ([[assignLValue]]), and the scalar arithmetic / comparison /
  * conversion primitives that the binary-op handler dispatches into.
  *
  * Cross-trait calls reach into [[NexInterpArrays]] for indexing, field
  * access, element-wise / broadcast, the `mapArray` / `reduceArray`
  * helpers, view construction, struct cloning, and value formatting;
  * the host class supplies `globalEnv`, `structFields`,
  * `enumVariantInfo`, and `constructStruct`.
  */
protected trait NexInterpEval:
  self: NexInterpreter =>

  def evalExpr(e: TExpr, env: Env): Value = e match
    case TIntLit(v, _, _)     => VInt(v)
    case TRealLit(v, _, _)    => VReal(v)
    case TBoolLit(v, _, _)    => VBool(v)
    case TStringLit(v, _, _)  => VString(v)
    case TUnitLit(_)          => VUnit

    case TIntrinsic(opId, p, _) =>
      // A TIntrinsic node should only ever appear as the *body* of a
      // VUserFunc and be dispatched by evalUserBody before evalExpr sees
      // it. Reaching this case means an intrinsic was placed somewhere
      // the type system shouldn't allow (a value position in user code).
      trap(s"intrinsic `$opId` cannot be evaluated as a value", p)

    case TInterpStringLit(parts, _, _) =>
      val sb = new StringBuilder
      for p <- parts do p match
        case TInterpText(s)         => sb.append(s)
        case TInterpRef(sym, spec)  =>
          env.lookup(sym.id) match
            case Some(c) => sb.append(formatWithSpec(c.v, spec, e.pos))
            case None    => trap(s"interpolation: undefined `${sym.name}`", e.pos)
        case TInterpExpr(x, spec)   => sb.append(formatWithSpec(evalExpr(x, env), spec, e.pos))
        case TInterpRaw(raw, _)     =>
          // Only reached when the elaborator failed to parse `${raw}` and
          // accumulated the parse error; the program should already have
          // aborted before run-time. Emit something useful if we do hit it.
          sb.append("${").append(raw).append("}")
      VString(sb.toString)

    case TVarRef(sym, _, _) =>
      env.lookup(sym.id) match
        case Some(c) => c.v
        case None    => trap(s"undefined symbol `${sym.name}` (id=${sym.id})", e.pos)

    case TBinOp(op, l, r, p, _) =>
      // short-circuit logical ops
      op match
        case "and" =>
          val lv = evalExpr(l, env)
          lv match
            case VBool(false) => VBool(false)
            case VBool(true)  => evalExpr(r, env)
            case _             => trap(s"and: non-bool left operand", p)
        case "or" =>
          val lv = evalExpr(l, env)
          lv match
            case VBool(true)  => VBool(true)
            case VBool(false) => evalExpr(r, env)
            case _             => trap(s"or: non-bool left operand", p)
        case _ =>
          val lv = evalExpr(l, env)
          val rv = evalExpr(r, env)
          applyBinOp(op, lv, rv, p)

    case TUnaryOp(op, x, p, _) =>
      val v = evalExpr(x, env)
      op match
        case "-"   => negV(v, p)
        case "not" => v match
          case VBool(b) => VBool(!b)
          case _         => trap("not: non-bool operand", p)
        case _ => trap(s"unknown unary op `$op`", p)

    case TJuxtapose(c, b, p, _) =>
      // Stage 3 normally lowers TJuxtapose; treat any survivor as scalar mul.
      applyBinOp("*", evalExpr(c, env), evalExpr(b, env), p)

    case TElementWise(op, l, r, p, _) =>
      elementWise(op, evalExpr(l, env), evalExpr(r, env), p)

    case TBroadcast(scalar, arr, op, scalarFirst, p, _) =>
      broadcast(op, evalExpr(scalar, env), evalExpr(arr, env), scalarFirst, p)

    case TMap(arr, fn, _, _) =>
      mapArray(evalExpr(arr, env), evalExpr(fn, env).asInstanceOf[VFunc])

    case TReduce(arr, init, fn, _, _) =>
      reduceArray(evalExpr(arr, env), evalExpr(init, env), evalExpr(fn, env).asInstanceOf[VFunc])

    case TMatMul(l, r, p, _) =>
      matMul(evalExpr(l, env), evalExpr(r, env), p)

    case TFusedLoop(loopVar, length, body, cols, p, _) =>
      // Materialize an array by evaluating `body` once per flat i in
      // 0..length-1 with `loopVar` bound to i. cols=None → rank-1
      // (VArray1); cols=Some(c) → rank-2 (VArray2(rows=length/c, c)).
      // Introduced by NexFusion; the un-fused TElementWise/TBroadcast/
      // map paths are still valid and produce the same result.
      val n = evalExpr(length, env) match
        case VInt(v) => v.toInt
        case other   => trap(s"TFusedLoop: length not an integer, got ${formatValue(other)}", p)
      val out = mutable.ArrayBuffer.empty[Value]
      var i = 0
      while i < n do
        val frame = env.child
        frame.define(loopVar.id, VInt(i.toLong))
        out += evalExpr(body, frame)
        i += 1
      cols match
        case None => VArray1(out)
        case Some(cExpr) =>
          val c = evalExpr(cExpr, env) match
            case VInt(v) => v.toInt
            case other   => trap(s"TFusedLoop: cols not an integer, got ${formatValue(other)}", p)
          if c <= 0 then trap(s"TFusedLoop: cols must be positive, got $c", p)
          VArray2(out, n / c, c)

    case TFlatIndex(arr, idx, p, _) =>
      // Flat single-element access, rank-agnostic. Used inside fused-loop
      // bodies so the same loop shape works for rank-1 and rank-2 sources.
      val av = evalExpr(arr, env)
      val iv = evalExpr(idx, env) match
        case VInt(v) => v.toInt
        case other   => trap(s"TFlatIndex: idx not an integer, got ${formatValue(other)}", p)
      av match
        case VArray1(b)       => b(iv)
        case VArray2(b, _, _) => b(iv)
        case other            => trap(s"TFlatIndex: not an array: ${formatValue(other)}", p)

    case TCall(callee, args, p, _) =>
      // Struct construction: callee is a TypeName symbol.
      callee match
        case TVarRef(s, _, _) if s.kind == SymKind.TypeName =>
          constructStruct(s, args.map(evalExpr(_, env)), p)
        case TVarRef(s, _, _) if s.kind == SymKind.Prelude && s.name == "view" && (args.size == 2 || args.size == 3) =>
          // `view(a, lo..hi)`, `view(a, lo..hi, step)`, or
          // `view(m, rRange, cRange)` — bypass the normal materialising
          // call path. The range args are `TBinOp` nodes whose
          // evaluation would build throwaway arrays; we want the
          // bounds and inclusivity flags directly. The 3rd arg
          // disambiguates by AST shape (range vs integer) — rank-1
          // takes a stride, rank-2 takes a column range.
          val src = evalExpr(args(0), env)
          val (step, colRange) =
            if args.size == 3 then
              args(2) match
                case TBinOp(cop, clo, chi, _, _) if cop == ".." || cop == "..=" =>
                  val cLoV = asReal(evalExpr(clo, env)).toLong
                  val cHiV = asReal(evalExpr(chi, env)).toLong
                  (1, Some((cLoV, cHiV, cop == "..=")))
                case stepE =>
                  (asReal(evalExpr(stepE, env)).toInt, None)
            else (1, None)
          args(1) match
            case TBinOp(op, lo, hi, _, _) if op == ".." || op == "..=" =>
              val loV = asReal(evalExpr(lo, env)).toLong
              val hiV = asReal(evalExpr(hi, env)).toLong
              buildView(src, loV, hiV, inclusive = op == "..=", step, colRange, p)
            case other =>
              trap(s"view: second arg must be a range, got ${other.tpe}", p)
        case _ =>
          val cv = evalExpr(callee, env)
          cv match
            case uf: VUserFunc =>
              // Route user-function calls through the mode-aware path so
              // `mut` parameters get aliased to the caller's storage.
              // The call-site mode check has already validated that
              // mut-position args are l-value-rooted.
              callUserFunctionWithModes(uf, args, callee.tpe, env, p)
            case bf: VBuiltin =>
              callFunction(bf, args.map(evalExpr(_, env)), p)
            case ec: VEnumCtor =>
              callFunction(ec, args.map(evalExpr(_, env)), p)
            case other =>
              trap(s"call: not a function: ${formatValue(other)}", p)

    case TIndex(arr, idx, p, _) =>
      val av = evalExpr(arr, env)
      val iv = idx.map(evalExpr(_, env))
      indexGet(av, iv, p)

    case TSlice2(arr, rowAx, colAx, p, _) =>
      // Spec §4.14 rank-2 slice. Each axis is either:
      //   - TAxisAll: keep the full extent of this axis.
      //   - TAxisIndex(e): collapse this axis to a single position.
      //   - TAxisRange(lo, hi, inclusive): keep a sub-extent.
      // Result rank = number of non-collapsed axes; freshly-owned.
      val av = evalExpr(arr, env)
      val (rows, cols, buf) = av match
        case VArray2(b, r, c) => (r, c, b)
        case other            => trap(s"rank-2 slice requires a rank-2 array, got ${formatValue(other)}", p)

      def resolveAxis(spec: TAxisSpec, extent: Int, label: String): (Int, Int, Boolean) =
        // returns (lo, hi_exclusive, collapsed). Negative axis indices
        // and slice bounds wrap by `+ extent` before the bounds check
        // (spec §4.14) so `m[-1, :]` is the last row and `m[-3..-1, :]`
        // is rows len-3 and len-2.
        spec match
          case TAxisAll => (0, extent, false)
          case TAxisIndex(e) =>
            evalExpr(e, env) match
              case VInt(i) =>
                val k = wrapNeg(i, extent).toInt
                if k < 0 || k >= extent then trap(s"$label index $i out of bounds for extent $extent", p)
                (k, k + 1, true)
              case other => trap(s"$label index must be integer, got ${formatValue(other)}", p)
          case TAxisRange(lo, hi, inclusive, stride) =>
            if stride.isDefined then
              trap("strided rank-2 axis range — chunk-2 follow-up", p)
            def axisBound(o: Option[TExpr], default: Int): Long = o match
              case None    => default.toLong
              case Some(e) =>
                evalExpr(e, env) match
                  case VInt(x) => x
                  case other   => trap(s"$label slice bounds must be integers, got ${formatValue(other)}", p)
            val l     = axisBound(lo, 0)
            val h     = axisBound(hi, extent)
            val lI    = wrapNeg(l.toInt, extent).toInt
            val hI    = wrapNeg(h.toInt, extent).toInt
            val hExcl = if inclusive then hI + 1 else hI
            if lI < 0 || hExcl > extent || lI > hExcl then
              trap(s"$label slice [$l..${if inclusive then "=" else ""}${h}] out of bounds for extent $extent", p)
            (lI, hExcl, false)

      val (rLo, rHi, rCollapsed) = resolveAxis(rowAx, rows, "row")
      val (cLo, cHi, cCollapsed) = resolveAxis(colAx, cols, "col")
      val outRows = rHi - rLo
      val outCols = cHi - cLo

      // Materialize the sub-buffer in row-major order (regardless of
      // spec-§3.3 column-major external storage — that reconciliation
      // lives in flatten/reshape; here we just produce a fresh array).
      val out = mutable.ArrayBuffer.empty[Value]
      var r = rLo
      while r < rHi do
        var c = cLo
        while c < cHi do
          out += buf(r * cols + c)
          c += 1
        r += 1

      (rCollapsed, cCollapsed) match
        case (true, true)   => out.head                              // both collapsed → scalar (rare via this path)
        case (true, false)  => VArray1(out)                          // row collapsed → rank-1 of cols
        case (false, true)  => VArray1(out)                          // col collapsed → rank-1 of rows
        case (false, false) => VArray2(out, outRows, outCols)

    case _: TAxisAllMark =>
      trap("internal: TAxisAllMark survived to interpreter; should be Stage-2-only", e.pos)

    case _: TOpenSliceMark =>
      trap("internal: TOpenSliceMark survived to interpreter; should be Stage-2-only", e.pos)

    case TClone(arr, p, _) =>
      // Spec §8.3: deep-copy an array. Inserted by NexLifetime at move
      // sites where the source has a later use. The result is a freshly-
      // owned VArray1 / VArray2 with element-by-element copies (scalar
      // values clone trivially; nested arrays would recurse but aren't a
      // v0 surface).
      evalExpr(arr, env) match
        case VArray1(b)       => VArray1(b.clone())
        case VArray2(b, r, c) => VArray2(b.clone(), r, c)
        case other            => other

    case TSlice(arr, lo, hi, inclusive, stride, p, _) =>
      // Spec §4.14: rank-1 slice. Half-open `lo..hi` or closed
      // `lo..=hi`. Negative bounds count from the end; over-negative
      // input still traps. Open bounds fill from runtime extent
      // (0 / length). An optional `by k` stride samples every k-th
      // element from the selected window — stride must be a positive
      // integer; ≤ 0 traps. The result is a fresh VArray1.
      val av = evalExpr(arr, env)
      def asIntBound(o: Option[TExpr], default: Int): Int = o match
        case None    => default
        case Some(e) =>
          evalExpr(e, env) match
            case VInt(x) => x.toInt
            case other   => trap(s"slice bounds must be integers, got ${formatValue(other)}", p)
      val strideI = stride match
        case None    => 1
        case Some(e) => evalExpr(e, env) match
          case VInt(x) => x.toInt
          case other   => trap(s"slice stride must be an integer, got ${formatValue(other)}", p)
      if strideI <= 0 then trap(s"slice stride must be positive, got $strideI", p)
      av match
        case VArray1(b) =>
          val loRaw = asIntBound(lo, 0)
          val hiRaw = asIntBound(hi, b.size)
          val loI   = wrapNeg(loRaw, b.size).toInt
          val hiI   = wrapNeg(hiRaw, b.size).toInt
          val upper = if inclusive then hiI + 1 else hiI
          if loI < 0 || upper > b.size || loI > upper then
            trap(s"slice [$loRaw..${if inclusive then "=" else ""}$hiRaw] out of bounds for array of size ${b.size}", p)
          if strideI == 1 then
            VArray1(b.slice(loI, upper).to(mutable.ArrayBuffer))
          else
            val out = mutable.ArrayBuffer.empty[Value]
            var k   = loI
            while k < upper do
              out += b(k)
              k += strideI
            VArray1(out)
        case other =>
          trap(s"rank-1 slice requires a rank-1 array, got ${formatValue(other)}", p)

    case TField(receiver, name, p, _) =>
      val rv = evalExpr(receiver, env)
      fieldGet(rv, name, p)

    case TTupleProj(receiver, idx, p, _) =>
      evalExpr(receiver, env) match
        case VTuple(es) if idx < es.size => es(idx)
        case VTuple(es) =>
          trap(s"tuple projection index $idx out of bounds (size ${es.size})", p)
        case other => trap(s"tuple projection on non-tuple value: ${formatValue(other)}", p)

    case TMethodCall(_, name, _, p, _) =>
      // Stage 3 should have lowered this; if not, it's a bug.
      trap(s"interpreter: un-lowered method call `$name`", p)

    case TLambda(params, body, _, _) =>
      VUserFunc(params, body, env)

    case TTuple(elems, _, _) =>
      VTuple(elems.map(evalExpr(_, env)))

    case TArrayLit(elems, _, _) =>
      val vs = elems.map(evalExpr(_, env))
      // rank-2 if every element is itself a VArray1 of the same length
      vs match
        case (VArray1(_) :: _) if vs.forall(_.isInstanceOf[VArray1]) =>
          val rows = vs.map(_.asInstanceOf[VArray1].buf)
          val r    = rows.size
          val c    = rows.head.size
          if rows.exists(_.size != c) then trap("rank-2 array: row length mismatch", None)
          val flat = mutable.ArrayBuffer.empty[Value]
          for row <- rows; v <- row do flat += v
          VArray2(flat, r, c)
        case _ =>
          VArray1(mutable.ArrayBuffer.from(vs))

    case TIf(cond, th, el, p, _) =>
      evalExpr(cond, env) match
        case VBool(true)  => evalExpr(th, env)
        case VBool(false) => el match
          case Some(e2) => evalExpr(e2, env)
          case None     => VUnit
        case other => trap(s"if: non-bool condition: ${formatValue(other)}", p)

    case TFor(loopVars, iter, body, p, _) =>
      val it = evalExpr(iter, env)
      it match
        case VArray1(buf) =>
          val inner = env.child
          // Pre-define each loop var so we can rebind in-place each iteration.
          val cells = loopVars.map(s => inner.define(s.id, VUnit))
          for v <- buf do
            loopVars match
              case s :: Nil => cells.head.v = v
              case _        =>
                v match
                  case VTuple(es) if es.size == loopVars.size =>
                    cells.zip(es).foreach { case (c, x) => c.v = x }
                  case _ =>
                    trap(s"for: tuple destructuring mismatch", p)
            evalExpr(body, inner)
          VUnit
        case other => trap(s"for: iterable is not an array: ${formatValue(other)}", p)

    case TWhile(cond, body, p, _) =>
      var run = true
      while run do
        evalExpr(cond, env) match
          case VBool(true)  => evalExpr(body, env)
          case VBool(false) => run = false
          case other         => trap(s"while: non-bool condition: ${formatValue(other)}", p)
      VUnit

    case TReturn(v, _, _) =>
      val rv = v.map(evalExpr(_, env)).getOrElse(VUnit)
      throw new ReturnException(rv)

    case TAssign(target, value, p, _) =>
      val rhs = evalExpr(value, env)
      assignLValue(target, rhs, env, p)
      VUnit

    case TBlock(items, result, _, _) =>
      val inner = env.child
      for it <- items do it match
        case TBlockBinding(sym, _, v) =>
          // Clone struct values at the binding site so two bindings
          // never alias the same field map. Spec §4.15's "field
          // reassignment requires var" + the AOT's value-typed
          // struct ABI together mean `var c = b; c.v = 99` must
          // not affect `b`. Arrays inside the struct also clone so
          // nested mutation can't leak across the copy.
          inner.define(sym.id, cloneStructValue(evalExpr(v, inner)))
        case TBlockExpr(x) =>
          evalExpr(x, inner)
      evalExpr(result, inner)

    case TMatch(scrutinee, cases, p, _) =>
      val sv = evalExpr(scrutinee, env)
      // Try each arm in source order. A successful match returns the
      // arm's value; a successful pattern with a body that traps still
      // throws (we don't catch). Falling off the end means
      // exhaustiveness checking missed something — that's a compiler
      // bug, so trap defensively.
      val it = cases.iterator
      var result: Option[Value] = None
      while result.isEmpty && it.hasNext do
        val c = it.next()
        val arm = env.child
        if matchPattern(c.pat, sv, arm) then
          result = Some(evalExpr(c.body, arm))
      result.getOrElse(trap(s"non-exhaustive match on ${formatValue(sv)}", p))

  /** Try a pattern against a scrutinee value, binding the pattern's
    * fresh symbols into `arm` when the match succeeds. Returns true on
    * a successful match. Variant patterns dispatch on the runtime tag
    * (Symbol id lookup in [[enumVariantInfo]]) and recurse into the
    * payload's fields.
    */
  protected def matchPattern(p: TPattern, v: Value, arm: Env): Boolean = p match
    case TWildcardPat(_) => true
    case TVarPat(sym, _) =>
      arm.define(sym.id, cloneStructValue(v))
      true
    case TVariantPat(vs, args, _) =>
      enumVariantInfo.get(vs.id) match
        case None =>
          // Variant info missing — must be a malformed program. Treat as no-match.
          false
        case Some((_, idx, _)) =>
          v match
            case VEnum(_, _, vidx, fields) if vidx == idx =>
              if args.size != fields.size then false
              else
                args.zip(fields).forall { case (sp, fv) => matchPattern(sp, fv, arm) }
            case _ => false

  // --------------------------------------------------------------------------
  // Function call / struct construction
  // --------------------------------------------------------------------------

  private[nex] def callFunction(f: VFunc, args: List[Value], p: Option[scala.util.parsing.input.Position]): Value =
    f match
      case VBuiltin(_, fn) => fn(args)
      case VEnumCtor(en, vn, idx, fs) =>
        if fs.size != args.size then
          trap(s"variant `$en.$vn` expects ${fs.size} fields, got ${args.size}", p)
        VEnum(en, vn, idx, args)
      case VUserFunc(params, body, env) =>
        if params.size != args.size then
          trap(s"arity mismatch: function expects ${params.size}, got ${args.size}", p)
        val frame = env.child
        params.zip(args).foreach { case (s, v) => frame.define(s.id, v) }
        evalUserBody(body, params, frame, p)

  /** Evaluate the body of a `VUserFunc`. For ordinary functions this is
    * just `evalExpr(body, frame)` with the `ReturnException` catch. For an
    * `@intrinsic` function — body is a [[TIntrinsic]] — the param values
    * are pulled from `frame` (already bound by the caller) and handed to
    * the matching entry in [[intrinsicDispatch]].
    */
  protected def evalUserBody(
      body: TExpr,
      params: List[Symbol],
      frame: Env,
      p: Option[scala.util.parsing.input.Position],
  ): Value =
    body match
      case TIntrinsic(opId, _, _) =>
        NexIntrinsics.require(opId)
        val argVals = params.map { ps =>
          frame.lookup(ps.id).map(_.v).getOrElse(
            trap(s"intrinsic `$opId`: parameter `${ps.name}` unbound", p),
          )
        }
        intrinsicDispatch.get(opId) match
          case Some(impl) => impl(argVals, p)
          case None       => trap(s"intrinsic `$opId` has no interpreter implementation", p)
      case _ =>
        try evalExpr(body, frame)
        catch case r: ReturnException => r.value

  /** Per-backend dispatch table. The keys must be a subset of
    * [[NexIntrinsics.Ids]]; new intrinsics need an entry here AND in the
    * codegen backends.
    */
  protected val intrinsicDispatch: Map[String, (List[Value], Option[scala.util.parsing.input.Position]) => Value] =
    Map(
      "test.identity" -> { (args, p) =>
        if args.size != 1 then trap(s"test.identity: expected 1 arg, got ${args.size}", p)
        args.head
      },
      "libm.cbrt"  -> realUnary("libm.cbrt",  math.cbrt),
      "libm.floor" -> realUnary("libm.floor", math.floor),
      "libm.ceil"  -> realUnary("libm.ceil",  math.ceil),
      "libm.round" -> realUnary("libm.round", x => math.round(x).toDouble),
      "libm.trunc" -> realUnary("libm.trunc", x => if x < 0 then math.ceil(x) else math.floor(x)),
      "libm.asin"  -> realUnary("libm.asin",  math.asin),
      "libm.acos"  -> realUnary("libm.acos",  math.acos),
      "libm.atan"  -> realUnary("libm.atan",  math.atan),
      "libm.sinh"  -> realUnary("libm.sinh",  math.sinh),
      "libm.cosh"  -> realUnary("libm.cosh",  math.cosh),
      "libm.tanh"  -> realUnary("libm.tanh",  math.tanh),
      "libm.asinh" -> realUnary("libm.asinh", x => math.log(x + math.sqrt(x * x + 1.0))),
      "libm.acosh" -> realUnary("libm.acosh", x => math.log(x + math.sqrt(x * x - 1.0))),
      "libm.atanh" -> realUnary("libm.atanh", x => 0.5 * math.log((1.0 + x) / (1.0 - x))),
      "libm.log2"  -> realUnary("libm.log2",  x => math.log(x) / math.log(2.0)),
      "libm.log10" -> realUnary("libm.log10", math.log10),
      "libm.atan2" -> { (args, p) =>
        args match
          case List(VReal(y), VReal(x)) => VReal(math.atan2(y, x))
          case _ => trap(s"libm.atan2: expected (real, real), got ${args.map(formatValue).mkString(", ")}", p)
      },
      "libm.hypot" -> { (args, p) =>
        args match
          case List(VReal(x), VReal(y)) => VReal(math.hypot(x, y))
          case _ => trap(s"libm.hypot: expected (real, real), got ${args.map(formatValue).mkString(", ")}", p)
      },
      "libm.sqrt"  -> realUnary("libm.sqrt", math.sqrt),
      "libm.exp"   -> realUnary("libm.exp",  math.exp),
      "libm.log"   -> realUnary("libm.log",  math.log),
      "libm.sin"   -> realUnary("libm.sin",  math.sin),
      "libm.cos"   -> realUnary("libm.cos",  math.cos),
      "libm.tan"   -> realUnary("libm.tan",  math.tan),
    )

  /** Bridge a unary real → real libm function into the intrinsic
    * dispatch table. Reports a typed trap if a non-real argument
    * arrives (the elaborator's signature check should make this
    * unreachable from user code; the guard catches codegen drift).
    */
  private def realUnary(
      opId: String,
      f:    Double => Double,
  ): (List[Value], Option[scala.util.parsing.input.Position]) => Value =
    (args, p) =>
      args match
        case List(VReal(x)) => VReal(f(x))
        case _              =>
          trap(s"$opId: expected real argument, got ${args.map(formatValue).mkString(", ")}", p)

  /** Mode-aware user-function call. For each `mut` parameter whose
    * call-site argument is a [[TVarRef]] (or projection thereof) the
    * callee's parameter Cell is aliased to the caller's storage, so the
    * callee's `param = expr` writes through to the caller's variable.
    * Read-mode parameters and non-l-value mut args (which the elaborator's
    * call-site check forbids in well-formed programs) fall back to the
    * by-value path.
    *
    * Only direct `TVarRef` mut args take the by-ref path today. Struct-
    * field / array-index / tuple-projection mut args (which the elaborator
    * accepts when rooted at a var/mut) fall back to by-value — full by-ref
    * for those slot kinds needs a proper LValueRef abstraction that v0
    * doesn't provide.
    */
  protected def callUserFunctionWithModes(
      f: VUserFunc,
      args: List[TExpr],
      calleeTpe: Type,
      callerEnv: Env,
      p: Option[scala.util.parsing.input.Position],
  ): Value =
    if f.params.size != args.size then
      trap(s"arity mismatch: function expects ${f.params.size}, got ${args.size}", p)

    val modes: List[ParamMode] = calleeTpe match
      case TyFunc(ps, _) if ps.size == args.size => ps.map(_._2)
      case _                                      => List.fill(args.size)(ParamMode.Read)

    val frame = f.env.child
    f.params.zip(args).zip(modes).foreach { case ((param, argExpr), mode) =>
      mode match
        case ParamMode.Mut =>
          argExpr match
            case TVarRef(s, _, _) =>
              callerEnv.lookup(s.id) match
                case Some(cell) => frame.bind(param.id, cell)
                case None       => frame.define(param.id, evalExpr(argExpr, callerEnv))
            case _ =>
              // Slot-into-struct / array element / tuple-proj fall back to
              // by-value (no LValueRef abstraction in v0).
              frame.define(param.id, evalExpr(argExpr, callerEnv))
        case ParamMode.Read =>
          frame.define(param.id, evalExpr(argExpr, callerEnv))
    }
    evalUserBody(f.body, f.params, frame, p)

  // --------------------------------------------------------------------------
  // L-value assignment
  // --------------------------------------------------------------------------

  protected def assignLValue(target: TExpr, rhs: Value, env: Env, p: Option[scala.util.parsing.input.Position]): Unit =
    target match
      case TVarRef(sym, _, _) =>
        env.lookup(sym.id) match
          case Some(c) => c.v = rhs
          case None    => trap(s"assign: undefined `${sym.name}`", p)

      case TField(r, name, _, _) =>
        evalExpr(r, env) match
          case VStruct(_, fields) =>
            if !fields.contains(name) then trap(s"assign: no field `$name`", p)
            fields(name) = rhs
          case VComplex(re, im) =>
            // Cannot rebind components of a complex through TField (they're
            // read-only views of a value type). Trap.
            trap(s"cannot assign to `$name` of a complex value", p)
          case other => trap(s"assign field: not a struct: ${formatValue(other)}", p)

      case TIndex(arr, idx, _, _) =>
        val av = evalExpr(arr, env)
        val iv = idx.map(evalExpr(_, env))
        indexSet(av, iv, rhs, p)

      case TSlice(arr, lo, hi, inclusive, stride, _, _) =>
        val av = evalExpr(arr, env)
        def asIntBound(o: Option[TExpr], default: Int): Int = o match
          case None    => default
          case Some(e) =>
            evalExpr(e, env) match
              case VInt(x) => x.toInt
              case other   => trap(s"slice bounds must be integers, got ${formatValue(other)}", p)
        val strideI = stride match
          case None    => 1
          case Some(e) => evalExpr(e, env) match
            case VInt(x) => x.toInt
            case other   => trap(s"slice stride must be an integer, got ${formatValue(other)}", p)
        if strideI <= 0 then trap(s"slice stride must be positive, got $strideI", p)
        av match
          case VArray1(b) =>
            val loRaw = asIntBound(lo, 0)
            val hiRaw = asIntBound(hi, b.size)
            val loI   = wrapNeg(loRaw, b.size).toInt
            val hiI   = wrapNeg(hiRaw, b.size).toInt
            val upper = if inclusive then hiI + 1 else hiI
            if loI < 0 || upper > b.size || loI > upper then
              trap(s"slice-assign [$loRaw..${if inclusive then "=" else ""}$hiRaw] out of bounds for array of size ${b.size}", p)
            val span     = upper - loI
            // Strided length matches the corresponding read: ceil(span / k).
            val sliceLen = if strideI == 1 then span else (math.max(0, span) + strideI - 1) / strideI
            rhs match
              case VArray1(src) =>
                if src.size != sliceLen then
                  trap(s"slice-assign: length mismatch — rhs length ${src.size}, slice length $sliceLen", p)
                var i = 0
                while i < sliceLen do
                  b(loI + i * strideI) = src(i)
                  i += 1
              case _ =>
                trap(s"slice-assign: rhs must be a rank-1 array, got ${formatValue(rhs)}", p)
          case other =>
            trap(s"rank-1 slice-assign requires a rank-1 array, got ${formatValue(other)}", p)

      case TSlice2(arr, rowAx, colAx, _, _) =>
        val av = evalExpr(arr, env)
        val (rows, cols, buf) = av match
          case VArray2(b, r, c) => (r, c, b)
          case other            => trap(s"rank-2 slice-assign requires a rank-2 array, got ${formatValue(other)}", p)

        def resolveAxis(spec: TAxisSpec, extent: Int, label: String): (Int, Int, Boolean) =
          spec match
            case TAxisAll => (0, extent, false)
            case TAxisIndex(e) =>
              evalExpr(e, env) match
                case VInt(i) =>
                  val k = wrapNeg(i, extent).toInt
                  if k < 0 || k >= extent then trap(s"$label index $i out of bounds for extent $extent", p)
                  (k, k + 1, true)
                case other => trap(s"$label index must be integer, got ${formatValue(other)}", p)
            case TAxisRange(lo, hi, inclusive, stride) =>
              if stride.isDefined then
                trap("strided rank-2 axis range in slice-assign — chunk 2", p)
              def axisBound(o: Option[TExpr], default: Int): Long = o match
                case None    => default.toLong
                case Some(e) =>
                  evalExpr(e, env) match
                    case VInt(x) => x
                    case other   => trap(s"$label slice-assign bounds must be integers, got ${formatValue(other)}", p)
              val l     = axisBound(lo, 0)
              val h     = axisBound(hi, extent)
              val lI    = wrapNeg(l.toInt, extent).toInt
              val hI    = wrapNeg(h.toInt, extent).toInt
              val hExcl = if inclusive then hI + 1 else hI
              if lI < 0 || hExcl > extent || lI > hExcl then
                trap(s"$label slice-assign [$l..${if inclusive then "=" else ""}$h] out of bounds for extent $extent", p)
              (lI, hExcl, false)

        val (rLo, rHi, rCollapsed) = resolveAxis(rowAx, rows, "row")
        val (cLo, cHi, cCollapsed) = resolveAxis(colAx, cols, "col")
        val outRows = rHi - rLo
        val outCols = cHi - cLo

        // Result rank of the LHS matches what TSlice2's read path would produce.
        (rCollapsed, cCollapsed) match
          case (true, true) =>
            // Both axes collapsed: this never reaches TSlice2 from the
            // parser (two integer indices route through TIndex). Defensive.
            buf(rLo * cols + cLo) = rhs
          case (true, false) | (false, true) =>
            val sliceLen = outRows * outCols
            rhs match
              case VArray1(src) =>
                if src.size != sliceLen then
                  trap(s"slice-assign: length mismatch — rhs length ${src.size}, slice length $sliceLen", p)
                var k = 0
                var r = rLo
                while r < rHi do
                  var c = cLo
                  while c < cHi do
                    buf(r * cols + c) = src(k)
                    k += 1
                    c += 1
                  r += 1
              case _ =>
                trap(s"slice-assign: rhs must be a rank-1 array, got ${formatValue(rhs)}", p)
          case (false, false) =>
            rhs match
              case VArray2(src, sr, sc) =>
                if sr != outRows || sc != outCols then
                  trap(s"slice-assign: shape mismatch — rhs shape ${sr}x${sc}, slice shape ${outRows}x${outCols}", p)
                var i = 0
                while i < outRows do
                  var j = 0
                  while j < outCols do
                    buf((rLo + i) * cols + (cLo + j)) = src(i * sc + j)
                    j += 1
                  i += 1
              case _ =>
                trap(s"slice-assign: rhs must be a rank-2 array of shape ${outRows}x${outCols}, got ${formatValue(rhs)}", p)

      case other => trap(s"invalid assignment target", p)

  // --------------------------------------------------------------------------
  // Arithmetic, comparison, conversions
  // --------------------------------------------------------------------------

  protected def applyBinOp(op: String, l: Value, r: Value, p: Option[scala.util.parsing.input.Position]): Value =
    op match
      case "+"   => addV(l, r)
      case "-"   => subV(l, r)
      case "*"   => mulV(l, r)
      case "/"   => divV(l, r, p)
      case "div" => idivV(l, r, p)
      case "%"   => modV(l, r, p)
      case "^"   => powV(l, r)
      case "=="  => VBool(valueEq(l, r))
      case "!="  => VBool(!valueEq(l, r))
      // Ordered comparisons follow IEEE-754: every relational op with
      // NaN on either side returns false. `cmpNum` is the total-order
      // helper used by min/max where NaN handling is implementation-
      // defined; using it for `<`/`<=`/`>`/`>=` would silently treat
      // NaN as the largest value, contradicting the spec.
      case "<"   => VBool(ordLt(l, r))
      case "<="  => VBool(ordLe(l, r))
      case ">"   => VBool(ordGt(l, r))
      case ">="  => VBool(ordGe(l, r))
      case ".."  => rangeV(l, r, inclusive = false, p)
      case "..=" => rangeV(l, r, inclusive = true, p)
      case _     => trap(s"unknown binary op `$op`", p)

  protected def addV(a: Value, b: Value): Value = (a, b) match
    case (VInt(x), VInt(y))         => VInt(x + y)
    case (VInt(x), VReal(y))        => VReal(x + y)
    case (VReal(x), VInt(y))        => VReal(x + y)
    case (VReal(x), VReal(y))       => VReal(x + y)
    case (VComplex(r, i), VInt(y))  => VComplex(r + y, i)
    case (VInt(x), VComplex(r, i))  => VComplex(x + r, i)
    case (VComplex(r, i), VReal(y)) => VComplex(r + y, i)
    case (VReal(x), VComplex(r, i)) => VComplex(x + r, i)
    case (VComplex(r1, i1), VComplex(r2, i2)) => VComplex(r1 + r2, i1 + i2)
    case (VString(x), y)            => VString(x + formatValue(y))
    case (x, VString(y))            => VString(formatValue(x) + y)
    case _ => throw new NexTrap(s"cannot add ${formatValue(a)} and ${formatValue(b)}", None)

  protected def subV(a: Value, b: Value): Value = (a, b) match
    case (VInt(x), VInt(y))         => VInt(x - y)
    case (VInt(x), VReal(y))        => VReal(x - y)
    case (VReal(x), VInt(y))        => VReal(x - y)
    case (VReal(x), VReal(y))       => VReal(x - y)
    case (VComplex(r, i), VInt(y))  => VComplex(r - y, i)
    case (VInt(x), VComplex(r, i))  => VComplex(x - r, -i)
    case (VComplex(r, i), VReal(y)) => VComplex(r - y, i)
    case (VReal(x), VComplex(r, i)) => VComplex(x - r, -i)
    case (VComplex(r1, i1), VComplex(r2, i2)) => VComplex(r1 - r2, i1 - i2)
    case _ => throw new NexTrap(s"cannot subtract ${formatValue(b)} from ${formatValue(a)}", None)

  protected def mulV(a: Value, b: Value): Value = (a, b) match
    case (VInt(x), VInt(y))         => VInt(x * y)
    case (VInt(x), VReal(y))        => VReal(x * y)
    case (VReal(x), VInt(y))        => VReal(x * y)
    case (VReal(x), VReal(y))       => VReal(x * y)
    case (VComplex(r, i), VInt(y))  => VComplex(r * y, i * y)
    case (VInt(x), VComplex(r, i))  => VComplex(x * r, x * i)
    case (VComplex(r, i), VReal(y)) => VComplex(r * y, i * y)
    case (VReal(x), VComplex(r, i)) => VComplex(x * r, x * i)
    case (VComplex(r1, i1), VComplex(r2, i2)) =>
      VComplex(r1 * r2 - i1 * i2, r1 * i2 + i1 * r2)
    case _ => throw new NexTrap(s"cannot multiply ${formatValue(a)} and ${formatValue(b)}", None)

  protected def divV(a: Value, b: Value, p: Option[scala.util.parsing.input.Position]): Value =
    // `/` is real-division on numerics; int/int → real.
    (a, b) match
      case (_, VInt(0))            => trap("division by zero", p)
      case (_, VReal(0.0))         => trap("division by zero", p)
      case (VInt(x), VInt(y))      => VReal(x.toDouble / y.toDouble)
      case (VInt(x), VReal(y))     => VReal(x / y)
      case (VReal(x), VInt(y))     => VReal(x / y)
      case (VReal(x), VReal(y))    => VReal(x / y)
      case (VComplex(_, _), _) | (_, VComplex(_, _)) => complexDiv(a, b)
      case _ => trap(s"cannot divide ${formatValue(a)} by ${formatValue(b)}", p)

  private def complexDiv(a: Value, b: Value): Value =
    val (ar, ai) = toComplexParts(a)
    val (br, bi) = toComplexParts(b)
    val denom    = br * br + bi * bi
    if denom == 0.0 then throw new NexTrap("complex division by zero", None)
    VComplex((ar * br + ai * bi) / denom, (ai * br - ar * bi) / denom)

  private def toComplexParts(v: Value): (Double, Double) = v match
    case VInt(x)         => (x.toDouble, 0.0)
    case VReal(x)        => (x, 0.0)
    case VComplex(r, i)  => (r, i)
    case _               => throw new NexTrap(s"not numeric: ${formatValue(v)}", None)

  protected def idivV(a: Value, b: Value, p: Option[scala.util.parsing.input.Position]): Value = (a, b) match
    case (_, VInt(0))           => trap("integer division by zero", p)
    case (VInt(x), VInt(y))     => VInt(x / y)
    case _ => trap(s"`div` requires integer operands, got ${formatValue(a)}, ${formatValue(b)}", p)

  protected def modV(a: Value, b: Value, p: Option[scala.util.parsing.input.Position]): Value = (a, b) match
    case (_, VInt(0))           => trap("`%` by zero", p)
    case (VInt(x), VInt(y))     => VInt(x % y)
    case _ => trap(s"`%` requires integer operands", p)

  protected def powV(a: Value, b: Value): Value = (a, b) match
    case (VInt(x), VInt(y)) if y >= 0 => VInt(intPow(x, y))
    case _ =>
      val ar = asReal(a); val br = asReal(b)
      VReal(math.pow(ar, br))

  private def intPow(base: Long, exp: Long): Long =
    var result: Long = 1L
    var b = base; var e = exp
    while e > 0 do
      if (e & 1L) == 1L then result *= b
      e >>= 1
      if e > 0 then b *= b
    result

  protected def negV(v: Value, p: Option[scala.util.parsing.input.Position]): Value = v match
    case VInt(x)        => VInt(-x)
    case VReal(x)       => VReal(-x)
    case VComplex(r, i) => VComplex(-r, -i)
    case VArray1(buf)   => VArray1(buf.map(x => negV(x, p)))
    case VArray2(b, r, c) => VArray2(b.map(x => negV(x, p)), r, c)
    case _              => trap(s"cannot negate ${formatValue(v)}", p)

  protected def cmpNum(a: Value, b: Value): Int = (a, b) match
    case (VInt(x), VInt(y))   => java.lang.Long.compare(x, y)
    case (VInt(x), VReal(y))  => java.lang.Double.compare(x.toDouble, y)
    case (VReal(x), VInt(y))  => java.lang.Double.compare(x, y.toDouble)
    case (VReal(x), VReal(y)) => java.lang.Double.compare(x, y)
    case _ => throw new NexTrap(s"cannot order ${formatValue(a)} and ${formatValue(b)}", None)

  /** IEEE-754 ordered relational helpers — used by the spec's `<` / `<=` /
    * `>` / `>=` operators. Any comparison with NaN returns false; `-0.0`
    * and `+0.0` compare equal (neither less than nor greater than). For
    * integer-only operand pairs we delegate to Scala's primitive
    * comparisons which match IEEE for the int-to-double promotion path.
    */
  private def ordLt(a: Value, b: Value): Boolean = (a, b) match
    case (VInt(x), VInt(y))   => x < y
    case (VInt(x), VReal(y))  => x.toDouble < y
    case (VReal(x), VInt(y))  => x < y.toDouble
    case (VReal(x), VReal(y)) => x < y
    case _ => throw new NexTrap(s"cannot order ${formatValue(a)} and ${formatValue(b)}", None)

  private def ordLe(a: Value, b: Value): Boolean = (a, b) match
    case (VInt(x), VInt(y))   => x <= y
    case (VInt(x), VReal(y))  => x.toDouble <= y
    case (VReal(x), VInt(y))  => x <= y.toDouble
    case (VReal(x), VReal(y)) => x <= y
    case _ => throw new NexTrap(s"cannot order ${formatValue(a)} and ${formatValue(b)}", None)

  private def ordGt(a: Value, b: Value): Boolean = (a, b) match
    case (VInt(x), VInt(y))   => x > y
    case (VInt(x), VReal(y))  => x.toDouble > y
    case (VReal(x), VInt(y))  => x > y.toDouble
    case (VReal(x), VReal(y)) => x > y
    case _ => throw new NexTrap(s"cannot order ${formatValue(a)} and ${formatValue(b)}", None)

  private def ordGe(a: Value, b: Value): Boolean = (a, b) match
    case (VInt(x), VInt(y))   => x >= y
    case (VInt(x), VReal(y))  => x.toDouble >= y
    case (VReal(x), VInt(y))  => x >= y.toDouble
    case (VReal(x), VReal(y)) => x >= y
    case _ => throw new NexTrap(s"cannot order ${formatValue(a)} and ${formatValue(b)}", None)

  protected def valueEq(a: Value, b: Value): Boolean = (a, b) match
    case (VInt(x), VInt(y))     => x == y
    case (VReal(x), VReal(y))   => x == y
    case (VInt(x), VReal(y))    => x.toDouble == y
    case (VReal(x), VInt(y))    => x == y.toDouble
    case (VBool(x), VBool(y))   => x == y
    case (VString(x), VString(y)) => x == y
    case (VComplex(r1, i1), VComplex(r2, i2)) => r1 == r2 && i1 == i2
    case (VComplex(r, i), VInt(y))  => i == 0 && r == y.toDouble
    case (VComplex(r, i), VReal(y)) => i == 0 && r == y
    case (VInt(x), VComplex(r, i))  => i == 0 && r == x.toDouble
    case (VReal(x), VComplex(r, i)) => i == 0 && r == x
    case (VUnit, VUnit)         => true
    case (VTuple(xs), VTuple(ys)) => xs.size == ys.size && xs.zip(ys).forall((a, b) => valueEq(a, b))
    case (VArray1(x), VArray1(y)) => x.size == y.size && x.zip(y).forall((a, b) => valueEq(a, b))
    case (VArray2(b1, r1, c1), VArray2(b2, r2, c2)) =>
      r1 == r2 && c1 == c2 && b1.zip(b2).forall((a, b) => valueEq(a, b))
    case (VStruct(n1, f1), VStruct(n2, f2)) =>
      n1 == n2 && f1.keys.toList == f2.keys.toList &&
        f1.values.zip(f2.values).forall((a, b) => valueEq(a, b))
    case _ => false

  protected def rangeV(lo: Value, hi: Value, inclusive: Boolean, p: Option[scala.util.parsing.input.Position]): Value =
    (lo, hi) match
      case (VInt(a), VInt(b)) =>
        val buf = mutable.ArrayBuffer.empty[Value]
        var i   = a
        val end = if inclusive then b else b - 1
        while i <= end do { buf += VInt(i); i += 1 }
        VArray1(buf)
      case _ => trap(s"range bounds must be integers", p)
