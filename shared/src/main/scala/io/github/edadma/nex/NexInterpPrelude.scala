package io.github.edadma.nex

import scala.collection.mutable

/** Prelude binding and dispatch for the tree-walking interpreter.
  *
  * Walks the program's symbol table for `SymKind.Prelude` symbols and binds
  * each one to a [[VBuiltin]] closure under its Symbol id, except the three
  * constant values (`inf`, `nan`, `i`) which bind directly. The huge match
  * inside [[preludeFn]] is the runtime dispatch table for every named
  * prelude function — `print`, `sum`, `map`, `transpose`, `assert_approx`,
  * and the rest of spec §10.
  *
  * Array arguments arrive pre-coerced by [[coerceViewForRead]] so an owned-
  * array `case VArray1(b)` pattern transparently matches a `VArray1View`
  * snapshot. The `view` builtin itself takes the bypass path in
  * [[NexInterpEval]] (it needs the raw range AST to preserve `lo`/`hi`/
  * inclusivity), so it never reaches this dispatch.
  */
protected trait NexInterpPrelude:
  self: NexInterpreter =>

  protected def registerPrelude(p: TProgram): Unit =
    for s <- p.symbols.all if s.kind == SymKind.Prelude do
      val v: Value = s.name match
        case "inf" => VReal(Double.PositiveInfinity)
        case "nan" => VReal(Double.NaN)
        case "i"   => VComplex(0.0, 1.0)
        case other => VBuiltin(other, preludeFn(other))
      globalEnv.define(s.id, v)

  /** Materialize a `VArray1View` or `VArray2View` into a fresh owned
    * array by copying its current window. Used at prelude entry for
    * read-only ops that don't need write-through (sum, map, filter,
    * transpose, …) — these see a snapshot of the view's contents at
    * call time, which agrees with the AOT semantics (the AOT path
    * reads the view's buffer the same way). Pass-through for already-
    * owned arrays and non-array values.
    */
  protected def coerceViewForRead(v: Value): Value = v match
    case VArray1View(buf, off, len, stride) =>
      val out = mutable.ArrayBuffer.empty[Value]
      var i = 0
      while i < len do { out += buf(off + i * stride); i += 1 }
      VArray1(out)
    case VArray2View(buf, rowOff, rows, cols, rowStride, colOff) =>
      val out = mutable.ArrayBuffer.empty[Value]
      var r = 0
      while r < rows do
        var c = 0
        while c < cols do
          out += buf((rowOff + r) * rowStride + colOff + c)
          c += 1
        r += 1
      VArray2(out, rows, cols)
    case other => other

  protected def preludeFn(name: String): List[Value] => Value = args0 =>
    // Most prelude ops only read their array args; coerce views to
    // owned snapshots upfront so the existing `case VArray1(b)`
    // patterns match transparently. The `view` builtin itself is
    // dispatched specially in the TCall handler (it needs the raw
    // range AST to preserve inclusivity and lo/hi), so it never
    // reaches this normalisation step.
    val args = args0.map(coerceViewForRead)
    name match
    case "print"  => doPrint(args); VUnit
    case "cbrt"   => unary1(args, "cbrt")(v => VReal(math.cbrt(asReal(v))))
    case "abs"    => unary1(args, "abs")(absV)
    case "sign"   => unary1(args, "sign")(signV)
    case "asin"   => unary1(args, "asin")(v => VReal(math.asin(asReal(v))))
    case "acos"   => unary1(args, "acos")(v => VReal(math.acos(asReal(v))))
    case "atan"   => unary1(args, "atan")(v => VReal(math.atan(asReal(v))))
    case "atan2"  =>
      args match
        case List(y, x) => VReal(math.atan2(asReal(y), asReal(x)))
        case _          => trap(s"atan2 expects 2 args, got ${args.size}", None)
    case "sinh"   => unary1(args, "sinh")(v => VReal(math.sinh(asReal(v))))
    case "cosh"   => unary1(args, "cosh")(v => VReal(math.cosh(asReal(v))))
    case "tanh"   => unary1(args, "tanh")(v => VReal(math.tanh(asReal(v))))
    case "asinh"  => unary1(args, "asinh")(v => VReal(asinh(asReal(v))))
    case "acosh"  => unary1(args, "acosh")(v => VReal(acosh(asReal(v))))
    case "atanh"  => unary1(args, "atanh")(v => VReal(atanh(asReal(v))))
    case "floor"  => unary1(args, "floor")(v => VReal(math.floor(asReal(v))))
    case "ceil"   => unary1(args, "ceil")(v => VReal(math.ceil(asReal(v))))
    case "round"  => unary1(args, "round")(v => VReal(math.round(asReal(v)).toDouble))
    case "trunc"  => unary1(args, "trunc")(v => VReal(asReal(v).toLong.toDouble))
    case "min"    =>
      args match
        case List(a, b)           => if cmpNum(a, b) <= 0 then a else b
        case List(VArray1(b))     =>
          if b.isEmpty then trap("min: empty array", None)
          else b.tail.foldLeft(b.head)((acc, v) => if cmpNum(acc, v) <= 0 then acc else v)
        case List(VArray2(b, _, _)) =>
          if b.isEmpty then trap("min: empty array", None)
          else b.tail.foldLeft(b.head)((acc, v) => if cmpNum(acc, v) <= 0 then acc else v)
        case _ => trap(s"min expects (scalar, scalar) or (array), got ${args.size} args", None)
    case "max"    =>
      args match
        case List(a, b)           => if cmpNum(a, b) >= 0 then a else b
        case List(VArray1(b))     =>
          if b.isEmpty then trap("max: empty array", None)
          else b.tail.foldLeft(b.head)((acc, v) => if cmpNum(acc, v) >= 0 then acc else v)
        case List(VArray2(b, _, _)) =>
          if b.isEmpty then trap("max: empty array", None)
          else b.tail.foldLeft(b.head)((acc, v) => if cmpNum(acc, v) >= 0 then acc else v)
        case _ => trap(s"max expects (scalar, scalar) or (array), got ${args.size} args", None)
    case "conj"   =>
      unary1(args, "conj") {
        case VComplex(re, im) => VComplex(re, -im)
        case v                => v // real conjugate is itself
      }
    case "arg"    =>
      unary1(args, "arg") {
        case VComplex(re, im) => VReal(math.atan2(im, re))
        case v                => if asReal(v) >= 0 then VReal(0) else VReal(math.Pi)
      }
    case "length" =>
      unary1(args, "length") {
        case VArray1(b)       => VInt(b.size.toLong)
        case VArray2(_, r, _) => VInt(r.toLong)
        case VString(s)       => VInt(s.length.toLong)
        case v                => trap(s"length: not an array/string: $v", None)
      }
    case "rows" =>
      unary1(args, "rows") {
        case VArray2(_, r, _) => VInt(r.toLong)
        case VArray1(b)       => VInt(b.size.toLong)
        case v                => trap(s"rows: not an array: $v", None)
      }
    case "cols" =>
      unary1(args, "cols") {
        case VArray2(_, _, c) => VInt(c.toLong)
        case VArray1(_)       => VInt(1)
        case v                => trap(s"cols: not an array: $v", None)
      }
    case "shape" =>
      unary1(args, "shape") {
        case VArray1(b)       => VTuple(List(VInt(b.size.toLong)))
        case VArray2(_, r, c) => VTuple(List(VInt(r.toLong), VInt(c.toLong)))
        case v                => trap(s"shape: not an array: $v", None)
      }
    case "sum"     => unary1(args, "sum")(sumV)
    case "product" => unary1(args, "product")(productV)
    case "dot"     =>
      args match
        case List(VArray1(a), VArray1(b)) =>
          if a.size != b.size then trap(s"dot: length mismatch (${a.size} vs ${b.size})", None)
          var acc: Value = VInt(0)
          var i = 0
          while i < a.size do
            acc = addV(acc, mulV(a(i), b(i)))
            i += 1
          acc
        case _ => trap(s"dot expects two rank-1 arrays, got $args", None)
    case "map" =>
      args match
        case List(arr, fn: VFunc) => mapArray(arr, fn)
        case List(fn: VFunc, arr) => mapArray(arr, fn) // forgiving arg order
        case _                     => trap(s"map expects (array, fn)", None)
    case "flatMap" =>
      args match
        case List(arr: VArray1, fn: VFunc) =>
          val out = mutable.ArrayBuffer.empty[Value]
          for x <- arr.buf do
            callFunction(fn, List(x), None) match
              case VArray1(b) => out ++= b
              case other =>
                trap(s"flatMap: fn returned non-array (got ${formatValue(other)})", None)
          VArray1(out)
        case _ => trap(s"flatMap expects (rank-1 array, fn)", None)
    case "reduce" =>
      args match
        case List(arr, init, fn: VFunc) => reduceArray(arr, init, fn)
        case _                           => trap(s"reduce expects (array, init, fn)", None)
    case "filter" =>
      args match
        case List(arr: VArray1, fn: VFunc) =>
          val out = mutable.ArrayBuffer.empty[Value]
          for v <- arr.buf do
            callFunction(fn, List(v), None) match
              case VBool(true) => out += v
              case _            => ()
          VArray1(out)
        case _ => trap(s"filter expects (array, fn)", None)
    case "range" =>
      args match
        case List(VInt(lo), VInt(hi)) =>
          val buf = mutable.ArrayBuffer.empty[Value]
          var i   = lo
          while i < hi do { buf += VInt(i); i += 1 }
          VArray1(buf)
        case _ => trap(s"range expects (integer, integer)", None)
    case "enumerate" =>
      args match
        case List(VArray1(b)) =>
          val out = mutable.ArrayBuffer.empty[Value]
          var i   = 0
          while i < b.size do { out += VTuple(List(VInt(i.toLong), b(i))); i += 1 }
          VArray1(out)
        case _ => trap(s"enumerate expects (rank-1 array)", None)
    case "zip" =>
      args match
        case List(VArray1(a), VArray1(b)) =>
          val n   = math.min(a.size, b.size)
          val out = mutable.ArrayBuffer.empty[Value]
          var i   = 0
          while i < n do { out += VTuple(List(a(i), b(i))); i += 1 }
          VArray1(out)
        case _ => trap(s"zip expects two rank-1 arrays", None)
    case "transpose" =>
      args match
        case List(VArray2(b, r, c)) =>
          val out = new mutable.ArrayBuffer[Value](r * c)
          for _ <- 0 until (r * c) do out += VUnit
          var i = 0
          while i < r do
            var j = 0
            while j < c do
              out(j * r + i) = b(i * c + j)
              j += 1
            i += 1
          VArray2(out, c, r)
        case _ => trap(s"transpose expects a rank-2 array", None)
    case "matmul" =>
      args match
        case List(a, b) => matMul(a, b)
        case _          => trap(s"matmul expects 2 args", None)
    case "diag" =>
      args match
        case List(VArray1(b)) =>
          val n   = b.size
          val out = new mutable.ArrayBuffer[Value](n * n)
          for _ <- 0 until n * n do out += VInt(0)
          var i = 0
          while i < n do
            out(i * n + i) = b(i)
            i += 1
          VArray2(out, n, n)
        case _ => trap(s"diag expects a rank-1 array", None)
    case "reshape" =>
      // Spec §10.4 signature: `reshape(a: [T], m: integer, n: integer)`.
      // Treats `a` as a column-major flat buffer of an `m x n` matrix,
      // i.e. input[c * m + r] is the element at (r, c) of the result.
      // Internal storage is row-major (output[r * n + c]).
      args match
        case List(arr, VInt(rL), VInt(cL)) =>
          val flat   = flatten1(arr)
          val rows   = rL.toInt
          val cols   = cL.toInt
          if flat.size != rows * cols then
            trap(s"reshape: size mismatch (${flat.size} into ${rows}×${cols})", None)
          val out = mutable.ArrayBuffer.fill(rows * cols)(VInt(0).asInstanceOf[Value])
          var r = 0
          while r < rows do
            var c = 0
            while c < cols do
              out(r * cols + c) = flat(c * rows + r)
              c += 1
            r += 1
          VArray2(out, rows, cols)
        case _ => trap(s"reshape expects (array, rows: integer, cols: integer)", None)

    case "flatten" =>
      // Spec §10.4: column-major flatten. For a row-major `VArray2(b, r, c)`
      // we materialize `[b[0,0], b[1,0], ..., b[r-1,0], b[0,1], ...]` — i.e.
      // walk columns first, rows inside.
      args match
        case List(VArray1(b))       => VArray1(b.clone())
        case List(VArray2(b, r, c)) =>
          val out = mutable.ArrayBuffer.fill(r * c)(VInt(0).asInstanceOf[Value])
          var idx = 0
          var col = 0
          while col < c do
            var row = 0
            while row < r do
              out(idx) = b(row * c + col)
              idx += 1
              row += 1
            col += 1
          VArray1(out)
        case _ => trap(s"flatten expects 1 arg", None)
    case "sum_axis" =>
      // Spec §10.4: collapse one axis of a rank-2 matrix into a rank-1
      // vector by summing along it. axis=0 collapses rows → result of
      // len = cols (sum down each column); axis=1 collapses cols →
      // result of len = rows (sum across each row). Storage is
      // row-major: m(row*cols + col).
      args match
        case List(VArray2(b, r, c), VInt(0)) =>
          val out = mutable.ArrayBuffer.empty[Value]
          var j = 0
          while j < c do
            var acc: Value = VInt(0)
            var i = 0
            while i < r do
              acc = addV(acc, b(i * c + j))
              i += 1
            out += acc
            j += 1
          VArray1(out)
        case List(VArray2(b, r, c), VInt(1)) =>
          val out = mutable.ArrayBuffer.empty[Value]
          var i = 0
          while i < r do
            var acc: Value = VInt(0)
            var j = 0
            while j < c do
              acc = addV(acc, b(i * c + j))
              j += 1
            out += acc
            i += 1
          VArray1(out)
        case List(VArray2(_, _, _), VInt(k)) =>
          trap(s"sum_axis: axis must be 0 or 1, got $k", None)
        case _ => trap(s"sum_axis expects (rank-2 array, integer axis)", None)
    case "zeros" =>
      args match
        case List(VInt(n))                 => VArray1(mutable.ArrayBuffer.fill(n.toInt)(VInt(0).asInstanceOf[Value]))
        case List(VTuple(List(VInt(r), VInt(c)))) =>
          VArray2(mutable.ArrayBuffer.fill(r.toInt * c.toInt)(VInt(0).asInstanceOf[Value]), r.toInt, c.toInt)
        case _ => trap(s"zeros expects integer or (rows, cols)", None)
    case "ones" =>
      args match
        case List(VInt(n))                 => VArray1(mutable.ArrayBuffer.fill(n.toInt)(VInt(1).asInstanceOf[Value]))
        case List(VTuple(List(VInt(r), VInt(c)))) =>
          VArray2(mutable.ArrayBuffer.fill(r.toInt * c.toInt)(VInt(1).asInstanceOf[Value]), r.toInt, c.toInt)
        case _ => trap(s"ones expects integer or (rows, cols)", None)
    case "fill" =>
      args match
        case List(VInt(n), v)                                    => VArray1(mutable.ArrayBuffer.fill(n.toInt)(v))
        case List(VTuple(List(VInt(r), VInt(c))), v)             =>
          VArray2(mutable.ArrayBuffer.fill(r.toInt * c.toInt)(v), r.toInt, c.toInt)
        case _ => trap(s"fill expects (n, v) or ((rows, cols), v)", None)
    case "linspace" =>
      args match
        case List(a, b, VInt(n)) =>
          val lo = asReal(a); val hi = asReal(b); val k = n.toInt
          if k <= 1 then VArray1(mutable.ArrayBuffer(VReal(lo)))
          else
            val step = (hi - lo) / (k - 1)
            val buf  = mutable.ArrayBuffer.empty[Value]
            var i    = 0
            while i < k do { buf += VReal(lo + step * i); i += 1 }
            VArray1(buf)
        case _ => trap(s"linspace expects (a, b, n)", None)
    case "identity" =>
      args match
        case List(VInt(n)) =>
          val k   = n.toInt
          val buf = new mutable.ArrayBuffer[Value](k * k)
          for _ <- 0 until k * k do buf += VInt(0)
          var i = 0
          while i < k do { buf(i * k + i) = VInt(1); i += 1 }
          VArray2(buf, k, k)
        case _ => trap(s"identity expects an integer", None)
    case "to_real"    => unary1(args, "to_real")(v => VReal(asReal(v)))
    case "to_integer" => unary1(args, "to_integer")(v => VInt(asReal(v).toLong))
    case "to_complex" =>
      unary1(args, "to_complex") {
        case VComplex(re, im) => VComplex(re, im)
        case v                => VComplex(asReal(v), 0.0)
      }
    case "assert" =>
      args match
        case List(VBool(true))                  => VUnit
        case List(VBool(true), VString(_))      => VUnit
        case List(VBool(false))                 => trap("assertion failed", None)
        case List(VBool(false), VString(msg))   => trap(s"assertion failed: $msg", None)
        case _                                   => trap(s"assert expects (bool) or (bool, string)", None)
    case "assert_eq" =>
      args match
        case List(a, b) =>
          if valueEq(a, b) then VUnit
          else trap(s"assert_eq: ${formatValue(a)} != ${formatValue(b)}", None)
        case _ => trap(s"assert_eq expects 2 args", None)
    case "assert_approx" =>
      args match
        case List(VArray1(as), VArray1(bs), eps) =>
          val tol = asReal(eps)
          if as.size != bs.size then
            trap(s"assert_approx: array length mismatch: ${as.size} vs ${bs.size}", None)
          var i = 0
          while i < as.size do
            val d = elementWiseDistance(as(i), bs(i))
            if d > tol then
              trap(s"assert_approx: element $i: |${formatValue(as(i))} - ${formatValue(bs(i))}| = $d > $tol", None)
            i += 1
          VUnit
        case List(VArray2(as, ar, ac), VArray2(bs, br, bc), eps) =>
          val tol = asReal(eps)
          if ar != br || ac != bc then
            trap(s"assert_approx: array shape mismatch: ($ar, $ac) vs ($br, $bc)", None)
          var i = 0
          while i < as.size do
            val d = elementWiseDistance(as(i), bs(i))
            if d > tol then
              val row = i / ac
              val col = i % ac
              trap(s"assert_approx: element ($row, $col): |${formatValue(as(i))} - ${formatValue(bs(i))}| = $d > $tol", None)
            i += 1
          VUnit
        case List(VComplex(ar, ai), VComplex(br, bi), eps) =>
          val tol  = asReal(eps)
          val dist = math.hypot(ar - br, ai - bi)
          if dist <= tol then VUnit
          else trap(s"assert_approx: |${formatValue(VComplex(ar, ai))} - ${formatValue(VComplex(br, bi))}| = $dist > $tol", None)
        case List(a, b, eps) =>
          val diff = math.abs(asReal(a) - asReal(b))
          if diff <= asReal(eps) then VUnit
          else trap(s"assert_approx: |${asReal(a)} - ${asReal(b)}| = $diff > ${asReal(eps)}", None)
        case _ => trap(s"assert_approx expects (a, b, eps)", None)
    case "assert_traps" =>
      args match
        case List(fn: VFunc) =>
          val caught =
            try { callFunction(fn, Nil, None); None }
            catch case t: NexTrap => Some(t)
          caught match
            case Some(_) => VUnit
            case None    => trap("assert_traps: expected trap, got normal return", None)
        case List(fn: VFunc, VString(expected)) =>
          val caught =
            try { callFunction(fn, Nil, None); None }
            catch case t: NexTrap => Some(t)
          caught match
            case Some(t) =>
              if t.msg.contains(expected) then VUnit
              else trap(s"assert_traps: expected substring `$expected`, got `${t.msg}`", None)
            case None =>
              trap(s"assert_traps: expected trap containing `$expected`, got normal return", None)
        case _ => trap("assert_traps expects (() -> _) or (() -> _, string)", None)
    case other => trap(s"prelude function `$other` is not yet implemented", None)

  private def asinh(x: Double): Double = math.log(x + math.sqrt(x * x + 1))
  private def acosh(x: Double): Double = math.log(x + math.sqrt(x * x - 1))
  private def atanh(x: Double): Double = 0.5 * math.log((1 + x) / (1 - x))
