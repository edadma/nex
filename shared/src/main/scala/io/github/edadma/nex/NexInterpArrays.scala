package io.github.edadma.nex

import scala.collection.mutable

/** Array-shaped runtime helpers for the tree-walking interpreter:
  *
  *   - numeric coercion ([[asReal]]) and per-element distance for
  *     [[NexInterpPrelude]]'s `assert_approx`;
  *   - element-wise and scalar-broadcast binary ops driven by the
  *     `TElementWise` / `TBroadcast` AST cases;
  *   - non-copying view construction ([[buildView]] / [[buildView1]] /
  *     [[buildView2]]) plus negative-index wrap;
  *   - read/write indexing for owned arrays and views (`m[i]`,
  *     `a[i] = x`);
  *   - field access for structs and `complex.re` / `complex.im`;
  *   - the array-reducer helpers (`sum`, `product`, `abs`, `sign`,
  *     `map`, `reduce`, `matmul`, `flatten1`) that the prelude
  *     dispatches into;
  *   - the structural-clone walker preserving value semantics for
  *     `var c = b` on struct/tuple values;
  *   - value formatting / printing (`print`, `s"..."`, `f"..."`).
  *
  * Cross-trait calls reach into [[NexInterpEval]] for `callFunction`
  * (so user closures passed to map/reduce/filter run with the same
  * mode/return semantics as direct calls) and for the scalar
  * arithmetic primitives `addV` / `mulV` used by the reducer helpers.
  */
protected trait NexInterpArrays:
  self: NexInterpreter =>

  // --------------------------------------------------------------------------
  // Element-wise and broadcast
  // --------------------------------------------------------------------------

  protected def elementWise(op: String, l: Value, r: Value, p: Option[scala.util.parsing.input.Position]): Value =
    (l, r) match
      case (VArray1(a), VArray1(b)) =>
        if a.size != b.size then trap(s"element-wise `$op`: size mismatch (${a.size} vs ${b.size})", p)
        VArray1(a.zip(b).map((x, y) => applyBinOp(op, x, y, p)).to(mutable.ArrayBuffer))
      case (VArray2(a, r1, c1), VArray2(b, r2, c2)) =>
        if r1 != r2 || c1 != c2 then trap(s"element-wise `$op`: shape mismatch", p)
        VArray2(a.zip(b).map((x, y) => applyBinOp(op, x, y, p)).to(mutable.ArrayBuffer), r1, c1)
      case _ => trap(s"element-wise `$op`: not arrays", p)

  protected def broadcast(op: String, scalar: Value, arr: Value, scalarFirst: Boolean, p: Option[scala.util.parsing.input.Position]): Value =
    // Order matters for non-commutative ops: `xs - 1` is `x - 1` per
    // element (scalarFirst=false), while `1 - xs` is `1 - x`.
    def step(x: Value): Value =
      if scalarFirst then applyBinOp(op, scalar, x, p)
      else                applyBinOp(op, x, scalar, p)
    arr match
      case VArray1(b)       => VArray1(b.map(step))
      case VArray2(b, r, c) => VArray2(b.map(step), r, c)
      case _                 => trap(s"broadcast: not an array", p)

  // --------------------------------------------------------------------------
  // Indexing
  // --------------------------------------------------------------------------

  /** Construct a non-copying view over an array. For a rank-1 source
    * (either owned `VArray1` or another `VArray1View`), `lo`/`hi` pick
    * an element range; the result is a `VArray1View` aliasing the
    * source's buffer. For a rank-2 source (`VArray2` or `VArray2View`),
    * `lo`/`hi` pick a row range; the result is a `VArray2View` with
    * the same column count. View-of-view chains collapse to the root
    * buffer. `inclusive` lifts `hi` by one. `step` selects every k-th
    * logical element (rank-1 only; rank-2 ignores stride for chunk 2 —
    * chunk 4 lands sub-rectangle views). Out-of-bounds bounds trap.
    */
  protected def buildView(
    src: Value,
    loRaw: Long,
    hiRaw: Long,
    inclusive: Boolean,
    step: Int,
    colRange: Option[(Long, Long, Boolean)],
    p: Option[scala.util.parsing.input.Position],
  ): Value =
    src match
      case VArray1(b) =>
        if colRange.isDefined then trap("view: sub-rectangle requires a rank-2 source", p)
        buildView1(b, 0, b.size, 1, loRaw, hiRaw, inclusive, step, p)
      case VArray1View(b, off, len, srcStride) =>
        if colRange.isDefined then trap("view: sub-rectangle requires a rank-2 source", p)
        buildView1(b, off, len, srcStride, loRaw, hiRaw, inclusive, step, p)
      case VArray2(b, r, c) =>
        if step != 1 then trap("view: stride is rank-1 only", p)
        buildView2(b, 0, r, c, c, 0, loRaw, hiRaw, inclusive, colRange, p)
      case VArray2View(b, rowOff, r, c, rowStride, colOff) =>
        if step != 1 then trap("view: stride is rank-1 only", p)
        buildView2(b, rowOff, r, c, rowStride, colOff, loRaw, hiRaw, inclusive, colRange, p)
      case other =>
        trap(s"view: not an array: ${formatValue(other)}", p)

  private def buildView1(
    buf: mutable.ArrayBuffer[Value],
    baseOff: Int,
    srcLen: Int,
    srcStride: Int,
    loRaw: Long,
    hiRaw: Long,
    inclusive: Boolean,
    step: Int,
    p: Option[scala.util.parsing.input.Position],
  ): Value =
    if step <= 0 then trap(s"view: stride must be positive, got $step", p)
    val lo = wrapNeg(loRaw, srcLen).toInt
    val hiExclusive = (if inclusive then wrapNeg(hiRaw, srcLen) + 1 else wrapNeg(hiRaw, srcLen)).toInt
    if lo < 0 || hiExclusive > srcLen || hiExclusive < lo then
      trap(s"view: out-of-bounds slice $loRaw..${if inclusive then "=" else ""}$hiRaw on length-$srcLen array", p)
    // Logical view length = ceil((hi - lo) / step). View elements live at
    // physical offset `baseOff + (lo + i*step) * srcStride` in the root
    // buffer; we fold srcStride*step into the new view's `stride` so
    // every i in the new view's iteration walks the right physical step.
    val span = hiExclusive - lo
    val vlen = if span <= 0 then 0 else (span + step - 1) / step
    val physOff = baseOff + lo * srcStride
    val newStride = srcStride * step
    VArray1View(buf, physOff, vlen, newStride)

  private def buildView2(
    buf: mutable.ArrayBuffer[Value],
    rowBase: Int,
    srcRows: Int,
    srcCols: Int,
    srcRowStride: Int,
    srcColOff: Int,
    loRaw: Long,
    hiRaw: Long,
    inclusive: Boolean,
    colRange: Option[(Long, Long, Boolean)],
    p: Option[scala.util.parsing.input.Position],
  ): Value =
    val lo = wrapNeg(loRaw, srcRows).toInt
    val hiExclusive = (if inclusive then wrapNeg(hiRaw, srcRows) + 1 else wrapNeg(hiRaw, srcRows)).toInt
    if lo < 0 || hiExclusive > srcRows || hiExclusive < lo then
      trap(s"view: out-of-bounds slice $loRaw..${if inclusive then "=" else ""}$hiRaw on $srcRows×$srcCols matrix", p)
    val newRows = hiExclusive - lo
    colRange match
      case None =>
        // Row-range view: inherit source col count + colOff, advance
        // rowOff by `lo` rows.
        VArray2View(buf, rowBase + lo, newRows, srcCols, srcRowStride, srcColOff)
      case Some((cLoRaw, cHiRaw, cInclusive)) =>
        // Sub-rectangle view: also bounds-wrap the column range; new
        // colOff = srcColOff + cLo, rowStride stays the source's
        // rowStride so per-element indexing skips the gap between
        // visible rows in the underlying flat buffer.
        val cLo = wrapNeg(cLoRaw, srcCols).toInt
        val cHiExcl = (if cInclusive then wrapNeg(cHiRaw, srcCols) + 1 else wrapNeg(cHiRaw, srcCols)).toInt
        if cLo < 0 || cHiExcl > srcCols || cHiExcl < cLo then
          trap(s"view: out-of-bounds col slice $cLoRaw..${if cInclusive then "=" else ""}$cHiRaw on $srcRows×$srcCols matrix", p)
        VArray2View(buf, rowBase + lo, newRows, cHiExcl - cLo, srcRowStride, srcColOff + cLo)

  protected def indexGet(arr: Value, idx: List[Value], p: Option[scala.util.parsing.input.Position]): Value =
    (arr, idx) match
      case (VArray1View(buf, off, len, stride), List(VInt(i))) =>
        val k = wrapNeg(i, len)
        if k < 0 || k >= len then trap(s"index out of bounds: $i (len=$len)", p)
        buf(off + k.toInt * stride)
      case (VArray1(b), List(VInt(i))) =>
        val k = wrapNeg(i, b.size)
        if k < 0 || k >= b.size then trap(s"index out of bounds: $i (len=${b.size})", p)
        b(k.toInt)
      case (VByteArray(b), List(VInt(i))) =>
        val k = wrapNeg(i, b.size)
        if k < 0 || k >= b.size then trap(s"index out of bounds: $i (len=${b.size})", p)
        VInt((b(k.toInt) & 0xFF).toLong)
      case (VArray2View(buf, rowOff, r, c, rowStride, colOff), List(VInt(i), VInt(j))) =>
        val ki = wrapNeg(i, r)
        val kj = wrapNeg(j, c)
        if ki < 0 || ki >= r || kj < 0 || kj >= c then
          trap(s"index out of bounds: ($i, $j) (shape=$r×$c)", p)
        buf((rowOff + ki.toInt) * rowStride + colOff + kj.toInt)
      case (VArray2View(buf, rowOff, r, c, rowStride, colOff), List(VInt(i))) =>
        val ki = wrapNeg(i, r)
        if ki < 0 || ki >= r then trap(s"index out of bounds: $i (rows=$r)", p)
        val row = mutable.ArrayBuffer.empty[Value]
        var k   = 0
        while k < c do { row += buf((rowOff + ki.toInt) * rowStride + colOff + k); k += 1 }
        VArray1(row)
      case (VArray2(b, r, c), List(VInt(i), VInt(j))) =>
        val ki = wrapNeg(i, r)
        val kj = wrapNeg(j, c)
        if ki < 0 || ki >= r || kj < 0 || kj >= c then
          trap(s"index out of bounds: ($i, $j) (shape=$r×$c)", p)
        b(ki.toInt * c + kj.toInt)
      case (VArray2(b, r, c), List(VInt(i))) =>
        val ki = wrapNeg(i, r)
        if ki < 0 || ki >= r then trap(s"index out of bounds: $i (rows=$r)", p)
        val row = mutable.ArrayBuffer.empty[Value]
        var k   = 0
        while k < c do { row += b(ki.toInt * c + k); k += 1 }
        VArray1(row)
      case (VString(s), List(VInt(i))) =>
        val k = wrapNeg(i, s.length)
        if k < 0 || k >= s.length then trap(s"string index out of bounds: $i", p)
        VString(s.charAt(k.toInt).toString)
      case _ => trap(s"cannot index ${formatValue(arr)} with ${idx.map(formatValue).mkString(", ")}", p)

  protected def indexSet(arr: Value, idx: List[Value], rhs: Value, p: Option[scala.util.parsing.input.Position]): Unit =
    (arr, idx) match
      case (VArray1View(buf, off, len, stride), List(VInt(i))) =>
        val k = wrapNeg(i, len)
        if k < 0 || k >= len then trap(s"index out of bounds: $i (len=$len)", p)
        buf(off + k.toInt * stride) = rhs
      case (VArray1(b), List(VInt(i))) =>
        val k = wrapNeg(i, b.size)
        if k < 0 || k >= b.size then trap(s"index out of bounds: $i (len=${b.size})", p)
        b(k.toInt) = rhs
      case (VByteArray(b), List(VInt(i))) =>
        val k = wrapNeg(i, b.size)
        if k < 0 || k >= b.size then trap(s"index out of bounds: $i (len=${b.size})", p)
        rhs match
          case VInt(v) if v >= 0 && v <= 255 => b(k.toInt) = v.toByte
          case VInt(v)                       => trap(s"byte value out of range 0..255: $v", p)
          case _                             => trap(s"cannot store ${formatValue(rhs)} in a [byte] (need integer in 0..255)", p)
      case (VArray2View(buf, rowOff, r, c, rowStride, colOff), List(VInt(i), VInt(j))) =>
        val ki = wrapNeg(i, r)
        val kj = wrapNeg(j, c)
        if ki < 0 || ki >= r || kj < 0 || kj >= c then
          trap(s"index out of bounds: ($i, $j) (shape=$r×$c)", p)
        buf((rowOff + ki.toInt) * rowStride + colOff + kj.toInt) = rhs
      case (VArray2(b, r, c), List(VInt(i), VInt(j))) =>
        val ki = wrapNeg(i, r)
        val kj = wrapNeg(j, c)
        if ki < 0 || ki >= r || kj < 0 || kj >= c then
          trap(s"index out of bounds: ($i, $j) (shape=$r×$c)", p)
        b(ki.toInt * c + kj.toInt) = rhs
      case _ => trap(s"cannot index-assign ${formatValue(arr)}", p)

  /** Negative-index wrap: `i < 0` → `i + len`; otherwise return `i`
    * unchanged. Mirrors the AOT `__nex_arr*_slot` helpers so byte-exact
    * parity holds for negative-index trap messages and successful lookups.
    */
  protected def wrapNeg(i: Long, len: Int): Long =
    if i < 0 then i + len.toLong else i

  // --------------------------------------------------------------------------
  // Field access
  // --------------------------------------------------------------------------

  protected def fieldGet(v: Value, name: String, p: Option[scala.util.parsing.input.Position]): Value = v match
    case VStruct(_, fs) =>
      fs.get(name) match
        case Some(x) => x
        case None    => trap(s"no field `$name` on struct", p)
    case VComplex(re, im) =>
      name match
        case "re" => VReal(re)
        case "im" => VReal(im)
        case _     => trap(s"no field `$name` on complex", p)
    case _ => trap(s"cannot access field `$name` on ${formatValue(v)}", p)

  // --------------------------------------------------------------------------
  // Numeric coercions used by element-wise distance / prelude conversions
  // --------------------------------------------------------------------------

  protected def asReal(v: Value): Double = v match
    case VInt(x)        => x.toDouble
    case VReal(x)       => x
    case VComplex(r, 0) => r
    case _              => throw new NexTrap(s"expected numeric, got ${formatValue(v)}", None)

  /** Euclidean distance between two scalar / complex values, treating
    * integers and reals as points on the real line and complex values
    * as points in the plane. Used by `assert_approx` over arrays so a
    * mixed-element-type array still yields a sensible per-element
    * distance.
    */
  protected def elementWiseDistance(a: Value, b: Value): Double = (a, b) match
    case (VComplex(ar, ai), VComplex(br, bi)) => math.hypot(ar - br, ai - bi)
    case _                                    => math.abs(asReal(a) - asReal(b))

  // --------------------------------------------------------------------------
  // Array helpers
  // --------------------------------------------------------------------------

  protected def sumV(v: Value): Value = v match
    case VArray1(b)       => b.foldLeft(VInt(0): Value)((acc, x) => addV(acc, x))
    case VArray2(b, _, _) => b.foldLeft(VInt(0): Value)((acc, x) => addV(acc, x))
    case _                 => trap(s"sum: not an array", None)

  protected def productV(v: Value): Value = v match
    case VArray1(b)       => b.foldLeft(VInt(1): Value)((acc, x) => mulV(acc, x))
    case VArray2(b, _, _) => b.foldLeft(VInt(1): Value)((acc, x) => mulV(acc, x))
    case _                 => trap(s"product: not an array", None)

  protected def absV(v: Value): Value = v match
    case VInt(x)        => VInt(math.abs(x))
    case VReal(x)       => VReal(math.abs(x))
    case VComplex(r, i) => VReal(math.hypot(r, i))
    case _              => trap(s"abs: non-numeric", None)

  protected def signV(v: Value): Value = v match
    case VInt(x)  => VInt(java.lang.Long.signum(x).toLong)
    case VReal(x) => VReal(math.signum(x))
    case _        => trap(s"sign: non-numeric", None)

  protected def mapArray(arr: Value, fn: VFunc): Value = arr match
    case VArray1(b)       => VArray1(b.map(x => callFunction(fn, List(x), None)))
    case VArray2(b, r, c) => VArray2(b.map(x => callFunction(fn, List(x), None)), r, c)
    case _                 => trap(s"map: not an array", None)

  protected def reduceArray(arr: Value, init: Value, fn: VFunc): Value =
    arr match
      case VArray1(b) =>
        var acc = init
        for x <- b do acc = callFunction(fn, List(acc, x), None)
        acc
      case VArray2(b, _, _) =>
        var acc = init
        for x <- b do acc = callFunction(fn, List(acc, x), None)
        acc
      case _ => trap(s"reduce: not an array", None)

  protected def matMul(a: Value, b: Value, p: Option[scala.util.parsing.input.Position] = None): Value = (a, b) match
    case (VArray2(ab, ar, ac), VArray2(bb, br, bc)) =>
      if ac != br then trap(s"matmul: shape mismatch ($ar×$ac) @ ($br×$bc)", p)
      val out = new mutable.ArrayBuffer[Value](ar * bc)
      for _ <- 0 until ar * bc do out += VInt(0)
      var i = 0
      while i < ar do
        var j = 0
        while j < bc do
          var acc: Value = VInt(0)
          var k = 0
          while k < ac do
            acc = addV(acc, mulV(ab(i * ac + k), bb(k * bc + j)))
            k += 1
          out(i * bc + j) = acc
          j += 1
        i += 1
      VArray2(out, ar, bc)
    case (VArray2(ab, ar, ac), VArray1(bb)) =>
      if ac != bb.size then trap(s"matmul: shape mismatch ($ar×$ac) @ (${bb.size})", p)
      val out = new mutable.ArrayBuffer[Value](ar)
      var i   = 0
      while i < ar do
        var acc: Value = VInt(0)
        var k = 0
        while k < ac do
          acc = addV(acc, mulV(ab(i * ac + k), bb(k)))
          k += 1
        out += acc
        i += 1
      VArray1(out)
    case (VArray1(ab), VArray2(bb, br, bc)) =>
      if ab.size != br then trap(s"matmul: shape mismatch (${ab.size}) @ ($br×$bc)", p)
      val out = new mutable.ArrayBuffer[Value](bc)
      var j   = 0
      while j < bc do
        var acc: Value = VInt(0)
        var k = 0
        while k < br do
          acc = addV(acc, mulV(ab(k), bb(k * bc + j)))
          k += 1
        out += acc
        j += 1
      VArray1(out)
    case (VArray1(ab), VArray1(bb)) =>
      if ab.size != bb.size then trap(s"matmul: dot length mismatch", p)
      var acc: Value = VInt(0)
      var i = 0
      while i < ab.size do
        acc = addV(acc, mulV(ab(i), bb(i)))
        i += 1
      acc
    case _ => trap(s"matmul: non-array operands", p)

  protected def flatten1(v: Value): mutable.ArrayBuffer[Value] = v match
    case VArray1(b)       => b.clone()
    case VArray2(b, _, _) => b.clone()
    case _                 => trap(s"flatten: not an array", None)

  /** Deep-copy a struct value so the new binding's fields can't alias
    * the source's. Spec §4.15 + the AOT's value-typed `{ ... }`
    * aggregate semantics together imply structs are values: a `var c
    * = b` should produce two independent bindings, and `c.v = 99` must
    * not be observable through `b`. The interpreter's VStruct fields
    * map is a shared mutable, so we recursively clone here.
    *
    * Recurses through nested structs and through tuple/array elements
    * so a struct that contains another struct (or a tuple/array of
    * structs) clones every layer. Non-struct leaves pass through
    * unchanged — integers, reals, strings, closures, and the array
    * descriptor itself (array-vs-array uniqueness is handled by
    * NexLifetime's TClone insertion).
    */
  protected def cloneStructValue(v: Value): Value = v match
    case VStruct(name, fields) =>
      val copy = mutable.LinkedHashMap.empty[String, Value]
      for (k, fv) <- fields do copy(k) = cloneStructValue(fv)
      VStruct(name, copy)
    case VTuple(es)       => VTuple(es.map(cloneStructValue))
    case other            => other

  // --------------------------------------------------------------------------
  // I/O and formatting
  // --------------------------------------------------------------------------

  protected def doPrint(args: List[Value]): Unit =
    Console.out.println(args.map(formatValue).mkString(" "))

  /** Render a value using an optional `f"..."` format spec (spec §10.6).
    * `spec = None` falls back to [[formatValue]]'s default form (used by
    * `s"..."` and bare interpolations). `spec = Some("%5d")` etc. dispatches
    * by the conversion character to the right host-side formatter. We
    * route through Java's `String.format` for the standard types, but
    * special-case `%b` (binary integer in Nex; Java uses `%b` for
    * boolean) and re-route `%x`/`%X`/`%o` on real / complex inputs to a
    * Nex-specific error.
    */
  protected def formatWithSpec(v: Value, spec: Option[String], p: Option[scala.util.parsing.input.Position]): String =
    spec match
      case None    => formatValue(v)
      case Some(s) => renderSpec(v, s, p)

  /** Apply a printf-style spec to a single value. The spec arrives
    * including the leading `%` and trailing conversion character (lexer
    * already validated the shape). Conversion-vs-value-type mismatches
    * trap; otherwise we hand the spec off to `String.format` (or a small
    * special-case for `%b` / `%x` / `%o` on integers).
    */
  private def renderSpec(v: Value, spec: String, p: Option[scala.util.parsing.input.Position]): String =
    val conv = spec.last
    conv match
      case 'd' =>
        val n = v match
          case VInt(x) => x
          case other   => trap(s"format spec `$spec` expects integer, got ${formatValue(other)}", p)
        String.format(java.util.Locale.ROOT, spec, java.lang.Long.valueOf(n))
      case 'f' | 'e' | 'g' | 'E' | 'G' =>
        val x = v match
          case VReal(x) => x
          case VInt(n)  => n.toDouble
          case other    => trap(s"format spec `$spec` expects real, got ${formatValue(other)}", p)
        String.format(java.util.Locale.ROOT, spec, java.lang.Double.valueOf(x))
      case 's' =>
        val s = v match
          case VString(x) => x
          case other      => formatValue(other)
        String.format(java.util.Locale.ROOT, spec, s)
      case 'x' | 'X' | 'o' =>
        val n = v match
          case VInt(x) => x
          case other   => trap(s"format spec `$spec` expects integer, got ${formatValue(other)}", p)
        String.format(java.util.Locale.ROOT, spec, java.lang.Long.valueOf(n))
      case 'b' =>
        // Nex: `%b` formats an integer as a binary numeric string. (Java
        // hijacks `%b` for boolean, so we build it ourselves — toBinaryString
        // plus the optional width / flags.)
        val n = v match
          case VInt(x) => x
          case other   => trap(s"format spec `$spec` expects integer, got ${formatValue(other)}", p)
        val raw = java.lang.Long.toBinaryString(n)
        applyWidthAndFlags(raw, spec)
      case _ =>
        trap(s"unknown format conversion `$conv` in spec `$spec`", p)

  /** Apply the width / `-` / `0` flags from the spec to an already-
    * computed raw representation. Used by `%b` (binary integer) since
    * we don't route binary through Java's printf.
    */
  private def applyWidthAndFlags(raw: String, spec: String): String =
    // Parse: %[flags][width].b
    val body = spec.substring(1, spec.length - 1) // drop leading `%` and trailing `b`
    var i        = 0
    var leftAlign = false
    var zeroPad   = false
    while i < body.length && "-+0 ".contains(body.charAt(i)) do
      body.charAt(i) match
        case '-' => leftAlign = true
        case '0' => zeroPad   = true
        case _   => ()
      i += 1
    val widthStr = body.substring(i).takeWhile(_.isDigit)
    val width    = if widthStr.isEmpty then 0 else widthStr.toInt
    if raw.length >= width then raw
    else if leftAlign then raw + " " * (width - raw.length)
    else (if zeroPad then "0" else " ") * (width - raw.length) + raw

  /** Default printable form. Numeric → its literal; arrays → bracketed
    * comma-list; strings → unquoted; structs → `Name { field=value, ... }`.
    */
  def formatValue(v: Value): String = v match
    case VInt(x)        => x.toString
    case VReal(x)       =>
      if x.isNaN then "nan"
      else if x.isInfinite then if x > 0 then "inf" else "-inf"
      else if x == x.toLong.toDouble && math.abs(x) < 1e15 then s"${x.toLong}.0"
      else x.toString
    case VComplex(r, i) =>
      val rs = formatValue(VReal(r))
      val is = formatValue(VReal(math.abs(i)))
      val sign = if i >= 0 then "+" else "-"
      s"$rs$sign${is}i"
    case VBool(b)       => b.toString
    case VString(s)     => s
    case VUnit          => "()"
    case VArray1(b)     => b.map(formatValue).mkString("[", ", ", "]")
    case VByteArray(b)  =>
      b.map(by => f"0x${by & 0xFF}%02X").mkString("[", ", ", "]")
    case VArray1View(buf, off, len, stride) =>
      (0 until len).map(i => formatValue(buf(off + i * stride))).mkString("[", ", ", "]")
    case VArray2(b, r, c) =>
      val rows = for i <- 0 until r yield
        (for j <- 0 until c yield formatValue(b(i * c + j))).mkString("[", ", ", "]")
      rows.mkString("[", ", ", "]")
    case VArray2View(buf, rowOff, r, c, rowStride, colOff) =>
      val rows = for i <- 0 until r yield
        (for j <- 0 until c yield formatValue(buf((rowOff + i) * rowStride + colOff + j))).mkString("[", ", ", "]")
      rows.mkString("[", ", ", "]")
    case VTuple(es)     => es.map(formatValue).mkString("(", ", ", ")")
    case VStruct(n, fs) => fs.map((k, v) => s"$k=${formatValue(v)}").mkString(s"$n { ", ", ", " }")
    case VEnum(_, vn, _, Nil) => vn
    case VEnum(_, vn, _, vs)  => vs.map(formatValue).mkString(s"$vn(", ", ", ")")
    case VEnumCtor(en, vn, _, _) => s"<ctor $en.$vn>"
    case VBuiltin(n, _) => s"<builtin $n>"
    case VUserFunc(ps, _, _) => s"<func/${ps.size}>"
