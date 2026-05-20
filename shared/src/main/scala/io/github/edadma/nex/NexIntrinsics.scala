package io.github.edadma.nex

/** Registry of intrinsic identifiers — the dotted-name keys carried by
  * `TIntrinsic` nodes and surface `@intrinsic("opId")` attributes.
  *
  * This file is the single source of truth for *which* intrinsic IDs exist.
  * Per-backend dispatch tables (one in [[NexInterpreter]], one in
  * [[NexLLVMCodegen]], etc.) attach implementations to these IDs. Adding a
  * new intrinsic means:
  *
  *   1. Add its key to [[Ids]] below.
  *   2. Register an implementation in each backend's dispatch table.
  *
  * The opId format is `"<group>.<name>"`. Group conventions:
  *   - `libm.*`     — IEEE-754 scalar math reachable via the host's libm.
  *   - `test.*`     — test-only intrinsics; never ship in a release prelude.
  *
  * Every entry here corresponds to a *primitive* — something the language
  * cannot express in itself. Higher-level operations (complex transcendentals,
  * numeric utilities) are written in `prelude/scalar.nex` as ordinary Nex
  * source that composes these primitives.
  */
object NexIntrinsics:

  /** Every intrinsic opId known to the compiler. Backends MUST refuse to
    * compile a `TIntrinsic` whose opId is not in this set.
    */
  val Ids: Set[String] = Set(
    "test.identity",
    // §10.2 scalar math — direct libm bridges. Each id names a libm
    // function the backend lowers to without a wrapper. Complex versions
    // of sqrt/exp/log/sin/cos/tan are Nex source defs in
    // `prelude/scalar.nex`; no `$complex` opId exists because the
    // implementation isn't an intrinsic.
    "libm.sqrt",  "libm.cbrt",
    "libm.exp",   "libm.log",   "libm.log2",   "libm.log10",
    "libm.sin",   "libm.cos",   "libm.tan",
    "libm.asin",  "libm.acos",  "libm.atan",   "libm.atan2",
    "libm.sinh",  "libm.cosh",  "libm.tanh",
    "libm.asinh", "libm.acosh", "libm.atanh",
    "libm.floor", "libm.ceil",  "libm.round",  "libm.trunc",
    "libm.hypot",
  )

  /** Throw if `opId` is not a known intrinsic. Use this at the top of each
    * backend's dispatch path so a typo in source surfaces with a clear
    * "unknown intrinsic" message instead of MatchError.
    */
  def require(opId: String): Unit =
    if !Ids.contains(opId) then
      throw new RuntimeException(s"unknown intrinsic `$opId` — add it to NexIntrinsics.Ids")
