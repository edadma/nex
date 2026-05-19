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
  *   3. (Eventually) replace the matching compiler-internal prelude entry.
  *
  * The opId format is `"<group>.<name>"`. Group conventions:
  *   - `libm.*`     — IEEE-754 scalar math reachable via the host's libm.
  *   - `test.*`     — test-only intrinsics; never ship in a release prelude.
  */
object NexIntrinsics:

  /** Every intrinsic opId known to the compiler. Backends MUST refuse to
    * compile a `TIntrinsic` whose opId is not in this set.
    */
  val Ids: Set[String] = Set(
    "test.identity",
    // §10.2 real-only scalar math that the source prelude carries via
    // bodyless `@intrinsic` declarations. Each id names a libm function
    // the backend bridges to directly. Adding a new entry here requires
    // a registration in every backend's dispatch table — interpreter
    // (NexInterpreter.intrinsicDispatch) and LLVM (emitIntrinsicCall),
    // plus an explicit notYet in MLIR until that backend grows libm
    // wiring.
    "libm.cbrt",
    "libm.floor", "libm.ceil", "libm.round", "libm.trunc",
    "libm.asin",  "libm.acos", "libm.atan",  "libm.atan2",
    "libm.sinh",  "libm.cosh", "libm.tanh",
    "libm.asinh", "libm.acosh","libm.atanh",
    "libm.log2",  "libm.log10",
  )

  /** Throw if `opId` is not a known intrinsic. Use this at the top of each
    * backend's dispatch path so a typo in source surfaces with a clear
    * "unknown intrinsic" message instead of MatchError.
    */
  def require(opId: String): Unit =
    if !Ids.contains(opId) then
      throw new RuntimeException(s"unknown intrinsic `$opId` — add it to NexIntrinsics.Ids")
