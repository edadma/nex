package io.github.edadma.nex

import java.io.ByteArrayOutputStream
import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

/** Shared infrastructure for parity tests — runs the same Nex source
  * through the in-process interpreter AND one or more AOT backends,
  * then asserts byte-for-byte agreement.
  *
  * JVM-only by design: every backend shells out to native tooling
  * (clang, mlir-opt, mlir-translate). The cross-platform tests live
  * under `shared/src/test/scala/` and stay JS / Native / JVM
  * portable so the system's distribution-target suitability is
  * exercised independently.
  *
  * Two parity entry points:
  *   - `parityCheck(src, expected)` — default LLVM-backend-only path;
  *     used by every existing test, signature kept stable.
  *   - `parityCheckOn(backends, src, expected)` — explicit
  *     backend list; used by MLIR-specific tests and (eventually)
  *     cross-backend tests as the MLIR coverage grows.
  *
  * The "is MLIR ready to be default?" question reduces to "how many
  * `parityCheck` tests can flip to `parityCheckOn(Seq(LlvmBackend,
  * MlirBackend), ...)` without regressing." That's the measurable
  * progress bar for the strangler-fig migration.
  */
trait NexParityBase extends Matchers with Assertions:

  /** Run `src` through the in-process interpreter and capture stdout. */
  protected def runInterpreter(src: String): String =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"interpreter parse error: $err")
    val tp = new NexElaborator().elaborate(ast) match
      case Right(p)   => p
      case Left(errs) => fail(s"interpreter elab errors: ${errs.map(_.toString).mkString("; ")}")
    val buf = new ByteArrayOutputStream
    Console.withOut(buf):
      new NexInterpreter().runProgram(tp)
    buf.toString

  /** Convenience wrapper for the LLVM backend's binary path. Kept
    * for tests that want to inspect the native output without doing
    * a full parity comparison.
    */
  protected def runCompiled(src: String): String =
    LlvmBackend.compileAndRun(src)

  /** Default parity check: run on the in-process interpreter and the
    * LLVM backend, assert byte-for-byte agreement, optionally assert
    * both equal `expected`. Signature preserved across the
    * Backend-trait refactor so the ~900 existing call sites need
    * no churn.
    */
  protected def parityCheck(src: String, expected: String = null): Unit =
    parityCheckOn(Seq(LlvmBackend), src, expected)

  /** Backend-list parity check. Runs the in-process interpreter once,
    * each backend once, asserts every backend's stdout matches the
    * interpreter byte-for-byte. When `expected` is non-null,
    * additionally asserts the interpreter output equals that literal
    * (regression protection beyond just "they agree").
    */
  protected def parityCheckOn(
      backends: Seq[Backend],
      src:      String,
      expected: String = null,
  ): Unit =
    val interpOut = runInterpreter(src)
    for backend <- backends do
      val nativeOut = backend.compileAndRun(src)
      if interpOut != nativeOut then
        fail(
          s"interpreter and ${backend.name} disagree on:\n$src\n\n" +
          s"--- interpreter ---\n$interpOut\n--- ${backend.name} ---\n$nativeOut",
        )
    if expected != null && interpOut != expected then
      fail(
        s"output doesn't match expected on:\n$src\n\n" +
        s"--- expected ---\n$expected\n--- got ---\n$interpOut",
      )
