package io.github.edadma.nex

import java.io.InputStream
import io.github.edadma.path.Path
import org.scalatest.Assertions

/** A compilation backend that can take Nex source, produce a native
  * binary, run it, and return the captured stdout. Backends are the
  * parity harness's notion of "AOT compile + execute"; the in-process
  * interpreter is a separate path, used as the spec oracle every
  * backend's output is checked against.
  *
  * Backends are JVM-only — both clang and the MLIR toolchain are
  * shelled out, so there is no JS / Native equivalent. The shared
  * parity tests under `shared/src/test/scala/` exercise the
  * cross-platform pieces (parser, elaborator, interpreter) without
  * touching backends.
  */
trait Backend extends Assertions:
  /** Short human-readable name used in failure messages. */
  def name: String

  /** Compile and run `src`. Returns stdout (stderr merged via
    * `redirectErrorStream` so toolchain errors surface in the failure
    * message). Throws via `fail` on parse, elab, codegen, or tool
    * failure.
    */
  def compileAndRun(src: String): String

  /** Shared subprocess runner: capture combined stdout+stderr, return
    * exit code alongside output. Used by every backend's tool shell-outs.
    */
  protected def runProc(cmd: Seq[String]): (Int, String) =
    import scala.jdk.CollectionConverters.*
    val pb = new ProcessBuilder(cmd.asJava).redirectErrorStream(true)
    val p  = pb.start()
    val out = Backend.readAll(p.getInputStream)
    (p.waitFor(), out)

  protected def deleteRecursively(p: Path): Unit =
    if p.exists then
      if p.isDirectory then
        p.listDirectory().foreach(e => deleteRecursively(p / e.name))
        p.delete()
      else
        p.delete()

object Backend:
  def readAll(is: InputStream): String =
    val sb  = new StringBuilder
    val buf = new Array[Byte](8192)
    var n   = is.read(buf)
    while n > 0 do
      sb.append(new String(buf, 0, n, "UTF-8"))
      n = is.read(buf)
    sb.toString

/** LLVM backend: NexLLVMCodegen → `clang -O1` → binary. The
  * production codegen path; covers the full v0 surface.
  */
object LlvmBackend extends Backend:
  val name = "llvm"

  def compileAndRun(src: String): String =
    val tmpDir = Path.createTempDirectory("nex-parity-llvm-")
    try
      val srcFile = tmpDir / "main.nex"
      val llFile  = tmpDir / "main.ll"
      val binFile = tmpDir / "main"
      srcFile.writeText(src)

      val tp = parseAndElab(src)
      val ir = new NexLLVMCodegen().compile(tp)
      llFile.writeText(ir)

      val (rc, out) = runProc(Seq(
        "clang", "-O1", "-o", binFile.toPlatformString, llFile.toPlatformString,
      ))
      if rc != 0 then
        fail(
          s"clang failed (rc=$rc) on:\n$src\n\n" +
          s"--- clang output ---\n$out\n--- IR (first 60 lines) ---\n" +
          ir.linesIterator.take(60).mkString("\n"),
        )

      val (_, execOut) = runProc(Seq(binFile.toPlatformString))
      execOut
    finally deleteRecursively(tmpDir)

  private def parseAndElab(src: String): TProgram =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"compiler parse error: $err")
    new NexElaborator().elaborate(ast) match
      case Right(p)   => p
      case Left(errs) => fail(s"compiler elab errors: ${errs.map(_.toString).mkString("; ")}")

/** MLIR backend: NexMLIRCodegen → mlir-opt → mlir-translate →
  * `clang -O1` (linking the embedded mlir_runtime.c). Only handles
  * the small subset exercised by milestones 1–5; anything else
  * throws `NexMLIRCodegen.notYet`.
  *
  * Toolchain pinned to Homebrew LLVM 22 by default. Override via
  * `NEX_LLVM_HOME=/path/to/llvm` for other installs.
  */
object MlirBackend extends Backend:
  val name = "mlir"

  private def llvmHome: String =
    Option(System.getenv("NEX_LLVM_HOME")).getOrElse("/opt/homebrew/opt/llvm")

  private def tool(t: String): String = s"$llvmHome/bin/$t"

  /** Pass pipeline lifted from the milestone-1 PoC. Order matters:
    * bufferization runs before linalg-to-loops because loops operate
    * on memrefs, not tensors; `reconcile-unrealized-casts` is the
    * closing cleanup.
    */
  private val mlirOptPasses: List[String] = List(
    "--one-shot-bufferize=bufferize-function-boundaries",
    "--convert-linalg-to-loops",
    "--convert-scf-to-cf",
    "--expand-strided-metadata",
    "--finalize-memref-to-llvm",
    "--convert-arith-to-llvm",
    "--convert-func-to-llvm",
    "--convert-cf-to-llvm",
    "--reconcile-unrealized-casts",
  )

  def compileAndRun(src: String): String =
    val tmpDir = Path.createTempDirectory("nex-parity-mlir-")
    try
      val mlirFile    = tmpDir / "main.mlir"
      val loweredFile = tmpDir / "main.lowered.mlir"
      val llFile      = tmpDir / "main.ll"
      val rtFile      = tmpDir / "mlir_runtime.c"
      val binFile     = tmpDir / "main"

      val tp = parseAndElab(src)
      val ir = new NexMLIRCodegen().compile(tp)
      mlirFile.writeText(ir)
      rtFile.writeText(loadRuntimeC())

      shellOrFail(
        Seq(tool("mlir-opt"), mlirFile.toPlatformString) ++ mlirOptPasses ++
          Seq("-o", loweredFile.toPlatformString),
        ctx = s"mlir-opt failed on:\n$src\n\n--- MLIR ---\n$ir",
      )

      shellOrFail(
        Seq(tool("mlir-translate"), "--mlir-to-llvmir",
            loweredFile.toPlatformString, "-o", llFile.toPlatformString),
        ctx = s"mlir-translate failed on:\n$src",
      )

      shellOrFail(
        Seq(tool("clang"), "-O1", "-o", binFile.toPlatformString,
            llFile.toPlatformString, rtFile.toPlatformString),
        ctx = s"clang failed on:\n$src",
      )

      val (_, execOut) = runProc(Seq(binFile.toPlatformString))
      execOut
    finally deleteRecursively(tmpDir)

  private def parseAndElab(src: String): TProgram =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"compiler parse error: $err")
    new NexElaborator().elaborate(ast) match
      case Right(p)   => p
      case Left(errs) => fail(s"compiler elab errors: ${errs.map(_.toString).mkString("; ")}")

  private def shellOrFail(cmd: Seq[String], ctx: String): Unit =
    val (rc, out) = runProc(cmd)
    if rc != 0 then
      fail(s"$ctx\n--- command ---\n${cmd.mkString(" ")}\n--- output ---\n$out")

  private def loadRuntimeC(): String =
    val stream = getClass.getResourceAsStream("/io/github/edadma/nex/mlir_runtime.c")
    if stream == null then fail("mlir_runtime.c resource missing")
    try new String(stream.readAllBytes, "UTF-8")
    finally stream.close()
