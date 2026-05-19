package io.github.edadma.nex

import java.io.{ByteArrayOutputStream, InputStream}
import io.github.edadma.path.Path
import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

/** Parity harness for the MLIR backend. Same contract as
  * [[NexParityBase]] (interpreter vs AOT, byte-for-byte stdout
  * agreement) but the AOT path is the MLIR lowering pipeline rather
  * than direct LLVM-IR emission:
  *
  *   1. Run [[NexMLIRCodegen]] to produce MLIR text.
  *   2. Shell `mlir-opt` with the pinned pass pipeline to lower
  *      tensor → memref → scf → cf → LLVM dialect.
  *   3. Shell `mlir-translate --mlir-to-llvmir` to emit LLVM IR text.
  *   4. Shell `clang -O1` linking `mlir_runtime.c` from the codegen
  *      resources directory.
  *   5. Run the binary, capture stdout, compare.
  *
  * Pinned to Homebrew LLVM 22 — pass names move between LLVM majors,
  * so the version matters. Override with `NEX_LLVM_HOME=/path/to/llvm`
  * (whose `bin/` contains `mlir-opt`, `mlir-translate`, `clang`).
  */
trait NexMLIRParityBase extends Matchers with Assertions:

  private def llvmHome: String =
    Option(System.getenv("NEX_LLVM_HOME")).getOrElse("/opt/homebrew/opt/llvm")

  private def llvmBin(tool: String): String = s"$llvmHome/bin/$tool"

  /** MLIR-opt pass pipeline. Order matters:
    * `bufferize-function-boundaries` MUST precede `convert-linalg-to-loops`
    * because loops operate on memrefs, not tensors;
    * `reconcile-unrealized-casts` is the closing cleanup.
    */
  private val mlirOptPasses: List[String] = List(
    "--one-shot-bufferize=bufferize-function-boundaries",
    "--convert-linalg-to-loops",
    "--convert-scf-to-cf",
    "--finalize-memref-to-llvm",
    "--convert-arith-to-llvm",
    "--convert-func-to-llvm",
    "--convert-cf-to-llvm",
    "--reconcile-unrealized-casts",
  )

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

  protected def runCompiled(src: String): String =
    val tmpDir = Path.createTempDirectory("nex-mlir-parity-")
    try
      val mlirFile    = tmpDir / "main.mlir"
      val loweredFile = tmpDir / "main.lowered.mlir"
      val llFile      = tmpDir / "main.ll"
      val rtFile      = tmpDir / "mlir_runtime.c"
      val binFile     = tmpDir / "main"

      val ast = new NexParser().parseProgram(src) match
        case Right(p)  => p
        case Left(err) => fail(s"compiler parse error: $err")
      val tp = new NexElaborator().elaborate(ast) match
        case Right(p)   => p
        case Left(errs) => fail(s"compiler elab errors: ${errs.map(_.toString).mkString("; ")}")
      val ir = new NexMLIRCodegen().compile(tp)
      mlirFile.writeText(ir)
      rtFile.writeText(loadRuntimeC())

      runProc(
        Seq(llvmBin("mlir-opt"), mlirFile.toPlatformString) ++ mlirOptPasses ++
          Seq("-o", loweredFile.toPlatformString),
        ctx = s"mlir-opt failed on:\n$src\n\n--- MLIR ---\n$ir",
      )

      runProc(
        Seq(llvmBin("mlir-translate"), "--mlir-to-llvmir",
            loweredFile.toPlatformString, "-o", llFile.toPlatformString),
        ctx = s"mlir-translate failed on:\n$src",
      )

      runProc(
        Seq(llvmBin("clang"), "-O1", "-o", binFile.toPlatformString,
            llFile.toPlatformString, rtFile.toPlatformString),
        ctx = s"clang failed on:\n$src",
      )

      val execProc = new ProcessBuilder(binFile.toPlatformString)
        .redirectErrorStream(true)
        .start()
      val out = readAll(execProc.getInputStream)
      execProc.waitFor()
      out
    finally
      deleteRecursively(tmpDir)

  protected def parityCheck(src: String, expected: String = null): Unit =
    val interpOut = runInterpreter(src)
    val nativeOut = runCompiled(src)
    if interpOut != nativeOut then
      fail(
        s"interpreter and MLIR-AOT disagree on:\n$src\n\n" +
        s"--- interpreter ---\n$interpOut\n--- mlir ---\n$nativeOut",
      )
    if expected != null && interpOut != expected then
      fail(
        s"output doesn't match expected on:\n$src\n\n" +
        s"--- expected ---\n$expected\n--- got ---\n$interpOut",
      )

  private def runProc(cmd: Seq[String], ctx: String): Unit =
    import scala.jdk.CollectionConverters.*
    val p   = new ProcessBuilder(cmd.asJava).redirectErrorStream(true).start()
    val out = readAll(p.getInputStream)
    val rc  = p.waitFor()
    if rc != 0 then
      fail(s"$ctx\n--- command ---\n${cmd.mkString(" ")}\n--- output ---\n$out")

  private def loadRuntimeC(): String =
    val stream = getClass.getResourceAsStream("/io/github/edadma/nex/mlir_runtime.c")
    if stream == null then fail("mlir_runtime.c resource missing")
    try new String(stream.readAllBytes, "UTF-8")
    finally stream.close()

  private def readAll(is: InputStream): String =
    val sb  = new StringBuilder
    val buf = new Array[Byte](8192)
    var n   = is.read(buf)
    while n > 0 do
      sb.append(new String(buf, 0, n, "UTF-8"))
      n = is.read(buf)
    sb.toString

  private def deleteRecursively(p: Path): Unit =
    if p.exists then
      if p.isDirectory then
        p.listDirectory().foreach(e => deleteRecursively(p / e.name))
        p.delete()
      else
        p.delete()
