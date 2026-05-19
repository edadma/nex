package io.github.edadma.nex

import java.io.{ByteArrayOutputStream, InputStream}
import io.github.edadma.path.Path
import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

/** Shared infrastructure for parity tests — runs the same Nex source
  * through the in-process interpreter AND the AOT codegen + clang,
  * then asserts byte-for-byte agreement.
  *
  * JVM-only by design: shelling out to `clang -O1` is the whole point.
  * The cross-platform tests live under `shared/src/test/scala/` and
  * stay JS / Native / JVM portable so the system's distribution-target
  * suitability is exercised independently.
  *
  * Each parity test gets its own tempdir (via `Path.createTempDirectory`)
  * for `main.nex`, `main.ll`, and the compiled binary; the directory
  * is recursive-deleted on completion, pass or fail.
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

  /** Run `src` through the AOT path: codegen → `clang -O1` → execute.
    * Returns the captured stdout (stderr is merged in via redirectErrorStream
    * so clang errors surface in the test failure message).
    */
  protected def runCompiled(src: String): String =
    val tmpDir = Path.createTempDirectory("nex-parity-")
    try
      val srcFile = tmpDir / "main.nex"
      val llFile  = tmpDir / "main.ll"
      val binFile = tmpDir / "main"
      srcFile.writeText(src)

      // The parser / elaborator already proved correct for the
      // interpreter path; re-running here would be wasted work, but
      // the codegen needs its own elaborator output, so we redo it.
      val ast = new NexParser().parseProgram(src) match
        case Right(p)  => p
        case Left(err) => fail(s"compiler parse error: $err")
      val tp = new NexElaborator().elaborate(ast) match
        case Right(p)   => p
        case Left(errs) => fail(s"compiler elab errors: ${errs.map(_.toString).mkString("; ")}")
      val ir = new NexLLVMCodegen().compile(tp)
      llFile.writeText(ir)

      val clangProc = new ProcessBuilder(
        "clang", "-O1", "-o", binFile.toPlatformString, llFile.toPlatformString
      ).redirectErrorStream(true).start()
      val clangOut = readAll(clangProc.getInputStream)
      val clangRc  = clangProc.waitFor()
      if clangRc != 0 then
        fail(
          s"clang failed (rc=$clangRc) on:\n$src\n\n" +
          s"--- clang output ---\n$clangOut\n--- IR (first 60 lines) ---\n" +
          ir.linesIterator.take(60).mkString("\n"),
        )

      val execProc = new ProcessBuilder(binFile.toPlatformString)
        .redirectErrorStream(true)
        .start()
      val out = readAll(execProc.getInputStream)
      execProc.waitFor()
      out
    finally
      deleteRecursively(tmpDir)

  /** Run `src` through BOTH paths and assert the captured stdouts
    * agree byte-for-byte. When `expected` is non-null, additionally
    * assert both equal that literal (regression protection beyond
    * just "they agree").
    */
  protected def parityCheck(src: String, expected: String = null): Unit =
    val interpOut = runInterpreter(src)
    val nativeOut = runCompiled(src)
    if interpOut != nativeOut then
      fail(
        s"interpreter and AOT disagree on:\n$src\n\n" +
        s"--- interpreter ---\n$interpOut\n--- native ---\n$nativeOut",
      )
    if expected != null then
      if interpOut != expected then
        fail(
          s"output doesn't match expected on:\n$src\n\n" +
          s"--- expected ---\n$expected\n--- got ---\n$interpOut",
        )

  // ---------------------------------------------------------------------------
  // Internal helpers.
  // ---------------------------------------------------------------------------

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
