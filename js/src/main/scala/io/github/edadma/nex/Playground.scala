package io.github.edadma.nex

import java.io.ByteArrayOutputStream
import scala.scalajs.js.annotation.JSExportTopLevel

/** Browser-facing entry point for the docs-site playground.
  *
  * Bypasses [[NexModuleLoader]] entirely so the bundle doesn't need a
  * filesystem at runtime — the playground runs single-file programs
  * that the user types into a textarea. Multi-file projects would
  * need Node + NodeFS, which is appropriate for a CLI but not for a
  * browser bundle.
  *
  * Returns a single String containing whichever of: parse-error
  * diagnostics, elaboration-error diagnostics, the program's captured
  * stdout, or a trap message (prepended to whatever output was
  * produced before the trap). The caller renders the string into
  * whatever output pane it wants.
  */
object Playground:

  @JSExportTopLevel("nexRunSource")
  def runSource(src: String): String =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => return s"parse error: $err"

    val tp = new NexElaborator().elaborate(ast) match
      case Right(p)   => p
      case Left(errs) =>
        return s"elaboration errors:\n${errs.map(e => "  " + e.toString).mkString("\n")}"

    val buf = new ByteArrayOutputStream
    try
      Console.withOut(buf):
        new NexInterpreter().runProgram(tp)
      buf.toString
    catch
      case t: Throwable =>
        // Surface whatever output the program emitted before the trap,
        // then the trap message itself.
        val pre = buf.toString
        if pre.nonEmpty then s"$pre\ntrap: ${t.getMessage}"
        else s"trap: ${t.getMessage}"
