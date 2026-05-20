package io.github.edadma.nex

import java.io.ByteArrayOutputStream
import scala.scalajs.js.annotation.JSExportTopLevel

/** Browser-facing entry point for the docs-site playground.
  *
  * The playground runs single-file programs the user types into a
  * textarea. To keep the JS bundle filesystem-free (the same code
  * runs on Node CLI for `nex run` / `nex test` with full file I/O, but
  * THIS entry must stay browser-safe), the elaboration call here goes
  * directly through [[NexElaborator.elaborateProject]] with an
  * in-memory [[LoadedModule]] — skipping the [[NexElaborator.elaborate]]
  * convenience entry that auto-loads the source-prelude via
  * [[NexSysroot.findPreludeRoot]]. Otherwise Scala.js's link graph
  * picks up [[NexSysroot]] → `platform.scala` → `cross_platform`,
  * which emits a top-level `import * as $i_fs from "fs"` the browser
  * can't satisfy.
  *
  * The source prelude itself (the libm transcendentals plus their
  * complex extensions in `prelude/scalar.nex`) is baked into the
  * bundle at build time via [[BakedPrelude]] — see the
  * `sourceGenerators` block in `build.sbt`. At runtime the playground
  * parses each baked file, builds a `LoadedModule(path = ["prelude"],
  * ...)`, and prepends it to the user module list. Result: the
  * playground sees the same prelude bindings the JVM CLI sees,
  * including `sin`, `cos`, `sqrt(complex)`, etc.
  *
  * Returns a single String containing whichever of: parse-error
  * diagnostics, elaboration-error diagnostics, the program's captured
  * stdout, or a trap message (prepended to whatever output was
  * produced before the trap). The caller renders the string into
  * whatever output pane it wants.
  */
object Playground:

  /** Lazily-parsed baked prelude module. Re-used across `runSource`
    * calls so we pay the parse cost once per page-load, not once per
    * Run-button click. A parse failure here is a build-time bug — the
    * generated `BakedPrelude.scala` must always contain valid Nex —
    * so we let the exception escape rather than swallowing it. */
  private lazy val preludeModule: LoadedModule =
    val files = BakedPrelude.files.map { case (name, src) =>
      new NexParser().parseProgram(src) match
        case Right(ast) => FileEntry(name, ast)
        case Left(err)  =>
          throw new RuntimeException(s"baked prelude `$name` failed to parse: $err")
    }
    LoadedModule(List("prelude"), files, isTestOnly = false, imports = Nil)

  @JSExportTopLevel("nexRunSource")
  def runSource(src: String): String =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => return s"parse error: $err"

    // Wrap the single inline source into a one-file root module and
    // hand it straight to `elaborateProject` so the source-prelude
    // auto-load path (which reaches NexSysroot → cross_platform → fs)
    // stays unreachable from the playground's link graph. Prepend the
    // baked prelude module so the elaborator sees the same overload
    // set the JVM CLI's `loadAndElaborate` provides.
    val modDecl  = ast.decls.collectFirst { case m: ModuleDeclAST => m }
    val path     = modDecl.map(_.path).getOrElse(Nil)
    val testOnly = modDecl.exists(_.isTestOnly)
    val userMod  = LoadedModule(path, List(FileEntry("<inline>", ast)), testOnly, Nil)

    val tp = new NexElaborator().elaborateProject(List(preludeModule, userMod)) match
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
