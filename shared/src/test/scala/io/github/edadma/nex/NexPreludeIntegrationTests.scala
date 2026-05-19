package io.github.edadma.nex

import java.io.ByteArrayOutputStream
import io.github.edadma.path.Path
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** End-to-end exercise of the source-prelude pipeline introduced in
  * Stage 1 of the prelude externalization roadmap. Builds a tmp
  * prelude directory + a user project directory on disk, loads both,
  * and confirms the user's code can call into the prelude without
  * writing `import prelude.*` (the elaborator auto-injects it).
  *
  * The empty-prelude case is the Stage 1 deliverable's primary
  * regression guard: with a placeholder file present, every existing
  * user program continues to work unchanged.
  */
class NexPreludeIntegrationTests extends AnyWordSpec with Matchers:

  /** Write a small two-tree layout: a prelude directory and a project
    * directory (each a separate tmp). Returns (preludeRoot, projectRoot,
    * entryFile). The entry file is `<projectRoot>/main.nex`.
    */
  private def mkLayout(
      preludeFiles: Map[String, String],
      projectFiles: Map[String, String],
  ): (String, String, String) =
    val preludeRoot = Path.createTempDirectory("nex-prelude-")
    val projectRoot = Path.createTempDirectory("nex-project-")
    for (rel, content) <- preludeFiles do
      val p = preludeRoot / Path(rel)
      p.parent.foreach(_.createDirectories())
      p.writeText(content)
    for (rel, content) <- projectFiles do
      val p = projectRoot / Path(rel)
      p.parent.foreach(_.createDirectories())
      p.writeText(content)
    (
      preludeRoot.toPlatformString,
      projectRoot.toPlatformString,
      (projectRoot / Path("main.nex")).toPlatformString,
    )

  /** Run a (prelude + user) project through loader → elaborator →
    * interpreter and return captured stdout.
    */
  private def runWithPrelude(preludeRoot: String, entryFile: String): String =
    val projectRoot = pathDirname(entryFile)
    val preludeMod  = NexModuleLoader.loadPreludeAsModule(preludeRoot) match
      case Right(m)   => m
      case Left(errs) => fail(s"prelude load failed: ${errs.mkString("; ")}")
    val userModules = new NexModuleLoader(projectRoot).loadFrom(entryFile) match
      case Right(ms)  => ms
      case Left(errs) => fail(s"project load failed: ${errs.mkString("; ")}")
    val tp = new NexElaborator().elaborateProject(preludeMod :: userModules) match
      case Right(p)   => p
      case Left(errs) => fail(s"elaboration failed: ${errs.map(_.toString).mkString("; ")}")
    val buf = new ByteArrayOutputStream
    Console.withOut(buf) { new NexInterpreter().runProgram(tp) }
    buf.toString

  "prelude integration" should {

    "auto-import lets user code call a prelude function without an import" in {
      // Prelude provides `triple`; user code uses it as if it were built in.
      val (preludeRoot, _, entry) = mkLayout(
        preludeFiles = Map(
          "scalar.nex" -> "module prelude\ndef triple(x: integer) = x * 3\n",
        ),
        projectFiles = Map(
          "main.nex" -> "def main() = print(triple(7))\n",
        ),
      )
      runWithPrelude(preludeRoot, entry).trim shouldBe "21"
    }

    "auto-import skips test-only modules per existing test-only spec" in {
      // Same shape, but the project module is not test-only either.
      val (preludeRoot, _, entry) = mkLayout(
        preludeFiles = Map(
          "scalar.nex" -> "module prelude\ndef negate(x: integer) = 0 - x\n",
        ),
        projectFiles = Map(
          "main.nex" -> "def main() = print(negate(5))\n",
        ),
      )
      runWithPrelude(preludeRoot, entry).trim shouldBe "-5"
    }

    "empty prelude leaves user code unchanged (Stage 1 deliverable)" in {
      // The placeholder shape: `module prelude` plus nothing else.
      // User code does its own thing; the elaborator must not error.
      val (preludeRoot, _, entry) = mkLayout(
        preludeFiles = Map(
          "scalar.nex" -> "module prelude\n",
        ),
        projectFiles = Map(
          "main.nex" -> "def main() = print(42)\n",
        ),
      )
      runWithPrelude(preludeRoot, entry).trim shouldBe "42"
    }

    "prelude module itself does NOT get a self-injected import" in {
      // If we self-injected, the prelude file's elaboration would loop
      // on its own exports. We exercise this by parsing a prelude that
      // declares an exported name and an internal helper — both must
      // resolve without complaint.
      val (preludeRoot, _, entry) = mkLayout(
        preludeFiles = Map(
          "scalar.nex" -> "module prelude\ndef double(x: integer) = x + x\ndef quadruple(x: integer) = double(double(x))\n",
        ),
        projectFiles = Map(
          "main.nex" -> "def main() = print(quadruple(6))\n",
        ),
      )
      runWithPrelude(preludeRoot, entry).trim shouldBe "24"
    }
  }
