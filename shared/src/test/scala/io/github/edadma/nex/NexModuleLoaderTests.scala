package io.github.edadma.nex

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Multi-file module loading + cross-module import resolution. Builds a
  * tiny project on disk in a fresh tmp directory per test, runs it through
  * the loader + elaborator + interpreter, and checks the program output.
  *
  * The tests target the JVM (the only platform with a real filesystem in
  * this build); shared-suite execution still skips Native/JS by virtue of
  * how the build is configured.
  */
class NexModuleLoaderTests extends AnyWordSpec with Matchers:

  /** Make a tmp project directory and write the named files into it.
    * Returns the (root path, entry file path).
    */
  private def mkProject(entry: String, files: Map[String, String]): (String, String) =
    val root = Files.createTempDirectory("nex-mod-test-")
    for (relPath, content) <- files do
      val abs = root.resolve(relPath)
      Files.createDirectories(abs.getParent)
      Files.writeString(abs, content)
    (root.toString, root.resolve(entry).toString)

  /** Load + elaborate + run a project. Captures stdout. */
  private def runProject(entryFile: String): String =
    val root    = pathDirname(entryFile)
    val modules = new NexModuleLoader(root).loadFrom(entryFile) match
      case Right(ms)  => ms
      case Left(errs) => fail(s"module loading failed: ${errs.mkString("; ")}")
    val tp = new NexElaborator().elaborateProject(modules) match
      case Right(p)   => p
      case Left(errs) => fail(s"elaboration failed: ${errs.map(_.toString).mkString("; ")}")
    val buf = new ByteArrayOutputStream
    Console.withOut(buf) { new NexInterpreter().runProgram(tp) }
    buf.toString

  "module loader" should {

    "load a single-file project (no imports, no module decl)" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex" -> "def main() = print(42)\n",
      ))
      runProject(entry).trim shouldBe "42"
    }

    "discover and load a sibling-directory module via import" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"        -> "import helpers.{square}\ndef main() = print(square(7))\n",
        "helpers/ops.nex" -> "module helpers\ndef square(x: integer) = x * x\n",
      ))
      runProject(entry).trim shouldBe "49"
    }

    "merge multiple files in the same module folder" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"          -> "import helpers.{square, twice}\ndef main() = print(twice(square(3)))\n",
        "helpers/sq.nex"    -> "module helpers\ndef square(x: integer) = x * x\n",
        "helpers/twice.nex" -> "module helpers\ndef twice(x: integer) = x + x\n",
      ))
      // square(3) = 9; twice(9) = 18
      runProject(entry).trim shouldBe "18"
    }

    "honor `as` aliases on imports" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"        -> "import helpers.{square as sq}\ndef main() = print(sq(8))\n",
        "helpers/ops.nex" -> "module helpers\ndef square(x: integer) = x * x\n",
      ))
      runProject(entry).trim shouldBe "64"
    }

    "chain imports across three modules in topo order" in {
      // main imports b, b imports a. Loader should visit a before b before main.
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"   -> "import b.{step2}\ndef main() = print(step2(5))\n",
        "a/a.nex"    -> "module a\ndef step1(x: integer) = x + 10\n",
        "b/b.nex"    -> "module b\nimport a.{step1}\ndef step2(x: integer) = step1(x) * 2\n",
      ))
      // step1(5) = 15; step2(5) = 30
      runProject(entry).trim shouldBe "30"
    }

    "report a clean error when an import path doesn't exist" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex" -> "import does_not_exist.{x}\ndef main() = print(0)\n",
      ))
      val root    = pathDirname(entry)
      val modules = new NexModuleLoader(root).loadFrom(entry)
      modules.left.toOption.get.exists(_.contains("does_not_exist")) shouldBe true
    }

    "report a clean error when an import names a missing member" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"        -> "import helpers.{nope}\ndef main() = print(0)\n",
        "helpers/ops.nex" -> "module helpers\ndef square(x: integer) = x * x\n",
      ))
      val root    = pathDirname(entry)
      val modules = new NexModuleLoader(root).loadFrom(entry).getOrElse(fail("module loading should have succeeded"))
      val result  = new NexElaborator().elaborateProject(modules)
      result.left.toOption.get.exists(_.toString.contains("no public member `nope`")) shouldBe true
    }

    "reject a `module` decl that doesn't match the directory path" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"        -> "import helpers.{f}\ndef main() = print(f(1))\n",
        "helpers/ops.nex" -> "module wrong_name\ndef f(x: integer) = x\n",
      ))
      val root    = pathDirname(entry)
      val modules = new NexModuleLoader(root).loadFrom(entry)
      modules.left.toOption.get.exists(_.contains("does not match")) shouldBe true
    }

    "import a struct type and its constructor across modules (spec §9.4)" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"        -> "import geom.{Point}\ndef main() = print(Point(3.0, 4.0).x)\n",
        "geom/types.nex"  -> "module geom\nstruct Point\n  x: real\n  y: real\nend\n",
      ))
      runProject(entry).trim shouldBe "3.0"
    }

    "private struct is invisible to importing modules (spec §9.4)" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"        -> "import geom.{Hidden}\ndef main() = print(0)\n",
        "geom/types.nex"  -> "module geom\nprivate struct Hidden\n  x: real\nend\n",
      ))
      val root    = pathDirname(entry)
      val modules = new NexModuleLoader(root).loadFrom(entry).getOrElse(fail("loader"))
      val result  = new NexElaborator().elaborateProject(modules)
      result.left.toOption.get.exists(_.toString.contains("no public member `Hidden`")) shouldBe true
    }

    "private val is invisible to importing modules" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"        -> "import lib.{secret}\ndef main() = print(secret)\n",
        "lib/c.nex"       -> "module lib\nprivate val secret = 42\n",
      ))
      val root    = pathDirname(entry)
      val modules = new NexModuleLoader(root).loadFrom(entry).getOrElse(fail("loader"))
      val result  = new NexElaborator().elaborateProject(modules)
      result.left.toOption.get.exists(_.toString.contains("no public member `secret`")) shouldBe true
    }

    "public val from another module is visible" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"        -> "import lib.{answer}\ndef main() = print(answer)\n",
        "lib/c.nex"       -> "module lib\nval answer = 42\n",
      ))
      runProject(entry).trim shouldBe "42"
    }

    "private const is invisible to importing modules" in {
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"  -> "import lib.{MAGIC}\ndef main() = print(MAGIC)\n",
        "lib/c.nex" -> "module lib\nprivate const MAGIC = 99\n",
      ))
      val root    = pathDirname(entry)
      val modules = new NexModuleLoader(root).loadFrom(entry).getOrElse(fail("loader"))
      val result  = new NexElaborator().elaborateProject(modules)
      result.left.toOption.get.exists(_.toString.contains("no public member `MAGIC`")) shouldBe true
    }

    "mark a module with `@test module` as test-only (spec §9.6)" in {
      // The loader records `isTestOnly` per spec §9.6. The CLI uses
      // this flag to filter test fixtures out of `nex run` builds.
      val (_, entry) = mkProject("main.nex", Map(
        "main.nex"              -> "import test_fixtures.{helper}\ndef main() = print(helper())\n",
        "test_fixtures/fix.nex" -> "@test module test_fixtures\ndef helper() = 42\n",
      ))
      val root    = pathDirname(entry)
      val modules = new NexModuleLoader(root).loadFrom(entry).getOrElse(fail("loader"))
      modules.find(_.path == List("test_fixtures")).get.isTestOnly shouldBe true
      modules.find(_.path == Nil).get.isTestOnly shouldBe false
    }
  }
