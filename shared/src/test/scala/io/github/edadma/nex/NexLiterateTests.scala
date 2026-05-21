package io.github.edadma.nex

import java.io.ByteArrayOutputStream
import io.github.edadma.path.Path
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Unit + end-to-end tests for the `.lnex` literate preprocessor.
  *
  * The unit half exercises [[NexLiterate.preprocess]] directly on
  * crafted strings — each rule (prose stripping, code dedent, fenced
  * blocks, line-number preservation) gets its own case.
  *
  * The end-to-end half writes a real `.lnex` file to a tmp project,
  * loads it through the module loader, runs the program, and asserts
  * the output. That covers the wiring in `readNexSource` /
  * `listNexFiles` plus the CLI suffix-stripping code path.
  */
class NexLiterateTests extends AnyWordSpec with Matchers:

  import NexLiterate.preprocess

  "the literate preprocessor" should {

    "leave a plain code line alone after dedent" in {
      preprocess("    def main() = print(42)\n") shouldBe "def main() = print(42)\n"
    }

    "treat a leading tab as one indent level" in {
      preprocess("\tdef main() = print(7)\n") shouldBe "def main() = print(7)\n"
    }

    "blank out column-0 prose lines" in {
      val src = "This is prose.\n    val x = 1\nMore prose.\n"
      preprocess(src) shouldBe "\nval x = 1\n\n"
    }

    "treat 1-3 leading spaces as prose, not code" in {
      val src = "   list item continuation\n    val x = 1\n"
      preprocess(src) shouldBe "\nval x = 1\n"
    }

    "preserve line numbers exactly" in {
      val src = "# Heading\n\n    val n = 10\n\n## Subheading\n\n    print(n)\n"
      val out = preprocess(src)
      out.count(_ == '\n') shouldBe src.count(_ == '\n')
      out shouldBe "\n\nval n = 10\n\n\n\nprint(n)\n"
    }

    "strip fenced code blocks entirely (non-Nex content)" in {
      val src =
        "Some prose.\n```\n+---+\n|abc|\n+---+\n```\n    val x = 1\n"
      preprocess(src) shouldBe "\n\n\n\n\n\nval x = 1\n"
    }

    "preserve indented code that itself uses further indentation" in {
      // Inside the code block, `if/then` uses Nex's own indent-sensitive
      // syntax. The preprocessor strips exactly one level, so the inner
      // indentation survives.
      val src =
        "Example:\n    if x > 0 then\n        print(x)\n    end\n"
      preprocess(src) shouldBe "\nif x > 0 then\n    print(x)\nend\n"
    }

    "preserve CRLF line endings" in {
      val src = "prose\r\n    val x = 1\r\n"
      preprocess(src) shouldBe "\r\nval x = 1\r\n"
    }

    "handle a file with no trailing newline" in {
      val src = "prose\n    val x = 1"
      preprocess(src) shouldBe "\nval x = 1"
    }
  }

  "the loader's `.lnex` integration" should {

    "load and run a literate Nex file end-to-end" in {
      val src =
        """# Greeting
          |
          |This file demonstrates literate Nex. The compiler ignores
          |the prose and reads only the indented blocks below.
          |
          |    def main() =
          |        val name = "world"
          |        print(name)
          |""".stripMargin
      val root  = Path.createTempDirectory("nex-lit-test-")
      val entry = root / Path("main.lnex")
      entry.writeText(src)

      val modules = new NexModuleLoader(root.toPlatformString)
        .loadFrom(entry.toPlatformString) match
        case Right(ms)  => ms
        case Left(errs) => fail(s"loader: ${errs.mkString("; ")}")
      val tp = new NexElaborator().elaborateProject(modules) match
        case Right(p)   => p
        case Left(errs) => fail(s"elab: ${errs.map(_.toString).mkString("; ")}")
      val buf = new ByteArrayOutputStream
      Console.withOut(buf) { new NexInterpreter().runProgram(tp) }
      buf.toString.trim shouldBe "world"
    }

    "let a module contain a mix of .nex and .lnex files" in {
      val root        = Path.createTempDirectory("nex-lit-mix-")
      val main        = root / Path("main.nex")
      val helperLit   = root / Path("helpers/sq.lnex")
      val helperPlain = root / Path("helpers/twice.nex")
      helperLit.parent.foreach(_.createDirectories())
      main.writeText("import helpers.{square, twice}\ndef main() = print(twice(square(3)))\n")
      helperLit.writeText(
        """module helpers
          |
          |The squaring helper:
          |
          |    def square(x: integer) = x * x
          |""".stripMargin,
      )
      helperPlain.writeText("module helpers\ndef twice(x: integer) = x + x\n")

      val modules = new NexModuleLoader(root.toPlatformString)
        .loadFrom(main.toPlatformString) match
        case Right(ms)  => ms
        case Left(errs) => fail(s"loader: ${errs.mkString("; ")}")
      val tp = new NexElaborator().elaborateProject(modules) match
        case Right(p)   => p
        case Left(errs) => fail(s"elab: ${errs.map(_.toString).mkString("; ")}")
      val buf = new ByteArrayOutputStream
      Console.withOut(buf) { new NexInterpreter().runProgram(tp) }
      buf.toString.trim shouldBe "18"
    }
  }
