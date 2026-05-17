package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Tests for [[NexLexer]]. Each test creates a fresh lexer and calls `.scan`
  * to get a `List[Token]`, then maps tokens to a compact string form for
  * easy assertion.
  */
class NexLexerTests extends AnyWordSpec with Matchers:

  /** Compact rendering of the token stream produced by `lex(src)`. */
  private def tokens(src: String): List[String] =
    val l = new NexLexer
    l.scan(src).map {
      case _: l.Newline.type           => "NL"
      case _: l.Indent.type            => "INDENT"
      case _: l.Dedent.type            => "DEDENT"
      case t: l.Keyword                => s"K(${t.chars})"
      case t: l.Identifier             => s"ID(${t.chars})"
      case t: l.NumericLit             => s"N(${t.chars})"
      case t: l.StringLit              => s"S(${escape(t.chars)})"
      case t: l.ErrorToken             => s"ERR(${t.chars})"
      case t                           => s"UNK(${t.chars})"
    }

  private def escape(s: String): String =
    s.flatMap {
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case '\\' => "\\\\"
      case c    => c.toString
    }

  // -- Numeric literals ----------------------------------------------------

  "integer literals" should {
    "lex plain decimal" in {
      tokens("42") should contain ("N(42)")
    }
    "lex with underscores (raw text preserved)" in {
      tokens("1_000_000") should contain ("N(1000000)")
    }
    "lex hex with 0x prefix" in {
      tokens("0xFF") should contain ("N(0xFF)")
    }
    "lex binary with 0b prefix" in {
      tokens("0b1010_1010") should contain ("N(0b10101010)")
    }
    "lex octal with 0o prefix" in {
      tokens("0o755") should contain ("N(0o755)")
    }
  }

  "real literals" should {
    "lex decimal with fractional" in {
      tokens("3.14") should contain ("N(3.14)")
    }
    "lex scientific with explicit decimal" in {
      tokens("2.5e-3") should contain ("N(2.5e-3)")
    }
    "lex scientific without decimal" in {
      tokens("6e23") should contain ("N(6e23)")
    }
    "lex with underscores" in {
      tokens("1_234.567_89") should contain ("N(1234.56789)")
    }
  }

  // -- String literals -----------------------------------------------------

  "string literals" should {
    "lex plain" in {
      tokens(""""hello"""") should contain ("S(hello)")
    }
    "decode escape sequences" in {
      tokens(""""line\nbreak"""") should contain ("S(line\\nbreak)")
    }
    "decode \\xNN hex byte escape" in {
      tokens(""""\x41"""") should contain ("S(A)")
    }
    "decode \\u{...} unicode escape" in {
      tokens(""""\u{41}"""") should contain ("S(A)")
    }
  }

  "interpolated string literals" should {
    "tag the body with the s: prefix" in {
      tokens("""s"hello"""") should contain ("S(s:hello)")
    }
    "preserve the entire body for later parsing" in {
      tokens("""s"hi $name, x=${x+1}"""") should contain ("S(s:hi $name, x=${x+1})")
    }
  }

  // -- Identifiers and keywords -------------------------------------------

  "identifiers and keywords" should {
    "distinguish keywords from identifiers" in {
      tokens("def foo") should contain inOrderOnly ("K(def)", "ID(foo)", "NL")
    }
    "treat keyword-prefix identifiers as plain identifiers" in {
      tokens("defy iffy whilex") should contain inOrderOnly
        ("ID(defy)", "ID(iffy)", "ID(whilex)", "NL")
    }
    "recognize every v0 keyword" in {
      val all = "and as const def div do else end false for if import in match module mut not or private return struct then true val var while"
      val ts = tokens(all).filterNot(t => t == "NL" || t.startsWith("ID"))
      ts.foreach(_ should startWith ("K("))
      ts.size shouldBe 26
    }
    "lex underscore-leading identifier" in {
      tokens("_foo _ x_y") should contain inOrderOnly
        ("ID(_foo)", "ID(_)", "ID(x_y)", "NL")
    }
  }

  // -- Operators ----------------------------------------------------------

  "operators (greedy longest-match)" should {
    "lex multi-char comparison ops as single tokens" in {
      tokens("a == b != c <= d >= e") shouldBe List(
        "ID(a)", "K(==)", "ID(b)", "K(!=)", "ID(c)", "K(<=)", "ID(d)", "K(>=)", "ID(e)", "NL",
      )
    }
    "lex ..= before .. (longest match)" in {
      tokens("0..=10 0..10") shouldBe List(
        "N(0)", "K(..=)", "N(10)", "N(0)", "K(..)", "N(10)", "NL",
      )
    }
    "treat // as the start of a line comment, not an operator" in {
      // `//` is reserved for comments; integer division uses the `div` keyword.
      tokens("a // this is a comment\nb") shouldBe List("ID(a)", "NL", "ID(b)", "NL")
    }
    "lex `div` as the integer-division keyword" in {
      tokens("a div b") shouldBe List("ID(a)", "K(div)", "ID(b)", "NL")
    }
    "lex single-char punctuation" in {
      tokens("(){}[],;:") shouldBe List(
        "K(()", "K())", "K({)", "K(})", "K([)", "K(])", "K(,)", "K(;)", "K(:)", "NL",
      )
    }
    "lex juxtaposition as two tokens (parser combines)" in {
      tokens("2x 2pi 3sin") shouldBe List(
        "N(2)", "ID(x)", "N(2)", "ID(pi)", "N(3)", "ID(sin)", "NL",
      )
    }
    "lex matrix multiply @ as its own token" in {
      tokens("A @ B") shouldBe List("ID(A)", "K(@)", "ID(B)", "NL")
    }
    "lex -> as a single arrow token" in {
      tokens("x -> x * x") shouldBe List("ID(x)", "K(->)", "ID(x)", "K(*)", "ID(x)", "NL")
    }
  }

  // -- Attributes (@test, @strict) ----------------------------------------

  "attributes" should {
    "lex @ as a keyword followed by an identifier" in {
      // The parser combines @ + Identifier into an attribute node.
      tokens("@test") should contain inOrderOnly ("K(@)", "ID(test)", "NL")
      tokens("@strict") should contain inOrderOnly ("K(@)", "ID(strict)", "NL")
    }
  }

  // -- Comments -----------------------------------------------------------

  "comments" should {
    "skip line comments" in {
      tokens("foo // ignored\nbar") shouldBe List("ID(foo)", "NL", "ID(bar)", "NL")
    }
    "skip block comments" in {
      tokens("foo /* ignored */ bar") shouldBe List("ID(foo)", "ID(bar)", "NL")
    }
  }

  // -- Indentation --------------------------------------------------------

  "indentation" should {
    "emit Newline / Indent / Dedent for a simple block" in {
      val src =
        """a
          |  b
          |c""".stripMargin
      tokens(src) shouldBe List(
        "ID(a)", "NL", "INDENT", "ID(b)", "NL", "DEDENT", "NL", "ID(c)", "NL",
      )
    }
    "handle multi-level indent / dedent" in {
      val src =
        """a
          |  b
          |    c
          |  d
          |e""".stripMargin
      tokens(src) shouldBe List(
        "ID(a)", "NL",
        "INDENT", "ID(b)", "NL",
        "INDENT", "ID(c)", "NL",
        "DEDENT", "NL", "ID(d)", "NL",
        "DEDENT", "NL", "ID(e)", "NL",
      )
    }
    "suppress newlines inside parens" in {
      val src =
        """f(
          |  1,
          |  2,
          |)""".stripMargin
      tokens(src) shouldBe List(
        "ID(f)", "K(()", "N(1)", "K(,)", "N(2)", "K(,)", "K())", "NL",
      )
    }
    "skip blank lines and comment-only lines without disturbing block structure" in {
      val src =
        """a
          |
          |  // a comment
          |  b
          |
          |c""".stripMargin
      tokens(src) shouldBe List(
        "ID(a)", "NL",
        "INDENT", "ID(b)", "NL",
        "DEDENT", "NL", "ID(c)", "NL",
      )
    }
  }

  // -- Line continuation after binary ops ---------------------------------

  "line continuation" should {
    "continue an expression after a trailing binary operator" in {
      // `a +\n  b` should NOT emit Newline+Indent; the `+` continues the line.
      val src =
        """a +
          |  b""".stripMargin
      tokens(src) shouldBe List("ID(a)", "K(+)", "ID(b)", "NL")
    }
    "continue after `and` / `or`" in {
      val src =
        """x and
          |  y or
          |  z""".stripMargin
      tokens(src) shouldBe List("ID(x)", "K(and)", "ID(y)", "K(or)", "ID(z)", "NL")
    }
    "still emit Newline+Indent after `=` (block-introduction, not continuation)" in {
      val src =
        """val x =
          |  a
          |  b""".stripMargin
      tokens(src) shouldBe List(
        "K(val)", "ID(x)", "K(=)", "NL",
        "INDENT", "ID(a)", "NL", "ID(b)", "NL", "DEDENT", "NL",
      )
    }
  }

  // -- Small whole-program shapes -----------------------------------------

  "small program shapes" should {
    "lex a hello-world function" in {
      val src =
        """def main() =
          |  print("Hello, Nex!")""".stripMargin
      tokens(src) shouldBe List(
        "K(def)", "ID(main)", "K(()", "K())", "K(=)", "NL",
        "INDENT", "ID(print)", "K(()", "S(Hello, Nex!)", "K())", "NL", "DEDENT", "NL",
      )
    }
    "lex a struct declaration with end Name" in {
      val src =
        """struct Point
          |  x: real
          |  y: real
          |end Point""".stripMargin
      tokens(src) shouldBe List(
        "K(struct)", "ID(Point)", "NL",
        "INDENT",
          "ID(x)", "K(:)", "ID(real)", "NL",
          "ID(y)", "K(:)", "ID(real)", "NL",
        "DEDENT", "NL",
        "K(end)", "ID(Point)", "NL",
      )
    }
    "lex a for loop with destructuring" in {
      val src =
        """for k, x in enumerate(arr) do
          |  print(k)""".stripMargin
      tokens(src) shouldBe List(
        "K(for)", "ID(k)", "K(,)", "ID(x)", "K(in)",
        "ID(enumerate)", "K(()", "ID(arr)", "K())", "K(do)", "NL",
        "INDENT", "ID(print)", "K(()", "ID(k)", "K())", "NL", "DEDENT", "NL",
      )
    }
    "lex a test function with attribute and assertion" in {
      val src =
        """@test
          |def test_addition() =
          |  assert_eq(2 + 2, 4)""".stripMargin
      tokens(src) shouldBe List(
        "K(@)", "ID(test)", "NL",
        "K(def)", "ID(test_addition)", "K(()", "K())", "K(=)", "NL",
        "INDENT", "ID(assert_eq)", "K(()", "N(2)", "K(+)", "N(2)", "K(,)", "N(4)", "K())", "NL", "DEDENT", "NL",
      )
    }
    "lex an interpolated string in a print call" in {
      val src = """print(s"x = ${x + 1}, name = $name")"""
      tokens(src) shouldBe List(
        "ID(print)", "K(()", "S(s:x = ${x + 1}, name = $name)", "K())", "NL",
      )
    }
  }

  // -- Empty / trivial inputs ---------------------------------------------

  "empty / trivial inputs" should {
    "handle empty input" in {
      // IndentationLexical emits a trailing Newline as an end-of-stream marker.
      tokens("") shouldBe List("NL")
    }
    "handle whitespace only" in {
      tokens("   \n  \n").filterNot(t => t == "NL" || t == "INDENT" || t == "DEDENT") shouldBe Nil
    }
    "handle comment-only input" in {
      tokens("// just a comment\n").filterNot(t => t == "NL" || t == "INDENT" || t == "DEDENT") shouldBe Nil
    }
  }
