package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Tests for [[NexParser]] (Pass 1). */
class NexParserTests extends AnyWordSpec with Matchers:

  private def parser = new NexParser

  /** Parse an expression, failing the test on parser error. */
  private def parseExpr(src: String): ExprAST =
    parser.parseExpression(src) match
      case Right(e)  => e
      case Left(err) => fail(s"parse error: $err")

  /** Parse a program, failing the test on parser error. */
  private def parseProg(src: String): ProgramAST =
    parser.parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")

  // ========================================================================
  // Literals
  // ========================================================================

  "literals" should {
    "parse integers" in {
      parseExpr("42")          shouldBe IntLitExpr(42)
      parseExpr("1_000_000")   shouldBe IntLitExpr(1000000)
      parseExpr("0xFF")        shouldBe IntLitExpr(255)
      parseExpr("0b1010_1010") shouldBe IntLitExpr(170)
      parseExpr("0o755")       shouldBe IntLitExpr(493)
    }
    "parse reals" in {
      parseExpr("3.14")   shouldBe RealLitExpr(3.14)
      parseExpr("2.5e-3") shouldBe RealLitExpr(0.0025)
      parseExpr("6e23")   shouldBe RealLitExpr(6e23)
    }
    "parse booleans" in {
      parseExpr("true")  shouldBe BoolLitExpr(true)
      parseExpr("false") shouldBe BoolLitExpr(false)
    }
    "parse strings" in {
      parseExpr(""""hello"""")           shouldBe StringLitExpr("hello")
      parseExpr(""""line\nbreak"""")     shouldBe StringLitExpr("line\nbreak")
    }
    "parse the unit literal" in {
      parseExpr("()") shouldBe UnitLitExpr()
    }
  }

  "interpolated strings" should {
    "split a body with text + $ident + ${expr} + $$" in {
      // The lexer splits the body; the parser stores ${expr} bodies as raw
      // text (to be re-parsed by the elaborator), so this AST has the
      // structural split without nested expression parsing.
      val ast = parseExpr("""s"hi $name, x=${x + 1}, $$ done"""")
      ast shouldBe InterpStringLitExpr(List(
        InterpText("hi "),
        InterpVar("name"),
        InterpText(", x="),
        InterpExprPart("x + 1"),
        InterpText(", $ done"),
      ))
    }
    "handle empty body" in {
      parseExpr("""s""""") shouldBe InterpStringLitExpr(Nil)
    }
  }

  // ========================================================================
  // Variable references
  // ========================================================================

  "variable references" should {
    "parse a bare identifier" in {
      parseExpr("foo") shouldBe VarRefExpr("foo")
    }
    "parse prelude constants by name" in {
      parseExpr("pi") shouldBe VarRefExpr("pi")
      parseExpr("i")  shouldBe VarRefExpr("i")
    }
  }

  // ========================================================================
  // Binary operators and precedence
  // ========================================================================

  "binary operators" should {
    "parse left-associative additive chains" in {
      parseExpr("1 + 2 + 3") shouldBe
        BinOpExpr("+", BinOpExpr("+", IntLitExpr(1), IntLitExpr(2)), IntLitExpr(3))
    }
    "parse left-associative multiplicative chains" in {
      parseExpr("a * b * c") shouldBe
        BinOpExpr("*", BinOpExpr("*", VarRefExpr("a"), VarRefExpr("b")), VarRefExpr("c"))
    }
    "parse * binding tighter than +" in {
      parseExpr("a + b * c") shouldBe
        BinOpExpr("+", VarRefExpr("a"), BinOpExpr("*", VarRefExpr("b"), VarRefExpr("c")))
    }
    "parse right-associative power" in {
      parseExpr("a ^ b ^ c") shouldBe
        BinOpExpr("^", VarRefExpr("a"), BinOpExpr("^", VarRefExpr("b"), VarRefExpr("c")))
    }
    "parse `^` binding tighter than `*`" in {
      parseExpr("a * b ^ c") shouldBe
        BinOpExpr("*", VarRefExpr("a"), BinOpExpr("^", VarRefExpr("b"), VarRefExpr("c")))
    }
    "parse `div` as integer division at multiplicative precedence" in {
      parseExpr("7 div 3") shouldBe
        BinOpExpr("div", IntLitExpr(7), IntLitExpr(3))
    }
    "parse single comparison as plain BinOpExpr" in {
      parseExpr("a == b") shouldBe BinOpExpr("==", VarRefExpr("a"), VarRefExpr("b"))
      parseExpr("a < b")  shouldBe BinOpExpr("<",  VarRefExpr("a"), VarRefExpr("b"))
    }
    "parse 3-way chained comparison as ChainedCmpExpr" in {
      parseExpr("a < b < c") shouldBe ChainedCmpExpr(
        List(VarRefExpr("a"), VarRefExpr("b"), VarRefExpr("c")),
        List("<", "<"),
      )
    }
    "parse 4-way mixed chained comparison" in {
      parseExpr("a <= b < c <= d") shouldBe ChainedCmpExpr(
        List(VarRefExpr("a"), VarRefExpr("b"), VarRefExpr("c"), VarRefExpr("d")),
        List("<=", "<", "<="),
      )
    }
    "parse `and` / `or` with proper precedence" in {
      // `or` is looser, so `a or b and c` = `a or (b and c)`.
      parseExpr("a or b and c") shouldBe
        BinOpExpr("or", VarRefExpr("a"),
          BinOpExpr("and", VarRefExpr("b"), VarRefExpr("c")))
    }
    "parse range constructors" in {
      parseExpr("0..n")  shouldBe BinOpExpr("..",  IntLitExpr(0), VarRefExpr("n"))
      parseExpr("0..=n") shouldBe BinOpExpr("..=", IntLitExpr(0), VarRefExpr("n"))
    }
  }

  // ========================================================================
  // Juxtaposition multiplication
  // ========================================================================

  "juxtaposition" should {
    "treat numeric × identifier as juxtaposition" in {
      parseExpr("2x") shouldBe JuxtaposeExpr(IntLitExpr(2), VarRefExpr("x"))
    }
    "treat numeric × paren as juxtaposition" in {
      parseExpr("2(x + 1)") shouldBe
        JuxtaposeExpr(IntLitExpr(2),
          BinOpExpr("+", VarRefExpr("x"), IntLitExpr(1)))
    }
    "parse `2x^3` as `2 * (x^3)` (^ tighter than juxt)" in {
      parseExpr("2x^3") shouldBe
        JuxtaposeExpr(IntLitExpr(2),
          BinOpExpr("^", VarRefExpr("x"), IntLitExpr(3)))
    }
    "parse `2x * 3` as `(2x) * 3` (juxt tighter than *)" in {
      parseExpr("2x * 3") shouldBe
        BinOpExpr("*",
          JuxtaposeExpr(IntLitExpr(2), VarRefExpr("x")),
          IntLitExpr(3))
    }
    "parse `1/2x` as `1 / (2x)` (juxt tighter than /)" in {
      parseExpr("1/2x") shouldBe
        BinOpExpr("/",
          IntLitExpr(1),
          JuxtaposeExpr(IntLitExpr(2), VarRefExpr("x")))
    }
    "treat real × identifier as juxtaposition" in {
      parseExpr("2.0pi") shouldBe JuxtaposeExpr(RealLitExpr(2.0), VarRefExpr("pi"))
    }
    "parse `5 - 2` as binary subtraction, not `5 * (-2)` (regression)" in {
      parseExpr("5 - 2") shouldBe BinOpExpr("-", IntLitExpr(5), IntLitExpr(2))
    }
    "parse `3 - 4i` as `3 - 4i`, with juxt on the RHS of binary -" in {
      parseExpr("3 - 4i") shouldBe
        BinOpExpr("-",
          IntLitExpr(3),
          JuxtaposeExpr(IntLitExpr(4), VarRefExpr("i")))
    }
    "still reject juxtaposition through a unary minus" in {
      // `3 -x` is `3 - x`, NOT `3 * (-x)` (juxt body forbids unary prefix).
      parseExpr("3 -x") shouldBe BinOpExpr("-", IntLitExpr(3), VarRefExpr("x"))
    }
    "fold `-4i` into a signed juxt coefficient" in {
      parseExpr("-4i") shouldBe JuxtaposeExpr(IntLitExpr(-4), VarRefExpr("i"))
    }
    "fold `-2.5pi` into a signed real juxt coefficient" in {
      parseExpr("-2.5pi") shouldBe JuxtaposeExpr(RealLitExpr(-2.5), VarRefExpr("pi"))
    }
    "fold `-3(x + 1)` into a signed juxt coefficient over a paren body" in {
      parseExpr("-3(x + 1)") shouldBe
        JuxtaposeExpr(IntLitExpr(-3),
          BinOpExpr("+", VarRefExpr("x"), IntLitExpr(1)))
    }
    "parse `5 - 4i` as `5 - 4i`, NOT `5 + (-4i)` (binary - wins at addExpr level)" in {
      parseExpr("5 - 4i") shouldBe
        BinOpExpr("-",
          IntLitExpr(5),
          JuxtaposeExpr(IntLitExpr(4), VarRefExpr("i")))
    }
    "parse `-2^2` as `(-2)^2`, NOT `-(2^2)` (unary - tighter than ^)" in {
      // Spec §4.3: unary `-` is precedence 3, `^` is precedence 4. The
      // signed-juxt alternative can't capture `-2^2` because its juxtBody
      // demands a postfixExpr — `^...` doesn't match — so the parse falls
      // through to `powExpr` where `^` binds outside the unary minus.
      parseExpr("-2^2") shouldBe
        BinOpExpr("^", UnaryOpExpr("-", IntLitExpr(2)), IntLitExpr(2))
    }
  }

  // ========================================================================
  // Unary operators
  // ========================================================================

  "unary operators" should {
    "parse unary minus" in {
      parseExpr("-x") shouldBe UnaryOpExpr("-", VarRefExpr("x"))
    }
    "parse `not`" in {
      parseExpr("not x") shouldBe UnaryOpExpr("not", VarRefExpr("x"))
    }
    "parse unary minus binding tighter than binary +" in {
      parseExpr("-a + b") shouldBe
        BinOpExpr("+", UnaryOpExpr("-", VarRefExpr("a")), VarRefExpr("b"))
    }
  }

  // ========================================================================
  // Calls, indexing, field access, method chaining
  // ========================================================================

  "function calls" should {
    "parse a simple call" in {
      parseExpr("f(x)") shouldBe CallExpr(VarRefExpr("f"), List(VarRefExpr("x")))
    }
    "parse a call with multiple args" in {
      parseExpr("f(a, b, c)") shouldBe
        CallExpr(VarRefExpr("f"), List(VarRefExpr("a"), VarRefExpr("b"), VarRefExpr("c")))
    }
    "parse a chained call" in {
      parseExpr("f(x)(y)") shouldBe
        CallExpr(CallExpr(VarRefExpr("f"), List(VarRefExpr("x"))), List(VarRefExpr("y")))
    }
  }

  "indexing" should {
    "parse single index" in {
      parseExpr("a[0]") shouldBe IndexExpr(VarRefExpr("a"), List(IntLitExpr(0)))
    }
    "parse multi-index (rank-2 future)" in {
      parseExpr("m[i, j]") shouldBe
        IndexExpr(VarRefExpr("m"), List(VarRefExpr("i"), VarRefExpr("j")))
    }
  }

  "field access" should {
    "parse a single dot access" in {
      parseExpr("z.re") shouldBe FieldExpr(VarRefExpr("z"), "re")
    }
    "parse a method-call form (sugar)" in {
      parseExpr("arr.map(f)") shouldBe
        MethodCallExpr(VarRefExpr("arr"), "map", List(VarRefExpr("f")))
    }
    "parse chained method calls" in {
      parseExpr("xs.map(f).sum()") shouldBe
        MethodCallExpr(
          MethodCallExpr(VarRefExpr("xs"), "map", List(VarRefExpr("f"))),
          "sum",
          Nil,
        )
    }
  }

  // ========================================================================
  // Lambdas
  // ========================================================================

  "lambdas" should {
    "parse single-arg lambda without parens" in {
      parseExpr("x -> x * x") shouldBe
        LambdaExpr(List(LambdaParam("x", None)),
          BinOpExpr("*", VarRefExpr("x"), VarRefExpr("x")))
    }
    "parse multi-arg lambda" in {
      parseExpr("(x, y) -> x + y") shouldBe
        LambdaExpr(
          List(LambdaParam("x", None), LambdaParam("y", None)),
          BinOpExpr("+", VarRefExpr("x"), VarRefExpr("y")),
        )
    }
    "parse lambda with type annotation" in {
      parseExpr("(x: real) -> x * 2.0") shouldBe
        LambdaExpr(
          List(LambdaParam("x", Some(NamedType("real")))),
          BinOpExpr("*", VarRefExpr("x"), RealLitExpr(2.0)),
        )
    }
    "parse right-associative chained lambdas" in {
      // `x -> y -> z` parses as `x -> (y -> z)`.
      parseExpr("x -> y -> z") shouldBe
        LambdaExpr(List(LambdaParam("x", None)),
          LambdaExpr(List(LambdaParam("y", None)), VarRefExpr("z")))
    }
  }

  // ========================================================================
  // Tuples (paren-less)
  // ========================================================================

  "tuples" should {
    "parse paren-less tuple" in {
      parseExpr("1, 2.0, true") shouldBe
        TupleExpr(List(IntLitExpr(1), RealLitExpr(2.0), BoolLitExpr(true)))
    }
    "parse parenthesized tuple identically" in {
      parseExpr("(1, 2, 3)") shouldBe
        TupleExpr(List(IntLitExpr(1), IntLitExpr(2), IntLitExpr(3)))
    }
    "treat single-element parens as grouping (not a 1-tuple)" in {
      parseExpr("(42)") shouldBe IntLitExpr(42)
    }
    "NOT treat function-call commas as tuple commas" in {
      // f(1, 2, 3) is 3 args, not f((1, 2, 3)).
      parseExpr("f(1, 2, 3)") shouldBe
        CallExpr(VarRefExpr("f"), List(IntLitExpr(1), IntLitExpr(2), IntLitExpr(3)))
    }
    "treat parens around a tuple as one argument" in {
      parseExpr("f((1, 2))") shouldBe
        CallExpr(VarRefExpr("f"), List(TupleExpr(List(IntLitExpr(1), IntLitExpr(2)))))
    }
  }

  // ========================================================================
  // Array literals (rank-1)
  // ========================================================================

  "array literals" should {
    "parse empty array" in {
      parseExpr("[]") shouldBe CallExpr(VarRefExpr("__array"), Nil)
    }
    "parse rank-1 with elements" in {
      parseExpr("[1, 2, 3]") shouldBe
        CallExpr(VarRefExpr("__array"),
          List(IntLitExpr(1), IntLitExpr(2), IntLitExpr(3)))
    }
  }

  // ========================================================================
  // val / var / const declarations (programs of a single binding)
  // ========================================================================

  "val / var / const declarations" should {
    "parse simple val" in {
      parseProg("val x = 42").decls shouldBe List(
        ValDeclAST(VarPat("x"), None, IntLitExpr(42)),
      )
    }
    "parse val with type annotation" in {
      parseProg("val x: integer = 42").decls shouldBe List(
        ValDeclAST(VarPat("x"), Some(NamedType("integer")), IntLitExpr(42)),
      )
    }
    "parse val with paren-less tuple destructuring" in {
      parseProg("val a, b, c = 1, 2, 3").decls shouldBe List(
        ValDeclAST(
          TuplePat(List(VarPat("a"), VarPat("b"), VarPat("c"))),
          None,
          TupleExpr(List(IntLitExpr(1), IntLitExpr(2), IntLitExpr(3))),
        ),
      )
    }
    "parse val with wildcard in destructuring" in {
      parseProg("val x, _ = 3.14, ignored").decls shouldBe List(
        ValDeclAST(
          TuplePat(List(VarPat("x"), WildcardPat())),
          None,
          TupleExpr(List(RealLitExpr(3.14), VarRefExpr("ignored"))),
        ),
      )
    }
    "parse var" in {
      parseProg("var n = 0").decls shouldBe List(
        VarDeclAST(VarPat("n"), None, IntLitExpr(0)),
      )
    }
    "parse const" in {
      parseProg("const PI = 3.14159").decls shouldBe List(
        ConstDeclAST(VarPat("PI"), None, RealLitExpr(3.14159)),
      )
    }
    "parse multiple declarations separated by newlines" in {
      val src =
        """val x = 1
          |val y = 2
          |const Z = 3""".stripMargin
      parseProg(src).decls shouldBe List(
        ValDeclAST(VarPat("x"), None, IntLitExpr(1)),
        ValDeclAST(VarPat("y"), None, IntLitExpr(2)),
        ConstDeclAST(VarPat("Z"), None, IntLitExpr(3)),
      )
    }
    "accept paren-grouped patterns (spec §4.16: required when annotated)" in {
      // Spec §4.16: `val (a, b): (integer, real) = 1, 2.0` — parens
      // around the pattern are required when there's a type annotation.
      // The unannotated form `val (a, b) = pair` is also accepted; the
      // parens just collapse to the same TuplePat as `val a, b`. Single-
      // element parens (`val (x) = expr`) are grouping per spec.
      new NexParser().parseProgram("val (a, b) = pair") match
        case Left(err) => fail(s"expected the parser to accept `val (a, b) = pair`: $err")
        case Right(_)  => succeed
      new NexParser().parseProgram("val (a, b): (integer, real) = 1, 2.0") match
        case Left(err) => fail(s"expected the parser to accept the annotated form: $err")
        case Right(_)  => succeed
    }
    "parse val with an indented-block body (same shape as def)" in {
      val src =
        """val total =
          |  val a = 1
          |  val b = 2
          |  a + b""".stripMargin
      val Right(prog) = new NexParser().parseProgram(src): @unchecked
      prog.decls.head shouldBe a [ValDeclAST]
      prog.decls.head.asInstanceOf[ValDeclAST].init shouldBe a [BlockExpr]
    }
    "parse var with an indented-block body" in {
      val src =
        """var counter =
          |  val seed = 7
          |  seed * 2""".stripMargin
      val Right(prog) = new NexParser().parseProgram(src): @unchecked
      prog.decls.head.asInstanceOf[VarDeclAST].init shouldBe a [BlockExpr]
    }
  }

  // ========================================================================
  // def function body shapes
  // ========================================================================

  "def function body" should {
    "parse a plain inline expression body" in {
      val Right(prog) =
        new NexParser().parseProgram("def f(x: integer) = x * x"): @unchecked
      val fn = prog.decls.head.asInstanceOf[FunDeclAST]
      fn.body.get shouldBe BinOpExpr("*", VarRefExpr("x"), VarRefExpr("x"))
    }
    "parse an inline assignment body — `def bump(x: mut T) = x = x + 1`" in {
      val Right(prog) =
        new NexParser().parseProgram("def bump(x: mut integer) = x = x + 1"): @unchecked
      val fn = prog.decls.head.asInstanceOf[FunDeclAST]
      fn.body.get shouldBe AssignExpr(
        VarRefExpr("x"),
        BinOpExpr("+", VarRefExpr("x"), IntLitExpr(1)),
      )
    }
    "parse an inline assignment body to a field" in {
      val Right(prog) =
        new NexParser().parseProgram("def reset(b: mut Box) = b.v = 0"): @unchecked
      val fn = prog.decls.head.asInstanceOf[FunDeclAST]
      fn.body.get shouldBe AssignExpr(
        FieldExpr(VarRefExpr("b"), "v"),
        IntLitExpr(0),
      )
    }
    "still parse the indented-block body form" in {
      val src =
        """def bump(x: mut integer) =
          |  x = x + 1""".stripMargin
      val Right(prog) = new NexParser().parseProgram(src): @unchecked
      prog.decls.head.asInstanceOf[FunDeclAST].body.get shouldBe AssignExpr(
        VarRefExpr("x"),
        BinOpExpr("+", VarRefExpr("x"), IntLitExpr(1)),
      )
    }
  }

  // ========================================================================
  // Compound assignment (spec §5.2)
  // ========================================================================

  "compound assignment" should {

    def bodyOf(src: String): ExprAST =
      val Right(prog) = new NexParser().parseProgram(src): @unchecked
      prog.decls.head.asInstanceOf[FunDeclAST].body.get

    "desugar `+=` on a name to `name = name + rhs`" in {
      bodyOf("def bump(x: mut integer) = x += 5") shouldBe AssignExpr(
        VarRefExpr("x"),
        BinOpExpr("+", VarRefExpr("x"), IntLitExpr(5)),
      )
    }

    "desugar `-=` on a name" in {
      bodyOf("def dec(x: mut integer) = x -= 1") shouldBe AssignExpr(
        VarRefExpr("x"),
        BinOpExpr("-", VarRefExpr("x"), IntLitExpr(1)),
      )
    }

    "desugar `*=` on a name" in {
      bodyOf("def scale(x: mut integer) = x *= 2") shouldBe AssignExpr(
        VarRefExpr("x"),
        BinOpExpr("*", VarRefExpr("x"), IntLitExpr(2)),
      )
    }

    "desugar `/=` on a name" in {
      bodyOf("def half(x: mut real) = x /= 2.0") shouldBe AssignExpr(
        VarRefExpr("x"),
        BinOpExpr("/", VarRefExpr("x"), RealLitExpr(2.0)),
      )
    }

    "desugar `%=` on a name" in {
      bodyOf("def wrap(x: mut integer) = x %= 10") shouldBe AssignExpr(
        VarRefExpr("x"),
        BinOpExpr("%", VarRefExpr("x"), IntLitExpr(10)),
      )
    }

    "desugar `+=` on a field target" in {
      bodyOf("def add(b: mut Box) = b.v += 3") shouldBe AssignExpr(
        FieldExpr(VarRefExpr("b"), "v"),
        BinOpExpr("+", FieldExpr(VarRefExpr("b"), "v"), IntLitExpr(3)),
      )
    }

    "desugar `+=` on an index target" in {
      bodyOf("def bump(a: mut [integer], i: integer) = a[i] += 1") shouldBe AssignExpr(
        IndexExpr(VarRefExpr("a"), List(VarRefExpr("i"))),
        BinOpExpr("+",
          IndexExpr(VarRefExpr("a"), List(VarRefExpr("i"))),
          IntLitExpr(1)),
      )
    }

    "RHS is the full expr (binds the whole right-hand side, not just an atom)" in {
      bodyOf("def f(x: mut integer, y: integer) = x += y * 2 + 1") shouldBe AssignExpr(
        VarRefExpr("x"),
        BinOpExpr("+",
          VarRefExpr("x"),
          BinOpExpr("+",
            BinOpExpr("*", VarRefExpr("y"), IntLitExpr(2)),
            IntLitExpr(1))),
      )
    }

    "compound assignment is allowed inside a block body" in {
      val src =
        """def sum_into(acc: mut integer, xs: [integer]) =
          |  for x in xs do
          |    acc += x
          |  end for""".stripMargin
      // Just verify it parses without error — shape-check would duplicate
      // the for-loop AST and obscure the assignment shape.
      val r = new NexParser().parseProgram(src)
      r.isRight shouldBe true
    }
  }

  // ========================================================================
  // Cross-cutting small program shapes
  // ========================================================================

  "small program shapes" should {
    "parse `2a + b` (fusion expression from Example 3)" in {
      parseExpr("2a + b") shouldBe
        BinOpExpr("+",
          JuxtaposeExpr(IntLitExpr(2), VarRefExpr("a")),
          VarRefExpr("b"))
    }
    "parse `sqrt(a^2 + b^2)` (hypotenuse)" in {
      parseExpr("sqrt(a^2 + b^2)") shouldBe
        CallExpr(VarRefExpr("sqrt"), List(
          BinOpExpr("+",
            BinOpExpr("^", VarRefExpr("a"), IntLitExpr(2)),
            BinOpExpr("^", VarRefExpr("b"), IntLitExpr(2))),
        ))
    }
    "parse a stats-style closure pipeline" in {
      // `xs.map(x -> (x - mean)^2).sum() / n` — covers chaining, lambda,
      // grouping parens, power, division.
      val ast = parseExpr("xs.map(x -> (x - mean)^2).sum() / n")
      ast shouldBe BinOpExpr("/",
        MethodCallExpr(
          MethodCallExpr(VarRefExpr("xs"), "map", List(
            LambdaExpr(List(LambdaParam("x", None)),
              BinOpExpr("^",
                BinOpExpr("-", VarRefExpr("x"), VarRefExpr("mean")),
                IntLitExpr(2))),
          )),
          "sum", Nil),
        VarRefExpr("n"))
    }
  }

  // ========================================================================
  // Semicolon as statement separator (spec §2.8)
  // ========================================================================

  "`;` as a statement separator" should {
    "join two top-level declarations on one line" in {
      val prog = parseProg("def f() = 1; def g() = 2")
      prog.decls.size shouldBe 2
      prog.decls(0).asInstanceOf[FunDeclAST].name shouldBe "f"
      prog.decls(1).asInstanceOf[FunDeclAST].name shouldBe "g"
    }

    "join two block-level val declarations on one line" in {
      val src =
        """def main() =
          |  val x = 1; val y = 2
          |  x + y""".stripMargin
      val prog = parseProg(src)
      val body = prog.decls.head.asInstanceOf[FunDeclAST].body.get
      val block = body.asInstanceOf[BlockExpr]
      block.items.size shouldBe 2
      block.items(0) shouldBe BlockDecl(ValDeclAST(VarPat("x"), None, IntLitExpr(1)))
      block.items(1) shouldBe BlockDecl(ValDeclAST(VarPat("y"), None, IntLitExpr(2)))
      block.result shouldBe BinOpExpr("+", VarRefExpr("x"), VarRefExpr("y"))
    }

    "mix `;` with newlines on the same line" in {
      val src =
        """def main() =
          |  val a = 1; val b = 2; val c = 3
          |  a + b + c""".stripMargin
      val prog = parseProg(src)
      val block = prog.decls.head.asInstanceOf[FunDeclAST].body.get.asInstanceOf[BlockExpr]
      block.items.size shouldBe 3
    }

    "accept a trailing `;` on a block-item line" in {
      val src =
        """def main() =
          |  val x = 1;
          |  x""".stripMargin
      val prog = parseProg(src)
      val block = prog.decls.head.asInstanceOf[FunDeclAST].body.get.asInstanceOf[BlockExpr]
      block.items.size shouldBe 1
      block.result shouldBe VarRefExpr("x")
    }

    "treat repeated `;` like a single separator" in {
      val src =
        """def main() =
          |  val x = 1;; val y = 2
          |  x + y""".stripMargin
      val prog = parseProg(src)
      val block = prog.decls.head.asInstanceOf[FunDeclAST].body.get.asInstanceOf[BlockExpr]
      block.items.size shouldBe 2
    }
  }
