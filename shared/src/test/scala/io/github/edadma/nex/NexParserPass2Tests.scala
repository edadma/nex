package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Tests for the Pass 2 extensions to NexParser: def declarations with
  * modes, structs, modules, imports, attributes, if/for/while/return,
  * block-form lambdas, rank-2 arrays, matmul.
  */
class NexParserPass2Tests extends AnyWordSpec with Matchers:

  private def parser = new NexParser

  private def parseProg(src: String): ProgramAST =
    parser.parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err\nin source:\n$src")

  private def parseExpr(src: String): ExprAST =
    parser.parseExpression(src) match
      case Right(e)  => e
      case Left(err) => fail(s"parse error: $err\nin source:\n$src")

  // ========================================================================
  // def declarations
  // ========================================================================

  "def declarations" should {
    "parse a no-arg single-expression def" in {
      parseProg("def main() = print()").decls shouldBe List(
        FunDeclAST("main", Nil, None,
          Some(CallExpr(VarRefExpr("print"), Nil))),
      )
    }
    "parse a one-arg def with inferred (read) mode" in {
      parseProg("def square(x: real) = x * x").decls shouldBe List(
        FunDeclAST("square",
          List(FunParam("x", NamedType("real"), ParamMode.Read)),
          None,
          Some(BinOpExpr("*", VarRefExpr("x"), VarRefExpr("x")))),
      )
    }
    "parse a def with declared mut parameter" in {
      parseProg("def fill(v: mut [real], x: real) = x").decls shouldBe List(
        FunDeclAST("fill",
          List(
            FunParam("v", ArrayType(NamedType("real")), ParamMode.Mut),
            FunParam("x", NamedType("real"), ParamMode.Read),
          ),
          None,
          Some(VarRefExpr("x"))),
      )
    }
    "parse explicit return type annotation" in {
      parseProg("def factorial(n: integer): integer = n").decls shouldBe List(
        FunDeclAST("factorial",
          List(FunParam("n", NamedType("integer"), ParamMode.Read)),
          Some(NamedType("integer")),
          Some(VarRefExpr("n"))),
      )
    }
    "parse a def with a block body" in {
      val src =
        """def normalize(v: [real]) =
          |  val mag = sqrt(sum(v * v))
          |  v / mag""".stripMargin
      val Right(p) = parser.parseProgram(src): @unchecked
      p.decls.head shouldBe a [FunDeclAST]
      val fn = p.decls.head.asInstanceOf[FunDeclAST]
      fn.name shouldBe "normalize"
      fn.body shouldBe a [Some[?]]
      fn.body.get shouldBe a [BlockExpr]
      val block = fn.body.get.asInstanceOf[BlockExpr]
      block.items.size shouldBe 1
      block.items.head shouldBe a [BlockDecl]
      block.result shouldBe BinOpExpr("/", VarRefExpr("v"), VarRefExpr("mag"))
    }
    "parse private modifier" in {
      parseProg("private def helper(x: real) = x").decls shouldBe List(
        FunDeclAST("helper",
          List(FunParam("x", NamedType("real"), ParamMode.Read)),
          None,
          Some(VarRefExpr("x")),
          isPrivate = true),
      )
    }
  }

  // ========================================================================
  // struct declarations
  // ========================================================================

  "struct declarations" should {
    "parse a struct with two fields and an end Name marker" in {
      val src =
        """struct Point
          |  x: real
          |  y: real
          |end Point""".stripMargin
      parseProg(src).decls shouldBe List(
        StructDeclAST("Point", List(
          StructField("x", NamedType("real")),
          StructField("y", NamedType("real")),
        )),
      )
    }
    "parse a struct without the end marker" in {
      val src =
        """struct Pair
          |  a: integer
          |  b: integer""".stripMargin
      parseProg(src).decls shouldBe List(
        StructDeclAST("Pair", List(
          StructField("a", NamedType("integer")),
          StructField("b", NamedType("integer")),
        )),
      )
    }
    "parse private struct" in {
      val src =
        """private struct Hidden
          |  v: real""".stripMargin
      parseProg(src).decls shouldBe List(
        StructDeclAST("Hidden",
          List(StructField("v", NamedType("real"))),
          isPrivate = true),
      )
    }
  }

  // ========================================================================
  // module + import declarations
  // ========================================================================

  "module / import" should {
    "parse a module declaration" in {
      parseProg("module foo.bar").decls shouldBe List(
        ModuleDeclAST(List("foo", "bar")),
      )
    }
    "parse a bare import" in {
      parseProg("import math").decls shouldBe List(
        ImportDeclAST(List("math"), Nil),
      )
    }
    "parse a selective import" in {
      parseProg("import math.{sqrt, abs}").decls shouldBe List(
        ImportDeclAST(List("math"), List(
          ImportSelector("sqrt"),
          ImportSelector("abs"),
        )),
      )
    }
    "parse a selective import with as-rename" in {
      parseProg("import linalg.dense.{Matrix, lu_decompose as lu}").decls shouldBe List(
        ImportDeclAST(List("linalg", "dense"), List(
          ImportSelector("Matrix"),
          ImportSelector("lu_decompose", Some("lu")),
        )),
      )
    }
    "parse a wildcard import" in {
      parseProg("import math.*").decls shouldBe List(
        ImportDeclAST(List("math"), Nil, isWildcard = true),
      )
    }
    "parse a dotted-path wildcard import" in {
      parseProg("import linalg.dense.*").decls shouldBe List(
        ImportDeclAST(List("linalg", "dense"), Nil, isWildcard = true),
      )
    }
  }

  // ========================================================================
  // attributes
  // ========================================================================

  "attributes" should {
    "parse @test on a def (on separate lines)" in {
      val src =
        """@test
          |def test_addition() = 1""".stripMargin
      parseProg(src).decls shouldBe List(
        FunDeclAST("test_addition", Nil, None,
          Some(IntLitExpr(1)),
          attributes = List(Attribute("test"))),
      )
    }
    "parse @strict on a def" in {
      parseProg("@strict\ndef hot(a: [real]) = a").decls shouldBe List(
        FunDeclAST("hot",
          List(FunParam("a", ArrayType(NamedType("real")), ParamMode.Read)),
          None,
          Some(VarRefExpr("a")),
          attributes = List(Attribute("strict"))),
      )
    }
    "parse @test module" in {
      parseProg("@test module foo.tests").decls shouldBe List(
        ModuleDeclAST(
          List("foo", "tests"),
          isTestOnly = true,
          attributes = List(Attribute("test")),
        ),
      )
    }
    "parse @test\\nmodule (newline-separated)" in {
      parseProg("@test\nmodule foo.tests").decls shouldBe List(
        ModuleDeclAST(
          List("foo", "tests"),
          isTestOnly = true,
          attributes = List(Attribute("test")),
        ),
      )
    }

    // Stage 0 — `@intrinsic("op_id")` attribute carries a string arg the
    // compiler keys into per-backend dispatch tables.
    "parse @intrinsic with a single string argument" in {
      val src = """@intrinsic("libm.sqrt")
                  |def sqrt(x: real): real""".stripMargin
      val decls = parseProg(src).decls
      decls shouldBe List(
        FunDeclAST(
          "sqrt",
          List(FunParam("x", NamedType("real"), ParamMode.Read)),
          Some(NamedType("real")),
          None,
          attributes = List(Attribute("intrinsic", List("libm.sqrt"))),
        ),
      )
    }

    "parse @intrinsic with multiple string arguments" in {
      // Currently no intrinsic uses more than one arg, but the grammar
      // accepts a list for future symmetry with multi-key attributes.
      val src = """@intrinsic("a", "b")
                  |def foo(): integer""".stripMargin
      val Right(prog) = parser.parseProgram(src): @unchecked
      prog.decls.head.attributes shouldBe List(Attribute("intrinsic", List("a", "b")))
    }

    "parse a bodyless def carrying @intrinsic" in {
      // The body is None; the elaborator (sub-step B) attaches the
      // TIntrinsic node from the attribute's opId.
      val src = """@intrinsic("test.identity")
                  |def identity(x: integer): integer""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      fn.body shouldBe None
      fn.attributes shouldBe List(Attribute("intrinsic", List("test.identity")))
    }
  }

  // Stage 3-α — type-parameter syntax on def heads.
  "kind-parameterized defs (Stage 3-α)" should {
    "parse a single bounded type parameter" in {
      val src   = "def f[T: Float](x: T): T = x"
      val fn    = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      fn.typeParams shouldBe List(TypeParamAST("T", Some("Float")))
      fn.params shouldBe List(FunParam("x", NamedType("T"), ParamMode.Read))
      fn.returnType shouldBe Some(NamedType("T"))
    }
    "parse an unbounded type parameter" in {
      val src = "def id[T](x: T): T = x"
      val fn  = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      fn.typeParams shouldBe List(TypeParamAST("T", None))
    }
    "parse multiple type parameters with mixed constraints" in {
      val src = "def zip[T, U: Numeric](xs: [T], ys: [U]): integer = 0"
      val fn  = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      fn.typeParams shouldBe List(
        TypeParamAST("T", None),
        TypeParamAST("U", Some("Numeric")),
      )
    }
    "type-parameter list combines with @intrinsic and a bodyless decl" in {
      val src = """@intrinsic("libm.sqrt", T)
                  |def sqrt[T: Float](x: T): T""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      fn.typeParams shouldBe List(TypeParamAST("T", Some("Float")))
      fn.body shouldBe None
    }
  }

  // ========================================================================
  // if / for / while / return expressions
  // ========================================================================

  "if expressions" should {
    "parse a single-line if/then/else" in {
      parseExpr("if x then 1 else 2") shouldBe
        IfExpr(VarRefExpr("x"), IntLitExpr(1), Some(IntLitExpr(2)))
    }
    "parse an if without else" in {
      parseExpr("if x then 1") shouldBe
        IfExpr(VarRefExpr("x"), IntLitExpr(1), None)
    }
    "honor the spec rule: `if a then b else c, d` is `(if a then b else c), d`" in {
      parseExpr("if a then b else c, d") shouldBe
        TupleExpr(List(
          IfExpr(VarRefExpr("a"), VarRefExpr("b"), Some(VarRefExpr("c"))),
          VarRefExpr("d"),
        ))
    }
    "parse a multi-line if/then/else inside a def block" in {
      val src =
        """def normalize(v: [real]) =
          |  val mag = sqrt(sum(v * v))
          |  if mag == 0.0 then v
          |  else v / mag""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      val block = fn.body.get.asInstanceOf[BlockExpr]
      block.result shouldBe IfExpr(
        BinOpExpr("==", VarRefExpr("mag"), RealLitExpr(0.0)),
        VarRefExpr("v"),
        Some(BinOpExpr("/", VarRefExpr("v"), VarRefExpr("mag"))),
      )
    }

    // ---- Spec §4.10 / §7.1: `then` optional when body is on a new
    // indented line. Inline form still requires `then`.
    "parse if without `then` when the body is an indented block" in {
      val src =
        """def f(x: integer) =
          |  if x > 0
          |    1
          |  else
          |    -1""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      // A def body that's a single multi-line statement parses as
      // the statement directly (no BlockExpr wrapper); a single-stmt
      // indented body likewise unwraps to just that statement.
      val ifNode = fn.body.get.asInstanceOf[IfExpr]
      ifNode.cond shouldBe BinOpExpr(">", VarRefExpr("x"), IntLitExpr(0))
      ifNode.thenBranch shouldBe IntLitExpr(1)
      ifNode.elseBranch.get shouldBe UnaryOpExpr("-", IntLitExpr(1))
    }

    "still parse if with `then` when the body is indented" in {
      val src =
        """def f(x: integer) =
          |  if x > 0 then
          |    1
          |  else
          |    -1""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      val ifNode = fn.body.get.asInstanceOf[IfExpr]
      // Same shape as the no-`then` form.
      ifNode.thenBranch shouldBe IntLitExpr(1)
    }

    // ---- Spec §4.10: `elif` is a single-token shorthand for
    // `else if`. Parses to the same nested IfExpr in the else-branch
    // position — so a chain built with `elif` and one built with
    // `else if` must produce identical ASTs.
    "parse a single `elif` clause as `else if`" in {
      val withElif = parseExpr("if a then 1 elif b then 2 else 3")
      val withElseIf = parseExpr("if a then 1 else if b then 2 else 3")
      withElif shouldBe withElseIf
    }

    "parse a chain of `elif`s as nested `else if`s" in {
      val withElif = parseExpr(
        "if a then 1 elif b then 2 elif c then 3 else 4",
      )
      val withElseIf = parseExpr(
        "if a then 1 else if b then 2 else if c then 3 else 4",
      )
      withElif shouldBe withElseIf
    }

    "parse `elif` with the multi-line indented-body form (no `then`)" in {
      val src =
        """def f(x: integer) =
          |  if x > 0
          |    1
          |  elif x < 0
          |    -1
          |  else
          |    0""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      val outer = fn.body.get.asInstanceOf[IfExpr]
      outer.cond shouldBe BinOpExpr(">", VarRefExpr("x"), IntLitExpr(0))
      // The elif is rendered as an IfExpr in the else-branch.
      outer.elseBranch.get shouldBe a [IfExpr]
      val mid = outer.elseBranch.get.asInstanceOf[IfExpr]
      mid.cond shouldBe BinOpExpr("<", VarRefExpr("x"), IntLitExpr(0))
    }
  }

  "for expressions" should {
    "parse a simple for with range" in {
      parseExpr("for i in 0..n do print(i)") shouldBe
        ForExpr(
          VarPat("i"),
          BinOpExpr("..", IntLitExpr(0), VarRefExpr("n")),
          CallExpr(VarRefExpr("print"), List(VarRefExpr("i"))),
        )
    }
    "parse for with paren-less tuple destructuring" in {
      parseExpr("for k, x in enumerate(arr) do print(k)") shouldBe
        ForExpr(
          TuplePat(List(VarPat("k"), VarPat("x"))),
          CallExpr(VarRefExpr("enumerate"), List(VarRefExpr("arr"))),
          CallExpr(VarRefExpr("print"), List(VarRefExpr("k"))),
        )
    }

    // ---- Spec §7.2: `do` optional when the body is on a new
    // indented line. Inline form still requires `do`.
    "parse for without `do` when the body is an indented block" in {
      val src =
        """def main() =
          |  for k in 0..n
          |    print(k)""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      val forNode = fn.body.get.asInstanceOf[ForExpr]
      forNode.pat shouldBe VarPat("k")
      forNode.body shouldBe CallExpr(VarRefExpr("print"), List(VarRefExpr("k")))
    }
  }

  "while expressions" should {
    "parse a simple while" in {
      parseExpr("while x > 0 do print(x)") shouldBe
        WhileExpr(
          BinOpExpr(">", VarRefExpr("x"), IntLitExpr(0)),
          CallExpr(VarRefExpr("print"), List(VarRefExpr("x"))),
        )
    }

    // ---- Spec §7.3: `do` optional when the body is on a new
    // indented line.
    "parse while without `do` when the body is an indented block" in {
      val src =
        """def main() =
          |  while x > 0
          |    print(x)""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      val whileNode = fn.body.get.asInstanceOf[WhileExpr]
      whileNode.cond shouldBe BinOpExpr(">", VarRefExpr("x"), IntLitExpr(0))
      whileNode.body shouldBe CallExpr(VarRefExpr("print"), List(VarRefExpr("x")))
    }
  }

  "return expressions" should {
    "parse a return without value" in {
      parseExpr("return") shouldBe ReturnExpr(None)
    }
    "parse a return with value" in {
      parseExpr("return x + 1") shouldBe
        ReturnExpr(Some(BinOpExpr("+", VarRefExpr("x"), IntLitExpr(1))))
    }
  }

  // ========================================================================
  // Assignment (statement form, only at block-item position)
  // ========================================================================

  "assignment" should {
    "parse simple `x = expr` inside a function block" in {
      val src =
        """def main() =
          |  x = 42""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      fn.body.get shouldBe AssignExpr(VarRefExpr("x"), IntLitExpr(42))
    }
    "parse indexed assignment `a[i] = x`" in {
      val src =
        """def main() =
          |  a[i] = 0.0""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      fn.body.get shouldBe AssignExpr(
        IndexExpr(VarRefExpr("a"), List(VarRefExpr("i"))),
        RealLitExpr(0.0),
      )
    }
    "parse field assignment `s.field = y`" in {
      val src =
        """def main() =
          |  p.x = 5.0""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      fn.body.get shouldBe AssignExpr(
        FieldExpr(VarRefExpr("p"), "x"),
        RealLitExpr(5.0),
      )
    }
    "parse mixed block: val + assignment + final expr" in {
      val src =
        """def step(n: integer) =
          |  var x = n
          |  x = x + 1
          |  x""".stripMargin
      val fn = parseProg(src).decls.head.asInstanceOf[FunDeclAST]
      fn.body.get shouldBe a [BlockExpr]
      val block = fn.body.get.asInstanceOf[BlockExpr]
      block.items.size shouldBe 2  // var decl + assignment
      block.items(0) shouldBe a [BlockDecl]
      block.items(1) shouldBe BlockExprItem(
        AssignExpr(VarRefExpr("x"),
          BinOpExpr("+", VarRefExpr("x"), IntLitExpr(1))),
      )
      block.result shouldBe VarRefExpr("x")
    }
  }

  // ========================================================================
  // Block-form lambda bodies
  // ========================================================================

  "block-form lambdas" should {
    "parse a multi-statement lambda body" in {
      val src =
        """x ->
          |  val y = x * x
          |  y + 1""".stripMargin
      parseExpr(src) shouldBe
        LambdaExpr(List(LambdaParam("x", None)),
          BlockExpr(
            List(BlockDecl(ValDeclAST(VarPat("y"), None,
              BinOpExpr("*", VarRefExpr("x"), VarRefExpr("x"))))),
            BinOpExpr("+", VarRefExpr("y"), IntLitExpr(1)),
          ))
    }
  }

  // ========================================================================
  // Matrix multiplication (@) and rank-2 array literals
  // ========================================================================

  "matrix multiplication" should {
    "parse the @ operator at multiplicative precedence" in {
      parseExpr("A @ v") shouldBe
        BinOpExpr("@", VarRefExpr("A"), VarRefExpr("v"))
    }
    "parse matmul with chaining" in {
      parseExpr("A @ B @ v") shouldBe
        BinOpExpr("@",
          BinOpExpr("@", VarRefExpr("A"), VarRefExpr("B")),
          VarRefExpr("v"))
    }
  }

  "rank-2 array literals (nested)" should {
    "parse as nested __array calls" in {
      parseExpr("[[1.0, 2.0], [3.0, 4.0]]") shouldBe
        CallExpr(VarRefExpr("__array"), List(
          CallExpr(VarRefExpr("__array"), List(RealLitExpr(1.0), RealLitExpr(2.0))),
          CallExpr(VarRefExpr("__array"), List(RealLitExpr(3.0), RealLitExpr(4.0))),
        ))
    }
  }

  // ========================================================================
  // Whole programs from the spec examples
  // ========================================================================

  "whole-program shapes" should {
    "parse the hello-world program" in {
      val src =
        """def main() =
          |  print("Hello, Nex!")""".stripMargin
      val prog = parseProg(src)
      prog.decls.size shouldBe 1
      prog.decls.head shouldBe a [FunDeclAST]
      val fn = prog.decls.head.asInstanceOf[FunDeclAST]
      fn.name shouldBe "main"
      fn.params shouldBe Nil
      fn.body.get shouldBe CallExpr(VarRefExpr("print"),
        List(StringLitExpr("Hello, Nex!")))
    }

    "parse the hypotenuse + main program" in {
      val src =
        """def hypotenuse(a: real, b: real) = sqrt(a^2 + b^2)
          |
          |def main() =
          |  print(s"hypotenuse(3, 4) = ${hypotenuse(3.0, 4.0)}")""".stripMargin
      val prog = parseProg(src)
      prog.decls.size shouldBe 2
      prog.decls(0) shouldBe a [FunDeclAST]
      prog.decls(1) shouldBe a [FunDeclAST]
      prog.decls(0).asInstanceOf[FunDeclAST].name shouldBe "hypotenuse"
      prog.decls(1).asInstanceOf[FunDeclAST].name shouldBe "main"
    }

    "parse a module declaration + imports + a function" in {
      val src =
        """module stats
          |
          |import math.{sqrt, abs}
          |
          |def mean(xs: [real]) = sum(xs) / to_real(length(xs))""".stripMargin
      val prog = parseProg(src)
      prog.decls.size shouldBe 3
      prog.decls(0) shouldBe ModuleDeclAST(List("stats"))
      prog.decls(1) shouldBe ImportDeclAST(List("math"), List(
        ImportSelector("sqrt"),
        ImportSelector("abs"),
      ))
      prog.decls(2) shouldBe a [FunDeclAST]
    }

    "parse an attribute-decorated test function" in {
      val src =
        """@test
          |def test_addition() =
          |  assert_eq(2 + 2, 4)""".stripMargin
      val prog = parseProg(src)
      prog.decls.size shouldBe 1
      val fn = prog.decls.head.asInstanceOf[FunDeclAST]
      fn.attributes shouldBe List(Attribute("test"))
      fn.name shouldBe "test_addition"
    }

    "parse a struct + a function that uses it" in {
      val src =
        """struct Point
          |  x: real
          |  y: real
          |end Point
          |
          |def distance(p1: Point, p2: Point) =
          |  val dx = p1.x - p2.x
          |  val dy = p1.y - p2.y
          |  sqrt(dx^2 + dy^2)""".stripMargin
      val prog = parseProg(src)
      prog.decls.size shouldBe 2
      prog.decls(0) shouldBe a [StructDeclAST]
      prog.decls(1) shouldBe a [FunDeclAST]
      val s = prog.decls(0).asInstanceOf[StructDeclAST]
      s.name shouldBe "Point"
      s.fields.map(_.name) shouldBe List("x", "y")
    }
  }
