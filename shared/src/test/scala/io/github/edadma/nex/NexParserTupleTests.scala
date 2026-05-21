package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Paren-less tuple construction in every grammar position that doesn't
  * have a real ambiguity. Spec §4.17: comma is the loosest operator and
  * parentheses are not required around tuple construction.
  *
  * The grammar deliberately keeps `exprNoTuple` in places where commas
  * mean something else (function args, array elements, indices, function
  * parameter defaults). Everywhere else — block items, def block bodies,
  * lambda bodies, return values, branch bodies (if/then, else, elif, do,
  * match arms), assignment RHS — a bare comma-separated list should
  * construct a tuple.
  */
class NexParserTupleTests extends AnyWordSpec with Matchers:

  private def parser = new NexParser

  private def parseProg(src: String): ProgramAST =
    parser.parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err\nin source:\n$src")

  private def parseExpr(src: String): ExprAST =
    parser.parseExpression(src) match
      case Right(e)  => e
      case Left(err) => fail(s"parse error: $err\nin source:\n$src")

  private def v(n: String): ExprAST = VarRefExpr(n)
  private def i(n: Long):   ExprAST = IntLitExpr(n)

  /** The function body of a single-fn program — strip the wrapping
    * decl for compact assertions.
    */
  private def bodyOf(src: String): ExprAST =
    parseProg(src).decls match
      case List(FunDeclAST(_, _, _, Some(body), _, _, _)) => body
      case other => fail(s"expected one def with a body, got: $other")

  // ========================================================================
  // Top-level expression — baseline
  // ========================================================================

  "top-level tuple" should {
    "construct without parens" in {
      parseExpr("a, b, c") shouldBe TupleExpr(List(v("a"), v("b"), v("c")))
    }
    "still construct with parens (parens are grouping, tuple is the comma)" in {
      parseExpr("(a, b, c)") shouldBe TupleExpr(List(v("a"), v("b"), v("c")))
    }
  }

  // ========================================================================
  // val / var / const binding RHS
  // ========================================================================

  "val/var/const binding RHS" should {
    "accept a bare tuple on the right of =" in {
      parseProg("val p = 1, 2, 3").decls shouldBe List(
        ValDeclAST(VarPat("p"), None,
          TupleExpr(List(i(1), i(2), i(3)))),
      )
    }
    "accept a bare tuple destructured into a paren-less LHS" in {
      parseProg("val a, b, c = 1, 2, 3").decls shouldBe List(
        ValDeclAST(
          TuplePat(List(VarPat("a"), VarPat("b"), VarPat("c"))),
          None,
          TupleExpr(List(i(1), i(2), i(3)))),
      )
    }
    "accept var with paren-less tuple" in {
      parseProg("var p = 1, 2, 3").decls match
        case List(VarDeclAST(VarPat("p"), None, TupleExpr(_), _, _)) => succeed
        case other => fail(s"got: $other")
    }
  }

  // ========================================================================
  // def body — single-line (= already works) and block-form
  // ========================================================================

  "def body" should {
    "accept a bare tuple after = on one line" in {
      bodyOf("def split(x: real) = x, -x") shouldBe
        TupleExpr(List(v("x"), UnaryOpExpr("-", v("x"))))
    }

    "accept a bare tuple as the result of an indented block body" in {
      val src =
        """def dims() =
          |  10, 20, 30""".stripMargin
      bodyOf(src) shouldBe TupleExpr(List(i(10), i(20), i(30)))
    }

    "accept a bare tuple as the LAST item of a multi-line block body" in {
      val src =
        """def split(x: real) =
          |  val y = x * 2.0
          |  y, -y""".stripMargin
      bodyOf(src) match
        case BlockExpr(_, TupleExpr(List(VarRefExpr("y"), UnaryOpExpr("-", VarRefExpr("y"))))) =>
          succeed
        case other => fail(s"got: $other")
    }
  }

  // ========================================================================
  // Lambda body
  // ========================================================================

  "lambda body" should {
    // Single-line lambda body does NOT consume a trailing comma, because
    // commas are the loosest operator (spec §4.3) and the lambda might
    // be sitting inside a call's arg list — `f(() -> x, y)` is a 2-arg
    // call, not a single-arg call with a tuple-returning lambda. Wrap
    // the body in parens to return a tuple inline.
    "accept a paren-wrapped tuple on the right of ->" in {
      parseExpr("(a, b) -> (b, a)") shouldBe
        LambdaExpr(
          List(LambdaParam("a", None), LambdaParam("b", None)),
          TupleExpr(List(v("b"), v("a"))),
        )
    }

    "single-line lambda body without parens does NOT swallow the outer comma" in {
      // Top-level expression: `x -> y, z` binds as `(x -> y), z`.
      parseExpr("x -> y, z") shouldBe
        TupleExpr(List(
          LambdaExpr(List(LambdaParam("x", None)), v("y")),
          v("z"),
        ))
    }

    "accept a bare tuple from a block-body lambda's last item" in {
      val src =
        """val swap = (a, b) ->
          |  b, a""".stripMargin
      parseProg(src).decls.head match
        case ValDeclAST(VarPat("swap"), _,
              LambdaExpr(_, TupleExpr(List(VarRefExpr("b"), VarRefExpr("a")))),
              _, _) => succeed
        case other => fail(s"got: $other")
    }
  }

  // ========================================================================
  // return
  // ========================================================================

  "return" should {
    "accept a bare tuple as the value" in {
      parseExpr("return a, b, c") shouldBe
        ReturnExpr(Some(TupleExpr(List(v("a"), v("b"), v("c")))))
    }

    "still accept return with no value" in {
      parseExpr("return") shouldBe ReturnExpr(None)
    }

    "still accept return with a single non-tuple value" in {
      parseExpr("return x") shouldBe ReturnExpr(Some(v("x")))
    }
  }

  // ========================================================================
  // if / elif / else branch bodies
  // ========================================================================

  "if-expression branches" should {
    // Inline if-then-else branches do NOT consume a trailing comma —
    // comma is the loosest operator (spec §4.3), so it binds at the
    // outer expression level. Wrap the branch in parens to return a
    // tuple inline: `if c then (a, b) else d`. The block-form branch
    // (Newline-Indent) has no such restriction.
    "accept a paren-wrapped tuple after `then`" in {
      parseExpr("if c then (a, b) else d") shouldBe
        IfExpr(VarRefExpr("c"),
               TupleExpr(List(VarRefExpr("a"), VarRefExpr("b"))),
               Some(VarRefExpr("d")))
    }

    "honor the spec rule: `if c then a else b, d` is `(if c then a else b), d`" in {
      parseExpr("if c then a else b, d") shouldBe
        TupleExpr(List(
          IfExpr(VarRefExpr("c"), VarRefExpr("a"), Some(VarRefExpr("b"))),
          VarRefExpr("d"),
        ))
    }

    "accept a bare tuple as the last item of an indented if-branch block" in {
      val src =
        """def main() =
          |  if c then
          |    a, b""".stripMargin
      bodyOf(src) match
        case IfExpr(VarRefExpr("c"),
                    TupleExpr(List(VarRefExpr("a"), VarRefExpr("b"))),
                    None) => succeed
        case other => fail(s"got: $other")
    }
  }

  // ========================================================================
  // for / while body
  // ========================================================================

  "loop body" should {
    // Inline `do` bodies follow the same rule as `then` / `else`: comma
    // is loosest, so a tuple in the body needs parens (`do (k, k)`) or
    // an indented block.
    "accept a bare tuple as the last item of an indented for-loop block" in {
      val src =
        """def main() =
          |  for k in 0..3 do
          |    k, k""".stripMargin
      bodyOf(src) match
        case ForExpr(_, _,
              TupleExpr(List(VarRefExpr("k"), VarRefExpr("k")))) => succeed
        case other => fail(s"got: $other")
    }

    "accept a bare tuple as the last item of an indented while-loop block" in {
      val src =
        """def main() =
          |  while true do
          |    1, 2""".stripMargin
      bodyOf(src) match
        case WhileExpr(_, TupleExpr(List(IntLitExpr(1), IntLitExpr(2)))) =>
          succeed
        case other => fail(s"got: $other")
    }
  }

  // ========================================================================
  // match arms
  // ========================================================================

  "match arm body" should {
    // Inline arm bodies follow the spec rule: comma is loosest, so
    // returning a tuple from an arm requires parens or an indented
    // block. (A bare comma on a single-line arm would otherwise be
    // ambiguous with the arm separator if multiple arms were chained
    // by `;`.)
    "accept a paren-wrapped tuple after ->" in {
      val src =
        """def f(s: Solver) =
          |  s match
          |    Converged(x) -> (x, x)
          |    Diverged     -> (0.0, 0.0)""".stripMargin
      // Note: `Diverged` (no parens) parses as `VarPat` — the elaborator
      // distinguishes "variable binding" from "no-arg variant" using scope.
      bodyOf(src) match
        case MatchExpr(VarRefExpr("s"), List(
              MatchCase(VariantPat("Converged", _),
                TupleExpr(List(VarRefExpr("x"), VarRefExpr("x")))),
              MatchCase(VarPat("Diverged"),
                TupleExpr(List(RealLitExpr(0.0), RealLitExpr(0.0)))))) => succeed
        case other => fail(s"got: $other")
    }

    "accept a bare tuple in an indented arm body" in {
      val src =
        """def f(s: Solver) =
          |  s match
          |    Converged(x) ->
          |      x, x
          |    Diverged ->
          |      0.0, 0.0""".stripMargin
      bodyOf(src) match
        case MatchExpr(VarRefExpr("s"), List(
              MatchCase(VariantPat("Converged", _),
                TupleExpr(List(VarRefExpr("x"), VarRefExpr("x")))),
              MatchCase(VarPat("Diverged"),
                TupleExpr(List(RealLitExpr(0.0), RealLitExpr(0.0)))))) => succeed
        case other => fail(s"got: $other")
    }
  }

  // ========================================================================
  // Assignment RHS
  // ========================================================================

  "assignment RHS" should {
    "accept a bare tuple" in {
      val src =
        """def main() =
          |  var p = 0
          |  p = 1, 2, 3""".stripMargin
      bodyOf(src) match
        case BlockExpr(_, AssignExpr(VarRefExpr("p"),
              TupleExpr(List(IntLitExpr(1), IntLitExpr(2), IntLitExpr(3))))) =>
          succeed
        case other => fail(s"got: $other")
    }
  }

  // ========================================================================
  // Block items
  // ========================================================================

  "block items" should {
    "accept a bare tuple as a non-final block item" in {
      val src =
        """def main() =
          |  1, 2
          |  3""".stripMargin
      bodyOf(src) match
        case BlockExpr(
              List(BlockExprItem(TupleExpr(List(IntLitExpr(1), IntLitExpr(2))))),
              IntLitExpr(3)) => succeed
        case other => fail(s"got: $other")
    }
  }

  // ========================================================================
  // Destructuring regression — already works, lock in
  // ========================================================================

  "tuple destructuring (regression)" should {
    "accept paren-less LHS in val" in {
      parseProg("val a, b = 1, 2").decls.head match
        case ValDeclAST(TuplePat(List(VarPat("a"), VarPat("b"))), _,
              TupleExpr(List(IntLitExpr(1), IntLitExpr(2))), _, _) => succeed
        case other => fail(s"got: $other")
    }

    "accept paren-less for-loop pattern" in {
      val src =
        """def main() =
          |  for k, x in pairs do k""".stripMargin
      bodyOf(src) match
        case ForExpr(TuplePat(List(VarPat("k"), VarPat("x"))), _, _) => succeed
        case other => fail(s"got: $other")
    }
  }

  // ========================================================================
  // Positions where commas mean something else — must NOT construct tuples
  // ========================================================================

  "comma-separator positions reject tuple construction" should {
    "call arguments stay as separate args" in {
      parseExpr("f(a, b, c)") shouldBe
        CallExpr(v("f"), List(v("a"), v("b"), v("c")))
    }

    "array literal elements stay as separate elements" in {
      parseExpr("[a, b, c]") shouldBe
        CallExpr(VarRefExpr("__array"), List(v("a"), v("b"), v("c")))
    }

    "index list stays as separate indices" in {
      parseExpr("m[i, j]") shouldBe
        IndexExpr(v("m"), List(v("i"), v("j")))
    }
  }
