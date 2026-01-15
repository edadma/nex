package io.github.edadma.nex

import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers with PackratParsers:
  override def skipWhitespace: Boolean = true
  override val whiteSpace: Regex       = "[ \t]+".r

  def parse(input: String): Either[String, Expr] =
    val reader = new PackratReader(new CharSequenceReader(input))
    parseAll(statement, reader) match
      case Success(result, _) => Right(result)
      case Failure(msg, next) => Left(s"Parse error at ${next.pos}: $msg")
      case Error(msg, next)   => Left(s"Parse error at ${next.pos}: $msg")

  private lazy val statement: PackratParser[Expr] =
    assignment | expression

  private lazy val assignment: PackratParser[AssignExpr] =
    positioned((ident <~ "=") ~ expression ^^ { case name ~ expr => AssignExpr(name, expr) })

  private lazy val expression: PackratParser[Expr] =
    lambda | pipeline

  private lazy val lambda: PackratParser[Expr] =
    positioned(lambdaParams ~ ("->" ~> expression) ^^ {
      case params ~ body => FunctionExpr(params, body)
    })

  private lazy val lambdaParams: PackratParser[List[String]] =
    // (x, y, z) -> ... or (x) -> ... or x -> ...
    ("(" ~> repsep(ident, ",") <~ ")") | (ident ^^ { List(_) })

  private lazy val pipeline: PackratParser[Expr] =
    (pipeline ~ ("|" ~> composition) ^^ { case left ~ right =>
      PipeExpr(left, right).setPos(left.pos)
    }) | composition

  private lazy val composition: PackratParser[Expr] =
    (additive ~ ("." ~> composition) ^^ { case left ~ right =>
      ComposeExpr(left, right).setPos(left.pos)
    }) | additive

  private lazy val additive: PackratParser[Expr] =
    (additive ~ ("+" | "-") ~ comparison ^^ { case left ~ op ~ right =>
      BinaryExpr(op, left, right).setPos(left.pos)
    }) | comparison

  private lazy val comparison: PackratParser[Expr] =
    (comparison ~ (">=" | "<=" | "==" | "!=" | ">" | "<") ~ multiplicative ^^ { case left ~ op ~ right =>
      BinaryExpr(op, left, right).setPos(left.pos)
    }) | multiplicative

  private lazy val multiplicative: PackratParser[Expr] =
    (multiplicative ~ ("*" | "/" | keyword("mod") | "\\") ~ power ^^ { case left ~ op ~ right =>
      BinaryExpr(op, left, right).setPos(left.pos)
    }) | power

  // Keywords - must not be followed by identifier characters
  private def keyword(s: String): PackratParser[String] =
    s <~ guard(not(elem("identifier char", c => c.isLetterOrDigit || c == '_')))

  // Power is right-associative and higher precedence than multiplicative
  private lazy val power: PackratParser[Expr] =
    (factor ~ ("^" ~> power) ^^ { case left ~ right =>
      BinaryExpr("^", left, right).setPos(left.pos)
    }) | factor

  private lazy val factor: PackratParser[Expr] =
    positioned("-" ~> factor ^^ { e => UnaryExpr("-", e) }) | application

  // application handles chained function calls: f(x)(y)(z)
  private lazy val application: PackratParser[Expr] =
    primary ~ rep("(" ~> repsep(expression, ",") <~ ")") ^^ {
      case expr ~ argLists =>
        argLists.foldLeft(expr) { (fn, args) =>
          ApplyExpr(fn, args).setPos(fn.pos)
        }
    }

  private lazy val primary: PackratParser[Expr] =
    number | stringLit | placeholder | variable | arrayLit | parens

  private lazy val placeholder: PackratParser[PlaceholderExpr] =
    positioned("_" ^^^ PlaceholderExpr())

  private lazy val stringLit: PackratParser[StringExpr] =
    positioned("\"" ~> """[^"]*""".r <~ "\"" ^^ { s => StringExpr(s) })

  private lazy val number: PackratParser[NumberExpr] =
    positioned(rational | decimal | integer)

  private lazy val rational: PackratParser[NumberExpr] =
    """\d+/\d+""".r ^^ { s => NumberExpr(s) }

  private lazy val decimal: PackratParser[NumberExpr] =
    """\d+\.\d+""".r ^^ { s => NumberExpr(s) }

  private lazy val integer: PackratParser[NumberExpr] =
    """\d+""".r ^^ { s => NumberExpr(s) }

  private lazy val ident: PackratParser[String] =
    """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ identity

  private lazy val variable: PackratParser[VarExpr] =
    positioned(ident ^^ { name => VarExpr(name) })

  private lazy val arrayLit: PackratParser[ArrayExpr] =
    positioned("[" ~> repsep(expression, ",") <~ "]" ^^ { elems => ArrayExpr(elems) })

  private lazy val parens: PackratParser[Expr] =
    "(" ~> expression <~ ")"
