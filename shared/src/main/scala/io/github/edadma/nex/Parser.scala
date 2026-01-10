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

  private lazy val assignment: PackratParser[Assign] =
    positioned((ident <~ "=") ~ expression ^^ { case name ~ expr => Assign(name, expr) })

  private lazy val expression: PackratParser[Expr] =
    pipeline

  private lazy val pipeline: PackratParser[Expr] =
    (pipeline ~ ("|" ~> composition) ^^ { case left ~ right =>
      Pipe(left, right).setPos(left.pos)
    }) | composition

  private lazy val composition: PackratParser[Expr] =
    (additive ~ ("." ~> composition) ^^ { case left ~ right =>
      Compose(left, right).setPos(left.pos)
    }) | additive

  private lazy val additive: PackratParser[Expr] =
    (additive ~ ("+" | "-") ~ comparison ^^ { case left ~ op ~ right =>
      BinOp(op, left, right).setPos(left.pos)
    }) | comparison

  private lazy val comparison: PackratParser[Expr] =
    (comparison ~ (">=" | "<=" | "==" | "!=" | ">" | "<") ~ multiplicative ^^ { case left ~ op ~ right =>
      BinOp(op, left, right).setPos(left.pos)
    }) | multiplicative

  private lazy val multiplicative: PackratParser[Expr] =
    (multiplicative ~ ("*" | "/") ~ factor ^^ { case left ~ op ~ right =>
      BinOp(op, left, right).setPos(left.pos)
    }) | factor

  private lazy val factor: PackratParser[Expr] =
    positioned("-" ~> factor ^^ { e => UnaryOp("-", e) }) | primary

  private lazy val primary: PackratParser[Expr] =
    number | stringLit | functionCall | placeholder | variable | arrayLit | parens

  private lazy val placeholder: PackratParser[Placeholder] =
    positioned("_" ^^^ Placeholder())

  private lazy val stringLit: PackratParser[Str] =
    positioned("\"" ~> """[^"]*""".r <~ "\"" ^^ { s => Str(s) })

  private lazy val number: PackratParser[Num] =
    positioned(rational | decimal | integer)

  private lazy val rational: PackratParser[Num] =
    """\d+/\d+""".r ^^ { s => Num(s) }

  private lazy val decimal: PackratParser[Num] =
    """\d+\.\d+""".r ^^ { s => Num(s) }

  private lazy val integer: PackratParser[Num] =
    """\d+""".r ^^ { s => Num(s) }

  private lazy val ident: PackratParser[String] =
    """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ identity

  private lazy val variable: PackratParser[Var] =
    positioned(ident ^^ { name => Var(name) })

  private lazy val functionCall: PackratParser[Call] =
    positioned(ident ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
      case name ~ args => Call(name, args)
    })

  private lazy val arrayLit: PackratParser[ArrayLit] =
    positioned("[" ~> repsep(expression, ",") <~ "]" ^^ { elems => ArrayLit(elems) })

  private lazy val parens: PackratParser[Expr] =
    "(" ~> expression <~ ")"
