package io.github.edadma.nex

import scopt.OParser

case class Config(
    eval: Option[String] = None,
    file: Option[String] = None,
)

@main def run(args: String*): Unit =
  val builder = OParser.builder[Config]
  import builder.*

  val parser = OParser.sequence(
    programName("nex"),
    head("nex", "0.0.1"),
    opt[String]('e', "eval")
      .action((x, c) => c.copy(eval = Some(x)))
      .text("evaluate expression and print result"),
    opt[String]('f', "file")
      .action((x, c) => c.copy(file = Some(x)))
      .text("execute file"),
    help("help").text("print this help message"),
  )

  OParser.parse(parser, args, Config()) match
    case Some(config) =>
      config match
        case Config(Some(expr), _) => evalExpr(expr)
        case Config(_, Some(file)) => evalFile(file)
        case _ => Repl.run()
    case None => ()

def evalExpr(input: String): Unit =
  val evaluator = new Evaluator()
  Parser.parse(input) match
    case Left(err) => println(err)
    case Right(ast) =>
      try
        val rewritten = Rewriter.rewrite(ast)
        val result = evaluator.eval(rewritten)
        println(Value.display(result))
      catch
        case e: RuntimeError => println(e.getMessage)
        case e: Exception => println(s"Error: ${e.getMessage}")

def evalFile(path: String): Unit =
  println(s"File execution not yet implemented: $path")
