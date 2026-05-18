package io.github.edadma.nex

import scopt.OParser

/** Command-line driver for the Nex toolchain. v0 surface: `tokens` and
  * `parse` subcommands; the eventual `run` (interpreter) and `compile`
  * (LLVM backend) land here too.
  */
object Cli:

  /** Parsed CLI configuration. `command` is the subcommand name; per-
    * subcommand options live alongside.
    */
  case class Config(
      command: String = "",
      file:    String = "",
  )

  private val builder = OParser.builder[Config]

  private val parser =
    import builder.*
    OParser.sequence(
      programName("nex"),
      head("nex", buildVersion),
      help("help").text("Show this help."),

      cmd("tokens")
        .action((_, c) => c.copy(command = "tokens"))
        .text("Lex the file and print the token stream.")
        .children(
          arg[String]("<file>")
            .required()
            .action((x, c) => c.copy(file = x))
            .text("Source file to lex."),
        ),

      cmd("parse")
        .action((_, c) => c.copy(command = "parse"))
        .text("Parse the file and print the AST.")
        .children(
          arg[String]("<file>")
            .required()
            .action((x, c) => c.copy(file = x))
            .text("Source file to parse."),
        ),

      cmd("elaborate")
        .action((_, c) => c.copy(command = "elaborate"))
        .text("Parse and elaborate (name resolution); print the typed AST.")
        .children(
          arg[String]("<file>")
            .required()
            .action((x, c) => c.copy(file = x))
            .text("Source file to elaborate."),
        ),

      cmd("run")
        .action((_, c) => c.copy(command = "run"))
        .text("Parse, elaborate, and execute the program.")
        .children(
          arg[String]("<file>")
            .required()
            .action((x, c) => c.copy(file = x))
            .text("Source file to run."),
        ),

      checkConfig { c =>
        if c.command.isEmpty then failure("no subcommand given (try `nex help`)")
        else success
      },
    )

  /** Entry point invoked by the platform-specific `@main`. Returns an exit
    * code; the caller should exit with it.
    */
  def run(args: Seq[String]): Int =
    OParser.parse(parser, args, Config()) match
      case None      => 1 // scopt already printed an error
      case Some(cfg) =>
        try cfg.command match
          case "tokens"    => doTokens(cfg.file); 0
          case "parse"     => doParse(cfg.file)
          case "elaborate" => doElaborate(cfg.file)
          case "run"       => doRun(cfg.file)
          case other       =>
            Console.err.println(s"nex: unknown command '$other'")
            1
        catch
          case t: NexTrap =>
            val where = t.pos.map(p => s"${p.line}:${p.column}: ").getOrElse("")
            Console.err.println(s"nex: ${where}trap: ${t.msg}")
            1
          case e: Throwable =>
            Console.err.println(s"nex: ${e.getMessage}")
            1

  // -- Command implementations --------------------------------------------

  private def doTokens(file: String): Unit =
    val source = readFile(file)
    val lexer  = new NexLexer
    lexer.scan(source).foreach(t => println(s"${t.getClass.getSimpleName.padTo(16, ' ')} ${t.chars}"))

  private def doParse(file: String): Int =
    val source = readFile(file)
    new NexParser().parseProgram(source) match
      case Right(ast) =>
        pprint.pprintln(ast)
        0
      case Left(err) =>
        Console.err.println(s"nex: parse error:\n$err")
        1

  private def doElaborate(file: String): Int =
    val source = readFile(file)
    new NexParser().parseProgram(source) match
      case Left(err)  => Console.err.println(s"nex: parse error:\n$err"); 1
      case Right(ast) =>
        new NexElaborator().elaborate(ast) match
          case Right(tp) =>
            pprint.pprintln(tp)
            0
          case Left(errs) =>
            Console.err.println("nex: elaboration errors:")
            errs.foreach(e => Console.err.println(s"  ${e.toString}"))
            1

  private def doRun(file: String): Int =
    val source = readFile(file)
    new NexParser().parseProgram(source) match
      case Left(err)  => Console.err.println(s"nex: parse error:\n$err"); 1
      case Right(ast) =>
        new NexElaborator().elaborate(ast) match
          case Left(errs) =>
            Console.err.println("nex: elaboration errors:")
            errs.foreach(e => Console.err.println(s"  ${e.toString}"))
            1
          case Right(tp) =>
            new NexInterpreter().runProgram(tp)
            0

  /** Hard-coded for now; in v1 we'll wire it to build.sbt's `version`. */
  private def buildVersion: String = "0.0.1"
