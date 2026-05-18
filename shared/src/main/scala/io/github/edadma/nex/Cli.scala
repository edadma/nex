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
          case "tokens" => doTokens(cfg.file); 0
          case "parse"  => doParse(cfg.file)
          case other    =>
            Console.err.println(s"nex: unknown command '$other'")
            1
        catch case e: Throwable =>
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

  /** Hard-coded for now; in v1 we'll wire it to build.sbt's `version`. */
  private def buildVersion: String = "0.0.1"
