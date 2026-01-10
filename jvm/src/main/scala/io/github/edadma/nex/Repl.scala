package io.github.edadma.nex

import org.jline.reader.{LineReaderBuilder, EndOfFileException, UserInterruptException}
import org.jline.terminal.TerminalBuilder

object Repl:
  def run(): Unit =
    val terminal = TerminalBuilder.builder()
      .name("nex")
      .system(true)
      .build()

    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      .appName("nex")
      .build()

    val evaluator = new Evaluator()

    println("Nex Array REPL v0.1")
    println("Type 'quit' or 'exit' to exit, Ctrl-C to cancel, Ctrl-D to quit")
    println()

    var running = true
    while running do
      try
        val line = reader.readLine("> ")
        if line == null || line.trim == "quit" || line.trim == "exit" then
          running = false
        else if line.trim.nonEmpty then
          Parser.parse(line) match
            case Left(err) => println(err)
            case Right(ast) =>
              try
                val rewritten = Rewriter.rewrite(ast)
                val result = evaluator.eval(rewritten)
                println(Value.display(result))
              catch
                case e: RuntimeError => println(e.getMessage)
                case e: Exception => println(s"Error: ${e.getMessage}")
      catch
        case _: EndOfFileException => running = false
        case _: UserInterruptException => println()

    terminal.close()
