package io.github.edadma.nex

import io.github.edadma.readline.*
import scala.annotation.tailrec

object Repl:
  def run(): Unit =
    val homeDir = System.getProperty("user.home")
    val historyFile = s"$homeDir/.nex_history"
    var historyExists = read_history(historyFile) == 0

    val evaluator = new Evaluator()

    println("Nex Array REPL v0.0.1")
    println("Type 'quit' or 'exit' to exit, Ctrl-D to quit")
    println()

    @tailrec
    def loop(): Unit =
      val line = readline("> ")

      if line != null then
        val trimmed = line.trim

        if trimmed == "quit" || trimmed == "exit" then
          ()
        else
          if trimmed.nonEmpty then
            Parser.parse(trimmed) match
              case Left(err) => println(err)
              case Right(ast) =>
                try
                  val rewritten = Rewriter.rewrite(ast)
                  val result = evaluator.eval(rewritten)
                  println(Value.display(result))
                catch
                  case e: RuntimeError => println(e.getMessage)
                  case e: Exception => println(s"Error: ${e.getMessage}")

            // Add to history (avoid duplicates)
            val prev = history_get(history_base + history_length - 1)
            if prev == null || prev != trimmed then
              add_history(trimmed)
              if historyExists then append_history(1, historyFile)
              else
                historyExists = true
                write_history(historyFile)

          loop()

    loop()
