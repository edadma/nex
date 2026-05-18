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

      cmd("test")
        .action((_, c) => c.copy(command = "test"))
        .text("Discover and run every `@test`-annotated function in the project.")
        .children(
          arg[String]("<file>")
            .required()
            .action((x, c) => c.copy(file = x))
            .text("Project entry file (its directory is the project root)."),
        ),

      cmd("compile")
        .action((_, c) => c.copy(command = "compile"))
        .text("Emit LLVM IR for the program (v0 scaffolding: scalar arithmetic + print).")
        .children(
          arg[String]("<file>")
            .required()
            .action((x, c) => c.copy(file = x))
            .text("Project entry file."),
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
          case "test"      => doTest(cfg.file)
          case "compile"   => doCompile(cfg.file)
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
    loadAndElaborate(file) match
      case Right(tp) => pprint.pprintln(tp); 0
      case Left(rc)  => rc

  private def doRun(file: String): Int =
    loadAndElaborate(file) match
      case Left(rc)  => rc
      case Right(tp) =>
        new NexInterpreter().runProgram(tp)
        0

  /** Emit LLVM IR for the program and shell out to `clang` to produce a
    * native binary. Output paths: `<entry>.ll` (IR) and `<entry-basename>`
    * (executable). v0 scaffolding — only covers `def main() = print(N)`
    * and scalar arithmetic; richer programs will hit "not yet" diags in
    * the emitted IR and fail at the clang stage.
    */
  private def doCompile(file: String): Int =
    loadAndElaborate(file) match
      case Left(rc) => rc
      case Right(tp) =>
        val ir       = new NexLLVMCodegen().compile(tp)
        val llPath   = if file.endsWith(".nex") then file.stripSuffix(".nex") + ".ll" else file + ".ll"
        val binPath  = if file.endsWith(".nex") then file.stripSuffix(".nex") else file + ".out"
        writeFile(llPath, ir)
        println(s"nex: wrote $llPath")
        // Shell out to clang. JVM-only; throws on JS/Native if invoked.
        try
          val pb = new java.lang.ProcessBuilder("clang", "-O1", "-o", binPath, llPath)
            .inheritIO()
          val rc = pb.start().waitFor()
          if rc == 0 then
            println(s"nex: wrote $binPath")
            0
          else
            Console.err.println(s"nex: clang exited with status $rc")
            rc
        catch
          case e: Throwable =>
            Console.err.println(s"nex: failed to invoke clang: ${e.getMessage}")
            1

  /** Walk the elaborated project for every `@test`-annotated nullary
    * function, run each one in isolation (fresh interpreter so module-
    * init side effects and top-level `var` mutations don't bleed
    * across tests), and report pass/fail with a one-line summary.
    *
    * Per spec §6.9: tests pass by returning normally, fail by trapping.
    * The runner catches `NexTrap` and prints its message + position;
    * any unrecognised throwable also fails the test (defensive).
    */
  private def doTest(file: String): Int =
    loadAndElaborate(file) match
      case Left(rc) => rc
      case Right(tp) =>
        val tests = tp.decls.collect {
          case f: TFunDecl if f.attributes.contains("test") && f.params.isEmpty => f
        }
        if tests.isEmpty then
          println("no `@test` functions found.")
          return 0

        var passed = 0
        var failed = 0
        val start  = System.nanoTime
        for f <- tests do
          val tStart = System.nanoTime
          try
            val interp = new NexInterpreter()
            interp.initializeProgram(tp)
            interp.callNullary(f.sym)
            val ms = (System.nanoTime - tStart) / 1_000_000
            println(f"  ok    ${f.sym.name}%-40s ($ms%4d ms)")
            passed += 1
          catch
            case t: NexTrap =>
              val where = t.pos.map(p => s" at ${p.line}:${p.column}").getOrElse("")
              val ms    = (System.nanoTime - tStart) / 1_000_000
              println(f"  FAIL  ${f.sym.name}%-40s ($ms%4d ms)")
              println(s"        ${t.msg}$where")
              failed += 1
            case e: Throwable =>
              val ms = (System.nanoTime - tStart) / 1_000_000
              println(f"  FAIL  ${f.sym.name}%-40s ($ms%4d ms)")
              println(s"        unexpected exception: ${e.getMessage}")
              failed += 1

        val totalMs = (System.nanoTime - start) / 1_000_000
        println()
        println(s"$passed passed, $failed failed (${tests.size} total, ${totalMs} ms)")
        if failed == 0 then 0 else 1

  /** Load + elaborate a project starting from `entryFile`. The project
    * root is the directory containing the entry file; imports resolve as
    * subdirectories of that root (per spec §9). Returns the elaborated
    * TProgram or an exit code (after printing the relevant errors).
    */
  private def loadAndElaborate(entryFile: String): Either[Int, TProgram] =
    val projectRoot = pathDirname(entryFile)
    new NexModuleLoader(projectRoot).loadFrom(entryFile) match
      case Left(errs) =>
        Console.err.println("nex: module loading errors:")
        errs.foreach(e => Console.err.println(s"  $e"))
        Left(1)
      case Right(modules) =>
        new NexElaborator().elaborateProject(modules) match
          case Right(tp) => Right(tp)
          case Left(errs) =>
            Console.err.println("nex: elaboration errors:")
            errs.foreach(e => Console.err.println(s"  ${e.toString}"))
            Left(1)

  /** Hard-coded for now; in v1 we'll wire it to build.sbt's `version`. */
  private def buildVersion: String = "0.0.1"
