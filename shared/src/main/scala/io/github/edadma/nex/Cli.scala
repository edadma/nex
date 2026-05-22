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
      command:     String         = "",
      file:        String         = "",
      backend:     String         = "llvm",
      preludePath: Option[String] = None,
  )

  private val builder = OParser.builder[Config]

  private val parser =
    import builder.*
    OParser.sequence(
      programName("nex"),
      head("nex", buildVersion),
      help("help").text("Show this help."),

      opt[String]("prelude-path")
        .valueName("<dir>")
        .action((p, c) => c.copy(preludePath = Some(p)))
        .text("Override the source-prelude directory (default: discover via NEX_HOME / sysprop / walk-up)."),

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
        .text("Compile the program to a native binary. Default backend is LLVM IR; pass --backend mlir for the MLIR pipeline (much smaller subset).")
        .children(
          opt[String]("backend")
            .valueName("<llvm|mlir>")
            .validate {
              case "llvm" | "mlir" => success
              case other           => failure(s"--backend must be llvm or mlir, got '$other'")
            }
            .action((b, c) => c.copy(backend = b))
            .text("Codegen backend (default: llvm)."),
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
          case "elaborate" => doElaborate(cfg.file, cfg.preludePath)
          case "run"       => doRun(cfg.file, cfg.preludePath)
          case "test"      => doTest(cfg.file, cfg.preludePath)
          case "compile"   => doCompile(cfg.file, cfg.backend, cfg.preludePath)
          case other       =>
            Console.err.println(s"nex: unknown command '$other'")
            1
        catch
          case t: NexTrap =>
            // Format matches the AOT runtime's `__nex_trap` output
            // (write(2, "trap: <msg>\n", ...)) so user-visible stderr
            // bytes from interp-via-CLI and a Nex-compiled binary
            // agree byte-for-byte on the same trap. A leading source
            // position is added when the trap carries one.
            val where = t.pos.map(p => s"${p.line}:${p.column}: ").getOrElse("")
            Console.err.println(s"${where}trap: ${t.msg}")
            1
          case e: Throwable =>
            Console.err.println(s"nex: ${e.getMessage}")
            1

  // -- Command implementations --------------------------------------------

  private def doTokens(file: String): Unit =
    val source = readNexSource(file)
    val lexer  = new NexLexer
    lexer.scan(source).foreach(t => println(s"${t.getClass.getSimpleName.padTo(16, ' ')} ${t.chars}"))

  private def doParse(file: String): Int =
    val source = readNexSource(file)
    new NexParser().parseProgram(source) match
      case Right(ast) =>
        pprint.pprintln(ast)
        0
      case Left(err) =>
        Console.err.println(s"nex: parse error:\n$err")
        1

  private def doElaborate(file: String, preludePath: Option[String]): Int =
    loadAndElaborate(file, preludePath = preludePath) match
      case Right(tp) => pprint.pprintln(tp); 0
      case Left(rc)  => rc

  private def doRun(file: String, preludePath: Option[String]): Int =
    loadAndElaborate(file, preludePath = preludePath) match
      case Left(rc)  => rc
      case Right(tp) =>
        new NexInterpreter().runProgram(tp)
        0

  /** Compile the program to a native binary via one of the supported
    * backends. The default LLVM backend covers the full v0 surface; the
    * MLIR backend (`--backend mlir`) only handles the small subset
    * exercised by `NexMLIRParityTests` (sum, element-wise, matmul,
    * array printing) and rejects anything else with `notYet`.
    *
    * Output paths in both modes follow the same convention: an IR
    * file next to the source plus an executable named after the entry
    * file. MLIR mode additionally writes `<entry>.mlir` and
    * `<entry>.lowered.mlir` for inspection.
    */
  private def doCompile(file: String, backend: String, preludePath: Option[String]): Int =
    loadAndElaborate(file, preludePath = preludePath) match
      case Left(rc)  => rc
      case Right(tp) => backend match
        case "llvm" => doCompileLlvm(file, tp)
        case "mlir" => doCompileMlir(file, tp)
        case other  =>
          Console.err.println(s"nex: unknown backend '$other'")
          1

  private def doCompileLlvm(file: String, tp: TProgram): Int =
    // Fuse element-wise / broadcast / map chains into TFusedLoop before
    // codegen. The pass is semantically transparent — the interpreter
    // has eval cases for both shapes and corpus parity tests pin the
    // equivalence — so wiring it here lets compiled binaries enjoy the
    // single-loop output without changing observable behaviour.
    val fused = new NexFusion(tp.symbols).fuseProgram(tp)
    val ir =
      try new NexLLVMCodegen().compile(fused)
      catch case e: NexCodegenError =>
        Console.err.println(s"nex: ${e.getMessage}")
        return 1
    val base    = stripNexSuffix(file)
    val llPath  = base.map(_ + ".ll").getOrElse(file + ".ll")
    val binPath = base.getOrElse(file + ".out")
    writeFile(llPath, ir)
    println(s"nex: wrote $llPath")
    runProcess(s"clang -O1 -o $binPath $llPath", Seq("clang", "-O1", "-o", binPath, llPath)) match
      case 0  => println(s"nex: wrote $binPath"); 0
      case rc => rc

  /** MLIR-backend compile: emit MLIR, run mlir-opt + mlir-translate from
    * Homebrew LLVM 22 (overridable via `NEX_LLVM_HOME`), extract the
    * embedded `mlir_runtime.c` to a temp file, link with clang.
    */
  private def doCompileMlir(file: String, tp: TProgram): Int =
    val llvmHome = sys.env.getOrElse("NEX_LLVM_HOME", "/opt/homebrew/opt/llvm")
    val tool     = (name: String) => s"$llvmHome/bin/$name"

    // NexFusion isn't applied for MLIR yet — its `emitFusedLoop1D` only
    // handles rank-1 fused loops with statically-known lengths; rank-2
    // and dynamic-length cases (e.g. broadcasts inside `def`s, ranges
    // computed at runtime) trap with `notYet`. Picking up rank-2 and
    // dynamic lengths is part of the MLIR strangler-fig follow-up.
    val mlirSrc      = new NexMLIRCodegen().compile(tp)
    val base         = stripNexSuffix(file).getOrElse(file)
    val mlirPath     = base + ".mlir"
    val loweredPath  = base + ".lowered.mlir"
    val llPath       = base + ".ll"
    val binPath      = base
    val runtimePath  = base + ".mlir_runtime.c"
    writeFile(mlirPath, mlirSrc)
    println(s"nex: wrote $mlirPath")

    val runtime = loadResourceText("/io/github/edadma/nex/mlir_runtime.c")
    if runtime.isEmpty then
      Console.err.println("nex: mlir_runtime.c resource missing from jar")
      return 1
    writeFile(runtimePath, runtime)

    val mlirOptArgs = Seq(
      tool("mlir-opt"), mlirPath,
      "--one-shot-bufferize=bufferize-function-boundaries",
      "--convert-linalg-to-loops",
      "--convert-scf-to-cf",
      "--expand-strided-metadata",
      "--finalize-memref-to-llvm",
      "--convert-arith-to-llvm",
      "--convert-func-to-llvm",
      "--convert-cf-to-llvm",
      "--reconcile-unrealized-casts",
      "-o", loweredPath,
    )
    runProcess(mlirOptArgs.mkString(" "), mlirOptArgs) match
      case 0 => ()
      case rc => return rc
    runProcess(s"mlir-translate $loweredPath -> $llPath",
      Seq(tool("mlir-translate"), "--mlir-to-llvmir", loweredPath, "-o", llPath)) match
      case 0 => ()
      case rc => return rc
    runProcess(s"clang -> $binPath",
      Seq(tool("clang"), "-O1", "-o", binPath, llPath, runtimePath,
          s"-L$llvmHome/lib", s"-Wl,-rpath,$llvmHome/lib", "-lmlir_c_runner_utils")) match
      case 0 => println(s"nex: wrote $binPath"); 0
      case rc => rc

  /** Shell-out helper used by both compile backends: prints the
    * command on failure (so the user can re-run by hand), inherits
    * stdio so child diagnostics surface directly. Returns the child
    * exit code; -1 if the process couldn't be launched.
    */
  private def runProcess(label: String, cmd: Seq[String]): Int =
    try
      import scala.jdk.CollectionConverters.*
      val pb = new java.lang.ProcessBuilder(cmd.asJava).inheritIO()
      val rc = pb.start().waitFor()
      if rc != 0 then Console.err.println(s"nex: $label exited with status $rc")
      rc
    catch
      case e: Throwable =>
        Console.err.println(s"nex: failed to invoke ${cmd.head}: ${e.getMessage}")
        -1

  /** Strip a recognised Nex source suffix (`.nex` or `.lnex`) from a
    * path. Returns `None` if the path has neither suffix, letting the
    * caller fall back to an output name that won't collide with the
    * input.
    */
  private def stripNexSuffix(file: String): Option[String] =
    if file.endsWith(".lnex") then Some(file.stripSuffix(".lnex"))
    else if file.endsWith(".nex") then Some(file.stripSuffix(".nex"))
    else None

  /** JVM-only resource loader (JS / Native have no classpath
    * resources). Returns the empty string if the resource is missing.
    */
  private def loadResourceText(path: String): String =
    val stream = getClass.getResourceAsStream(path)
    if stream == null then ""
    else
      try new String(stream.readAllBytes, "UTF-8")
      finally stream.close()

  /** Walk the elaborated project for every `@test`-annotated nullary
    * function, run each one in isolation (fresh interpreter so module-
    * init side effects and top-level `var` mutations don't bleed
    * across tests), and report pass/fail with a one-line summary.
    *
    * Per spec §6.9: tests pass by returning normally, fail by trapping.
    * The runner catches `NexTrap` and prints its message + position;
    * any unrecognised throwable also fails the test (defensive).
    */
  private def doTest(file: String, preludePath: Option[String]): Int =
    loadAndElaborate(file, includeTestOnly = true, preludePath = preludePath) match
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
    *
    * @param includeTestOnly  When false (the `nex run` / `nex compile`
    *   case), modules whose any file declares `@test module ...` are
    *   stripped from the loader's result per spec §9.6 — test fixtures
    *   should not appear in non-test builds. When true (`nex test`),
    *   every module is kept so the runner can discover `@test`
    *   functions in regular and test-only modules alike.
    *
    * @param preludePath  Optional explicit override for the source-prelude
    *   directory; threaded into [[NexSysroot.findPreludeRoot]]. When the
    *   final lookup returns `Some(dir)`, the prelude is prepended as a
    *   `LoadedModule(path = ["prelude"], …)` and every non-prelude module
    *   gets a synthetic `import prelude.*` injected at elaboration. When
    *   the lookup returns `None` (no override, env, sysprop, or walk-up
    *   match), the compiler runs with only the built-in [[NexElabState.registerPrelude]]
    *   — the silent-fallback policy that lets Stages 1-2 ship incrementally.
    */
  private def loadAndElaborate(
      entryFile:       String,
      includeTestOnly: Boolean         = false,
      preludePath:     Option[String]  = None,
  ): Either[Int, TProgram] =
    val projectRoot   = pathDirname(entryFile)
    val preludeModule = NexSysroot.findPreludeRoot(preludePath) match
      case Some(dir) => NexModuleLoader.loadPreludeAsModule(dir) match
        case Right(m) => Some(m)
        case Left(errs) =>
          Console.err.println("nex: prelude loading errors:")
          errs.foreach(e => Console.err.println(s"  $e"))
          return Left(1)
      case None      => None
    new NexModuleLoader(projectRoot).loadFrom(entryFile) match
      case Left(errs) =>
        Console.err.println("nex: module loading errors:")
        errs.foreach(e => Console.err.println(s"  $e"))
        Left(1)
      case Right(allModules) =>
        val userModules =
          if includeTestOnly then allModules
          else allModules.filterNot(_.isTestOnly)
        val modules = preludeModule.toList ++ userModules
        new NexElaborator().elaborateProject(modules) match
          case Right(tp) => Right(tp)
          case Left(errs) =>
            Console.err.println("nex: elaboration errors:")
            errs.foreach(e => Console.err.println(s"  ${e.toString}"))
            Left(1)

  /** Hard-coded for now; in v1 we'll wire it to build.sbt's `version`. */
  private def buildVersion: String = "0.0.1"
