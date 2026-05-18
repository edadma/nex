package io.github.edadma.nex

@main def run(args: String*): Unit =
  val exitCode = Cli.run(args)
  if exitCode != 0 then sys.exit(exitCode)
