package io.github.edadma.nex

import io.github.edadma.cross_platform

@main def run(args: String*): Unit =
  val exitCode = Cli.run(args)
  if exitCode != 0 then cross_platform.processExit(exitCode)
