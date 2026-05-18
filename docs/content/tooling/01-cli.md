---
title: CLI
summary: The `nex` subcommands — tokens, parse, elaborate, run, test, compile — and what each one does.
weight: 10
---

## Subcommands

```bash
nex tokens    <file>      # lex; print the token stream
nex parse     <file>      # lex + parse; print the AST
nex elaborate <file>      # parse + name resolution; print the typed AST
nex run       <file>      # parse, elaborate, execute via the tree-walking interpreter
nex test      <file>      # discover and run every @test function reachable from <file>'s project root
nex compile   <file>      # emit LLVM IR and invoke clang -O1 to produce a native binary
```

`<file>` is always a single `.nex` source. For `test` and `compile`, the directory containing `<file>` is the project root — the import graph is discovered from there.

## Running the CLI from sbt

There is no shipped binary yet; the canonical entry point is sbt's `runMain`:

```bash
sbt "nexJVM/runMain io.github.edadma.nex.run <subcommand> <file>"
```

Examples:

```bash
sbt "nexJVM/runMain io.github.edadma.nex.run run     examples/fft/main.nex"
sbt "nexJVM/runMain io.github.edadma.nex.run compile examples/fft/main.nex"
./examples/fft/main
```

`run` is also a published `@main` entry point on the JVM JAR — once packaged, `java -jar nex.jar <subcommand> <file>` does the same thing.

## Requirements

- **JVM 17+** to invoke the CLI.
- **`clang` on `$PATH`** for the `compile` subcommand. The compiler writes LLVM IR text to a temp file and shells out to `clang -O1` to produce a native binary.
- No other native dependencies.

## What each subcommand prints

- `tokens` — one token per line, with source position.
- `parse` — the untyped AST as a pretty-printed tree.
- `elaborate` — the elaborated (typed, name-resolved, lowered) AST. Useful for debugging type inference or fusion-pass output.
- `run` — the program's `stdout` (whatever it `print`s); exit status reflects whether a trap occurred.
- `test` — one line per discovered test, with a pass/fail marker; a summary at the bottom; non-zero exit on any failure.
- `compile` — diagnostics during code generation, then either a native binary at `<file-without-extension>` or a non-zero exit on error.
