---
title: CLI
summary: The `nex` subcommands — tokens, parse, elaborate, run, test, compile — and what each one does.
weight: 10
---

## Subcommands

```bash
nex tokens    <file>                # lex; print the token stream
nex parse     <file>                # lex + parse; print the AST
nex elaborate <file>                # parse + name resolution; print the typed AST
nex run       <file>                # parse, elaborate, execute via the tree-walking interpreter
nex test      <file>                # discover and run every @test function reachable from <file>'s project root
nex compile [--backend B] <file>    # emit IR and invoke a backend toolchain to produce a native binary
```

`<file>` is always a single `.nex` source. For `test` and `compile`, the directory containing `<file>` is the project root — the import graph is discovered from there.

`--backend` (only for `compile`) selects the codegen pipeline:

- `llvm` (default) — emits LLVM IR text and invokes `clang -O1`. Covers the full language surface.
- `mlir` — emits MLIR via `linalg` / `arith` / `scf` / `tensor` dialects and runs `mlir-opt` → `mlir-translate` → `clang -O1`. **Experimental** and growing on the strangler-fig pattern: scalar arithmetic (`+ - * / div % ^`, unary `-`), comparisons + bool + `and` / `or` / `not`, `min` / `max` / `abs`, the libm transcendentals (`sqrt` / `exp` / `log` / `sin` / `cos` / ...), array literals + `length` + indexing, scalar–array broadcasts, and the `@` operator (matmul, matvec, dot) all parity-check against the interpreter. The corpus opts cases in one at a time via a per-case `mlir = true` flag in `NexProgramCorpus` — see `NexCorpusMlirParityTests` for what's currently covered. Anything outside the opted-in surface raises a compile-time "not yet supported" diagnostic.

## Locating the source prelude

The scalar transcendentals plus their complex extensions live in a `prelude/*.nex` directory and are auto-imported on every elaboration (see [§10 of the spec](/spec/10-prelude/)). The CLI locates that directory at startup; the precedence order is:

1. `--prelude-path <dir>` — an explicit CLI flag (highest priority).
2. `NEX_HOME` environment variable — looks at `$NEX_HOME/lib/prelude/`.
3. `-Dnex.prelude.path=<dir>` JVM system property. `build.sbt` sets this automatically so `sbt run` and `sbt test` use the in-repo `prelude/` with no extra wiring.
4. Walk up from the current working directory — at each ancestor, check for a `prelude/` sibling and then `lib/prelude/`.

If none of the four locate a directory, the compiler falls back to the compiler-built-in name table and proceeds silently. This was the design during the Stage 1/2 migration and remains the fallback today for environments without a sysroot.

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
