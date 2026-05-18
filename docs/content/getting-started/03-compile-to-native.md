---
title: Compile to a Native Binary
summary: Use the AOT compiler to produce a standalone executable; verify it matches the interpreter.
weight: 30
---

## Compile

The `compile` subcommand emits LLVM IR text and shells out to `clang -O1` to produce a native binary:

```bash
sbt "nexJVM/runMain io.github.edadma.nex.run compile /path/to/hello.nex"
```

The resulting binary lands next to the source, with the `.nex` extension stripped:

```
/path/to/hello
```

Run it directly — no JVM, no sbt, no dependencies beyond the system C runtime:

```bash
/path/to/hello
```

Output is identical to what the interpreter produced.

## Verify parity

The intended way to use Nex during development is to write code, run it under the interpreter for the fast edit/run cycle, then occasionally compile to native to confirm the AOT path produces the same output. Every commit to the compiler runs around 700 unit tests that compare the two paths line-by-line on a representative corpus. The one documented divergence is real-number formatting of irrationals: the interpreter uses Java's shortest-round-trip `Double.toString`, the native binary uses libc's `%g` (6 significant figures). Whole reals match exactly; `sqrt(2)` differs in trailing digits.

## When you'd reach for which

- **Use `run`** while writing code. Sub-second turnaround. Same output as the binary for everything you care about.
- **Use `compile`** when shipping, benchmarking, or producing a self-contained executable to share. The native binary is roughly an order of magnitude faster on most numerical workloads (no interpreter dispatch overhead, real LLVM optimization at `-O1`).
- **Use `test`** to discover and run every `@test`-annotated function in a project — see the [Tooling chapter](/tooling/01-cli/) for details and the [Test modules example](/examples/11-test-modules/) for the convention.

## Next step

You're done with the getting-started path. From here:

- [Examples](/examples/) — sixteen complete programs from hello-world through a recursive FFT.
- [Specification](/spec/) — the chapter-by-chapter language reference.
- [Tooling](/tooling/) — CLI details, verification model, supported targets.
