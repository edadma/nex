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

The intended way to use Nex during development is to write code, run it under the interpreter for the fast edit/run cycle, then occasionally compile to native to confirm the AOT path produces the same output. Every commit to the compiler runs the full unit-test suite (over two thousand tests on JVM today) plus an interpreter-vs-AOT parity sweep that compares the two paths byte-for-byte on a shared corpus. The compiled binary's real-number printing matches Java's `Double.toString` — non-whole reals like `0.1 + 0.2` print as `0.30000000000000004`, magnitudes like `1e20` print as `1.0E20`, and runtime traps (assertions, array-index OOB, division by zero, slice OOB) all carry the same human-readable messages as the interpreter, written to stderr as `trap: <msg>\n` with exit code 1, so `assert_traps(fn, substring)` matches the same trap text on both sides and a shell pipeline that captures stderr sees identical bytes from either path.

## When you'd reach for which

- **Use `run`** while writing code. Sub-second turnaround. Same output as the binary for everything you care about.
- **Use `compile`** when shipping, benchmarking, or producing a self-contained executable to share. The native binary is roughly an order of magnitude faster on most numerical workloads (no interpreter dispatch overhead, real LLVM optimization at `-O1`).
- **Use `test`** to discover and run every `@test`-annotated function in a project — see the [Tooling chapter](/tooling/01-cli/) for details and the [Test modules example](/examples/11-test-modules/) for the convention.

## Next step

You're done with the getting-started path. From here:

- [Examples](/examples/) — a feature tour plus a dozen complete programs from hello-world through a recursive FFT.
- [Specification](/spec/) — the chapter-by-chapter language reference.
- [Tooling](/tooling/) — CLI details, verification model, supported targets.
