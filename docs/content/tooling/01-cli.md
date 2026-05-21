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
- `mlir` — emits MLIR via `linalg` / `arith` / `scf` / `tensor` / `func` dialects and runs `mlir-opt` → `mlir-translate` → `clang -O1`. **Experimental** and growing on the strangler-fig pattern: scalar arithmetic (`+ - * / div % ^`, unary `-`), comparisons (scalar and array-element-wise, returning `[bool]`) + bool + `and` / `or` / `not`, `min` / `max` / `abs`, the libm transcendentals (`sqrt` / `exp` / `log` / `sin` / `cos` / ...), the explicit conversions `to_real` / `to_integer`, array literals + `length` + indexing, array-array element-wise binops (matching element types, or mixed integer / real promoting up the lattice), scalar–array broadcasts (arithmetic *and* comparison), and the `@` operator (matmul, matvec, dot) all parity-check against the interpreter. Control flow lowers via the `scf` dialect — scalar `if`-expressions, statement-position `if` / `if`/`else` (lowered to a no-result `scf.if` with optional else region), `while cond do ...` loops, and `for` loops in both forms (`for i in lo..hi do ...` over an integer range, `for x in xs do ...` over a rank-1 array) are wired up. Mutable scalar `var` bindings lower to `memref.alloca` slots so a counter-style `var n = 0; while ... do n = n + 1` works end-to-end. Mutable *tensor* `var` bindings rebind through fresh SSA via `tensor.insert` / `tensor.insert_slice` / `scf.for` accumulators, covering whole-tensor reassign, scalar index-assign at rank-1 and rank-2 (negative-index wrap + OOB trap), rank-1 slice-assign (open bounds, inclusive, stride > 1), and rank-2 row/column replace (`m[i, :] = rhs` / `m[:, j] = rhs`); auto-clone insertions (spec §8.3) become emit-inner identities because SSA tensor semantics already diverge the bindings on the first mutation. Rank-1 and rank-2 slicing (`xs[2..5]`, `m[:, 1]`, `m[0..2, 1..3]`, etc.) routes through `tensor.extract_slice` for both literal *and* runtime bounds, including the strided forms (`xs[0..n by 2]`, `m[..r by k, c]`) — the rank-reducing form drops a dimension whenever an axis collapses to a scalar index. Rank-1 `filter(arr, pred)` and `flatMap(arr, x -> [...])` lower to two-pass `tensor.empty` + `tensor.insert` builders so the output shape is sized exactly. Index-style traps match the interpreter: negative-index wrap on `TIndex`, plus out-of-bounds traps on both indexing and slice bounds. User-defined `def` functions lower to `func.func @nex_user_<name>_<id>` at module level — scalar (i64 / f64 / i1) and string parameters and return types are supported, plus tensor params / tensor returns (including tensor-returning `if`-expressions) and unit-returning defs (`def scale(v: mut [real], k: real) = ...`); recursion, mutual recursion, forward references, and prelude consts (`pi` / `e`) referenced from inside a user def all flow naturally because module-level ops in MLIR are order-independent. Value-returning defs with early `return` inside a loop or branch are desugared to an equivalent `if`-expression chain before codegen, so `return` doesn't need a dedicated MLIR shape. String values are represented as opaque i64 descriptor pointers — literals share a module-level `memref.global` pool, the runtime ships refcounted `concat` / `eq` / `from_i64` / `from_bool` / `from_double` helpers, and refcount discipline (–1 immortal for literals, +1 heap for concat / value-to-string results, dec on every consume site) mirrors LLVM exactly; `print("...")`, string `==` / `!=` / `+`, `s"..."` interpolation with `integer` / `bool` / `real`, and `f"..."` format-spec values (`%d` / `%f` / `%e` / `%g` / `%x` / `%X` / `%o` / `%b` with width / `0` / `-` flags and `.precision`) all parity-check against the interpreter, and the real-value branch uses a shortest-round-trip helper that matches Java's `Double.toString` byte-for-byte. The array builders `range` / `zeros` / `ones` / `linspace` plus `transpose` round out the rank-1 / rank-2 surface. The corpus opts cases in one at a time via a per-case `mlir = true` flag in `NexProgramCorpus` — see `NexCorpusMlirParityTests` for what's currently covered. User-defined generic functions (Spec §6.10) are not yet lowered — only their concrete specializations would be visible to MLIR, and the user-`TCall` dispatch hasn't been generalized to the specialization registry yet. Anything outside the opted-in surface raises a compile-time "not yet supported" diagnostic.

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
