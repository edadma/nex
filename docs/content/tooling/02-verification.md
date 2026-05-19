---
title: Verification Model
summary: How the interpreter and the AOT compiler are kept in lock-step — two paths, one expected output.
weight: 20
---

The Nex implementation runs two execution paths in parallel and treats their disagreement as a bug:

1. **Tree-walking interpreter.** The reference semantics. Used by `nex run` and `nex test`. Easy to read, easy to step through, easy to extend when adding a new language feature.
2. **AOT compiler.** LLVM IR text → `clang -O1` → native binary. Used by `nex compile`. The path users ship.

## Three layers of testing

- **Unit tests on every subsystem.** Lexer, parser, three elaborator stages, interpreter, codegen, prelude. Over a thousand tests, all of which pass on all three CLI targets (JVM, Scala.js, Scala Native); every commit runs the full suite.
- **IR pattern checks.** Codegen tests assert against expected LLVM IR fragments — both that the right instructions are emitted and that the fusion pass actually produced a single loop where it claimed to. This catches regressions where the IR drifts shape but still happens to execute correctly.
- **End-to-end interpreter/AOT parity.** A program is run twice — once through the interpreter, once through the AOT binary — and the two stdouts are compared byte-for-byte. The AOT path emits reals via an iterative shortest-round-trip helper (`%.<p>g` + `strtod` for p = 1..17) plus a Java-`Double.toString`-style post-pass, so non-whole reals like `0.1 + 0.2` print as `0.30000000000000004` on both sides instead of `%g`'s 6-significant-figure approximation.

## Why this matters

Numerical code is unforgiving — silent codegen bugs corrupt computation in ways that look exactly like correct output until they don't. Running a reference interpreter alongside a compiler and demanding byte-exact agreement turns a class of "did the optimizer break it?" bugs into immediate test failures.

The model came from sysl, where it has been load-bearing across seven backends. Nex inherits the same discipline.
