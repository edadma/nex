---
title: Supported Targets
summary: Where the AOT path is verified, where the CLI builds, and what's planned.
weight: 30
---

## AOT path (native binaries via `nex compile`)

- **Mac arm64** — verified end-to-end on every commit. The `clang -O1` invocation produces a Mach-O binary; CI runs it and diffs the stdout against the interpreter.

Other Unix targets (Linux x86_64, Linux arm64) should work given a working `clang -O1`, but they are not yet exercised by CI.

## CLI builds

The Nex compiler is cross-compiled to three targets via Scala 3, and the full unit-test suite (over two thousand tests on JVM today) passes identically on all three on every commit. (Backend-shelling tests — those that drive `clang` or the MLIR toolchain — only run on JVM; the cross-platform tests cover the parser, elaborator, and interpreter.) Filesystem operations route through the [`path`](https://github.com/edadma/path) library so the module loader runs the same code on JVM filesystems, Node's `fs`, and Native via `java.nio.file`.

- **JVM** — primary development target. The full CLI (`tokens`, `parse`, `elaborate`, `run`, `test`, `compile`) lives here; the JVM is the only target that exercises the AOT path because it shells out to `clang`.
- **Scala.js (Node)** — the interpreter and module loader work the same as JVM. `nex run` / `nex test` are functional; no `compile` subcommand because there's no `clang` to shell out to.
- **Scala Native** — same story as Scala.js. `nex run` / `nex test` work; no AOT path.

## Planned

- GPU / accelerator targets — *deferred to v1+*.
- Linux x86_64 / arm64 — likely to be folded into CI once a runner is available; no language work required.
- WebAssembly — possible via a future LLVM IR → wasm backend; no concrete plan yet.
