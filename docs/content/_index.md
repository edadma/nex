---
title: Nex
splash: true
heroTitle: A modern numerical language with
heroHighlight: array-first semantics
summary: AOT-compiled. Fortran's heritage, modern syntax. Built-in complex numbers and matrices. Fusion-aware compilation. Designed for the kind of code that Fortran is still used for today.
---

## Why Nex?

Nex is shaped after modern Fortran: a numerical-computing-first language with array operations as the default, IEEE 754 throughout, AOT compilation, and predictable performance. It updates the surface syntax to something modern (Scala 3-style indentation, expression-oriented control flow, math-flavored juxtaposition like `2pi` and `4a*c`) without giving up Fortran's strengths.

```nex
def normalize(v: [real]) =
  val mag = sqrt(sum(v * v))
  if mag == 0.0 then v
  else v / mag
```

That lowers to a single pass over `v` — no intermediate arrays:

```nex
def normalize(v: [real]) =
  val n = length(v)
  var sum_sq = 0.0
  for i in 0..n do
    sum_sq = sum_sq + v[i]^2
  end for
  val mag = sqrt(sum_sq)
  if mag == 0.0 then v
  else
    var out = fill(n, 0.0)
    for i in 0..n do
      out[i] = v[i] / mag
    end for
    out
```

The hand-written loop form and the fused-source form produce the same machine code. That's the design promise.

## What's here

The cards above are the two reference sections. The **Specification** is the chapter-by-chapter language reference. The **Examples** section is a feature tour plus a set of complete programs. **Getting Started** (the hero button) walks you from a fresh checkout to a running native binary and points at the CLI reference along the way.

## Status

The current implementation covers the language end-to-end: a tree-walking interpreter for `nex run` and `nex test`, plus an AOT compiler (LLVM IR → `clang -O1`) producing native Mac arm64 binaries for the full surface — scalar arithmetic, arrays (rank-1 and rank-2) with ARC, slicing and slice assignment, element-wise + broadcast + clone + fused loops, tuples and structs, lambdas and closures (val + var capture, escape-safe heap boxing), prelude scalar math + assertions, higher-order array functions (`map` / `flatMap` / `reduce` / `filter`), and complex numbers. The standard prelude ships as Nex source (`prelude/*.nex`) — every libm transcendental and its complex extension lives there, auto-imported. Compiled output is byte-for-byte verified against the reference interpreter on every commit (over two thousand unit tests on JVM today, plus a mutation-fuzzed parity sweep). An experimental MLIR codegen path is available behind `nex compile --backend mlir`; coverage is growing strangler-fig style and the corpus tracks which programs already parity-match the interpreter on that backend.

```bash
# Run a program with the tree-walking interpreter:
nex run examples/fft/main.nex
# Compile to a native binary (Mac arm64 verified end-to-end):
nex compile examples/fft/main.nex
./examples/fft/main
```

Built with Scala 3, cross-compiled to JVM / JavaScript / Native via Scala.js and Scala Native. The AOT compile path requires `clang` on `$PATH`.
