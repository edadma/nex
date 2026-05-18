# Nex

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/nex_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/nex)](https://github.com/edadma/nex/commits)
![GitHub](https://img.shields.io/github/license/edadma/nex)
![Scala Version](https://img.shields.io/badge/Scala-3.8.3-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.20.2-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.10-blue.svg)

A modern, AOT-compiled, array-first numerical programming language.

Fortran's heritage — built-in complex numbers and matrices, IEEE 754 throughout, AOT performance.
Modern syntax — Scala 3-style indentation, expression-oriented, math-flavored juxtaposition (`2pi`, `4a*c`, `3sin(2pi*t)`).
Fusion-aware compilation — array expressions compile to fused loops with no intermediate temporaries.

```nex
def normalize(v: [real]) =
  val mag = sqrt(sum(v * v))
  if mag == 0.0 then v
  else v / mag
```

## Status

The current implementation runs the language end-to-end: a tree-walking interpreter plus an AOT compiler that produces native Mac arm64 binaries (LLVM IR → `clang -O1`) for the full surface — scalar arithmetic, arrays + ARC + slicing + fusion, tuples and structs, lambdas + closures (val + var capture), the prelude scalar math and assertion family, higher-order array functions (`map` / `reduce` / `filter`), and complex numbers. Every commit verifies the AOT output byte-for-byte against the reference tree-walking interpreter (around 700 unit tests).

```bash
sbt "nexJVM/runMain io.github.edadma.nex.run run     examples/fft/main.nex"
sbt "nexJVM/runMain io.github.edadma.nex.run compile examples/fft/main.nex"
./examples/fft/main
```

## See [nexlang.org](https://nexlang.org) for the full documentation.

- [Language specification](https://nexlang.org/spec/)
- [Examples](https://nexlang.org/examples/)
- [Tooling](https://nexlang.org/tooling/)

## License

ISC — see [LICENSE](LICENSE).
