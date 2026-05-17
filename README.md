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

## See [nexlang.org](https://nexlang.org) for the full documentation.

- [Language specification](https://nexlang.org/spec/v0/)
- [Examples and tour](https://nexlang.org/spec/examples/)

## License

ISC — see [LICENSE](LICENSE).
