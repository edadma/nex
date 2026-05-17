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

The whole expression fuses to a single loop with no intermediate arrays. That's the design promise.

## What's here

The sections above walk through the language. Start with [Spec](/spec/v0/) for the v0 reference, or jump to [Examples](/spec/examples/) for a tour and a set of complete programs.

## Status

v0 design is settled. Implementation in progress. Built with Scala 3, cross-compiled to JVM / JavaScript / Native via Scala.js and Scala Native.
