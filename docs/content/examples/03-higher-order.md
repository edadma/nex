---
title: Higher-Order Functions and Closures
summary: A closure captures `mean`, then `.map` fuses into the `.sum` reduction.
weight: 30
---

```nex
def main() =
  val xs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

  val n = to_real(length(xs))
  val mean = sum(xs) / n

  // Lambda captures `mean` from the enclosing scope.
  // The intermediate from .map fuses into the .sum reduction.
  val variance = xs.map(x -> (x - mean)^2).sum() / n
  val stddev = sqrt(variance)

  print(s"mean = $mean, stddev = $stddev")
```
