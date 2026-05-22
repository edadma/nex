---
title: Higher-Order Functions and Closures
summary: A closure captures `mean`, then `.map` fuses into the `.sum` reduction.
weight: 30
---

For a sample $x_0, \ldots, x_{n-1}$, the mean and the population standard deviation are

$$
\bar{x} \;=\; \frac{1}{n}\sum_{i=0}^{n-1} x_i, \qquad
\sigma   \;=\; \sqrt{\,\frac{1}{n}\sum_{i=0}^{n-1} (x_i - \bar{x})^2\,}.
$$

The Nex realization reads as one expression per formula. The lambda inside `.map` captures `mean` from the enclosing scope — that's the closure — and the intermediate array `.map` would otherwise produce fuses into the `.sum` reduction, so the whole variance computation runs in a single pass over `xs`.

```nex
def main() =
  val xs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]

  val n = to_real(length(xs))
  val mean = sum(xs) / n

  // The explicit `: real` annotation pins the binding's type — the
  // elaborator does not yet infer return types through `.map(lambda).sum()`
  // chains, so the downstream `sqrt(variance)` overload-resolution needs
  // the hint.
  val variance: real = xs.map(x -> (x - mean)^2).sum() / n
  val stddev = sqrt(variance)

  print(s"mean = $mean, stddev = $stddev")
```
