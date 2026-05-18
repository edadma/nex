---
title: Sum of Sinusoids
summary: A three-term wave function and a partial Fourier sum, sampled over [0, 1].
weight: 90
---

```nex
// Three-term wave: w(t) = 3 sin(2π t) + 2 cos(4π t) - sin(6π t)
def wave(t: real) =
  3sin(2pi*t) + 2cos(4pi*t) - sin(6pi*t)

// Partial Fourier sum: s(t) = Σ_{k=1..n} coeffs[k-1] · sin(2π k t)
def fourier_sample(coeffs: [real], t: real) =
  val n = length(coeffs)
  var result = 0.0
  for k in 0..n do
    result = result + coeffs[k] * sin(2pi * to_real(k + 1) * t)
  end for
  result

def main() =
  val ts = linspace(0.0, 1.0, 100)
  val ys = ts.map(wave)

  print(s"wave(0.25) = ${wave(0.25)}")
  print(s"max sample = ${max(ys)}")
  print(s"min sample = ${min(ys)}")

  val coeffs = [1.0, 0.5, 0.25, 0.125]
  print(s"fourier(0.1) = ${fourier_sample(coeffs, 0.1)}")
```
