---
title: Sum of Sinusoids
summary: A three-term wave function and a partial Fourier sum, sampled over [0, 1].
weight: 90
---

A pure sinusoid has the form $y(t) = A \sin(2\pi f t + \varphi)$ — amplitude $A$, frequency $f$ (cycles per unit of $t$), and phase $\varphi$. Adding several sinusoids with integer-multiple frequencies builds a periodic *waveform*; the wave below sums three such components on $[0, 1]$:

$$
w(t) \;=\; 3\sin(2\pi t) \;+\; 2\cos(4\pi t) \;-\; \sin(6\pi t).
$$

The second example evaluates a partial Fourier sine series at a single $t$,

$$
s(t) \;=\; \sum_{k=1}^{n} c_k \sin(2\pi k t),
$$

where `coeffs[k-1]` holds $c_k$. Both functions exploit juxtaposition multiplication so the source reads like the math: `3sin(2pi*t)`, `2cos(4pi*t)`.

```nex
// Three-term wave: w(t) = 3 sin(2π t) + 2 cos(4π t) - sin(6π t)
def wave(t: real) =
  3sin(2pi*t) + 2cos(4pi*t) - sin(6pi*t)

// Partial Fourier sum: s(t) = Σ_{k=1..n} coeffs[k-1] · sin(2π k t)
def fourier_sample(coeffs: [real], t: real) =
  val n = length(coeffs)
  var result = 0.0
  for k in 0..n do
    result += coeffs[k] * sin(2pi * to_real(k + 1) * t)
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
