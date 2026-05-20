---
title: FFT
summary: Cooley-Tukey radix-2 recursive FFT for any power-of-2 N — complex numbers, the `fill` constructor, per-element array-literal coercion, and slice assignment to stitch the two output halves.
weight: 120
---

A recursive discrete Fourier transform that showcases Nex's complex number support, the prelude `fill(n, v)` array constructor, per-element array-literal coercion, and slice assignment to write the two output halves in one statement each. Works for any power-of-2 length; the recursion bottoms out at the trivial one-point transform.

```nex
def fft(x: [complex]): [complex] =
  val n = length(x)
  if n == 1 then return x

  val half = n div 2

  // Split into even/odd-indexed sub-arrays. `fill(half, 0.0 + 0i)`
  // gives us a writable rank-1 complex buffer of the right size.
  // (Strided slice forms like `x[0..n by 2]` are deferred, so we
  // walk the indices by hand here.)
  var even = fill(half, 0.0 + 0i)
  var odd  = fill(half, 0.0 + 0i)
  for k in 0..half do
    even[k] = x[2 * k]
    odd[k]  = x[2 * k + 1]

  val ef = fft(even)
  val of = fft(odd)

  // Cooley-Tukey butterfly: Y[k]     = E[k] + W^k · O[k]
  //                        Y[k+N/2] = E[k] - W^k · O[k]   for k ∈ [0, N/2)
  //
  // Compute the two halves of the output as separate vectors. The
  // butterfly stays in scalar-complex form (one twiddle per element).
  var top    = fill(half, 0.0 + 0i)
  var bottom = fill(half, 0.0 + 0i)
  for k in 0..half do
    val angle = -2.0 * pi * to_real(k) / to_real(n)
    val w     = cos(angle) + sin(angle) * i
    val t     = w * of[k]
    top[k]    = ef[k] + t
    bottom[k] = ef[k] - t

  // Stitch the halves into `y` with two slice assignments — no
  // per-index `y[k] = ...; y[k + half] = ...` shuffle.
  var y = fill(n, 0.0 + 0i)
  y[0..half] = top
  y[half..n] = bottom
  y

def main() =
  // The `: [complex]` annotation pushes the element type into each
  // literal, so the real values 0.0 / 1.0 coerce per-element via
  // `to_complex(...)`.
  val x: [complex] = [
    1.0, 1.0, 1.0, 1.0,
    0.0, 0.0, 0.0, 0.0
  ]
  val y = fft(x)
  val n = length(y)
  print("FFT of [1, 1, 1, 1, 0, 0, 0, 0]:")
  for k in 0..n do
    print(y[k])
```

Output (DC term `Y[0]` is the sum of inputs; real input gives `Y[k] = conj(Y[N-k])` for k > 0):

```
FFT of [1, 1, 1, 1, 0, 0, 0, 0]:
4.0+0.0i
1.0-2.414213562373095i
0.0+0.0i
1.0-0.4142135623730949i
0.0+0.0i
0.9999999999999999+0.4142135623730949i
0.0+0.0i
0.9999999999999997+2.414213562373095i
```

The full source lives at `examples/fft/main.nex`. Change `x` to any power-of-2 length and the same code transforms it.
