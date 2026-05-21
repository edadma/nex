---
title: FFT
summary: Cooley-Tukey radix-2 recursive FFT for any power-of-2 N — complex numbers, strided slicing for the even/odd split, element-wise array arithmetic, and slice assignment to stitch the two output halves.
weight: 120
---

A recursive discrete Fourier transform that showcases Nex's complex number support, per-element array-literal coercion, strided slices for the even/odd split, and open-bound slice assignment for stitching the two output halves. Works for any power-of-2 length; the recursion bottoms out at the trivial one-point transform.

```nex
def fft(x: [complex]): [complex] =
  val n = length(x)
  if n == 1 then return x

  // Split into even/odd-indexed sub-arrays via strided slices.
  val ef = fft(x[..n by 2])     // even-indexed: 0, 2, 4, ...
  val of = fft(x[1..n by 2])    // odd-indexed:  1, 3, 5, ...

  val half = n div 2

  // Cooley-Tukey butterfly: Y[k]     = E[k] + W^k · O[k]
  //                        Y[k+N/2] = E[k] - W^k · O[k]   for k ∈ [0, N/2)
  //
  // Build the twiddled odd half as a vector first — t[k] = W^k · O[k].
  var t = fill(half, 0.0 + 0i)
  for k in 0..half do
    val angle = -2.0 * pi * to_real(k) / to_real(n)
    t[k] = (cos(angle) + sin(angle) * i) * of[k]

  // Stitch the two output halves into `y` with element-wise array ops
  // (`ef + t`, `ef - t` on `[complex]`) plus a slice assignment per
  // half — no per-index `y[k] = ...; y[k + half] = ...` shuffle.
  var y = fill(n, 0.0 + 0i)
  y[..half] = ef + t                    // open lo: same as 0..half
  y[half..] = ef - t                    // open hi: same as half..n
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
