---
title: FFT
summary: Cooley-Tukey radix-2 recursive FFT for any power-of-2 N — complex numbers, strided slicing for the even/odd split, element-wise array arithmetic, and slice assignment to stitch the two output halves.
weight: 120
---

A recursive discrete Fourier transform that showcases Nex's complex number support, per-element array-literal coercion, strided slices for the even/odd split, and open-bound slice assignment for stitching the two output halves. Works for any power-of-2 length; the recursion bottoms out at the trivial one-point transform.

The DFT of length $N$ is

$$
X_k = \sum_{n=0}^{N-1} x_n \, e^{-2\pi i\, k n / N}, \qquad k = 0, 1, \ldots, N-1.
$$

A direct evaluation is $\Theta(N^2)$. Cooley-Tukey splits the sum by even / odd index — $E_k$ is the DFT of the even-indexed samples, $O_k$ the DFT of the odd-indexed ones — and reuses each half twice:

$$
X_k         = E_k + W_N^{\,k}\, O_k, \qquad
X_{k+N/2}   = E_k - W_N^{\,k}\, O_k,
$$

for $k \in [0, N/2)$, where $W_N = e^{-2\pi i / N}$ is the principal $N$-th root of unity. Each recursive level halves the problem, giving $\Theta(N \log N)$ overall.

```nex
def fft(x: [complex]): [complex] =
  val n = length(x)
  if n == 1 then return x

  // Split into even/odd-indexed sub-arrays via strided slices.
  val ef = fft(x[..n by 2])     // even-indexed: 0, 2, 4, ...
  val of = fft(x[1..n by 2])    // odd-indexed:  1, 3, 5, ...

  val half = n div 2

  // Build the twiddled odd half as a vector first: t[k] = W_N^k · O[k].
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

Output (the DC term $X_0$ is the sum of inputs; real input gives the Hermitian symmetry $X_k = \overline{X_{N-k}}$ for $k > 0$):

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
