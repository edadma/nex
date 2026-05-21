---
title: Numerical Methods
summary: Newton's method for square roots, then one Runge-Kutta 4 step for a vector ODE.
weight: 60
---

## Newton's method for square roots

Newton's method for $f(x) = 0$ refines a guess by

$$
x_{n+1} \;=\; x_n - \frac{f(x_n)}{f'(x_n)}.
$$

Applied to $f(x) = x_n^2 - x$ (whose root in $x_n$ is $\sqrt{x}$), the recurrence collapses to the classical Heron / Babylonian average

$$
x_{n+1} \;=\; \tfrac{1}{2}\!\left(x_n + \tfrac{x}{x_n}\right),
$$

which converges quadratically — each step roughly doubles the number of correct digits.

```nex
def newton_sqrt(x: real, tol: real) =
  var guess = x / 2.0
  var delta = guess
  while abs(delta) > tol do
    val next = (guess + x / guess) / 2.0
    delta = next - guess
    guess = next
  end while
  guess

def main() =
  print(newton_sqrt(2.0,    1.0e-12))    // 1.41421356...
  print(newton_sqrt(1000.0, 1.0e-9))     // 31.6227766...
```

## RK4 — one step of a vector ODE solver

For the vector ODE $\dot y = f(t, y)$, the classical Runge-Kutta-4 step from $(t_n, y_n)$ to $(t_{n+1}, y_{n+1}) = (t_n + h, \, y_{n+1})$ samples the slope at four points,

$$
\begin{aligned}
k_1 &= f(t_n,           \; y_n)              \\
k_2 &= f(t_n + h/2,     \; y_n + (h/2)\, k_1) \\
k_3 &= f(t_n + h/2,     \; y_n + (h/2)\, k_2) \\
k_4 &= f(t_n + h,       \; y_n + h\, k_3),
\end{aligned}
$$

and combines them with Simpson-like weights,

$$
y_{n+1} \;=\; y_n + \tfrac{h}{6}\,(k_1 + 2k_2 + 2k_3 + k_4).
$$

The method is fourth-order accurate: the local truncation error is $O(h^5)$ and the global error over a fixed interval is $O(h^4)$. The example below applies it to the simple harmonic oscillator $\ddot p = -p$, written as the first-order system $\dot p = v$, $\dot v = -p$.

```nex
// One Runge-Kutta 4 step for dy/dt = f(t, y), where y is a vector.
def rk4_step(f: (real, [real]) -> [real], t: real, y: [real], h: real) =
  val k1 = f(t,       y)
  val k2 = f(t + h/2, y + h/2 * k1)
  val k3 = f(t + h/2, y + h/2 * k2)
  val k4 = f(t + h,   y + h   * k3)

  // The whole expression fuses to a single loop:
  y + h/6 * (k1 + 2k2 + 2k3 + k4)

// Simple harmonic oscillator: dy/dt = (velocity, -position)
def harmonic(t: real, y: [real]) =
  [y[1], -y[0]]

def main() =
  var y = [1.0, 0.0]              // start: position 1, velocity 0
  var t = 0.0
  val h = 0.01
  val steps = 1000

  for k in 0..steps do
    y = rk4_step(harmonic, t, y, h)
    t = t + h
  end for

  print(s"after $steps steps: position=${y[0]}, velocity=${y[1]}")
```
