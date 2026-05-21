---
title: Complex Numbers and the Quadratic Formula
summary: Complex arithmetic, then a quadratic solver returning a pair of complex roots.
weight: 40
---

## Complex numbers

A complex number $z = a + bi$ has modulus $\lvert z \rvert = \sqrt{a^2 + b^2}$ and argument $\arg(z) = \operatorname{atan2}(b, a)$, related by the polar form $z = \lvert z \rvert \, e^{i\arg(z)}$ — i.e. Euler's formula $e^{i\theta} = \cos\theta + i\sin\theta$. The prelude exposes both the field-style accessors `.re` / `.im` and the polar-form helpers `abs(z)` / `arg(z)`.

```nex
def main() =
  val z1 = 1.0 + 2i             // (1.0 + 2.0i)
  val z2 = 3.0 - 1i             // (3.0 - 1.0i)

  print(s"z1 + z2 = ${z1 + z2}")
  print(s"z1 * z2 = ${z1 * z2}")
  print(s"|z1| = ${z1.abs()}, arg(z1) = ${z1.arg()}")
  print(s"z1.re = ${z1.re}, z1.im = ${z1.im}")
```

## Quadratic formula

The roots of $a x^2 + b x + c = 0$ are

$$
x \;=\; \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}.
$$

The sign of the discriminant $\Delta = b^2 - 4ac$ decides whether the roots are real ($\Delta \ge 0$) or a complex-conjugate pair ($\Delta < 0$). The function below returns both roots as `complex` regardless, so callers see a single result type.

```nex
// Solve ax² + bx + c = 0; returns a pair of complex roots.
// Each indented if-branch is a block; the block's last expression is
// a paren-less tuple — no wrapping parens are needed here.
def solve_quadratic(a: real, b: real, c: real) =
  val disc = b^2 - 4a*c
  if disc >= 0.0 then
    val sd = sqrt(disc)
    (-b + sd) / (2a) + 0i, (-b - sd) / (2a) + 0i
  else
    val real_part = -b / (2a)
    val imag_part = sqrt(-disc) / (2a)
    real_part + imag_part*i, real_part - imag_part*i
  end if

def main() =
  val r1, r2 = solve_quadratic(1.0, -3.0, 2.0)      // x² - 3x + 2 = 0
  print(s"real roots: $r1, $r2")

  val c1, c2 = solve_quadratic(1.0, 0.0, 1.0)       // x² + 1 = 0
  print(s"complex roots: $c1, $c2")
```

Notice the math-flavored juxtaposition: `4a*c`, `(2a)`. The `imag_part*i` uses explicit `*` because `imag_part` is an identifier (no juxtaposition between two identifiers).
