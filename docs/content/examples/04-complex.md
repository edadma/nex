---
title: Complex Numbers and the Quadratic Formula
summary: Complex arithmetic, then a quadratic solver returning a pair of complex roots.
weight: 40
---

## Complex numbers

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

```nex
// Solve ax² + bx + c = 0; returns a pair of complex roots.
// Parens around each tuple are required: comma binds looser than every
// other operator (§4.3), so without them the comma would bind across
// the surrounding `if` / `else`.
def solve_quadratic(a: real, b: real, c: real) =
  val disc = b^2 - 4a*c
  if disc >= 0.0 then
    val sd = sqrt(disc)
    ((-b + sd) / (2a) + 0i, (-b - sd) / (2a) + 0i)
  else
    val real_part = -b / (2a)
    val imag_part = sqrt(-disc) / (2a)
    (real_part + imag_part*i, real_part - imag_part*i)
  end if

def main() =
  val r1, r2 = solve_quadratic(1.0, -3.0, 2.0)      // x² - 3x + 2 = 0
  print(s"real roots: $r1, $r2")

  val c1, c2 = solve_quadratic(1.0, 0.0, 1.0)       // x² + 1 = 0
  print(s"complex roots: $c1, $c2")
```

Notice the math-flavored juxtaposition: `4a*c`, `(2a)`. The `imag_part*i` uses explicit `*` because `imag_part` is an identifier (no juxtaposition between two identifiers).
