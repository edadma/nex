---
title: Mandelbrot Set
summary: Rank-2 matrix plus complex iteration — escape-time rendered as ASCII art. Row-vector slice assignment writes a whole row of the image with one statement.
weight: 80
---

For each complex parameter $c$, the Mandelbrot set studies the iteration

$$
z_0 = 0, \qquad z_{n+1} = z_n^2 + c.
$$

The point $c$ belongs to the set if the sequence $\{z_n\}$ stays bounded; otherwise it escapes to infinity. A simple sufficient escape test is $\lvert z_n \rvert > 2$ — once the orbit leaves the disk of radius 2, it never returns. The renderer records the first $n$ at which that happens (the *escape time*) and maps it to an ASCII gradient; pixels that never escape after `max_iter` are drawn solid.

```nex
def escape_iters(c: complex, max_iter: integer) =
  var z = 0i
  var k = 0
  while k < max_iter and z.abs() <= 2.0 do
    z = z*z + c
    k = k + 1
  end while
  k

// Compute one row of escape-times as a rank-1 vector. The whole row
// is built independently of the result matrix and then written into
// it via slice assignment — `result[py, :] = row` — in one statement.
def row_at(py: integer, width: integer, height: integer, max_iter: integer) =
  var row = fill(width, 0)
  for px in 0..width do
    val x = -2.0 + 3.0 * to_real(px) / to_real(width)
    val y = -1.5 + 3.0 * to_real(py) / to_real(height)
    row[px] = escape_iters(x + y*i, max_iter)
  end for
  row

def mandelbrot(width: integer, height: integer, max_iter: integer) =
  var result = fill((height, width), 0)
  for py in 0..height do
    // Slice assignment replaces the entire py-th row of the image in
    // place. No per-pixel writes to `result`, no temporary copy.
    result[py, :] = row_at(py, width, height, max_iter)
  end for
  result

def main() =
  val img = mandelbrot(80, 24, 50)
  for py in 0..24 do
    // `print` always appends a newline, so we assemble one row of
    // pixels as a string and print it once per row.
    var line = ""
    for px in 0..80 do
      val n = img[py, px]
      val ch =
        if n == 50 then "#"
        elif n > 25 then ":"
        elif n > 10 then "."
        else " "
      line = line + ch
    end for
    print(line)
  end for
```

Two slice idioms in one program:

1. **Whole-row write** — `result[py, :] = row` replaces the entire py-th row of `result` in place, with no per-pixel `result[py, px] = ...` writes and no temporary copy of the row buffer.
2. **Single-row read** — `img[py]` (single integer on a rank-2) reads the py-th row as a freshly-owned rank-1, used implicitly above via the explicit `img[py, px]` element access. You could also lift that with `val row = img[py]; for px in ... do ... row[px]`.
