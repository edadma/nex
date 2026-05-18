---
title: Mandelbrot Set
summary: Rank-2 matrix plus complex iteration — escape-time rendered as ASCII art.
weight: 80
---

```nex
def escape_iters(c: complex, max_iter: integer) =
  var z = 0i
  var i = 0
  while i < max_iter and z.abs() <= 2.0 do
    z = z*z + c
    i = i + 1
  end while
  i

def mandelbrot(width: integer, height: integer, max_iter: integer) =
  var result = fill(height, width, 0)        // rank-2 of integer
  for py in 0..height do
    for px in 0..width do
      val x = -2.0 + 3.0 * to_real(px) / to_real(width)
      val y = -1.5 + 3.0 * to_real(py) / to_real(height)
      val c = x + y*i
      result[py, px] = escape_iters(c, max_iter)
    end for
  end for
  result

def main() =
  val img = mandelbrot(80, 24, 50)
  for py in 0..24 do
    for px in 0..80 do
      val n = img[py, px]
      if n == 50 then print("#")
      else if n > 25 then print(":")
      else if n > 10 then print(".")
      else print(" ")
    end for
    print()
  end for
```
