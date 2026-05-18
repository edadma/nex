---
title: In-Place vs Functional
summary: Same `normalize` algorithm written two ways — a fresh-array functional form and a `mut`-parameter in-place form.
weight: 70
---

```nex
// Functional: fresh array, fusion-friendly
def normalize(v: [real]) =
  val mag = sqrt(sum(v * v))
  if mag == 0.0 then v
  else v / mag

// In-place: declared `mut` on the parameter
def normalize_in_place(v: mut [real]) =
  val n = length(v)
  var sum_sq = 0.0
  for i in 0..n do
    sum_sq = sum_sq + v[i]^2
  end for
  val mag = sqrt(sum_sq)
  if mag > 0.0 then
    for i in 0..n do
      v[i] = v[i] / mag
    end for
  end if

def main() =
  // Functional: a is immutable, b is a fresh array
  val a = [3.0, 4.0]
  val b = normalize(a)
  print(b)                          // [0.6, 0.8]

  // In-place: c is mutable, normalize_in_place modifies it
  var c = [3.0, 4.0]
  normalize_in_place(c)
  print(c)                          // [0.6, 0.8]
```
