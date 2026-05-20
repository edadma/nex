---
title: Three Ways to Express the Same Computation
summary: The same `normalize` algorithm written three ways — functional (fuses to one loop), explicit-loop with a local `var`, and slice assignment for partial in-place updates.
weight: 70
---

```nex
// 1) Functional: fresh array, fusion-friendly. The expression `v / mag`
//    lowers to a single fused loop with no intermediate.
def normalize(v: [real]) =
  val mag = sqrt(sum(v * v))
  if mag == 0.0 then v
  else v / mag

// 2) Explicit-loop with a local `var`: same machine code as (1), useful
//    when the loop body is more complex than a single element-wise
//    expression can capture. `out` is unique inside the function, so
//    indexed writes are in-place; the return value moves the buffer out.
def normalize_loop(v: [real]) =
  val n = length(v)
  val mag = sqrt(sum(v * v))
  if mag == 0.0 then v
  else
    var out = fill(n, 0.0)
    for i in 0..n do
      out[i] = v[i] / mag
    end for
    out

// 3) Slice assignment: overwrite a sub-extent of an existing var array
//    in place. The assignment mutates the underlying buffer in the
//    binding's own scope — no clone, no reallocation, no new identity.

def main() =
  // 1) Functional:
  val a = [3.0, 4.0]
  val b = normalize(a)
  print(b)                          // [0.6, 0.8]

  // 2) Explicit loop — same answer, same machine code (per design promise):
  val c = normalize_loop([3.0, 4.0])
  print(c)                          // [0.6, 0.8]

  // 3) Slice assignment: replace the first two elements in place.
  //    This is the v0 idiom for partial in-place updates on a var array.
  var d = [9.0, 9.0, 9.0, 9.0]
  d[0..2] = [0.6, 0.8]
  print(d)                          // [0.6, 0.8, 9.0, 9.0]
```
