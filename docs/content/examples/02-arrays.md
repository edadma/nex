---
title: Arrays and Structs
summary: Element-wise array operations that fuse to one loop, and a small struct with a function over it.
weight: 20
---

## Element-wise array operations (the fusion sweet spot)

```nex
def main() =
  val a = [1.0, 2.0, 3.0, 4.0, 5.0]
  val b = [10.0, 20.0, 30.0, 40.0, 50.0]

  // Fuses to a single loop, no intermediate arrays:
  //   for i in 0..5: result[i] = 2 * a[i] + b[i] - 1.0
  val result = 2a + b - 1.0

  print(result)         // [11.0, 23.0, 35.0, 47.0, 59.0]
```

## A struct and a function over it

```nex
struct Point
  x: real
  y: real
end

def distance(p1: Point, p2: Point) =
  val dx = p1.x - p2.x
  val dy = p1.y - p2.y
  sqrt(dx^2 + dy^2)

def main() =
  val origin = Point(0.0, 0.0)
  val p = Point(3.0, 4.0)
  print(distance(origin, p))        // 5.0
```
