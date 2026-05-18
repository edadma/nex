---
title: Hello, world
summary: The first program — print to stdout — followed by a function and a calculation.
weight: 10
---

## Hello, world

```nex
def main() =
  print("Hello, Nex!")
```

## A function and a calculation

```nex
def hypotenuse(a: real, b: real) = sqrt(a^2 + b^2)

def main() =
  print(s"hypotenuse(3, 4) = ${hypotenuse(3.0, 4.0)}")
```
