---
title: Control Flow
summary: if / else, for, while, block scoping.
weight: 70
---

## 7.1 `if` / `else`

See the Expressions chapter. `if` is an expression.

## 7.2 `for` loops

The `for` loop iterates over an iterable. Iterables are ranges and arrays:

```nex
for i in 0..n do
  process(i)
end for

for x in arr do
  print(x)
end for
```

The `do` keyword is required. The `end for` marker is optional.

A `for` loop is an expression of type `unit`.

Tuple destructuring in the loop variable — no parens needed:

```nex
for k, x in enumerate(arr) do
  print(k, x)
```

## 7.3 `while` loops

```nex
while cond do
  body
end while
```

A `while` loop is an expression of type `unit`.

## 7.4 Block scoping

Each control-flow body introduces a new scope. Bindings declared in a body are not visible outside it.
