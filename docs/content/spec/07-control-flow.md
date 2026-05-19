---
title: Control Flow
summary: if / else, for, while, block scoping.
weight: 70
---

## 7.1 `if` / `else`

See the Expressions chapter. `if` is an expression.

`then` is **required for an inline body** and **optional when the body starts on a new indented line**:

```nex
// Inline — `then` required:
val sign = if x > 0 then 1 else if x < 0 then -1 else 0

// Block body — `then` optional:
if cond
  do_something()
  do_more()
else
  fallback()
```

## 7.2 `for` loops

The `for` loop iterates over an iterable. Iterables are ranges and arrays:

```nex
// `do` required when the body is inline:
for k in 0..n do process(k)

// `do` optional when the body starts on a new indented line:
for k in 0..n
  process(k)
end for

for x in arr
  print(x)
end for
```

The `end for` marker is optional in both forms.

A `for` loop is an expression of type `unit`.

Tuple destructuring in the loop variable — no parens needed:

```nex
for k, x in enumerate(arr)
  print(k, x)
```

## 7.3 `while` loops

`while` follows the same rule as `for`: `do` required for inline bodies, optional when the body starts on a new indented line.

```nex
while cond
  body
end while

// Inline form:
while keep_going do step()
```

A `while` loop is an expression of type `unit`.

## 7.4 Block scoping

Each control-flow body introduces a new scope. Bindings declared in a body are not visible outside it.
