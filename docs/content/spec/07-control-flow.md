---
title: Control Flow
summary: if / else, for, while, match, block scoping.
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

## 7.5 `match` expressions

`match` inspects a sum-type value ([§3.6](/spec/03-types/#36-sum-types)) and dispatches on its variant. Each arm is a `case Pattern => body`:

```nex
enum Solver =
  Converged(x: real)
  Diverged
  MaxIters(iters: integer, last: real)

def describe(s: Solver): real =
  match s
    case Converged(x)      => x
    case Diverged          => -1.0
    case MaxIters(n, last) => last
```

The scrutinee follows the `match` keyword; arms are written on indented lines below. A `match` is an expression — all arm bodies must yield the same type, and the `match` itself evaluates to that type.

**Patterns.**
- `Variant` — matches a bare variant by name.
- `Variant(p1, p2, ...)` — matches a fielded variant and recursively binds each sub-pattern to the corresponding field.
- `name` — matches anything and binds the scrutinee to `name`. Acts as a catch-all when used as a top-level pattern.
- `_` — matches anything without binding. Same role as `name` but without introducing a name in scope.

When used inside a variant pattern, sub-patterns are restricted to plain bindings or `_` — nested variant patterns are *deferred* until variant fields can carry aggregate types.

**Exhaustiveness.** Every variant of the scrutinee's enum must be covered, either by an explicit `case Variant(...) =>` arm or by a catch-all (`case _ =>` or `case name =>`). The compiler rejects a match that leaves variants uncovered:

```nex
enum Color = Red; Green; Blue

def isRed(c: Color): bool =
  match c
    case Red => true
    case _   => false             // catch-all closes the set
```

Duplicate arms (two arms covering the same variant) and arms after a catch-all are also rejected. A non-exhaustive match without a catch-all is a compile error, not a runtime trap.

**Restrictions.** The scrutinee must have an enum type — `match` on integers, reals, strings, structs, etc. is not supported. Guards (`case Variant(x) if x > 0 => ...`) are *deferred*.
