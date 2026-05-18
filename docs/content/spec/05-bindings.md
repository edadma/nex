---
title: Bindings
summary: const, val, and var — what they bind, when they're evaluated, scoping and shadowing.
weight: 50
---

Nex has three binding kinds: `const`, `val`, and `var`. All three may appear at any scope (module level or inside a function/block). They differ in *what* they bind:

| Keyword | Mutability | Storage | Evaluated |
|---|---|---|---|
| `const` | Immutable | None (may be inlined) | Compile time |
| `val`   | Immutable | Has storage           | Runtime, at point of declaration |
| `var`   | Mutable   | Has storage           | Runtime, at point of declaration |

## 5.1 `val` bindings

A `val` binding introduces an immutable binding to a value. The bound name cannot be reassigned, and (for array and struct types) the bound value's contents cannot be mutated through this binding. The right-hand side may be any expression, evaluated once at the point of declaration (module load, function entry, or block entry).

```nex
val x = 42
val data = [1.0, 2.0, 3.0]
val mag = sqrt(sum(v * v))          // RHS may include function calls
```

Type annotation is optional and inferred from the initializer when omitted:

```nex
val x: integer = 42
val data: [real] = [1.0, 2.0, 3.0]
val empty: [real] = []              // empty literals require an annotation
```

For `val` arrays: contents are immutable, the buffer may be freely shared, no uniqueness constraint applies (see chapter 8).

## 5.2 `var` bindings

A `var` binding introduces a mutable binding. For scalar types, the binding may be reassigned. For array types and struct types, the contents may also be mutated (via `a[i] = ...` or `s.field = ...`). The right-hand side may be any expression.

```nex
var n = 0
n = n + 1                    // ok

var data = [1.0, 2.0, 3.0]
data[0] = 10.0               // ok: mutate contents
data = [4.0, 5.0]            // ok: rebind

var p = Point(0.0, 0.0)
p.x = 5.0                    // ok
```

Module-level `var`s are permitted (for caches, RNG state, configuration set at startup) but should be used sparingly — most program state belongs in function-local bindings or in `const`s.

`var` arrays are subject to the uniqueness rule (chapter 8).

## 5.3 `const` bindings

A `const` binding introduces a compile-time constant. The right-hand side must be a *constant expression* — evaluable entirely at compile time. The compiler may inline the value at use sites; there is no guaranteed storage location.

```nex
const PI       = 3.14159265358979
const TAU      = 2 * PI            // arithmetic on consts is constant
const MAX_ITER = 1000
const ENABLED  = true
```

A *constant expression* is one of:

- A numeric or boolean literal
- An application of `+`, `-`, `*`, `/`, `div`, `%`, `^`, juxtaposition, `not`, `and`, `or`, or comparison operators to constant expressions
- A reference to another `const` binding
- A reference to a prelude constant (`pi`, `e`, `inf`, `nan`, `i`)
- Unary `-` of a constant expression

Function calls in `const` expressions are *deferred to v1+* (compile-time evaluation of pure functions). For runtime-computed module-level values, use `val`:

```nex
val SQRT_2 = sqrt(2.0)              // function call → must be val
const TWO_PI = 2 * pi               // OK — pi is a prelude constant
```

Prefer `const` over `val` whenever the value is a true compile-time fact (mathematical constants, physical constants, configuration flags, fixed tolerances). It signals intent and lets the compiler inline at use sites.

## 5.4 Scoping

Bindings are lexically scoped. A binding is visible from its declaration to the end of the enclosing block.

## 5.5 Shadowing

A new binding may shadow an earlier binding with the same name in an enclosing scope. Shadowing within the same block is not permitted.

```nex
val x = 1
def foo() =
  val x = 2                  // ok: shadows outer x in foo's scope
  ...
```

```nex
val x = 1
val x = 2                    // error: redeclaration in same scope
```
