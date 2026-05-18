---
title: Type System
summary: Primitive types, numeric promotion, arrays, tuples, structs, function types.
weight: 30
---

## 3.1 Primitive types

| Type | Description | Size |
|---|---|---|
| `bool` | Boolean (`true` or `false`) | 1 byte |
| `integer` | Signed integer | 64 bits |
| `real` | IEEE 754 floating-point | 64 bits |
| `complex` | Pair of `real` (re, im) | 128 bits |
| `unit` | Single value `()` | 0 bits |
| `string` | UTF-8 byte string | reference-typed |

Sized variants (`integer32`, `real32`, `complex32`, etc.) are *deferred to v1+*.

## 3.2 Numeric promotion

The numeric types form a promotion lattice:

```
integer  →  real  →  complex
```

When an operator is applied to operands of different numeric types, the operand of the lower type is implicitly promoted. The result has the higher type.

```nex
1 + 2.0        // real, promotes 1 → 1.0, result 3.0
1.0 + 2.0i     // complex, promotes 1.0 → 1.0+0.0i, result 1.0+2.0i
1 + 2.0i       // complex, promotes 1 → 1.0 → 1.0+0.0i
```

Promotion is implicit in expression context. Conversion in the *narrowing* direction is never implicit — see chapter 4.

## 3.3 Array types

The array type `[T]` denotes a **rank-1** array (vector) of elements of type `T`, with runtime-determined length. The array type `[[T]]` denotes a **rank-2** array (matrix), with runtime-determined shape (rows × columns). `T` must be one of `bool`, `integer`, `real`, or `complex`.

```nex
[real]       // rank-1: vector of real
[integer]    // rank-1: vector of integer
[[real]]     // rank-2: matrix of real
[[complex]]  // rank-2: matrix of complex
```

Rank-2 arrays are **rectangular** — every row has the same number of columns. Constructing a jagged array (rows of differing lengths) traps at runtime. Storage is **column-major**, matching Fortran and the standard LAPACK/BLAS convention so that future FFI to those libraries is layout-compatible without transposing.

Higher-rank arrays (`[[[T]]]` and beyond) and statically-shaped arrays (`[T; N]`, `[T; M, N]`) are *deferred to v1+*.

## 3.4 Tuple types

A tuple type is a comma-separated list of types denoting a fixed-size heterogeneous product (n ≥ 2). Tuples are primarily used for multi-value return.

```nex
real, real
integer, [real], complex
```

Parentheses around a tuple type are optional grouping; they are required only when the surrounding context would otherwise parse the commas as something else (parameter declarations, type arguments, array element types). Both forms are equivalent:

```nex
def split(arr: [real]): real, real = ...           // paren-less return type
def split(arr: [real]): (real, real) = ...         // explicit grouping
```

In a parameter declaration the parens are mandatory because the outer parens of the parameter list use commas to separate parameters:

```nex
def join(left: (real, real), right: (real, real)) = ...    // two tuple parameters
def join(left: real, real, right: real, real) = ...        // ERROR: 4 parameters
```

## 3.5 Struct types

A struct type is declared with `struct`. Fields are declared one per line, with types:

```nex
struct Point
  x: real
  y: real
end

struct LineSegment
  start: Point
  finish: Point
  weight: real
end
```

Struct types are pure data — no methods, no inheritance, no constructors beyond the implicit field-list constructor. Struct values are constructed by naming the type followed by parenthesized arguments in declaration order:

```nex
val p = Point(3.0, 4.0)
val seg = LineSegment(Point(0.0, 0.0), Point(1.0, 1.0), 2.5)
```

Field access uses `.`:

```nex
val px = p.x
val end_x = seg.finish.x
```

## 3.6 Function types

A function type `(T1, T2, ..., Tn) -> R` denotes a function taking arguments of the given types and returning `R`. A single-argument function may be written without parentheses:

```nex
real -> real
(real, real) -> real
([real], real -> real) -> [real]
```

A function type may include parameter modes (see chapter 6):

```nex
(mut [real]) -> unit
```

## 3.7 Type expressions

Type expressions are used in declarations (function signatures, struct field types, explicit annotations on bindings). They consist of:

- Primitive type names (`bool`, `integer`, `real`, `complex`, `unit`, `string`)
- Array type constructors (`[T]`)
- Tuple type constructors (`(T1, ..., Tn)`)
- Function type constructors (`(T1, ..., Tn) -> R`)
- User-defined type names (referring to declared structs)
