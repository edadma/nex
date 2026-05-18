---
title: Expressions
summary: Operators and precedence, arithmetic, element-wise arrays, comparison, conditionals, lambdas, ranges, array literals and slicing, structs, tuples, blocks.
weight: 40
---

Nex is expression-oriented: nearly every syntactic construct is an expression with a type and a value.

## 4.1 Literal expressions

Each literal form (chapter 2) is an expression of the corresponding type.

## 4.2 Variable references

A bare identifier refers to a binding in scope. The expression has the type of the binding.

## 4.3 Operators and precedence

Operators, in order from highest to lowest precedence:

| Level | Operators | Associativity | Description |
|---|---|---|---|
| 1 | `.` | left | field / method access |
| 2 | `f(...)`, `a[...]` | left | application, indexing |
| 3 | unary `-`, `not` | right | unary minus, logical not |
| 4 | `^` | right | exponentiation |
| 5 | juxtaposition (`2x`, `2(e)`, `2pi`) | left | implicit multiplication (§4.4) |
| 6 | `*`, `/`, `div`, `%`, `@` | left | multiplication, division, integer division, modulo, matrix multiply |
| 7 | `+`, `-` | left | addition, subtraction |
| 8 | `..`, `..=` | left | range constructors |
| 9 | `==`, `!=`, `<`, `<=`, `>`, `>=` | non-associative | comparison |
| 10 | `and` | left | logical and |
| 11 | `or` | left | logical or |
| 12 | `->` | right | function / lambda |
| 13 | `,` | right | tuple constructor (§4.16) |

User-defined operators are *deferred to v1+*.

Comma binds looser than every other operator. That means `if cond then a else b, c` parses as `(if cond then a else b), c` — a 2-tuple. Use parentheses if you meant the comma inside an `else` branch: `if cond then (a, c) else (b, c)`.

## 4.4 Arithmetic operators

The arithmetic operators `+`, `-`, `*`, `/`, `^`, `%`, `div` apply to numeric operands with the following semantics:

- `+`, `-`, `*` — work on `integer`, `real`, `complex`. Promote per §3.2.
- `/` — *real* division. Both operands promoted to at least `real`. `integer / integer` produces `real`.
- `div` — *integer* division (keyword, not a symbol — `//` is reserved for line comments). Both operands must be `integer`. Truncates toward zero. Example: `7 div 3 == 2`.
- `%` — *modulo*. Both operands must be `integer`. Result has sign of dividend.
- `^` — exponentiation. `real ^ real → real`, `complex ^ complex → complex`, `integer ^ integer → integer` (only for non-negative exponent; negative exponent traps).
- unary `-` — negation. Works on any numeric type.

Integer arithmetic traps on overflow. Real arithmetic follows IEEE 754 (NaN, infinities propagate; no traps from arithmetic itself).

**Juxtaposition multiplication.** A numeric literal (integer or real) immediately followed by an identifier or an opening parenthesis — with or without intervening whitespace — denotes implicit multiplication. This makes mathematical notation read naturally:

```nex
2x                  // 2 * x
2(x + 1)            // 2 * (x + 1)
2pi                 // 2 * pi
3sin(t)             // 3 * sin(t)
2i                  // 2 * i    = (0.0 + 2.0i)
```

The rule applies *only* when the left operand is a numeric literal — not when it is an identifier and not when it is a parenthesized expression. Therefore:

- `xy` is the identifier `xy`, **not** `x * y`.
- `(x)(y)` is a function call (`x` applied to `y`), **not** multiplication.
- To multiply two identifier or parenthesized expressions, write `*` explicitly.

Per the precedence table (§4.3), juxtaposition binds tighter than `*`, `/`, `div`, and `%`, but looser than `^`. This matches how the math reads:

- `2x^3`   is `2 * (x^3)`        — `^` binds tighter than juxtaposition
- `1/2x`   is `1 / (2 * x)`      — juxtaposition binds tighter than `/`
- `2x * 3` is `(2 * x) * 3`      — juxtaposition binds tighter than `*`

The juxtaposition rule subsumes Nex's complex-number construction: `2i`, `3 + 4i`, and `(a + b)i` are all ordinary expressions under this rule, with `i` being a prelude constant of type `complex` (see the Prelude chapter).

## 4.5 Element-wise array operators

When an arithmetic operator is applied with one or both operands being an array, the operation is **element-wise**. The rules are:

- `[T] op [T]` — element-wise on rank-1, requires same length, returns `[T]`
- `[[T]] op [[T]]` — element-wise on rank-2, requires same shape, returns `[[T]]`
- `arr op T` and `T op arr` — broadcasts the scalar over the array (any rank), returns an array of the same shape and rank

Shape mismatch (length for rank-1; shape for rank-2) is a runtime trap.

```nex
val a = [1.0, 2.0, 3.0]
val b = [4.0, 5.0, 6.0]
val c = a + b                            // [5.0, 7.0, 9.0]
val d = a * 2.0                          // [2.0, 4.0, 6.0]
val e = 2a + b                           // [6.0, 9.0, 12.0]   (juxtaposition)

val M = [[1.0, 2.0], [3.0, 4.0]]         // 2×2
val N = [[10.0, 20.0], [30.0, 40.0]]
val P = M + N                            // [[11.0, 22.0], [33.0, 44.0]]
val Q = 2M                               // [[2.0, 4.0], [6.0, 8.0]]
```

Element-wise expressions are guaranteed to fuse: `2a + b` and its rank-2 equivalents lower to a single loop nest with no intermediate arrays allocated.

**Matrix multiplication** uses the `@` operator (not element-wise). It is defined between rank-1 and rank-2 arrays as follows:

```
[[T]] @ [[T]]    shapes (m, k) @ (k, n)  →  (m, n)     matrix-matrix
[[T]] @ [T]      shape  (m, k) @ k       →  m          matrix-vector
[T]  @ [[T]]     shape  k     @ (k, n)   →  n          vector-matrix
[T]  @ [T]       shape  k     @ k        →  T          dot product
```

Inner-dimension mismatch traps.

```nex
val A = [[1.0, 2.0], [3.0, 4.0]]         // 2×2
val v = [5.0, 6.0]
val Av = A @ v                           // [17.0, 39.0]   (rank-1)
val AA = A @ A                           // [[7.0, 10.0], [15.0, 22.0]]
val d  = v @ v                           // 61.0           (scalar = dot)
```

## 4.6 Comparison operators

The operators `==`, `!=`, `<`, `<=`, `>`, `>=` produce `bool`. Comparison is defined for:

- `bool == bool`, `bool != bool` only
- All ordered numeric comparisons between numeric types after promotion
- `complex == complex` and `complex != complex` (no ordered comparison on complex)
- Element-wise array comparison: `[T] op [T] → [bool]` (same broadcast rules as §4.5)
- `string == string`, `string != string` (no ordered comparison on string)

Comparison operators are non-associative: `a < b < c` is a syntax error.

## 4.7 Logical operators

`and`, `or`, `not` work on `bool` (or `[bool]` element-wise). `and` and `or` are short-circuiting on scalar `bool`.

## 4.8 Function calls

```nex
f(a, b, c)
sqrt(2.0)
map(arr, x -> x * 2.0)
```

If `f` has type `(T1, T2, ..., Tn) -> R`, the call has type `R`. Arguments are evaluated left-to-right.

## 4.9 Method-like notation

The expression `e.name(args...)` is sugar for `name(e, args...)` when `e.name` does not refer to a struct field. The compiler resolves `e.name` as follows:

1. If `e`'s type is a struct with a field named `name`, treat as field access.
2. Otherwise, if a function `name` is in scope whose first parameter matches the type of `e`, treat as `name(e, args...)`.
3. Otherwise, error.

This allows the natural chaining style:

```nex
arr.map(x -> x * 2.0).sum()       // equivalent to sum(map(arr, x -> x * 2.0))
```

## 4.10 Conditionals

An `if` expression has the form:

```nex
if cond then expr1 else expr2
```

It is an expression whose type is the common type of `expr1` and `expr2`. Multi-line form:

```nex
if cond then
  expr1
else
  expr2
```

For chained conditionals:

```nex
if cond1 then
  expr1
else if cond2 then
  expr2
else
  expr3
```

An `if` without an `else` is an expression of type `unit`. Both branches in that case must also have type `unit`:

```nex
if cond then
  print(x)         // type unit
```

## 4.11 Lambdas

Lambda expressions use `->`:

```nex
x -> x * x
(x, y) -> x + y
(x: real) -> x * 2.0          // optional parameter type annotation
```

Single-parameter lambdas may omit parentheses. Multi-parameter lambdas require them. Parameter types are inferred from context where possible; otherwise must be annotated.

Block-form lambdas:

```nex
x ->
  val y = x * x
  y + 1
```

Lambdas capture surrounding lexical bindings. Captured `val` bindings are captured by value (or by shared reference, per ARC). Captured `var` bindings are captured uniquely — only one closure may capture a given `var` binding, and the binding is moved into the closure (see chapter 8).

## 4.12 Ranges

The range expressions:

- `lo..hi` — half-open: `[lo, hi)`
- `lo..=hi` — closed: `[lo, hi]`

Operands are `integer`. The result is a `range`, a built-in iterable type used in `for` loops and convertible to `[integer]`.

```nex
0..n              // 0, 1, ..., n-1
0..=n             // 0, 1, ..., n
```

Materialization to `[integer]` is lazy — a range used in a `for` loop iterates without allocating an array.

## 4.13 Array literals and construction

**Rank-1 literals** use square brackets:

```nex
[1.0, 2.0, 3.0]              // [real]
[1, 2, 3]                    // [integer]
[true, false, true]          // [bool]
[1.0 + 2.0i, 3.0 + 4.0i]     // [complex]
```

All elements must have the same type. Empty rank-1 literals require a type annotation:

```nex
val empty: [real] = []
```

**Rank-2 literals** are nested. Each inner array is a row of the resulting matrix:

```nex
val m = [[1.0, 2.0, 3.0],
         [4.0, 5.0, 6.0]]              // [[real]], 2×3 matrix

val grid = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]    // [[integer]], 3×3
```

Inner arrays must all have the same length (rectangularity); a jagged literal traps at runtime.

Arrays may also be constructed from ranges via the `range` function (rank-1) and from the standard-library constructors `zeros`, `ones`, `fill`, `identity`, `linspace` (see the Prelude chapter).

## 4.14 Indexing and slicing

Array indexing is **0-based**. Out-of-bounds indexing traps. Negative indices are *not* supported (no Python-style `a[-1]`).

**Rank-1 indexing** with a single integer:

```nex
val a = [10.0, 20.0, 30.0]
a[0]    // 10.0
a[2]    // 30.0
```

**Rank-1 slicing** with a range expression returns a freshly owned array (copying elements):

```nex
a[0..2]       // [10.0, 20.0]
a[1..=2]      // [20.0, 30.0]
```

**Rank-2 indexing** uses two integer indices separated by a comma:

```nex
val m = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
m[0, 0]       // 1.0
m[1, 2]       // 6.0
```

The single-index form `m[i]` returns the i-th row as a freshly-owned rank-1 array:

```nex
m[0]          // [1.0, 2.0, 3.0]
m[1]          // [4.0, 5.0, 6.0]
```

**Rank-2 slicing** uses `:` to mean "all of this axis" and range expressions for sub-extents. Result rank depends on what's collapsed: an integer along an axis collapses it; `:` or a range preserves it.

```nex
m[:, 1]              // column 1 (rank-1)        → [2.0, 5.0]
m[0, 0..2]           // row 0, cols 0-1 (rank-1) → [1.0, 2.0]
m[0..2, 1..3]        // sub-matrix (rank-2)
m[:, :]              // full copy (rank-2)
```

All slice forms return freshly-owned arrays. View-style slicing (returning a borrow into the source array without copying) is *deferred to v1+*.

## 4.15 Struct construction and access

```nex
val p = Point(3.0, 4.0)
val x = p.x                  // 3.0
```

Struct field reassignment is allowed only on `var` bindings (see chapter 5):

```nex
var p = Point(3.0, 4.0)
p.x = 10.0                   // ok
```

## 4.16 Tuple construction and access

Tuples are constructed by **comma-separated expressions** (length ≥ 2). Parentheses are not required:

```nex
val t = 1, 2.0, "three"      // type: integer, real, string
val pair = 3.0, 4.0           // type: real, real
```

Parentheses are optional grouping; both forms are equivalent:

```nex
val t = (1, 2.0, "three")
```

Parentheses become **required** when the surrounding context already uses commas for another purpose:

```nex
f(1, 2, 3)                   // 3 arguments
f((1, 2, 3))                 // 1 argument, the tuple (1, 2, 3)

[1, 2, 3]                    // [integer] of 3 elements
[(1, 2), (3, 4)]             // [(integer, integer)] of 2 tuples
```

Tuple elements are accessed by 0-based numeric field:

```nex
t.0      // 1
t.1      // 2.0
t.2      // "three"
```

Tuple destructuring in `val` / `var` bindings — no parens needed unless type-annotated:

```nex
val a, b, c = t                          // bind components
val x, _ = 3.14, "ignored"               // _ discards
val (a, b): (integer, real) = 1, 2.0     // parens required when annotating types
```

Single-element tuples do not exist; `1` is just `1`. The empty tuple `()` is already the unit literal (chapter 2) — there is no zero-arity tuple distinct from `unit`.

## 4.17 Block expressions

A sequence of statements followed by a final expression is itself an expression with the type and value of the final expression. The block form arises from indentation:

```nex
val x =
  val y = compute()
  val z = transform(y)
  z + 1                      // type of the block is the type of z + 1
```

If the final form is a statement (e.g., assignment), the block has type `unit`.
