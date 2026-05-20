---
title: Standard Library / Prelude
summary: Constants, scalar math, complex operations, rank-1 and rank-2 array operations, construction, I/O, conversions, assertions.
weight: 100
---

The prelude is implicitly imported in every module. It contains the names available without any `import` statement.

The scalar math layer (the libm transcendentals plus their complex extensions) lives as ordinary Nex source in the sysroot's `prelude/` directory and is auto-imported at every program's elaboration. The compiler still seeds a handful of names whose dispatch depends on return-type information (`abs`, `sign`, `min`, `max`, `conj`, `arg`) plus the array prelude and I/O — those will migrate to source as the relevant overload-resolution rules generalize. The split is invisible to users; either way the names are in scope.

## 10.1 Constants

| Name | Type | Value |
|---|---|---|
| `pi` | `real` | π = 3.14159265358979... |
| `e` | `real` | Euler's number = 2.71828... |
| `inf` | `real` | positive infinity |
| `nan` | `real` | a quiet NaN |
| `i` | `complex` | imaginary unit = `(0.0, 1.0)` (shadowable; see the Lexical chapter) |

The boolean literals `true` and `false` (see the Lexical chapter) are globally available as keywords, not prelude names.

## 10.2 Mathematical functions on scalars

```
sqrt, cbrt, abs, sign
exp, log, log2, log10
sin, cos, tan, asin, acos, atan, atan2
sinh, cosh, tanh, asinh, acosh, atanh
hypot
floor, ceil, round, trunc
min, max
```

Each is overloaded over numeric types as appropriate. `sqrt`, `exp`, `log`, `log2`, `log10`, `sin`, `cos`, `tan` apply to both `real` and `complex` (the complex variants are source-level Nex `def`s in the prelude that compose the real-libm primitives — overload resolution picks the right one by argument type at the call site).

## 10.3 Complex-specific operations

The real and imaginary parts of a `complex` value are accessed as fields with `.`:

```nex
z.re        // real part        (type: real)
z.im        // imaginary part   (type: real)
```

Other complex operations:

```nex
conj(z: complex): complex     // complex conjugate
arg(z: complex): real         // argument (angle, in radians)
abs(z: complex): real         // magnitude
```

Equivalent method-style: `z.conj()`, `z.arg()`, `z.abs()` (see the method-like notation rules in the Expressions chapter).

Construction of complex values uses the prelude constant `i` (§10.1) together with arithmetic, supported by juxtaposition multiplication (see the Expressions chapter):

```nex
3 + 4i             // (3.0 + 4.0i)
2.5i               // (0.0 + 2.5i)
exp(pi * i)        // ≈ (-1.0 + 0.0i) — Euler's identity
exp(2pi * i)       // ≈ (1.0 + 0.0i)  — full rotation
```

## 10.4 Array operations

**Rank-1 operations:**

```nex
length(a: [T]): integer
sum(a: [T]): T                          // T must be numeric
product(a: [T]): T                      // T must be numeric
min(a: [T]): T                          // T must be ordered numeric
max(a: [T]): T                          // T must be ordered numeric
dot(a: [T], b: [T]): T                  // inner product; T numeric
map(a: [T], f: T -> U): [U]
flatMap(a: [T], f: T -> [U]): [U]              // concat results
reduce(a: [T], init: U, f: (U, T) -> U): U
filter(a: [T], pred: T -> bool): [T]
range(lo: integer, hi: integer): [integer]    // materialize range
enumerate(a: [T]): [(integer, T)]
zip(a: [T], b: [U]): [(T, U)]
```

**Calling convention.** All the higher-order array functions
(`map` / `flatMap` / `reduce` / `filter`) and the rank-1 accessors
(`length` / `sum` / `product` / `dot` / `enumerate` / `zip`)
are typically called via method-call sugar (§4.9). The two forms
are equivalent — the dot form just sugars `arr.name(...)` into
`name(arr, ...)`:

```nex
// Preferred — reads as a pipeline:
xs.map(x -> x * x).filter(y -> y > 10).sum()

// Same call shape, free-function form:
sum(filter(map(xs, x -> x * x), y -> y > 10))
```

Either compiles to the same AST; the dot form is the documented
convention for chains.

**Rank-2 operations:**

```nex
rows(m: [[T]]): integer                 // number of rows
cols(m: [[T]]): integer                 // number of columns
shape(m: [[T]]): (integer, integer)     // (rows, cols)

transpose(m: [[T]]): [[T]]              // m × n → n × m
matmul(a: [[T]], b: [[T]]): [[T]]       // also available via the @ operator
diag(d: [T]): [[T]]                     // n×n diagonal matrix with d on the diagonal

reshape(a: [T], m: integer, n: integer): [[T]]   // length(a) must equal m * n
flatten(m: [[T]]): [T]                  // column-major flatten

sum(m: [[T]]): T                        // sum over all elements
sum_axis(m: [[T]], axis: integer): [T]  // axis=0 → per-column; axis=1 → per-row
map(m: [[T]], f: T -> U): [[U]]         // element-wise
```

These are built-in: the compiler knows their types and lowers them with fusion-aware codegen. User-defined polymorphic functions of equivalent generality require generics, *deferred*.

## 10.5 Construction

**Rank-1:**

```nex
zeros(n: integer): [integer]
ones(n: integer): [integer]
fill(n: integer, x: T): [T]
linspace(lo: real, hi: real, n: integer): [real]
```

**Rank-2:**

```nex
zeros(shape: (integer, integer)): [[integer]]
ones(shape: (integer, integer)): [[integer]]
fill(shape: (integer, integer), x: T): [[T]]
identity(n: integer): [[integer]]       // n × n identity matrix
```

`zeros`, `ones`, and `fill` dispatch on shape: a single `integer` argument produces rank-1, a `(integer, integer)` tuple produces rank-2.

```nex
zeros(5)                 // rank-1: [0, 0, 0, 0, 0]
zeros((2, 3))            // rank-2: [[0, 0, 0], [0, 0, 0]]
fill(4, 7.0)             // rank-1: [7.0, 7.0, 7.0, 7.0]
fill((3, 2), 0.0)        // rank-2 reals
identity(3)              // rank-2 integer identity matrix
```

`zeros`, `ones`, and `identity` currently return **integer** element type. When you need a real-typed buffer, use `fill(n, 0.0)`, `fill(n, 1.0)`, or `linspace`; for a real identity, multiply by `1.0` (`identity(n) * 1.0`) or fill manually. A future refinement is expected once overload-by-return-type lands.

## 10.6 I/O

```nex
print(x: T)                  // print value with newline
print()                      // print newline alone
format(args...): string      // join args (in default form) with single spaces
```

For string composition, prefer interpolated string literals (`s"x = $x"`, see the Lexical chapter) — they are the idiomatic form. The `format` function is currently a minimal helper that converts each argument with the same rules as `print` (whole reals as `n.0`, strings unquoted, structs as `Name { ... }`) and joins them with single spaces; it does not interpret a format string.

Positional `{}` substitution and type-aware formatting (precision, padding, hex specifiers, the `f"..."` literal form) are *deferred*.

## 10.7 Type conversions

Explicit conversion functions (narrowing or non-promoting):

```nex
to_real(x: integer): real
to_integer(x: real): integer         // truncates toward zero
to_complex(x: real): complex
```

## 10.8 Assertions

For use in test functions (see the Functions chapter) and test modules (see the Modules chapter):

```nex
assert(cond: bool)
assert(cond: bool, msg: string)
assert_eq(actual: T, expected: T)
assert_approx(actual: real,      expected: real,      tol: real)
assert_approx(actual: complex,   expected: complex,   tol: real)
assert_approx(actual: [T],       expected: [T],       tol: real)
assert_traps(thunk: () -> T)
assert_traps(thunk: () -> T, expected_substring: string)
```

All assertions trap on failure with a message naming the assertion type and (where applicable) the user-supplied `msg`. The test runner catches the trap and reports the failure without halting the rest of the test suite.

**Prefer `assert_approx` over `assert_eq` for `real` and `complex` values** — exact floating-point equality is almost always incorrect. The `complex` overload checks the Euclidean distance `|a - b|` against `tol`. The array overload runs element-wise on rank-1 arrays of integer, real, or complex; integers lift to real, complex elements use the Euclidean distance, and a length mismatch between the two arrays traps. Rank-2 element-wise overloads are *deferred*.

`assert_traps` takes a zero-argument closure and passes if invoking it traps; it fails if the thunk returns normally. The 2-arg form additionally checks that the trap message contains `expected_substring` — useful for asserting a specific failure mode rather than "any trap fires".
