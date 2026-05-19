---
title: Standard Library / Prelude
summary: Constants, scalar math, complex operations, rank-1 and rank-2 array operations, construction, I/O, conversions, assertions.
weight: 100
---

The prelude is implicitly imported in every module. It contains the names available without any `import` statement.

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
floor, ceil, round, trunc
min, max
```

Each is overloaded over numeric types as appropriate. `sin`, `cos`, `exp`, `log`, `sqrt` apply to `real` and `complex`.

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
reduce(a: [T], init: U, f: (U, T) -> U): U
filter(a: [T], pred: T -> bool): [T]
range(lo: integer, hi: integer): [integer]    // materialize range
enumerate(a: [T]): [(integer, T)]
zip(a: [T], b: [U]): [(T, U)]
```

**Rank-2 operations:**

```nex
rows(m: [[T]]): integer                 // number of rows
cols(m: [[T]]): integer                 // number of columns
shape(m: [[T]]): (integer, integer)     // (rows, cols)

transpose(m: [[T]]): [[T]]              // m × n → n × m
matmul(a: [[T]], b: [[T]]): [[T]]       // also available via the @ operator
diag(m: [[T]]): [T]                     // diagonal as rank-1

reshape(a: [T], m: integer, n: integer): [[T]]   // length(a) must equal m * n
flatten(m: [[T]]): [T]                  // column-major flatten

sum(m: [[T]]): T                        // sum over all elements
sum_axis(m: [[T]], axis: integer): [T]  // axis=0 → per-column; axis=1 → per-row
map(m: [[T]], f: T -> U): [[U]]         // element-wise
```

These are built-in: the compiler knows their types and lowers them with fusion-aware codegen. User-defined polymorphic functions of equivalent generality require generics, *deferred to v1+*.

## 10.5 Construction

**Rank-1:**

```nex
zeros(n: integer): [real]
ones(n: integer): [real]
fill(n: integer, x: T): [T]
linspace(lo: real, hi: real, n: integer): [real]
```

**Rank-2:**

```nex
zeros(m: integer, n: integer): [[real]]
ones(m: integer, n: integer): [[real]]
fill(m: integer, n: integer, x: T): [[T]]
identity(n: integer): [[real]]          // n × n identity matrix
```

`zeros`, `ones`, and `fill` are arity-overloaded: with one integer argument they produce rank-1; with two, rank-2.

## 10.6 I/O

```nex
print(x: T)                  // print value with newline
print()                      // print newline alone
format(fmt: string, args...): string   // format args per fmt
```

For string composition, prefer interpolated string literals (`s"x = $x"`, see the Lexical chapter) — they are the idiomatic form. The `format` function exists for the less common case where the format string itself is computed at runtime.

The `format` function uses simple positional substitution: `{}` is replaced by successive arguments converted to strings. Type-aware formatting (precision, padding, etc.) is *deferred to v1+*.

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
assert_eq(actual: T, expected: T, msg: string)
assert_approx(actual: real, expected: real, tol: real)
assert_approx(actual: [real], expected: [real], tol: real)
assert_approx(actual: complex, expected: complex, tol: real)
assert_traps(thunk: () -> T)
```

All assertions trap on failure with structured information (source location, the compared values, the user-supplied `msg` if any). The test runner catches the trap and reports the failure without halting the rest of the test suite.

**Prefer `assert_approx` over `assert_eq` for `real` and `complex` values** — exact floating-point equality is almost always incorrect.

`assert_traps` takes a zero-argument closure and passes if invoking it traps; it fails if the thunk returns normally. Useful for testing that error paths fire.
