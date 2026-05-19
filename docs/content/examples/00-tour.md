---
title: Tour
summary: Every language feature in small, focused snippets — literals, bindings, blocks, arithmetic, arrays, structs, lambdas, functions, modes, closures, control flow, prelude, testing, modules.
weight: 5
---

## Comments

```nex
// Line comment.

/* Block comment. */

/* Block comments /* can nest */ — convenient for commenting out code. */
```

## Layout: separators and line continuation

```nex
val a = 1; val b = 2        // multiple statements on one line, separated by ;

val long_expression =       // a line ending with an operator or open
  some_function(x, y)       // delimiter continues on the next (more-
    + another(z)            // indented) line; no backslash needed
    * yet_another(w)
```

`;` is rarely used; line continuation is common for expression-heavy numerical code.

## Literals

### Booleans, integers, reals

```nex
val flag    = true
val n       = 42
val big     = 1_000_000
val hex     = 0xFF              // 255
val bin     = 0b1010_1010       // 170
val oct     = 0o755             // 493
val pi_ish  = 3.14159
val sci     = 6.022e23
val sep     = 1_234.567_89
```

### Strings and escapes

```nex
val hello   = "hello, world"
val multi   = "line one\nline two"
val tabbed  = "col1\tcol2"
val quoted  = "she said \"hi\""
val unicode = "\u{1F600}"           // 😀
val byte    = "\xC3\xA9"             // bytes for "é"
```

### Interpolated strings (`s"..."`)

```nex
val name = "Ada"
val n = 42
val z = 3.0 + 4i

print(s"hello, $name")                  // hello, Ada
print(s"n = $n, n+1 = ${n + 1}")        // n = 42, n+1 = 43
print(s"|z| = ${z.abs()}")              // |z| = 5.0
print(s"first element = ${[10, 20][0]}")
print(s"literal dollar: $$")            // literal dollar: $

// $ident.field treats only $ident as the splice — .field is literal text.
// Use ${ident.field} to interpolate a field access:
print(s"$z.re")                         // (3.0 + 4.0i).re
print(s"${z.re}")                       // 3.0
```

### Complex via the `i` constant

```nex
val a = i                   // (0.0 + 1.0i)  — imaginary unit
val b = 2i                  // (0.0 + 2.0i)  — juxtaposition: 2 * i
val c = 3 + 4i              // (3.0 + 4.0i)
val d = exp(2pi * i)        // ~ (-1.0 + 0.0i)  (Euler's identity)
```

`i` is the prelude name; `for i in 0..n` shadows it normally. Pick a different index name (`j`, `k`, `n`, `m`) if you need both in the same scope.

### Unit

```nex
val nothing = ()            // the single value of type `unit`
```

## Bindings

```nex
val x = 10                  // immutable + has storage
var y = 20                  // mutable + has storage
y = 30                      // OK

val n: integer = 7          // explicit type annotation
var arr: [real] = [1.0, 2.0, 3.0]
val empty: [real] = []      // empty literal needs annotation

// `const` — compile-time constant; RHS must be a constant expression.
// May appear at any scope; compiler may inline at use sites.
const PI_OVER_2 = pi / 2
const TOL       = 1.0e-12
const MAX_ITER  = 1000

// `const` inside a function works too:
def newton(f: real -> real, x0: real) =
  const LOCAL_TOL = 1.0e-8
  // ...

// Function calls are NOT allowed in `const` RHS (deferred to v1+).
// Use val for runtime-computed module-level values:
val SQRT_2 = sqrt(2.0)      // function call → must be val, not const

// Shadowing in a new scope is allowed:
val p = 1
if true then
  val p = 99                // OK: new scope shadows outer p
  print(p)                  // 99
end if
print(p)                    // 1
```

## Block expressions

A multi-statement block is itself an expression — the value of the block is its final expression. Block expressions arise wherever an expression is expected: `val` RHS, function bodies, lambda bodies, branches of `if`.

```nex
val computed =
  val temp   = sqrt(2.0)
  val scaled = temp * 100.0
  scaled + 1.0             // value of the block is this final expression

// `if` branches are block expressions too:
val classified =
  if x > 0.0 then
    val msg = "positive"
    print(s"got $msg")
    msg
  else
    val msg = "non-positive"
    print(s"got $msg")
    msg
```

## Arithmetic

```nex
7 + 3        // 10
7 - 3        // 4
7 * 3        // 21
7 / 3        // 2.333...  (real division — operands promoted)
7 div 3      // 2        (integer division — `div` keyword)
7 % 3        // 1        (modulo, integer)
2 ^ 10       // 1024     (exponentiation)
2.0 ^ 0.5    // 1.414...
-7           // unary minus
```

## Numeric promotion

```nex
1 + 2.0      // integer 1 → real 1.0; result real 3.0
1.0 + 2i     // real → complex; result (1.0 + 2.0i)
1 + 2i       // integer → real → complex; result (1.0 + 2.0i)
```

## Comparison and logical

```nex
5 < 10           // true
5 == 5           // true
5 != 5           // false

true and false   // false
true or false    // true
not true         // false

// Element-wise on arrays:
val v = [1, 2, 3, 4]
val mask = v < 3                          // [true, true, false, false]
```

## Juxtaposition multiplication

```nex
val x = 5.0
val a = 2x                  // 2 * x = 10.0
val b = 2(x + 1)            // 2 * (x + 1) = 12.0
val c = 2pi                 // 2 * pi
val d = 3sin(pi/4)          // 3 * sin(pi/4)
val e = 2i                  // 2 * i

// Identifier-prefix is NOT juxtaposition — xy is one identifier.
val y = 4.0
val p = x * y               // explicit * required between identifiers

// Precedence: ^ > juxtaposition > * / // %
val q = 2x^3                // 2 * (x^3) = 250.0
val r = 1/2x                // 1 / (2 * x) = 0.1
val s = 2x * 3              // (2x) * 3 = 30.0
```

## Arrays — rank-1

```nex
val v: [real] = [1.0, 2.0, 3.0]

length(v)              // 3
v[0]                   // 1.0
v[1..3]                // [2.0, 3.0]      slice (owned copy)
v[0..=1]               // [1.0, 2.0]      inclusive slice

// Element-wise arithmetic with broadcasting:
val a = [1.0, 2.0, 3.0]
val b = [4.0, 5.0, 6.0]
val c = 2a + b - 1.0   // [6.0, 9.0, 12.0]  — fuses to one loop
```

## Arrays — rank-2 (matrices)

```nex
val M: [[real]] = [[1.0, 2.0, 3.0],
                   [4.0, 5.0, 6.0]]        // 2×3

rows(M)                // 2
cols(M)                // 3
shape(M)               // (2, 3)
M[1, 2]                // 6.0               — element access
M[0]                   // [1.0, 2.0, 3.0]   — i-th row
M[:, 1]                // [2.0, 5.0]        — j-th column
M[0, 0..2]             // [1.0, 2.0]        — row 0, cols 0-1
M[0..2, 1..3]          // [[2.0, 3.0], [5.0, 6.0]]  — submatrix

// Element-wise on rank-2:
val N = [[10.0, 20.0, 30.0], [40.0, 50.0, 60.0]]
val P = M + N          // [[11.0, 22.0, 33.0], [44.0, 55.0, 66.0]]
val Q = 2M             // scalar broadcasts

// Matrix multiplication via @ (not element-wise):
val A = [[1.0, 2.0], [3.0, 4.0]]    // 2×2
val u = [5.0, 6.0]
val Au = A @ u                       // [17.0, 39.0]   matrix-vector
val AA = A @ A                       // [[7.0, 10.0], [15.0, 22.0]]  matrix-matrix
val d  = u @ u                       // 61.0           dot (rank-1 @ rank-1)
```

## Tuples

```nex
val t = 1, 2.0, "three"                     // no parens needed

t.0                    // 1
t.1                    // 2.0
t.2                    // "three"

// Destructuring at binding — no parens needed
val a, b, c = t
val x, _ = 3.14, "ignored"

// Parens are only needed in contexts where commas mean something else,
// e.g. function arguments or array elements:
f(1, 2, 3)             // 3 separate arguments
f((1, 2, 3))           // 1 argument, the tuple
[(1, 2), (3, 4)]       // array of 2 tuples
```

## Ranges

```nex
0..5                   // half-open: 0, 1, 2, 3, 4
0..=5                  // inclusive: 0, 1, 2, 3, 4, 5

for i in 0..3 do
  print(i)             // prints 0, 1, 2
end for

val nums = range(0, 10)         // materialize range to [integer]
```

## Structs

```nex
struct Point
  x: real
  y: real
end

val p = Point(3.0, 4.0)
p.x                    // 3.0
p.y                    // 4.0

// Mutate via var binding:
var q = Point(0.0, 0.0)
q.x = 5.0              // OK because q is var
q = Point(1.0, 1.0)    // rebind whole value
```

## Lambdas

```nex
val sq      = (x: real) -> x * x         // explicit param type
val sq2     = x -> x * x                  // type inferred from context
val sum_xy  = (x, y) -> x + y             // multi-arg
val biggy   = x ->                        // block-form
  val y = x * x
  y + 1
```

## Functions

```nex
// Single-expression form
def double(x: real) = 2x

// Block-body form
def normalize(v: [real]) =
  val m = sqrt(dot(v, v))
  v / m

// Explicit return type (required for recursive functions)
def factorial(n: integer): integer =
  if n <= 1 then 1
  else n * factorial(n - 1)

// Mutual recursion
def is_even(n: integer): bool =
  if n == 0 then true else is_odd(n - 1)

def is_odd(n: integer): bool =
  if n == 0 then false else is_even(n - 1)

// Early return
def first_positive(v: [real]) =
  for x in v do
    if x > 0.0 then return x
  end for
  0.0           // fallback if nothing matched

// Unit-returning function
def greet(name: string) =
  print(s"hello, $name")
  ()
```

## Parameter modes

```nex
// `read` mode is INFERRED when the body doesn't mutate the parameter.
// The function reads but never modifies v:
def sum_squares(v: [real]) =                  // inferred read
  var s = 0.0
  for x in v do s = s + x*x end for
  s

// `mut` mode is DECLARED when the body mutates the parameter:
def scale_in_place(v: mut [real], factor: real) =
  for i in 0..length(v) do
    v[i] = v[i] * factor
  end for

// `@strict` disables auto-clone insertion in this function body — every
// clone must then be written explicitly with .clone(). For performance-
// critical paths where every allocation must be visible.
@strict
def hot_loop(a: [real]) =
  val backup = a.clone()         // explicit clone required under @strict
  // ... transformations on a and backup ...
  backup
```

## Higher-order functions

```nex
def apply_twice(f: real -> real, x: real) = f(f(x))

apply_twice(x -> x + 1.0, 5.0)        // 7.0
apply_twice(sqrt, 16.0)               // 2.0
```

## Closures

```nex
def make_adder(a: real) =
  x -> x + a

val add5  = make_adder(5.0)
val add10 = make_adder(10.0)

add5(3.0)            // 8.0
add10(3.0)           // 13.0
```

## Control flow

```nex
// `if` is an expression:
val abs_x = if x < 0.0 then -x else x

// `if` with no else has type `unit`:
if verbose then
  print("doing the thing")
end if

// `for` over ranges or arrays, with optional tuple destructuring:
for i in 0..n do
  process(i)
end for

for k, x in enumerate([10.0, 20.0, 30.0]) do
  print(s"index $k = $x")
end for

// `while` loop:
var n = 100
while n > 1 do
  if n % 2 == 0 then n = n // 2
  else                   n = 3*n + 1
end while
```

## Constants

```nex
pi          // 3.14159265358979...
e           // 2.71828...
inf         // +infinity
nan         // a quiet NaN
i           // (0.0 + 1.0i) — imaginary unit
```

## Prelude — math

```nex
sqrt(2.0)              // 1.414...
exp(1.0)               // e
log(e)                 // 1.0
log2(8.0); log10(100.0)
sin(pi); cos(0.0); tan(pi/4)
asin(1.0); atan2(1.0, 1.0)
sinh(1.0); cosh(1.0); tanh(1.0)
abs(-5)                // 5
sign(-3.0)             // -1.0
floor(2.7); ceil(2.1); round(2.5); trunc(2.9)
min(3, 5); max(3, 5)
```

## Prelude — rank-1 array operations

```nex
val v = [1.0, 2.0, 3.0, 4.0, 5.0]

sum(v)                                          // 15.0
product(v)                                      // 120.0
min(v); max(v)                                  // 1.0; 5.0
dot(v, v)                                       // 55.0
length(v)                                       // 5

v.map(x -> x * x)                               // [1.0, 4.0, 9.0, 16.0, 25.0]
v.reduce(0.0, (acc, x) -> acc + x)              // 15.0  (left fold)
v.filter(x -> x > 2.0)                          // [3.0, 4.0, 5.0]

enumerate(v)                                    // [(0,1.0), (1,2.0), ...]
zip(v, [10.0, 20.0, 30.0, 40.0, 50.0])          // [(1.0,10.0), ...]
range(0, 5)                                     // [0, 1, 2, 3, 4]
```

## Prelude — rank-2 (matrix) operations

```nex
val M = [[1.0, 2.0],
         [3.0, 4.0],
         [5.0, 6.0]]                            // 3×2

rows(M); cols(M); shape(M)                      // 3; 2; (3, 2)
transpose(M)                                    // 2×3
diag([[1.0, 0.0], [0.0, 2.0]])                  // [1.0, 2.0]
reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], 2, 3)   // 2×3
flatten(M)                                      // [1.0, 3.0, 5.0, 2.0, 4.0, 6.0]  (column-major)

sum(M)                                          // 21.0  (all elements)
sum_axis(M, 0)                                  // [9.0, 12.0]  per-column
sum_axis(M, 1)                                  // [3.0, 7.0, 11.0]  per-row
M.map(x -> x + 1.0)                             // element-wise
matmul(transpose(M), M)                         // 2×2; same as transpose(M) @ M
```

## Prelude — construction

```nex
zeros(5)                                        // [0.0, 0.0, 0.0, 0.0, 0.0]
ones(3)                                         // [1.0, 1.0, 1.0]
fill(4, 7.0)                                    // [7.0, 7.0, 7.0, 7.0]
linspace(0.0, 1.0, 5)                           // [0.0, 0.25, 0.5, 0.75, 1.0]

zeros(2, 3)                                     // 2×3 zero matrix
ones(3, 3)                                      // 3×3 ones
fill(2, 2, 9)                                   // 2×2 of integer 9
identity(3)                                     // 3×3 identity matrix
```

## Prelude — I/O

```nex
print("hello")                                  // with newline
print()                                         // newline alone
print(s"x = $x")                                // interpolated (preferred)
val s = format("x =", x, "y =", y)              // join args with spaces
```

## Prelude — conversions

```nex
to_real(42)                                     // 42.0
to_integer(3.7)                                 // 3 (truncates toward zero)
to_complex(5.0)                                 // (5.0 + 0.0i)
```

## Testing

```nex
@test
def test_addition() =
  assert_eq(2 + 2, 4)

@test
def test_with_message() =
  assert(length([1.0, 2.0]) == 2, "expected length 2")

@test
def test_approx_real() =
  assert_approx(0.1 + 0.2, 0.3, 1e-10)

@test
def test_approx_array() =
  assert_approx([0.1 + 0.2, 0.7], [0.3, 0.7], 1e-10)

@test
def test_approx_complex() =
  assert_approx((1.0 + 2i) * i, -2.0 + 1i, 1e-12)

@test
def test_traps_on_bad_division() =
  assert_traps(() -> 1 // 0)
```

A whole module can be marked test-only:

```nex
@test module mylib.deep_tests

import mylib.{public_fn}

// helper functions live alongside tests — both are stripped from production builds
def make_input(n: integer) = linspace(0.0, 1.0, n)

@test
def test_public_fn_monotonic() =
  val ys = make_input(100).map(public_fn)
  for i in 1..length(ys) do
    assert(ys[i] >= ys[i - 1])
  end for
```

## Modules

Folder layout:

```nex
src/
  mylib/
    core.nex          ← module mylib
    util.nex          ← module mylib (same module, sibling file)
  main.nex
```

```nex
// in src/mylib/core.nex
module mylib

def public_fn(x: real) = x + 1.0

private def helper(x: real) = x * 2.0
```

```nex
// in src/mylib/util.nex
module mylib

// `helper` is visible because we're in the same module folder
def public_use_of_helper(x: real) = helper(x) + 1.0
```

```nex
// in src/main.nex
import mylib.{public_fn, public_use_of_helper}
import mylib.{public_fn as pf}      // rename on import

def main() =
  print(public_fn(3.0))             // 4.0
  print(pf(3.0))                    // 4.0
  // helper is NOT visible here — it's private to mylib
```
