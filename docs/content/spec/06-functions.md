---
title: Functions
summary: Declarations, return types, parameters, modes, closures, higher-order functions, recursion, early return, test functions.
weight: 60
---

## 6.1 Function declarations

A function is declared with `def`. The simplest form is single-expression:

```nex
def square(x: real) = x * x

def hypotenuse(a: real, b: real) = sqrt(a * a + b * b)
```

The block form uses an indented body after `=`:

```nex
def normalize(v: [real]) =
  val mag = sqrt(sum(v * v))
  if mag == 0.0 then v
  else v / mag
```

## 6.2 Return types

Return types are inferred by default. An explicit return type may be given after the parameter list:

```nex
def square(x: real): real = x * x
```

Explicit return types are required for recursive functions:

```nex
def factorial(n: integer): integer =
  if n <= 1 then 1
  else n * factorial(n - 1)
```

## 6.3 Parameters

Each parameter is declared with `name: type`. An optional `= default` clause supplies a value to use when the caller omits the argument (see §6.5).

## 6.4 Parameter modes

Function parameters that hold arrays (or struct types containing arrays) have a **mode** that determines how the parameter is passed and what the function may do with it.

Two modes exist:

- **read mode** (default, inferred): the function reads the parameter; it does not mutate, take ownership of, or store it. Read mode is inferred from the function body — if the body never mutates the parameter, never passes it to a `mut` parameter elsewhere, and never returns or stores it in a `var` binding, the parameter is read mode.
- **mut mode** (declared explicitly with `mut`): the function takes ownership of the parameter and may mutate it. Required if the body does any of the actions ruled out by read mode.

```nex
def sum(v: [real]) = ...                   // read mode (inferred)
def fill(v: mut [real], x: real) = ...     // mut mode (declared)
```

Mode mismatches at call sites:

- Passing a `val` array to a `mut` parameter requires a clone (compiler inserts it automatically; see chapter 8).
- Passing a `var` array to a `mut` parameter moves ownership — the caller cannot use the array after the call unless the function returns it.
- Passing any array to a `read` parameter never moves or mutates it.

Modes apply only to array and struct types. Scalar parameters (`integer`, `real`, `bool`, `complex`, `unit`) are always pass-by-value.

## 6.5 Default values and named arguments

A parameter may carry a default expression in its declaration:

```nex
def greet(name: string, greeting: string = "Hello") =
  print(s"${greeting}, ${name}!")

def scale(x: real, factor: real = 2.0, offset: real = 0.0) =
  factor * x + offset
```

Default-valued parameters must form a **contiguous trailing run** — once one parameter has a default, every later parameter must also have one. This guarantees that a positional caller can simply drop trailing arguments.

```nex
greet("World")                 // "Hello, World!" — greeting defaulted
scale(10.0)                    //  20.0 — factor + offset defaulted
scale(10.0, 3.0)               //  30.0 — offset defaulted
```

**Named arguments** at the call site identify a parameter by name with `name = expr`. Named arguments may appear in any order, may skip over middle parameters that have defaults, and must come *after* any positional arguments:

```nex
greet(name = "Nex")                            // "Hello, Nex!"
greet(greeting = "Hi", name = "Foo")           // "Hi, Foo!"
scale(10.0, offset = 5.0)                      //  25.0 — factor defaulted
scale(10.0, factor = 7.0, offset = 2.0)        //  72.0
```

Resolution rule (compile-time):

1. All positional arguments fill leading parameter positions in order.
2. Each named argument fills the parameter slot matching its name.
3. Any unfilled slot uses that parameter's declared default; an unfilled slot whose parameter has no default is a compile error.
4. Supplying the same parameter twice (positional + named, or named twice) is a compile error.

The default expression is **captured untouched at declaration time and re-evaluated at every call site that uses it** (Scala / JavaScript semantics, not Python's evaluate-once). A default like `id: integer = next_id()` calls `next_id()` fresh on each call.

**Restrictions.** Defaults and named arguments are only supported when the callee is a single, non-overloaded `def` referenced by name. Overload sets (multiple `def`s sharing a name) require fully-positional calls. Computed callees (lambda values, function-typed parameters, returned closures) likewise require positional arguments. Method-call syntax (`receiver.f(args)`) does not yet support named arguments — use the function-call form `f(receiver, ...)` if you need them.

## 6.6 Closures

Function values may be created with lambda syntax (chapter 4) and passed as arguments or returned from functions:

```nex
def make_adder(a: real) =
  x -> x + a

val add5 = make_adder(5.0)
add5(3.0)                    // 8.0
```

Closures capture lexical bindings. Capture of `val` bindings is unrestricted. Capture of `var` array bindings moves the binding into the closure (uniqueness preserved).

## 6.7 Higher-order functions

Functions are first-class values. They may be passed as arguments and returned from other functions:

```nex
def apply(f: real -> real, x: real) = f(x)
apply(square, 3.0)           // 9.0
apply(x -> x + 1.0, 3.0)     // 4.0
```

## 6.8 Recursion

Direct and mutual recursion are supported. Mutually-recursive functions must appear in the same module; forward declarations are not required.

## 6.9 `return`

The `return` expression terminates the enclosing function with the given value (or `()` if omitted). It is useful for early exit:

```nex
def first_negative(v: [real]) =
  for x in v do
    if x < 0.0 then return x
  end for
  0.0
```

A function's final expression provides its return value implicitly without `return`.

## 6.10 `@intrinsic` declarations

A bodyless `def` annotated with `@intrinsic("opId")` declares a function whose implementation is supplied by the compiler — typically a libm bridge or a runtime helper — rather than by Nex source. The `opId` string names the lowering: `@intrinsic("libm.sqrt")` lowers to a direct call to the host's libm `sqrt` primitive.

```nex
@intrinsic("libm.sqrt")
def sqrt(x: real): real
```

This is how the standard prelude bridges the real-libm transcendentals (see the Prelude chapter); user code generally has no reason to write `@intrinsic` directly. The attribute is the only sanctioned escape hatch — any other bodyless `def` is a parse error.

## 6.11 Test functions

A function declared with the `@test` attribute is a unit test: it takes no arguments, returns `unit`, and is discovered automatically by the test runner.

```nex
@test
def test_addition_is_commutative() =
  assert_eq(2 + 3, 3 + 2)

@test
def test_normalize_produces_unit_vector() =
  val v = [3.0, 4.0]
  val u = normalize(v)
  assert_approx(sum(u * u), 1.0, 1e-12)
```

Test functions are stripped from non-test builds — they do not appear in production binaries.

Tests in a regular module can see every binding in that module, including `private` ones. This makes it natural to test internal helpers without widening their public surface.

A test passes by returning normally and fails by trapping. The prelude provides assertion functions (see the Prelude chapter) that trap with structured failure information the test runner catches and reports.
