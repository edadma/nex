---
title: Type System
summary: Primitive types, numeric promotion, arrays, tuples, structs, sum types, function types.
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

Sized variants (`integer32`, `real32`, `complex32`, etc.) are *deferred*.

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

Higher-rank arrays (`[[[T]]]` and beyond) and statically-shaped arrays (`[T; N]`, `[T; M, N]`) are *deferred*.

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

## 3.6 Generic structs

A struct declaration may introduce one or more **type parameters** in square brackets after its name. Each type parameter stands for a type fixed at the use site; the field declarations may name the parameters, and the compiler generates a specialized clone of the struct for every distinct argument combination.

```nex
struct Box[T]
  value: T
end

struct Pair[A, B]
  fst: A
  snd: B
end
```

The bracket list follows the same shape as the one on a generic function (see [§6.10](/spec/06-functions/#610-generic-functions)): each entry is a bare `[T]` (admits any type) or a bounded `[T: Kind]` drawn from the same closed constraint set — `Any`, `Numeric`, `Real`, `Float`, `Complex`, `Ord`, `Eq`. A use site whose argument type isn't admitted by the constraint is rejected at elaboration time.

**Call-site inference.** A generic struct is constructed exactly like a monomorphic one — the type arguments are not written. The compiler infers them by unifying each field's declared type against the actual argument:

```nex
val p = Pair(1, "hi")          // A := integer, B := string
val q = Pair(1.0, 2.0)         // A := real,    B := real
val b = Box(42)                // T := integer
```

**Explicit type arguments.** A binding's declared type may carry explicit arguments, which is useful when call-site inference cannot pin every parameter on its own — for instance, to pin a payload's type ahead of the constructor or to disambiguate at a join point:

```nex
val p: Pair[integer, string] = Pair(1, "hi")
```

**Specialization.** Like generic functions, generic struct templates have no executable form on their own. Each unique combination of type arguments produces a specialized struct (e.g. `Pair$integer$string`, `Pair$real$real`) with the concrete fields already substituted. Two specializations of the same template with different arguments are independent types — a value of one cannot stand in for the other.

**Combining with generic functions.** A type parameter on a function can flow through to a generic struct constructor; the function's specialization carries the right struct specialization with it:

```nex
def pack[T](x: T, y: T): Pair[T, T] = Pair(x, y)

val a = pack(1, 2)             // Pair$integer$integer
val b = pack(1.5, 2.5)         // Pair$real$real
```

The same constraint vocabulary applies — `struct Sorted[T: Ord]` is well-formed and lets the field operations rely on `<` / `<=` / `>` / `>=`.

## 3.7 Sum types

A sum type is declared with `enum`. Each line of the body declares a **variant** — either a bare name (a single value of the enum type) or a constructor that takes named, typed fields:

```nex
enum Color =
  Red
  Green
  Blue

enum Solver =
  Converged(x: real)
  Diverged
  MaxIters(iters: integer, last: real)
```

A variant's field types are restricted to scalar types (`integer`, `real`, `bool`, `string`) in the current implementation. Aggregate fields (arrays, tuples, structs, complex, nested enums) are *deferred*.

Variant references are values directly when the variant is bare (`Red`, `Diverged`), or callable constructors when the variant has fields (`Converged(3.14)`, `MaxIters(100, 0.5)`). Both forms yield a value whose static type is the enclosing enum:

```nex
val a: Color  = Green
val b: Solver = Converged(2.5)
val c: Solver = Diverged
```

Sum-type values are inspected with `match` (see [§7.5](/spec/07-control-flow/#75-match-expressions)). Printing a sum-type value renders bare variants as their name (`Red`) and fielded variants as `Name(f0, f1, ...)` (`Converged(3.14)`).

## 3.8 Generic enums

An enum declaration may introduce **type parameters** in square brackets, exactly like a generic struct. The parameters may appear in variant payload field types; bare variants carry no payload but still participate in the enum's specialization.

```nex
enum Opt[T] =
  Some(value: T)
  None

enum Result[T, E] =
  Ok(value: T)
  Err(error: E)
```

The bracket list uses the same constraint vocabulary as [§3.6](#36-generic-structs) and [§6.10](/spec/06-functions/#610-generic-functions): a bare `[T]` admits any type; a bound `[T: Kind]` restricts the parameter to the closed kind set.

**Call-site inference.** A variant constructor (`Some(x)`, `Ok(v)`) infers its enum's type arguments from its argument types:

```nex
val x = Some(42)               // Opt$integer
val r = Ok(3.14)               // E unpinned — see below
```

**Bare-variant inference.** A bare variant (`None`, no payload) carries no information to infer its enclosing enum's type parameters from. Bare-variant references rely on push-down from a declared type on the surrounding binding:

```nex
val n: Opt[integer] = None     // T := integer via the annotation
```

A bare variant in a context with no declared type to push down (e.g. an `if … then Some(1) else None` with no outer annotation) is rejected the same way an unconstrained generic function call would be.

**Partial-inference annotations.** When only some of a multi-parameter enum's type parameters can be pinned from a single use, the binding must carry an explicit type. Constructing `Ok(7)` on a `Result[T, E]` pins `T` from the argument but leaves `E` open:

```nex
val r: Result[integer, string] = Ok(7)
```

Without the annotation the compiler rejects the construction with a "cannot infer type for type parameter `E`" diagnostic — the same shape Rust reports for an analogous case.

**Match patterns.** Patterns on a specialized enum reference the specialization's variants. The scrutinee's static type determines which specialization is matched; nested payload patterns bind values of the substituted field type:

```nex
val msg = x match
  Some(v) -> "got " + str(v)
  None    -> "nothing"
```

**Specialization.** Like generic structs, each unique combination of type arguments produces a specialized enum (`Opt$integer`, `Opt$string`, `Result$integer$string`, etc.) with its own per-variant constructors. Two specializations are independent types; a value of one cannot stand in for the other.

## 3.9 Function types

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

## 3.10 Type expressions

Type expressions are used in declarations (function signatures, struct field types, explicit annotations on bindings). They consist of:

- Primitive type names (`bool`, `integer`, `real`, `complex`, `unit`, `string`)
- Array type constructors (`[T]`)
- Tuple type constructors (`(T1, ..., Tn)`)
- Function type constructors (`(T1, ..., Tn) -> R`)
- User-defined type names (referring to declared structs or enums)
- Applied generic type names (`Pair[integer, string]`, `Opt[integer]`) — see [§3.6](#36-generic-structs) and [§3.8](#38-generic-enums)
