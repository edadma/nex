---
title: Sum Types and Match
summary: Modeling a Newton-style solver's outcomes with an enum, then dispatching with match.
weight: 130
---

A solver loop typically has three distinct outcomes: it converged on a value, the iterates diverged, or it ran out of iterations without converging. Modeling the *result* as a flat structure (`(success: bool, x: real, iters: integer)`) loses information: the meaning of `x` depends on `success`, and the meaning of `iters` depends on both. A sum type makes the three cases — and the data each one carries — explicit.

## The solver

```nex
enum Solver =
  Converged(x: real)
  Diverged
  MaxIters(iters: integer, last: real)

def newton(f: real -> real, fp: real -> real, x0: real, tol: real, max_iters: integer): Solver =
  var x = x0
  for i in 0..max_iters do
    val fx = f(x)
    if abs(fx) < tol then return Converged(x)
    val d = fp(x)
    if abs(d) < 1.0e-15 then return Diverged
    x = x - fx / d
  end for
  MaxIters(max_iters, x)

def f(x: real) : real = x^2 - 2.0
def fp(x: real): real = 2.0 * x
```

Each `return` site builds a value of the enum type. `Converged(x)` calls the fielded constructor (one `real` field); `Diverged` is a bare variant, so the name itself is the value; `MaxIters(...)` constructs the two-field variant. All three are typed `Solver`, so the function's declared return type is satisfied uniformly.

## Inspecting the outcome with `match`

```nex
def describe(s: Solver): string =
  match s
    case Converged(_)        => "converged"
    case Diverged            => "diverged"
    case MaxIters(iters, _)  => s"ran ${iters} iters"

def main() =
  print(describe(newton(f, fp, 1.0,    1.0e-12, 50)))
  print(describe(newton(f, fp, 0.0,    1.0e-12, 50)))     // fp(0)=0 → Diverged
  print(describe(newton(f, fp, 1.0e9,  1.0e-12, 3)))      // 3 iters too few
```

Output:

```
converged
diverged
ran 3 iters
```

Three properties to notice:

- **Exhaustiveness.** Every variant of `Solver` has its own arm. Drop one and the compiler refuses to build the program — there is no quiet "default fell through" path. A wildcard `case _ =>` arm is allowed when only some variants matter, but you opt in by writing it.
- **Field binding.** `Converged(x)` extracts the `real` field into a name visible inside the arm body. `MaxIters(iters, _)` binds two patterns in declaration order, with `_` discarding the field we don't need.
- **One result type.** Every arm body must evaluate to the same type — here, `string`. The whole `match` expression is itself a `string`, so it can flow into `print`, into a binding, or into another expression.

## Extracting the converged value

When you actually want the `x` out of `Converged(x)`, the same machinery applies — `match` binds it and you compute with it:

```nex
def root_or_zero(s: Solver): real =
  match s
    case Converged(x) => x
    case _            => 0.0

def main() =
  print(root_or_zero(newton(f, fp, 1.0, 1.0e-12, 50)))    // 1.4142135623730951
  print(root_or_zero(newton(f, fp, 0.0, 1.0e-12, 50)))    // 0.0
```

The wildcard arm covers both `Diverged` and `MaxIters`, returning a sentinel `0.0`. In a real solver you'd usually surface the failure to the caller through a sum-typed return rather than collapsing it to a sentinel — but the option is there when a default is genuinely the right behavior.

## When to reach for a sum type

A sum type fits when a value has a *bounded set of named cases*, each with its own associated data:

- Solver outcomes (`Converged | Diverged | MaxIters`)
- Parsed tokens (`Number(real) | Identifier(string) | LParen | RParen | ...`)
- Tree shapes (`Leaf(real) | Branch(real, real)` — once nested variant fields ship)
- Operation results that distinguish "succeeded with x" from "failed because of y"

If the cases all carry the *same* shape and only differ in a small tag, a struct with an integer/string tag field is simpler. If the cases are unbounded or grow at runtime (user-defined plugins, dynamic types), a sum type isn't the right tool — that's where traits / interfaces would fit, both of which are deferred from the current language.
