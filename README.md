# Nex - Array Math REPL

A Scala 3 array-oriented REPL with APL/J-style operations and exact rational arithmetic.

## Features

- **Exact arithmetic** - Rational numbers stay exact (`1/3 + 1/6` → `1/2`)
- **Complex numbers** - Full complex number support via `i` constant
- **Array operations** - Element-wise operations with broadcasting
- **Pipeline operator** - Data flows left to right (`[1,2,3] | sum`)
- **Currying** - All multi-argument functions support partial application
- **Function composition** - Compose functions with `.` operator
- **Lambda shorthand** - Use `_` as placeholder (`map(_ * 2, [1,2,3])`)

## Quick Start

```bash
# Interactive REPL
sbt nexJVM/run

# Evaluate expression
sbt 'nexJVM/run -e "iota(10) | filter(_ > 5) | sum"'
```

## Example Session

```
> 1 + 2
3

> 1 / 3
1/3

> 1/3 + 1/6
1/2

> [1, 2, 3] + [4, 5, 6]
[5, 7, 9]

> [1, 2, 3] * 2
[2, 4, 6]

> iota(5)
[0, 1, 2, 3, 4]

> iota(10) | sum
45

> iota(10) | filter(_ > 5) | map(_ * 2) | sum
60

> [1, 2, 3, 4, 5] | take(3)
[1, 2, 3]

> double = _ * 2
<function(_arg)>

> increment = _ + 1
<function(_arg)>

> f = increment . double
<composed>

> 5 | f
11

> [1, 2, 3] | map(f)
[3, 5, 7]

> i * i
-1

> (1 + i) * (1 - i)
2

> sqrt(-1)
i

> sqrt(4)
2

> abs([-3, -2, -1, 0, 1])
[3, 2, 1, 0, 1]
```

## Pipeline Operator

The `|` operator passes values through functions left-to-right:

```
> [1, 2, 3] | sum
6

> iota(10) | filter(_ > 5) | map(_ * 2) | take(3)
[12, 14, 16]

> 5 + 3 | _ * 2
16
```

## Currying

All multi-argument functions support partial application:

```
> take3 = take(3)
<take(1/2)>

> iota(10) | take3
[0, 1, 2]

> [1, 2, 3, 4, 5] | filter(_ > 2) | map(_ * 10)
[30, 40, 50]
```

## Function Composition

The `.` operator composes functions (right-associative):

```
> double = _ * 2
> increment = _ + 1
> f = increment . double
> 5 | f
11

> [1, 2, 3] | map(increment . double)
[3, 5, 7]
```

## Built-in Functions

### Array Functions

| Function | Description | Example |
|----------|-------------|---------|
| `iota(n)` | Generate [0, 1, ..., n-1] | `iota(5)` → `[0, 1, 2, 3, 4]` |
| `range(a, b)` | Generate [a, a+1, ..., b-1] | `range(2, 7)` → `[2, 3, 4, 5, 6]` |
| `reshape(shape, data)` | Reshape array | `reshape([2,3], iota(6))` → 2x3 matrix |
| `shape(arr)` | Get array dimensions | `shape([[1,2],[3,4]])` → `[2, 2]` |
| `sum(arr)` | Sum all elements | `[1, 2, 3] \| sum` → `6` |
| `product(arr)` | Product of elements | `[1, 2, 3, 4] \| product` → `24` |
| `count(arr)` | Number of elements | `[1, 2, 3] \| count` → `3` |
| `take(n)` | First n elements | `iota(10) \| take(3)` → `[0, 1, 2]` |
| `drop(n)` | Drop first n elements | `[1,2,3,4,5] \| drop(2)` → `[3, 4, 5]` |
| `map(fn)` | Apply function to each | `[1,2,3] \| map(_ * 2)` → `[2, 4, 6]` |
| `filter(fn)` | Keep matching elements | `[1,2,3,4] \| filter(_ > 2)` → `[3, 4]` |
| `min` | Minimum value | `min([3,1,4])` → `1`, `min(3,7)` → `3` |
| `max` | Maximum value | `max([3,1,4])` → `4`, `max(3,7)` → `7` |

### Math Functions

All math functions work element-wise on arrays and support complex numbers.

| Function | Description | Example |
|----------|-------------|---------|
| `abs(x)` | Absolute value | `abs(-5)` → `5` |
| `sqrt(x)` | Square root | `sqrt(4)` → `2`, `sqrt(-1)` → `i` |
| `floor(x)` | Floor | `floor(3.7)` → `3` |
| `ceil(x)` | Ceiling | `ceil(3.2)` → `4` |
| `sin(x)` | Sine | `sin(0)` → `0` |
| `cos(x)` | Cosine | `cos(0)` → `1` |
| `tan(x)` | Tangent | `tan(0)` → `0` |
| `asin(x)` | Arc sine | `asin(0)` → `0` |
| `acos(x)` | Arc cosine | `acos(1)` → `0` |
| `atan(x)` | Arc tangent | `atan(0)` → `0` |
| `exp(x)` | Exponential | `exp(0)` → `1` |
| `ln(x)` | Natural logarithm | `ln(1)` → `0` |
| `pow(x, y)` | Power | `pow(2, 3)` → `8` |
| `mod(x, y)` | Modulo | `mod(10, 3)` → `1` |

## Operators

| Operator | Description | Precedence |
|----------|-------------|------------|
| `\|` | Pipeline | Lowest |
| `.` | Composition | |
| `+`, `-` | Add, subtract | |
| `>`, `<`, `>=`, `<=`, `==`, `!=` | Comparison | |
| `*`, `/` | Multiply, divide | |
| `-` (unary) | Negate | Highest |

## Constants

| Constant | Value |
|----------|-------|
| `i` | Imaginary unit (sqrt(-1)) |
| `pi` | 3.141592653589793 |
| `e` | 2.718281828459045 |

## Lambda Shorthand

Use `_` as a placeholder to create anonymous functions:

```
> [1, 2, 3] | map(_ * 2)
[2, 4, 6]

> [1, 2, 3, 4, 5] | filter(_ > 2)
[3, 4, 5]

> 5 + 3 | _ * 2
16
```

## Command Line Options

```
nex 0.0.1
Usage: nex [options]

  -e, --eval <expr>  Evaluate expression and print result
  -f, --file <path>  Execute file
  --help             Print this help message
```

## Building

Requires sbt and JDK 11+.

```bash
sbt nexJVM/compile  # Compile
sbt nexJVM/test     # Run tests
sbt nexJVM/run      # Start REPL
```

## License

ISC
