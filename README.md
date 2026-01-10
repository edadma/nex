# Nex - Array Math REPL

A Scala 3 array-oriented REPL with APL/J-style operations and exact rational arithmetic.

## Features

- **Exact arithmetic** - Rational numbers stay exact (`1/3 + 1/6` → `1/2`)
- **Complex numbers** - Full complex number support via `i` constant
- **Array operations** - Element-wise operations with broadcasting
- **Built-in functions** - `iota`, `reshape`, `sum`, `product`, `map`, and more
- **Lambda shorthand** - Use `_` as placeholder (`map(_ * 2, [1,2,3])`)

## Quick Start

```bash
# Interactive REPL
sbt nexJVM/run

# Evaluate expression
sbt 'nexJVM/run -e "sum(iota(10))"'
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

> sum(iota(10))
45

> x = iota(6)
[0, 1, 2, 3, 4, 5]

> reshape([2, 3], x)
[[0, 1, 2], [3, 4, 5]]

> map(_ * 2, [1, 2, 3])
[2, 4, 6]

> i * i
-1

> (1 + i) * (1 - i)
2

> 3 + 2*i
3+2i
```

## Built-in Functions

| Function | Description | Example |
|----------|-------------|---------|
| `iota(n)` | Generate [0, 1, ..., n-1] | `iota(5)` → `[0, 1, 2, 3, 4]` |
| `range(a, b)` | Generate [a, a+1, ..., b-1] | `range(2, 7)` → `[2, 3, 4, 5, 6]` |
| `reshape(shape, data)` | Reshape array | `reshape([2,3], iota(6))` → 2x3 matrix |
| `shape(arr)` | Get array dimensions | `shape([[1,2],[3,4]])` → `[2, 2]` |
| `sum(arr)` | Sum all elements | `sum([1, 2, 3])` → `6` |
| `product(arr)` | Product of elements | `product([1, 2, 3, 4])` → `24` |
| `count(arr)` | Number of elements | `count([1, 2, 3])` → `3` |
| `take(n, arr)` | First n elements | `take(3, iota(10))` → `[0, 1, 2]` |
| `drop(n, arr)` | Drop first n elements | `drop(2, [1,2,3,4,5])` → `[3, 4, 5]` |
| `map(fn, arr)` | Apply function to each | `map(_ + 10, [1,2,3])` → `[11, 12, 13]` |
| `filter(fn, arr)` | Keep matching elements | `filter(_ > 2, [1,2,3,4])` → `[3, 4]` |

## Constants

| Constant | Value |
|----------|-------|
| `i` | Imaginary unit (√-1) |
| `pi` | 3.141592653589793 |
| `e` | 2.718281828459045 |

## Arithmetic

All operations work element-wise on arrays with broadcasting:

```
> 2 + 3          # Scalar arithmetic
5

> [1, 2, 3] + 10 # Broadcast scalar to array
[11, 12, 13]

> [1, 2] + [3, 4] # Element-wise
[4, 6]
```

Operators: `+`, `-`, `*`, `/` (exact division)

## Lambda Shorthand

Use `_` as a placeholder to create anonymous functions:

```
> map(_ * 2, [1, 2, 3])      # Multiply each by 2
[2, 4, 6]

> map(_ + 10, [1, 2, 3])     # Add 10 to each
[11, 12, 13]

> filter(_ > 2, [1, 2, 3, 4]) # Keep elements > 2
[3, 4]
```

## Command Line Options

```
nex 0.1
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
