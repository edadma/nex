---
title: Your First Program
summary: Write a hello-world, run it under the interpreter, see the output.
weight: 20
---

## Write a program

Create a file `hello.nex` anywhere on your filesystem:

```nex
def main() =
  print("Hello, Nex!")
  val k = 10
  for i in 1..=5 do
    print(i * k)
```

Two top-level pieces: a `def main()` that's the program's entry point, and a `for` loop that prints `10`, `20`, ... `50`. Everything in the body uses two-space indentation; there are no braces or semicolons. `1..=5` is an inclusive range; `1..5` would stop at 4.

## Run it

From the `nex` repository root, invoke the interpreter via sbt:

```bash
sbt "nexJVM/runMain io.github.edadma.nex.run run /path/to/hello.nex"
```

Output:

```
Hello, Nex!
10
20
30
40
50
```

`run` parses, elaborates (type-checks), and executes the program with the tree-walking interpreter. It's the fastest edit/run cycle for development — no native binary involved.

## What else `run` accepts

The same invocation works on any of the [examples](/examples/) in the repository:

```bash
sbt "nexJVM/runMain io.github.edadma.nex.run run examples/fft/main.nex"
```

If your program imports other modules (multi-file projects), point `run` at the entry file and the import graph is discovered from its directory upward — see the [Modules chapter](/spec/09-modules/) in the spec for layout rules.

## Next step

Now [compile that same program to a native binary](/getting-started/03-compile-to-native/).
