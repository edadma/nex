---
title: Modules
summary: Folder-as-module, declaration, imports, visibility, no .mod headers, test modules.
weight: 90
---

## 9.1 Folder-as-module

In Nex, **a module is a directory**. All `.nex` files within the directory belong to the same module. Subdirectories are submodules.

Given a project root, the module path is the directory path relative to the root, with `/` replaced by `.`:

```nex
src/
  linalg/
    dense/
      type.nex        ← all three files are part of module
      arith.nex       ← linalg.dense
      decompose.nex
    sparse/
      matrix.nex      ← module linalg.sparse
  main.nex            ← module (root)
```

Files in the same module share visibility: any binding in one file is visible to all other files in the same module, unless declared `private` (see §9.4).

## 9.2 Module declaration

A file may optionally declare its module at the top:

```nex
module linalg.dense

def add(a: Matrix, b: Matrix) = ...
```

If the declaration is present, it must match the directory path. If absent, the module is determined by the directory path alone. Declaration is recommended for clarity in large codebases but never required.

## 9.3 Imports

Imports use the `import` keyword. Selective imports list the names to import; `as` introduces a local alias:

```nex
import math.{sqrt, abs}
import linalg.dense.{Matrix, lu_decompose as lu}
import io.console as con
```

Whole-module imports (`import math`) are *not* allowed — selective import is required. This prevents namespace pollution and makes dependencies explicit.

Imports must appear at the top of a file, after the optional `module` declaration.

## 9.4 Visibility

The default visibility of all module-level declarations (functions, structs, constants) is **public** — visible to importing modules.

The `private` keyword restricts a declaration to the declaring module (the whole folder, including sibling files):

```nex
module geometry

def distance(p1: Point, p2: Point) = ...    // public
private def helper(x: real) = ...           // private to geometry
```

Private declarations are visible to all sibling files within the same module folder but invisible to importing modules.

## 9.5 No `.mod` binary headers

Nex does not use precompiled interface files (Fortran's `.mod` files, C++ modules' BMIs, etc.). The build system compiles from source in topological order based on the import graph.

## 9.6 Test modules

An entire module may be marked as test-only by placing `@test` before the `module` declaration. The module's contents — both test functions and support code — are stripped from non-test builds.

```nex
@test module stats.deep_tests

import stats.{mean, variance, stddev}

// Support code — not a test, available to tests in this module.
def make_constant_sample(n: integer, c: real) = fill(n, c)

val constant_cases = [
  (make_constant_sample(10,    0.0),  0.0),
  (make_constant_sample(100,   3.14), 3.14),
  (make_constant_sample(1000, -7.5), -7.5)
]

@test
def test_mean_recovers_constants() =
  for sample, c in constant_cases do
    assert_approx(mean(sample), c, 1e-12)
  end for
```

Test modules can import freely from regular modules but see only the public surface — `private` members of the imported modules are *not* visible. If a test needs an internal helper, place the `@test` function in the same module as the code (see the Functions chapter) rather than in a separate test module.

A test module may itself be a folder containing multiple files; each file declares `@test module foo.bar.tests` at the top.
