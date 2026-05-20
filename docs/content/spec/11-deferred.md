---
title: Deferred
summary: Features intentionally excluded from the current language version, split into v0.1+ (small additions) and v1.0+ (major work).
weight: 110
---

The features deferred from the current language version are split into two tiers:

- **v0.1+** — small, additive, non-breaking additions. Planned as natural follow-ups without redesigning anything in the core.
- **v1.0+** — major features, possibly breaking, requiring substantive design and implementation work.

Note that *language* version and *compiler* version are tracked independently. A given compiler release notes which language version(s) it implements; the two version numbers do not march in lockstep.

## 11.1 Deferred to v0.1+

**Syntax / expressions:**
- Literate programming via `.lnex` files (Markdown wrapper: column-0 prose, indented code, supports inline math, tables, and non-Nex code blocks)
- View-style slicing (non-copying — returns a borrow into the source array)
- Strided slice assignment (`a[lo..hi by k] = rhs`). The non-strided forms (`a[lo..hi] = rhs`, `m[i, lo..hi] = rhs`, full rank-2 sections) are shipped — see [§4.15](/spec/04-expressions/#415-slice-assignment)
- Negative indexing (`a[-1]`)
- Default parameter values (`def f(x: real = 0.0) = ...`)
- Named arguments at call sites (`f(x = 1.0, y = 2.0)`)
- Function calls in `const` expressions (compile-time evaluation of pure-function calls)
- Formatted output: positional `{}` substitution in `format()`, plus precision / padding / hex specifiers (`f"..."` literals and `format()` directives)
- Array and complex overloads of `assert_approx` — currently the scalar `assert_approx(real, real, real)` is the only form; element-wise array forms and the complex-valued form are useful for testing numeric code at higher rank

## 11.2 Deferred to v1.0+

**Type system:**
- User-defined generics (`def foo[T](...)`)
- Sized numeric variants (`integer8`, `integer32`, `real32`, `complex32`)
- Higher-rank arrays (rank-3 and beyond) and a static-shape language (`[real; M, N]`)
- Sum types / enums
- Pattern matching beyond simple destructuring
- Traits / type classes
- Type aliases

**Syntax / expressions:**
- User-defined operators (with precedence inheritance)
- Operator definitions on existing types
- `match` expressions

**Memory / runtime:**
- RAII for `var` arrays (currently ARC)
- Cycle detection for ARC
- Parallel-for / parallel reductions
- SIMD intrinsics

**Modules / build:**
- Module interface/implementation split (Fortran-2008-style submodules)
- Package manager
- Conditional compilation
- FFI / C ABI compatibility
- Calling external Fortran / BLAS / LAPACK

**Standard library:**
- `geometry3` stdlib module — quaternions plus the other 3D-rotation representations (rotation matrices, axis–angle, Euler angles) and the conversions between them. Quaternions are *not* planned as a built-in primitive: they don't extend the integer → real → complex promotion lattice cleanly (complex has no canonical embedding into quaternions — which of `i` / `j` / `k`?), their multiplication is non-commutative (which breaks the element-wise / broadcast story for `*`), and the workloads that need them (3D graphics, robotics, attitude estimation) sit outside the Fortran / numerical-array audience the language is shaped around. The library home is the right one once generics (`def foo[T](...)`) and structs with methods both ship — until then, users who need a `Q4` can roll a 4-field struct and write the ops by hand.

**Backend:**
- GPU / accelerator targets

**Tooling:**
- Language server / IDE integration
- Debugger
- Profiler
- Documentation generator
