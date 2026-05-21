---
title: Deferred
summary: Features intentionally excluded from the current language, split into near-term additions and long-term work.
weight: 110
---

The features deferred from the current language are split into two tiers by scope:

- **Near-term additions** — small, additive, non-breaking. Planned as natural follow-ups without redesigning anything in the core.
- **Long-term work** — major features, possibly breaking, requiring substantive design and implementation effort.

## 11.1 Near-term additions

*(All previously-listed near-term items are now shipped.)*

## 11.2 Long-term work

**Type system:**
- User-defined generics (`def foo[T](...)`)
- Sized numeric variants (`integer8`, `integer32`, `real32`, `complex32`)
- Higher-rank arrays (rank-3 and beyond) and a static-shape language (`[real; M, N]`)
- Pattern matching beyond enums (literal patterns, guards, range patterns) — current `match` shape is defined in [§7.5](/spec/07-control-flow/#75-match-expressions)
- Traits / type classes
- Type aliases

**Syntax / expressions:**
- User-defined operators (with precedence inheritance)
- Operator definitions on existing types

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
