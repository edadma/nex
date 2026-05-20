---
title: Introduction
summary: What Nex is, who it's for, what's in scope and what's deferred.
weight: 10
---

**Implementation language:** Scala 3.
**Target:** AOT compilation via LLVM IR text → `clang -O1` (Mac arm64 verified end-to-end on every commit). A reference tree-walking interpreter runs alongside; the two paths are required to produce byte-for-byte identical output (modulo a documented `%g` 6-significant-figure divergence on irrational reals).

## 1.1 What Nex is

Nex is a statically-typed, expression-oriented, array-first numerical programming language. It is designed for the kind of code that Fortran is still used for today: numerical simulation, scientific computing, computational mathematics, signal processing — workloads where array operations dominate, IEEE 754 behavior matters, and AOT-compiled performance is non-negotiable.

The language combines:

- **Array-first semantics** with element-wise operations as the default
- **Value semantics** throughout — no shared mutable state, no object identity
- **Uniqueness types** for mutable arrays, inferred and managed by the compiler
- **Fusion-aware compilation** — array expressions compile to fused loops with no temporaries
- **Modern syntax** in the Scala 3 family: indentation-based, expression-oriented, optional end markers
- **Built-in complex numbers**, in the Fortran tradition

## 1.2 Audience

The primary audience is the Fortran-using community: HPC, computational science, numerical libraries. Secondary audiences are anyone who currently reaches for Julia or NumPy for numerical work and would prefer an AOT-compiled, statically-typed alternative with cleaner syntax.

## 1.3 Goals and non-goals

**Goals:**
- Mathematical clarity in source code
- Predictable, AOT-compiled performance
- Fusion of array operations as a semantic guarantee, not an optimization
- Strong static typing without ceremony
- Clean module system that supports growing codebases

**Non-goals:**
- Web programming, systems programming, application programming
- Object-oriented design (no classes, no inheritance, no methods in the OO sense)
- General-purpose dynamic dispatch
- Pervasive metaprogramming

## 1.4 Scope

The language as currently specified is the minimum coherent slice that can run real numerical programs. It includes: scalar types, rank-1 and rank-2 arrays (vectors and matrices) with slice assignment, expression-oriented control flow, multi-file folder modules, the mode system, ARC-based memory management, a source-form standard prelude (the scalar transcendentals and their complex extensions live in `prelude/*.nex` and are auto-imported), built-in unit testing, and both a reference interpreter and an AOT compiler.

Excluded (v1+): user-defined generics, sized numeric variants (`real32`, `integer32`, etc.), rank-3+ arrays / a shape language, structs with methods, sum types / pattern matching, operator definitions, modules with submodule interface/implementation split, FFI, parallel-for, and a package manager.
