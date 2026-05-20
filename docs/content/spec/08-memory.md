---
title: Mode System and Memory Management
summary: The two array regimes, uniqueness for var arrays, auto-clone, read-mode parameters, ARC, future RAII.
weight: 80
---

This chapter describes the design that makes Nex's array semantics safe and analyzable. Most of it is invisible to users in everyday code — the compiler handles the bookkeeping. This chapter describes what the compiler is doing on the user's behalf.

## 8.1 The two array regimes

Arrays in Nex fall into two regimes based on the binding kind that owns them:

- **`val` arrays:** immutable contents, freely shareable. Multiple bindings may refer to the same underlying buffer because no mutation is possible.
- **`var` arrays:** mutable contents, uniquely owned. Only one binding may refer to a given buffer at any program point. Aliasing is prohibited by the type system.

This split is the foundation of Nex's fusion guarantees: the compiler can rewrite array expressions without proving non-aliasing analyses because the type system has already ensured no aliasing exists for mutable arrays.

## 8.2 Uniqueness for `var` arrays

A `var` binding to an array is a **unique owner**. The compiler enforces:

1. After a `var` binding's value is moved (passed to a `mut` parameter, returned, or assigned to another `var` binding), the original binding is invalid and may not be used.
2. A `var` binding may not be assigned the same array as another live `var` binding.

These rules ensure that for any `var` array, there is exactly one live binding referring to it at every program point.

## 8.3 Auto-clone insertion

The compiler **automatically inserts clones** wherever needed to satisfy the uniqueness rule. Users do not write `.clone()` in normal code. The clone insertion follows these rules:

1. If a `var` array is used after its value has been moved, the compiler inserts a clone of the original buffer at the point of the move so the later use sees an independent buffer.
2. If a `val` array is passed to a `mut` parameter, the compiler inserts a clone so the function receives a fresh mutable buffer.
3. The clone-placement optimization pass picks the optimal point for each clone (e.g., clones the *earlier* use so the *later* use can be a move), and elides clones that are provably unnecessary.

A planned `@strict` function attribute will disable auto-clone insertion within a function body — every clone must then be written explicitly as `.clone()`, and missing clones become compile errors. This mode is intended for library authors and performance-critical code where every allocation must be visible. The attribute is reserved at the parser level today; the enforcement pass is *deferred to v0.1+*.

## 8.4 Read-mode parameters need no clones

Because read-mode parameters (the default) do not mutate or take ownership of their argument, calls to read-mode functions never require clones — even when the caller passes a `var` array:

```nex
def sum(v: [real]) = ...               // read mode

var a = [1.0, 2.0, 3.0]
val s1 = sum(a)            // ok: read mode, a still valid
val s2 = sum(a)            // ok: a still valid
a[0] = 10.0                // ok: a is mutable, never moved
```

## 8.5 Memory management: ARC

The current implementation uses **automatic reference counting (ARC)** for all heap-allocated values (arrays, strings, structs containing those). Each heap-allocated value carries a reference count; increments occur when ownership is shared, decrements occur when a reference goes out of scope, and the value is freed when the count reaches zero.

ARC requires no garbage collector runtime, has no pause-style overhead, and is straightforward to implement. The per-operation cost (atomic increments and decrements) is acceptable for current workloads; performance-critical paths can be revisited in v0.5 / v1.

## 8.6 Future: RAII for `var` arrays

In v0.5 or v1, the memory management for `var` arrays will be upgraded from ARC to **RAII** (deterministic deallocation at scope exit). Because uniqueness guarantees a single owner, RAII can free a `var` array immediately when its binding goes out of scope, with no refcount overhead. This change is *invisible to user code* — only the runtime characteristic changes. `val` arrays continue to use ARC.
