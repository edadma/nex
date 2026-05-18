---
title: "Appendix B: Open Design Questions"
summary: Small items not yet pinned to a single answer in the reference. Most original v0.1 questions have been settled and removed.
weight: 210
---

The original v0.1 question list has been mostly settled during implementation. Items that remain open:

- Exact `format` substitution rules for edge cases (escaped braces, mismatched arg counts, unknown specifiers in future format-string work).
- Whether unused `val` bindings produce a warning (currently silent — the compiler does not warn).

## Resolved

The following items from the original draft have been resolved by the implementation:

- **Shape-mismatch traps include source location.** Element-wise rank-1 and rank-2 operations trap with a message such as `element-wise '+': shape mismatch` plus the source position of the operator. Rank-1 length mismatches in `dot` and similar prelude functions include both lengths.
- **`for x in arr` iterates by value.** Each loop iteration binds `x` to a copy of the next element. This matches the value-semantics design of the language and avoids a "borrow that escapes the loop" footgun.
