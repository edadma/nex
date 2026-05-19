# MLIR PoC — milestone 1 pipeline validation

Hand-written MLIR proving that `def main() = print(sum([1, 2, 3, 4, 5]))`
lowers through MLIR end-to-end and prints `15`, matching the Nex
interpreter.

## Working pipeline (Homebrew LLVM 22.1.3)

```sh
LLVM=/opt/homebrew/opt/llvm/bin

$LLVM/mlir-opt sum5.mlir \
  --one-shot-bufferize="bufferize-function-boundaries" \
  --convert-linalg-to-loops \
  --convert-scf-to-cf \
  --finalize-memref-to-llvm \
  --convert-arith-to-llvm \
  --convert-func-to-llvm \
  --convert-cf-to-llvm \
  --reconcile-unrealized-casts \
  -o sum5.lowered.mlir

$LLVM/mlir-translate --mlir-to-llvmir sum5.lowered.mlir -o sum5.ll

clang -O1 -o sum5 sum5.ll runtime.c

./sum5   # => 15
```

## What this validates

- `tensor.from_elements` for array literals.
- `linalg.reduce` with a `arith.addi` body for sum.
- `tensor.extract` for pulling a scalar out of a 0-d tensor.
- `--one-shot-bufferize` with `bufferize-function-boundaries` correctly
  rewrites tensors into `memref` allocations.
- `--convert-linalg-to-loops` materialises the reduction as an scf loop.
- The full lowering chain through `--reconcile-unrealized-casts` ends
  in pure LLVM dialect.
- `mlir-translate --mlir-to-llvmir` produces LLVM IR text consumable
  by clang.
- External C functions (`nex_print_i64` in `runtime.c`) link cleanly.

## What's NOT in this PoC

- No Scala-side codegen yet; the .mlir was hand-written.
- No parity-harness integration.
- Only one program (sum of integer literal array). Strings, closures,
  reals, complex, tuples, structs, HOFs, value-to-string all deferred.

## Why this layout

Keeping the runtime as a C source file (compiled fresh by clang each
build) rather than embedding it as MLIR/LLVM-text helpers in a preamble
means we can write printing / I/O helpers in a normal language and
not duplicate the work in two IR systems. This file is the
moral equivalent of `NexLLVMPreamble.scala`'s string-literal
`__nex_print_real` etc.

## Next: milestone 1 proper

See `project_nex_mlir_milestone1.md` in the auto-memory system for the
design of the Scala-side NexMLIRCodegen trait.
