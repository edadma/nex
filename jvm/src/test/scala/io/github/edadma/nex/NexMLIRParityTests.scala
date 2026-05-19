package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Parity testbed for the MLIR backend. Same shape as
  * [[NexParityTests]] but driving [[NexMLIRCodegen]] through the MLIR
  * lowering pipeline rather than the direct LLVM-IR codegen. Starts
  * tiny: one program per milestone, expanding as features land.
  *
  * Shells out to `mlir-opt`, `mlir-translate`, `clang` from Homebrew
  * LLVM 22 (`/opt/homebrew/opt/llvm/bin` by default; override via
  * `NEX_LLVM_HOME`). Will silently misbehave on other LLVM major
  * versions because pass names move between them.
  */
class NexMLIRParityTests extends AnyWordSpec with NexMLIRParityBase:

  "milestone 1" should:
    "sum of literal rank-1 integer array" in parityCheck(
      "def main() = print(sum([1, 2, 3, 4, 5]))",
      "15\n",
    )
