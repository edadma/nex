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

  "milestone 2" should:
    "sum of literal rank-1 real array" in parityCheck(
      "def main() = print(sum([1.0, 2.0, 3.0, 4.0, 5.0]))",
      "15.0\n",
    )
    "sum of literal rank-1 real array with a negative element" in parityCheck(
      "def main() = print(sum([10.0, -3.0, 2.0]))",
      "9.0\n",
    )

  "milestone 3" should:
    "sum of element-wise add of two integer array literals" in parityCheck(
      "def main() = print(sum([1, 2, 3] + [10, 20, 30]))",
      "66\n",
    )
    "sum of element-wise add of two real array literals" in parityCheck(
      "def main() = print(sum([1.0, 2.0, 3.0] + [10.0, 20.0, 30.0]))",
      "66.0\n",
    )
    "val-bound integer arrays in element-wise add" in parityCheck(
      """
        |def main() =
        |  val a = [1, 2, 3, 4, 5]
        |  val b = [10, 20, 30, 40, 50]
        |  print(sum(a + b))
      """.stripMargin,
      "165\n",
    )
    "val-bound real arrays in element-wise subtract" in parityCheck(
      """
        |def main() =
        |  val a = [10.0, 20.0, 30.0]
        |  val b = [1.0, 2.0, 3.0]
        |  print(sum(a - b))
      """.stripMargin,
      "54.0\n",
    )
    "val-bound real arrays in element-wise multiply" in parityCheck(
      """
        |def main() =
        |  val a = [1.0, 2.0, 3.0]
        |  val b = [4.0, 5.0, 6.0]
        |  print(sum(a * b))
      """.stripMargin,
      "32.0\n",
    )

  "milestone 4 — rank-1 array print" should:
    "print integer array literal" in parityCheck(
      "def main() = print([1, 2, 3])",
      "[1, 2, 3]\n",
    )
    "print single-element integer array literal" in parityCheck(
      "def main() = print([42])",
      "[42]\n",
    )
    "print real array literal with whole numbers" in parityCheck(
      "def main() = print([1.0, 2.0, 3.0])",
      "[1.0, 2.0, 3.0]\n",
    )
    "print result of element-wise integer add" in parityCheck(
      "def main() = print([1, 2, 3] + [10, 20, 30])",
      "[11, 22, 33]\n",
    )
    "print val-bound real element-wise multiply" in parityCheck(
      """
        |def main() =
        |  val a = [1.0, 2.0, 3.0]
        |  val b = [4.0, 5.0, 6.0]
        |  print(a * b)
      """.stripMargin,
      "[4.0, 10.0, 18.0]\n",
    )

  "milestone 4 — rank-2 array print" should:
    "print 2x3 integer matrix literal" in parityCheck(
      "def main() = print([[1, 2, 3], [4, 5, 6]])",
      "[[1, 2, 3], [4, 5, 6]]\n",
    )
    "print 2x2 real matrix literal" in parityCheck(
      "def main() = print([[1.0, 2.0], [3.0, 4.0]])",
      "[[1.0, 2.0], [3.0, 4.0]]\n",
    )
    "print 1x1 integer matrix literal" in parityCheck(
      "def main() = print([[7]])",
      "[[7]]\n",
    )
    "print val-bound 2x2 matrix" in parityCheck(
      """
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  print(m)
      """.stripMargin,
      "[[1, 2], [3, 4]]\n",
    )

  "milestone 5 — matmul" should:
    "2x2 integer matmul via @" in parityCheck(
      "def main() = print([[1, 2], [3, 4]] @ [[5, 6], [7, 8]])",
      "[[19, 22], [43, 50]]\n",
    )
    "2x2 real matmul via @" in parityCheck(
      "def main() = print([[1.0, 2.0], [3.0, 4.0]] @ [[5.0, 6.0], [7.0, 8.0]])",
      "[[19.0, 22.0], [43.0, 50.0]]\n",
    )
    "2x3 by 3x2 integer matmul via @" in parityCheck(
      "def main() = print([[1, 2, 3], [4, 5, 6]] @ [[7, 8], [9, 10], [11, 12]])",
      "[[58, 64], [139, 154]]\n",
    )
    "matmul(a, b) prelude form" in parityCheck(
      """
        |def main() =
        |  val a = [[1, 2], [3, 4]]
        |  val b = [[5, 6], [7, 8]]
        |  print(matmul(a, b))
      """.stripMargin,
      "[[19, 22], [43, 50]]\n",
    )
    "sum of matmul result" in parityCheck(
      """
        |def main() =
        |  val a = [[1, 2], [3, 4]]
        |  val b = [[1, 0], [0, 1]]
        |  print(sum(a @ b))
      """.stripMargin,
      "10\n",
    )
    "identity matmul leaves matrix unchanged" in parityCheck(
      """
        |def main() =
        |  val a = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
        |  val i = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]
        |  print(a @ i)
      """.stripMargin,
      "[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]\n",
    )
