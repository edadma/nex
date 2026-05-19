// Hand-written MLIR for: def main() = print(sum([1, 2, 3, 4, 5]))
//
// Goal: validate that this lowers through mlir-opt + mlir-translate
// into LLVM IR that links against a tiny C runtime providing
// nex_print_i64, and the resulting binary prints "15\n".

func.func private @nex_print_i64(i64)

func.func @main() -> i32 {
  %c0 = arith.constant 0 : i32
  %c1 = arith.constant 1 : i64
  %c2 = arith.constant 2 : i64
  %c3 = arith.constant 3 : i64
  %c4 = arith.constant 4 : i64
  %c5 = arith.constant 5 : i64

  // Build the literal tensor [1, 2, 3, 4, 5] : tensor<5xi64>.
  %arr = tensor.from_elements %c1, %c2, %c3, %c4, %c5 : tensor<5xi64>

  // Reduce with `+`, init 0. linalg.reduce takes an init operand that
  // also dictates the output element type and shape (here: a 0-d
  // tensor<i64>).
  %init_e = arith.constant 0 : i64
  %init = tensor.from_elements %init_e : tensor<i64>
  %sum_t = linalg.reduce ins(%arr : tensor<5xi64>) outs(%init : tensor<i64>) dimensions = [0]
    (%in: i64, %acc: i64) {
      %s = arith.addi %in, %acc : i64
      linalg.yield %s : i64
    }

  // Extract the scalar from the 0-d tensor.
  %sum = tensor.extract %sum_t[] : tensor<i64>

  func.call @nex_print_i64(%sum) : (i64) -> ()
  func.return %c0 : i32
}
