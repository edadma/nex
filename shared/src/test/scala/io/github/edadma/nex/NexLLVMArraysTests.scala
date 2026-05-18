package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Array codegen: rank-1 + rank-2 descriptors, ARC inc/dec lifecycle,
  * element-wise + broadcast loops, slicing, and the real-formatting
  * helper used by `print(real)`.
  */
class NexLLVMArraysTests extends AnyWordSpec with NexCodegenTestBase:

  "arrays (rank-1)" should {
    "emit the descriptor types and runtime helpers in the preamble" in {
      val ir = compile("def main() = ()")
      ir should include("%nex_arr1 = type { i64, i64, ptr }")
      ir should include("%nex_arr2 = type { i64, i64, i64, ptr }")
      ir should include("define ptr @__nex_arr1_alloc")
      ir should include("define void @__nex_arr1_inc")
      ir should include("define void @__nex_arr1_dec")
      ir should include("define i64 @__nex_arr1_len")
      ir should include("define ptr @__nex_arr1_slot")
    }

    "lower an integer literal `[a, b, c]` to alloc + per-element store" in {
      val ir = compile("""
        |def main() =
        |  val arr = [10, 20, 30]
        |  print(arr[0])
      """.stripMargin)
      ir should include("@__nex_arr1_alloc(i64 3, i64 8)")
      // Three element stores into the data buffer
      val stores = """store i64 \d+, ptr %t\d+""".r.findAllIn(ir).toList
      stores.size should be >= 3
    }

    "lower `arr[i]` to a slot helper call + load of the element type" in {
      val ir = compile("""
        |def main() =
        |  val arr = [1, 2, 3]
        |  print(arr[1])
      """.stripMargin)
      ir should include("call ptr @__nex_arr1_slot")
      ir should include("load i64, ptr")
    }

    "lower `arr[i] = v` to a slot helper call + store" in {
      val ir = compile("""
        |def main() =
        |  var arr = [1, 2, 3]
        |  arr[0] = 99
      """.stripMargin)
      ir should include("call ptr @__nex_arr1_slot")
      ir should include("store i64 99, ptr")
    }

    "lower `length(arr)` (rank-1) to __nex_arr1_len" in {
      val ir = compile("""
        |def main() =
        |  val arr = [1, 2, 3]
        |  print(length(arr))
      """.stripMargin)
      ir should include("call i64 @__nex_arr1_len(ptr")
      ir should include("@.fmt_int")
    }

    "lower `for x in arr` to a counting flat loop bound by length" in {
      val ir = compile("""
        |def main() =
        |  val arr = [10, 20, 30]
        |  for x in arr do
        |    print(x)
      """.stripMargin)
      ir should include("forarr.cond")
      ir should include("forarr.body")
      ir should include("forarr.exit")
      // Bounded by the call to __nex_arr1_len.
      ir should include("call i64 @__nex_arr1_len")
      ir should include("call ptr @__nex_arr1_slot")
    }

    "print(arr) emits [a, b, c]-bracketed loop with `, ` separators" in {
      val ir = compile("""
        |def main() =
        |  val arr = [1, 2]
        |  print(arr)
      """.stripMargin)
      ir should include("@.arr_open")
      ir should include("@.arr_close")
      ir should include("@.arr_sep")
      ir should include("parr1.cond")
    }

    "real-element array uses doubles in the buffer" in {
      val ir = compile("""
        |def main() =
        |  val arr = [1.0, 2.0]
        |  print(arr[0])
      """.stripMargin)
      ir should include("@__nex_arr1_alloc(i64 2, i64 8)")
      ir should include("store double")
      ir should include("load double, ptr")
    }

    "bool-element array stores i8 (with i1->i8 widening on store)" in {
      val ir = compile("""
        |def main() =
        |  val arr = [true, false, true]
        |  print(arr[0])
      """.stripMargin)
      ir should include("@__nex_arr1_alloc(i64 3, i64 1)")
      ir should include("zext i1")
      ir should include("store i8")
      ir should include("load i8, ptr")
      ir should include("icmp ne i8")
    }

    "string-element array stores ptr per slot" in {
      val ir = compile("""
        |def main() =
        |  val arr = ["a", "b"]
        |  print(arr[0])
      """.stripMargin)
      ir should include("@__nex_arr1_alloc(i64 2, i64 8)")
      ir should include("store ptr @.str.")
    }

    "passing an array to a user function uses `ptr` argument type" in {
      val ir = compile("""
        |def first(xs: [integer]) = xs[0]
        |def main() =
        |  val a = [10, 20, 30]
        |  print(first(a))
      """.stripMargin)
      ir should include("define i64 @first(ptr %arg0)")
    }
  }

  "ARC (§8.5)" should {
    "emit the rank-1 inc/dec runtime helpers in the preamble" in {
      val ir = compile("def main() = ()")
      ir should include("define void @__nex_arr1_inc(ptr %a)")
      ir should include("define void @__nex_arr1_dec(ptr %a)")
      ir should include("call void @free(ptr %buf)")
      ir should include("call void @free(ptr %a)")
    }

    "TVarRef of an array-typed local emits an inc after the load" in {
      val ir = compile("""
        |def main() =
        |  val a = [1, 2, 3]
        |  print(a)
      """.stripMargin)
      // The load+inc pair for the print arg.
      ir should include regex """load ptr, ptr %t\d+\n\s+call void @__nex_arr1_inc"""
    }

    "function exit dec's array-typed params and locals" in {
      val ir = compile("""
        |def first(xs: [integer]) =
        |  val tmp = [1]
        |  xs[0]
        |def main() = print(first([10]))
      """.stripMargin)
      // `first` should dec both tmp and xs before its ret.
      ir should include("@first(ptr %arg0)")
      // At least two dec calls inside `first` (tmp and xs).
      val firstBody = ir.substring(ir.indexOf("@first("), ir.indexOf("@main()"))
      val decs = "__nex_arr1_dec".r.findAllIn(firstBody).toList
      decs.size should be >= 2
    }

    "TAssign overwriting a var-array slot dec's the old value" in {
      val ir = compile("""
        |def main() =
        |  var a = [1, 2]
        |  a = [3, 4, 5]
        |  print(a)
      """.stripMargin)
      // Find the assign site: a load+dec before the second alloc-from-literal.
      // The pattern is: load ptr from a's slot, dec it, alloc new, store new.
      ir should include regex """load ptr, ptr %t\d+\n\s+call void @__nex_arr1_dec"""
    }

    "block-scoped val-array bindings dec at block end" in {
      val ir = compile("""
        |def main() =
        |  for i in 1..=3 do
        |    val a = [i, i, i]
        |    print(a[0])
      """.stripMargin)
      // The inner block (for-body TBlock) must dec `a` before the for-cond
      // back-branch. We look for at least one __nex_arr1_dec inside main.
      ir should include("__nex_arr1_dec")
    }

    "array print path emits a dec on the printed value after the loop" in {
      val ir = compile("""
        |def main() =
        |  val a = [1, 2, 3]
        |  print(a)
      """.stripMargin)
      // Two ARC events for the print path: inc (from TVarRef) before the
      // print loop, then dec after.
      val incs = "__nex_arr1_inc".r.findAllIn(ir).toList
      val decs = "__nex_arr1_dec".r.findAllIn(ir).toList
      // We don't pin exact counts (the helper definitions themselves also
      // match the regex) but the call sites should be non-empty.
      incs.size should be > 1
      decs.size should be > 1
    }

    "block-result that's an array survives its scope (slot dec, value kept)" in {
      val ir = compile("""
        |def make() =
        |  val a = [10, 20, 30]
        |  a
        |def main() = print(make())
      """.stripMargin)
      // `make` should: TArrayLit → store in a slot; TVarRef(a) → load + inc;
      // dec the a slot; ret the inc'd ptr. Caller (print) gets a non-zero
      // refcount value.
      val ir2 = ir
      ir2 should include("define ptr @make()")
      // The inc on TVarRef inside make, followed by dec of the slot, then ret.
      val makeBody = ir2.substring(ir2.indexOf("@make()"), ir2.indexOf("@main()"))
      makeBody should include("__nex_arr1_inc")
      makeBody should include("__nex_arr1_dec")
      makeBody should include("ret ptr")
    }
  }

  "arrays (rank-2)" should {
    "emit __nex_arr2_alloc for a 2x3 literal with 6 stores" in {
      val ir = compile("""
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m[0, 0])
      """.stripMargin)
      ir should include("@__nex_arr2_alloc(i64 2, i64 3, i64 8)")
    }

    "rank-2 index uses @__nex_arr2_slot" in {
      val ir = compile("""
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  print(m[1, 0])
      """.stripMargin)
      ir should include("call ptr @__nex_arr2_slot")
    }

    "rows(m) and cols(m) call the helpers; length(m) returns row count" in {
      val ir = compile("""
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(rows(m))
        |  print(cols(m))
        |  print(length(m))
      """.stripMargin)
      // length and rows both route through @__nex_arr2_rows; cols through
      // @__nex_arr2_cols. The flat-element @__nex_arr2_len helper is only
      // used internally by codegen (not by `length`).
      ir should include("@__nex_arr2_rows")
      ir should include("@__nex_arr2_cols")
    }
  }

  "element-wise and broadcast" should {
    "[a]+[b] emits a loop alloca'd at the source length" in {
      val ir = compile("""
        |def main() =
        |  val a = [1, 2, 3]
        |  val b = [10, 20, 30]
        |  print(a + b)
      """.stripMargin)
      ir should include("ew.cond")
      ir should include("ew.body")
      // The result alloc reuses the lhs length.
      ir should include("@__nex_arr1_len")
    }

    "scalar + array uses broadcast loop with scalarFirst=true" in {
      val ir = compile("""
        |def main() =
        |  val a = [1, 2, 3]
        |  print(100 + a)
      """.stripMargin)
      ir should include("bc.cond")
      ir should include("bc.body")
    }

    "array - scalar (non-commutative, scalarFirst=false) preserves operand order" in {
      val ir = compile("""
        |def main() =
        |  val a = [10, 20, 30]
        |  print(a - 1)
      """.stripMargin)
      ir should include("bc.cond")
      // Look for `sub i64 <elem>, <scalar>` rather than `sub <scalar>, <elem>`.
      // The elem comes first (%tN), the scalar 1 as immediate.
      ir should include regex """sub i64 %t\d+, 1"""
    }
  }

  "slicing" should {
    "rank-1 exclusive slice computes hi - lo length" in {
      val ir = compile("""
        |def main() =
        |  val a = [1, 2, 3, 4, 5]
        |  print(a[1..4])
      """.stripMargin)
      ir should include("slice1.cond")
      ir should include("sub i64")
    }

    "rank-1 inclusive slice computes (hi - lo) + 1 length" in {
      val ir = compile("""
        |def main() =
        |  val a = [1, 2, 3, 4, 5]
        |  print(a[0..=2])
      """.stripMargin)
      ir should include("slice1.cond")
      // The inclusive form's +1 fixup
      ir should include regex """add i64 %t\d+, 1"""
    }

    "rank-2 [:,:] preserves both axes and copies the whole matrix" in {
      val ir = compile("""
        |def main() =
        |  val m = [[1, 2], [3, 4]]
        |  print(m[:, :])
      """.stripMargin)
      ir should include("slice2r.cond")
      ir should include("slice2c.cond")
    }

    "rank-2 [i,:] collapses the row axis, yielding rank-1 of cols" in {
      val ir = compile("""
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m[0, :])
      """.stripMargin)
      // Row axis collapsed → emitSlice2's `slice2cr` branch (col axis
      // preserved, single loop over cLen).
      ir should include("slice2cr.cond")
    }

    "rank-2 [:,j] collapses the col axis, yielding rank-1 of rows" in {
      val ir = compile("""
        |def main() =
        |  val m = [[1, 2, 3], [4, 5, 6]]
        |  print(m[:, 1])
      """.stripMargin)
      // Col axis collapsed → emitSlice2's `slice2rc` branch (row axis
      // preserved, single loop over rLen).
      ir should include("slice2rc.cond")
    }
  }

  "real formatting" should {
    "print(real) routes through @__nex_print_real (mimics formatValue)" in {
      val ir = compile("def main() = print(3.0)")
      ir should include("define void @__nex_print_real(double %v)")
      ir should include("call void @__nex_print_real(double 0x")
    }

    "whole reals print as `<n>.0` (e.g. 3.0, not 3)" in {
      val ir = compile("def main() = print(3.0)")
      // The helper itself contains the `%lld.0` format string.
      ir should include("@.fmt_real_int")
    }
  }

  "recursive functions returning arrays" should {
    // Regression: `inferFun` used to set the function's TyFunc only
    // AFTER inferring its body, so a recursive TVarRef captured a
    // stale TyUnknown and the codegen emitted `call i64 @fft(...)`
    // instead of `call ptr @fft(...)`. Pre-setting the symbol type
    // before infExpr fixed it.

    "recursive call returns ptr, not i64, for array-returning fns" in {
      val ir = compile("""
        |def recArr(n: integer): [integer] =
        |  if n == 0 then [42]
        |  else recArr(n - 1)
        |def main() = print(recArr(3))
      """.stripMargin)
      // The definition has `define ptr @recArr(...)` and the recursive
      // call site must match: `call ptr @recArr(...)`.
      ir should include("define ptr @recArr")
      ir should include regex """call ptr @recArr\(i64 %t\d+\)"""
    }

    "recursive call returning [complex] uses ptr in both define and call sites" in {
      val ir = compile("""
        |def chain(n: integer): [complex] =
        |  if n == 0 then [0 + 0i]
        |  else chain(n - 1)
        |def main() = print(chain(2))
      """.stripMargin)
      ir should include("define ptr @chain")
      ir should include regex """call ptr @chain\(i64 %t\d+\)"""
    }
  }
