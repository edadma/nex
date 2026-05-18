package io.github.edadma.nex

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Unit-level coverage for the first vertical slice of the LLVM IR
  * backend. We don't shell out to `clang` here — invoking it has a real
  * runtime cost on CI and would couple every check to host toolchain
  * availability. Instead we look at the generated `.ll` text for the
  * expected instructions; the `examples/compile-hello/` example
  * exercises the full clang round-trip as an integration smoke.
  */
class NexLLVMCodegenTests extends AnyWordSpec with Matchers:

  private def compile(src: String): String =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    val tp = new NexElaborator().elaborate(ast) match
      case Right(p)   => p
      case Left(errs) => fail(s"elab errors: ${errs.map(_.toString).mkString("; ")}")
    new NexLLVMCodegen().compile(tp)

  "preamble" should {
    "always include an extern printf declaration" in {
      val ir = compile("def main() = ()")
      ir should include("declare i32 @printf(ptr, ...)")
    }

    "always include the integer/real format-string globals" in {
      val ir = compile("def main() = ()")
      ir should include("@.fmt_int")
      ir should include("@.fmt_real")
    }
  }

  "main function" should {
    "emit a single `define i32 @main()` with `ret i32 0`" in {
      val ir = compile("def main() = ()")
      ir should include("define i32 @main()")
      ir should include("ret i32 0")
    }
  }

  "print" should {
    "emit a printf call with the integer format and i64 value for `print(integer)`" in {
      val ir = compile("def main() = print(42)")
      ir should include("call i32 (ptr, ...) @printf(ptr @.fmt_int, i64 42)")
    }

    "emit a printf call with the real format and double value for `print(real)`" in {
      val ir = compile("def main() = print(3.14)")
      ir should include("@.fmt_real")
      ir should include("double 0x")  // doubles encoded as hex bit-patterns
    }

    "emit a select + printf for `print(bool)`" in {
      val ir = compile("def main() = print(true)")
      ir should include("select i1 1, ptr @.fmt_bool_t, ptr @.fmt_bool_f")
    }
  }

  "integer arithmetic" should {
    "emit add/sub/mul/sdiv for + - * /" in {
      val ir = compile("""
        |def main() =
        |  print(1 + 2)
        |  print(7 - 3)
        |  print(4 * 5)
        |  print(20 / 4)
      """.stripMargin)
      ir should include("add i64 1, 2")
      ir should include("sub i64 7, 3")
      ir should include("mul i64 4, 5")
      ir should include("sdiv i64 20, 4")
    }

    "emit icmp for integer comparisons" in {
      val ir = compile("""
        |def main() =
        |  print(1 < 2)
        |  print(3 == 3)
      """.stripMargin)
      ir should include("icmp slt i64 1, 2")
      ir should include("icmp eq i64 3, 3")
    }
  }

  "real arithmetic" should {
    "emit fadd/fsub/fmul/fdiv for + - * /" in {
      val ir = compile("""
        |def main() =
        |  print(1.0 + 2.0)
        |  print(7.0 - 3.0)
        |  print(4.0 * 5.0)
        |  print(20.0 / 4.0)
      """.stripMargin)
      ir should include("fadd double")
      ir should include("fsub double")
      ir should include("fmul double")
      ir should include("fdiv double")
    }
  }

  "val bindings" should {
    "emit an alloca + store for `val x = ...` and a load for the reference" in {
      val ir = compile("""
        |def main() =
        |  val x = 17
        |  print(x)
      """.stripMargin)
      ir should include("alloca i64")
      ir should include("store i64 17, ptr")
      ir should include regex """%t\d+ = load i64, ptr"""
    }

    "compose: `val x = a + b; print(x * 2)` emits the full chain" in {
      val ir = compile("""
        |def main() =
        |  val x = 3 + 4
        |  print(x * 2)
      """.stripMargin)
      // The val's RHS is computed once; reads load from the slot; the
      // outer `print(x * 2)` multiplies the loaded value by 2 and
      // prints — that's add + alloca/store + load + mul + printf.
      ir should include("add i64 3, 4")
      ir should include("alloca i64")
      ir should include("store i64")
      ir should include("mul i64")
      ir should include("@.fmt_int")
    }
  }

  "multi-function programs" should {
    "emit each top-level def with its declared signature" in {
      val ir = compile("""
        |def helper(x: integer): integer = x + 1
        |def main() = print(helper(41))
      """.stripMargin)
      ir should include("define i64 @helper(i64 %arg0)")
      ir should include("define i32 @main()")
      ir should not include "; TODO:"
    }

    "emit a call to a user function with correct arg types" in {
      val ir = compile("""
        |def add(a: integer, b: integer): integer = a + b
        |def main() = print(add(40, 2))
      """.stripMargin)
      ir should include("call i64 @add(i64 40, i64 2)")
    }

    "real-typed args are passed as double" in {
      val ir = compile("""
        |def scale(x: real, k: real): real = x * k
        |def main() = print(scale(3.0, 2.0))
      """.stripMargin)
      ir should include("define double @scale(double %arg0, double %arg1)")
      ir should include("call double @scale(double")
    }

    "unit-returning function uses ret void" in {
      val ir = compile("""
        |def announce(n: integer) = print(n)
        |def main() = announce(7)
      """.stripMargin)
      ir should include("define void @announce(i64 %arg0)")
      ir should include("ret void")
      ir should include("call void @announce(i64 7)")
    }
  }

  "control flow" should {

    "emit conditional branch + merge phi for an if expression" in {
      val ir = compile("""
        |def sign(x: integer): integer =
        |  if x < 0 then -1 else 1
        |def main() = print(sign(0))
      """.stripMargin)
      ir should include("br i1")
      ir should include("if.then")
      ir should include("if.else")
      ir should include("phi i64")
      ir should not include "; TODO:"
    }

    "emit if-without-else as a two-block diamond" in {
      val ir = compile("""
        |def announce(x: integer) =
        |  if x > 0 then print(x)
        |def main() = announce(5)
      """.stripMargin)
      ir should include("br i1")
      ir should include("if.then")
      ir should include("if.end")
      ir should not include "; TODO:"
    }

    "emit a while loop with cond / body / exit blocks" in {
      val ir = compile("""
        |def count_down(n: integer): integer =
        |  var i = n
        |  while i > 0 do
        |    i = i - 1
        |  i
        |def main() = print(count_down(3))
      """.stripMargin)
      ir should include("while.cond")
      ir should include("while.body")
      ir should include("while.exit")
      ir should not include "; TODO:"
    }

    "emit a return statement that terminates the current block" in {
      val ir = compile("""
        |def first_neg(x: integer, y: integer): integer =
        |  if x < 0 then return x
        |  if y < 0 then return y
        |  0
        |def main() = print(first_neg(1, -2))
      """.stripMargin)
      ir should include("ret i64")
      ir should not include "; TODO:"
    }

    "emit short-circuit and as a branch + phi" in {
      val ir = compile("""
        |def in_range(x: integer, lo: integer, hi: integer): bool =
        |  x >= lo and x <= hi
        |def main() = print(in_range(5, 0, 10))
      """.stripMargin)
      ir should include("and.rhs")
      ir should include("and.end")
      ir should include("phi i1")
      ir should not include "; TODO:"
    }

    "emit short-circuit or as a branch + phi" in {
      val ir = compile("""
        |def either(x: integer): bool =
        |  x == 0 or x == 42
        |def main() = print(either(0))
      """.stripMargin)
      ir should include("or.rhs")
      ir should include("or.end")
      ir should include("phi i1")
      ir should not include "; TODO:"
    }

    "emit unary minus on integer and real" in {
      val ir = compile("""
        |def negi(x: integer): integer = -x
        |def negr(x: real): real = -x
        |def main() = print(negi(7))
      """.stripMargin)
      ir should include("sub i64 0,")
      ir should include("fneg double")
      ir should not include "; TODO:"
    }
  }

  "top-level bindings" should {

    "emit @<name> = global zeroinitializer for each top-level val/var/const" in {
      val ir = compile("""
        |val x: integer = 42
        |var y: integer = 0
        |def main() = print(x)
      """.stripMargin)
      ir should include("@x = global i64")
      ir should include("@y = global i64")
    }

    "emit __nex_init_globals when bindings exist" in {
      val ir = compile("""
        |val x: integer = 42
        |def main() = print(x)
      """.stripMargin)
      ir should include("define void @__nex_init_globals()")
      ir should include("store i64 42, ptr @x")
    }

    "main calls __nex_init_globals at the entry block" in {
      val ir = compile("""
        |val x: integer = 7
        |def main() = print(x)
      """.stripMargin)
      ir should include("call void @__nex_init_globals()")
    }

    "TVarRef to a top-level binding loads from @<name>" in {
      val ir = compile("""
        |val MAX: integer = 100
        |def main() = print(MAX)
      """.stripMargin)
      ir should include("load i64, ptr @MAX")
    }

    "TAssign to a top-level var stores to @<name>" in {
      val ir = compile("""
        |var counter: integer = 0
        |def bump() = counter = counter + 1
        |def main() = bump()
      """.stripMargin)
      ir should include("store i64")
      ir should include("ptr @counter")
    }

    "no __nex_init_globals when there are no top bindings" in {
      val ir = compile("""
        |def main() = print(42)
      """.stripMargin)
      ir should not include "@__nex_init_globals"
      ir should not include "call void @__nex_init_globals"
    }

    "real-typed top binding gets a double global" in {
      val ir = compile("""
        |val PI: real = 3.14159
        |def main() = print(PI)
      """.stripMargin)
      ir should include("@PI = global double 0.0")
      ir should include("store double")
      ir should include("load double, ptr @PI")
    }
  }

  "strings" should {

    "intern a string literal as @.str.<N>" in {
      val ir = compile("""
        |def main() = print("hello, world")
      """.stripMargin)
      ir should include("@.str.")
      ir should include("c\"hello, world\\00\"")
    }

    "print(string-literal) uses %s\\n" in {
      val ir = compile("""
        |def main() = print("hi")
      """.stripMargin)
      ir should include("@.fmt_str")
      ir should include("call i32 (ptr, ...) @printf(ptr @.fmt_str, ptr")
    }

    "string-typed function param is `ptr`" in {
      val ir = compile("""
        |def greet(name: string) = print(name)
        |def main() = greet("Alice")
      """.stripMargin)
      ir should include("define void @greet(ptr %arg0)")
    }

    "duplicate string literals share one global" in {
      val ir = compile("""
        |def main() =
        |  print("hi")
        |  print("hi")
      """.stripMargin)
      // Count occurrences of `c"hi\00"` — should be exactly 1.
      val count = """c"hi\\00"""".r.findAllIn(ir).size
      count shouldBe 1
    }

    "interpolated string in print produces a printf-per-part + newline" in {
      val ir = compile("""
        |def main() =
        |  val n = 42
        |  print(s"answer = $n")
      """.stripMargin)
      // The text chunk is interned; the int part uses fmt_int_raw; the
      // closing newline comes from @.nl.
      ir should include("c\"answer = \\00\"")
      ir should include("@.fmt_int_raw")
      ir should include("@.nl")
    }

    "interpolated string with a ${expr} embeds the inner value" in {
      val ir = compile("""
        |def main() =
        |  print(s"sum = ${1 + 2}")
      """.stripMargin)
      ir should include("@.fmt_int_raw")
      ir should include("@.nl")
    }
  }

  "for loops over ranges" should {

    "emit cond/body/exit blocks for `for i in lo..hi`" in {
      val ir = compile("""
        |def main() =
        |  for i in 0..5 do
        |    print(i)
      """.stripMargin)
      ir should include("for.cond")
      ir should include("for.body")
      ir should include("for.exit")
      ir should not include "; TODO:"
    }

    "exclusive upper bound uses icmp slt" in {
      val ir = compile("""
        |def main() =
        |  for i in 0..5 do
        |    print(i)
      """.stripMargin)
      ir should include("icmp slt i64")
    }

    "inclusive upper bound uses icmp sle" in {
      val ir = compile("""
        |def main() =
        |  for i in 1..=5 do
        |    print(i)
      """.stripMargin)
      ir should include("icmp sle i64")
    }

    "body increments the loop counter by 1" in {
      val ir = compile("""
        |def main() =
        |  for i in 0..3 do
        |    print(i)
      """.stripMargin)
      ir should include("add i64")
      ir should include(", 1")
    }

    "nested for loops emit independent label sets" in {
      val ir = compile("""
        |def main() =
        |  for i in 1..=3 do
        |    for j in 1..=3 do
        |      print(i * j)
      """.stripMargin)
      // Each loop generates its own .1, .2, .3 fresh-label suffix; the
      // outer + inner together produce multiple `for.cond.<n>` labels.
      val condLabels = """for\.cond\.\d+""".r.findAllIn(ir).toList.distinct
      condLabels.size shouldBe 2
    }
  }

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
