package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Lambda codegen coverage: `{ ptr, ptr }` closure layout, env malloc,
  * synthetic `__nex_lambda_<N>` signatures, indirect dispatch, val vs
  * var capture (ByVal vs ByRef), multi-arg lambdas, and that captured
  * values are observed at closure-construction time.
  */
class NexLLVMLambdasTests extends AnyWordSpec with NexCodegenTestBase:

  "lambdas" should {
    "TyFunc lowers to a `{ ptr, ptr }` closure value" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() = print(callIt(x -> x * 2, 3))
      """.stripMargin)
      // callIt's `f` is closure-typed, so its alloca + signature use `{ ptr, ptr }`.
      ir should include("define i64 @callIt({ ptr, ptr } %arg0, i64 %arg1)")
      ir should include("alloca { ptr, ptr }")
    }

    "zero-capture lambda construction uses a null env_ptr" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() = print(callIt(x -> x * 2, 3))
      """.stripMargin)
      // The lambda captures nothing, so env_ptr is null and only the
      // function pointer is meaningful.
      ir should include regex """insertvalue \{ ptr, ptr \} undef, ptr @__nex_lambda_\d+, 0"""
      ir should include regex """insertvalue \{ ptr, ptr \} %t\d+, ptr null, 1"""
    }

    "zero-capture lambda body needs no malloc for its env" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() = print(callIt(x -> x + 1, 3))
      """.stripMargin)
      // We don't malloc anything for a no-capture lambda — verify by
      // counting mallocs outside the array runtime helpers.
      // (Negative assertion: no "call ptr @malloc" appears inside @main.)
      val mainBody = ir.linesIterator
        .dropWhile(l => !l.startsWith("define i32 @main"))
        .takeWhile(l => !l.startsWith("}"))
        .mkString("\n")
      mainBody should not include "call ptr @malloc"
    }

    "synthetic lambda function gets the `__nex_lambda_<N>(ptr %env, ...)` signature" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() = print(callIt(x -> x + 1, 3))
      """.stripMargin)
      ir should include("define i64 @__nex_lambda_0(ptr %env, i64 %arg0)")
    }

    "closure call extracts fn_ptr + env_ptr and dispatches indirectly" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() = print(callIt(x -> x + 1, 3))
      """.stripMargin)
      // Inside callIt, the `f(x)` call extracts both fields and runs an
      // indirect call with env_ptr prepended.
      ir should include regex """extractvalue \{ ptr, ptr \} %t\d+, 0"""
      ir should include regex """extractvalue \{ ptr, ptr \} %t\d+, 1"""
      ir should include regex """call i64 \(ptr, i64\) %t\d+\(ptr %t\d+, i64 %t\d+\)"""
    }

    "val capture stores the captured value into a heap-allocated env" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  val k = 10
        |  print(callIt(x -> x + k, 5))
      """.stripMargin)
      // env is a single-field `{ i64 }` struct, sizeof = 8. Allocation
      // goes through the refcounted `__nex_env_alloc` helper (16-byte
      // negative-offset header storing rc + dtor). No refcounted
      // captures here, so the dtor operand is null and the dec path
      // falls through to plain free.
      ir should include("call ptr @__nex_env_alloc(i64 8, ptr null)")
      ir should include regex """getelementptr inbounds \{ i64 \}, ptr %env, i32 0, i32 0"""
    }

    "var capture stores a pointer (ByRef) into the env" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  var k = 1
        |  print(callIt(x -> x + k, 10))
        |  k = 100
        |  print(callIt(x -> x + k, 10))
      """.stripMargin)
      // ByRef capture: env field is `ptr`, so the env type is `{ ptr }`.
      // The lambda body loads through TWO pointers: env → cell ptr → value.
      ir should include regex """getelementptr inbounds \{ ptr \}, ptr %env, i32 0, i32 0"""
      // Inside @main, we store the parent alloca's pointer into env[0].
      ir should include regex """store ptr %t\d+, ptr %t\d+"""
    }

    "multi-arg lambda gets one env param + N regular params" in {
      val ir = compile("""
        |def apply2(f: ((integer, integer) -> integer), a: integer, b: integer) = f(a, b)
        |def main() = print(apply2((x, y) -> x + y, 4, 5))
      """.stripMargin)
      ir should include("define i64 @__nex_lambda_0(ptr %env, i64 %arg0, i64 %arg1)")
      // The closure call uses the 2-arg signature `(ptr, i64, i64)`.
      ir should include regex """call i64 \(ptr, i64, i64\) %t\d+\(ptr %t\d+, i64 %t\d+, i64 %t\d+\)"""
    }

    "multiple lambdas in the same function get distinct synthetic ids" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  print(callIt(x -> x + 1, 3))
        |  print(callIt(x -> x * 10, 3))
      """.stripMargin)
      ir should include("define i64 @__nex_lambda_0(")
      ir should include("define i64 @__nex_lambda_1(")
    }

    "captured val is observed at closure-construction time, not call time" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  val k = 10
        |  print(callIt(x -> x + k, 5))
      """.stripMargin)
      // The store-into-env happens INSIDE @main (where k is in scope),
      // and the env_ptr is passed to callIt as part of the closure value.
      val mainBody = ir.linesIterator
        .dropWhile(l => !l.startsWith("define i32 @main"))
        .takeWhile(l => !l.startsWith("}"))
        .mkString("\n")
      mainBody should include regex """getelementptr inbounds \{ i64 \}, ptr %t\d+, i32 0, i32 0"""
      mainBody should include regex """store i64 %t\d+, ptr %t\d+"""
    }

    "closure env is released after the indirect dispatch" in {
      // emitClosureCall extracts env_ptr, calls the function, then dec's
      // the env. The capturing lambda's env_alloc rc=1 is balanced by
      // this dec when the value is dispatched and discarded.
      val ir = compile("""
        |def callIt(f: (integer -> integer), x: integer) = f(x)
        |def main() =
        |  val k = 10
        |  print(callIt(x -> x + k, 5))
      """.stripMargin)
      ir should include("call ptr @__nex_env_alloc(i64 8, ptr null)")
      ir should include regex """call void @__nex_env_dec\(ptr %t\d+\)"""
    }

    "captured string capture dtor walks the env and dec's the string" in {
      val ir = compile("""
        |def callIt(f: (integer -> string)) = f(0)
        |def main() =
        |  val s = "hi" + "!"
        |  print(callIt(_ -> s))
      """.stripMargin)
      // env has one ByVal string capture, so the alloc passes a dtor
      // operand pointing at the per-lambda dtor.
      ir should include regex """call ptr @__nex_env_alloc\(i64 8, ptr @__nex_lambda_\d+_env_dtor\)"""
      // The dtor function itself dec's the captured string and frees
      // the env header.
      ir should include regex """define void @__nex_lambda_\d+_env_dtor\(ptr %env\)"""
      val dtor = ir.substring(ir.indexOf("_env_dtor(ptr %env)"))
      dtor should include("call void @__nex_str_dec(ptr ")
      dtor should include("call void @free(ptr ")
    }

    "non-refcounted captures still use the plain free path (no dtor)" in {
      val ir = compile("""
        |def callIt(f: (integer -> integer)) = f(0)
        |def main() =
        |  val k = 7
        |  print(callIt(_ -> k))
      """.stripMargin)
      ir should include("call ptr @__nex_env_alloc(i64 8, ptr null)")
      ir should not include "_env_dtor"
    }
  }
