package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Control flow (if/while/return, short-circuit and/or, unary minus)
  * and top-level val/var/const bindings + `__nex_init_globals`.
  */
class NexLLVMControlFlowTests extends AnyWordSpec with NexCodegenTestBase:

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

    "__nex_init_globals always emits — source prelude has top bindings (const pi, const e)" in {
      // Before ε.7, a user program with no `val`/`var`/`const` decls
      // would emit no init function. The source prelude now contributes
      // `const pi` and `const e` to auxDecls, which the codegen counts
      // as top-level bindings — so the init function and main's call
      // to it are always present.
      val ir = compile("""
        |def main() = print(42)
      """.stripMargin)
      ir should include("define void @__nex_init_globals")
      ir should include("call void @__nex_init_globals()")
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
