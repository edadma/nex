package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Tuple and struct lowering: insertvalue / extractvalue chains,
  * destructuring, struct construction, struct field access, struct
  * print headers, struct return types, nested structs, arrays of
  * structs.
  */
class NexLLVMAggregatesTests extends AnyWordSpec with NexCodegenTestBase:

  "tuples" should {
    "lower (a, b) to insertvalue into the right anonymous struct type" in {
      val ir = compile("""
        |def main() =
        |  val p = (10, 20)
        |  val a, b = p
        |  print(a)
      """.stripMargin)
      // The tuple SSA value is built via two insertvalue calls into
      // `{ i64, i64 }` undef.
      ir should include("insertvalue { i64, i64 } undef, i64 10, 0")
      ir should include regex """insertvalue \{ i64, i64 \} %t\d+, i64 20, 1"""
    }

    "destructuring uses extractvalue from the receiver's struct type" in {
      val ir = compile("""
        |def main() =
        |  val p = (10, 20)
        |  val a, b = p
        |  print(a)
      """.stripMargin)
      ir should include regex """extractvalue \{ i64, i64 \} %t\d+, 0"""
    }

    "destructuring `val a, b = pair` lowers to per-name tuple projections" in {
      val ir = compile("""
        |def main() =
        |  val pair = (10, 20)
        |  val a, b = pair
        |  print(a)
        |  print(b)
      """.stripMargin)
      // The elaborator rewrites to two TTupleProj exprs — both should
      // extract from the pair's struct type.
      val ev = """extractvalue \{ i64, i64 \} %t\d+, 0""".r.findAllIn(ir).toList
      val ev1 = """extractvalue \{ i64, i64 \} %t\d+, 1""".r.findAllIn(ir).toList
      ev should not be empty
      ev1 should not be empty
    }

    "heterogeneous tuples use mixed element types in the struct" in {
      val ir = compile("""
        |def main() =
        |  val p = (42, "hi", true)
        |  print(p)
      """.stripMargin)
      // The struct type contains the three llvmType mappings: i64, ptr, i1.
      ir should include("{ i64, ptr, i1 }")
    }

    "function returning a tuple has the struct return type in its signature" in {
      val ir = compile("""
        |def pair() = (1, 2)
        |def main() = print(pair())
      """.stripMargin)
      ir should include("define { i64, i64 } @pair()")
      ir should include regex """ret \{ i64, i64 \} %t\d+"""
    }

    "int / int → real promotes both operands via sitofp before fdiv" in {
      val ir = compile("def main() = print(7 / 2)")
      ir should include("sitofp i64 7 to double")
      ir should include("sitofp i64 2 to double")
      ir should include("fdiv double")
    }
  }

  "structs" should {
    "struct construction lowers to insertvalue chain matching the field layout" in {
      val ir = compile("""
        |struct Point
        |  x: real
        |  y: real
        |def main() =
        |  val p = Point(3.0, 4.0)
        |  print(p.x)
      """.stripMargin)
      ir should include("insertvalue { double, double } undef, double")
      ir should include regex """insertvalue \{ double, double \} %t\d+, double 0x[A-Fa-f0-9]+, 1"""
    }

    "p.x lowers to extractvalue at index 0; p.y at index 1" in {
      val ir = compile("""
        |struct Point
        |  x: real
        |  y: real
        |def main() =
        |  val p = Point(3.0, 4.0)
        |  print(p.x)
        |  print(p.y)
      """.stripMargin)
      ir should include regex """extractvalue \{ double, double \} %t\d+, 0"""
      ir should include regex """extractvalue \{ double, double \} %t\d+, 1"""
    }

    "print(struct) emits `Name { field=value, ... }` headers" in {
      val ir = compile("""
        |struct Pair
        |  a: integer
        |  b: integer
        |def main() = print(Pair(1, 2))
      """.stripMargin)
      // The header string with `Name { ` and the closer ` }` get interned.
      ir should include regex """@\.str\.\d+ = private unnamed_addr constant \[\d+ x i8\] c"Pair \{ \\00""""
      ir should include regex """@\.str\.\d+ = private unnamed_addr constant \[\d+ x i8\] c" \}\\00""""
    }

    "function returning a struct has the struct return type in its signature" in {
      val ir = compile("""
        |struct Vec2
        |  x: real
        |  y: real
        |def make() = Vec2(1.0, 2.0)
        |def main() = print(make().x)
      """.stripMargin)
      ir should include("define { double, double } @make()")
    }

    "nested struct field access traverses two layers via two extractvalue ops" in {
      val ir = compile("""
        |struct Inner
        |  v: integer
        |struct Outer
        |  inner: Inner
        |def main() = print(Outer(Inner(42)).inner.v)
      """.stripMargin)
      // The outer extracts `inner` (field 0 of Outer), the inner
      // extracts `v` (field 0 of Inner). Each emits its own extractvalue.
      ir should include regex """extractvalue \{ \{ i64 \} \} %t\d+, 0"""
      ir should include regex """extractvalue \{ i64 \} %t\d+, 0"""
    }

    "array of structs uses the struct's full size as elem size" in {
      val ir = compile("""
        |struct Point
        |  x: real
        |  y: real
        |def main() =
        |  val pts = [Point(1.0, 2.0), Point(3.0, 4.0)]
        |  print(pts[0].x)
      """.stripMargin)
      // Point is 16 bytes (two doubles), so the alloc passes 16 for elem_size.
      ir should include("@__nex_arr1_alloc(i64 2, i64 16)")
    }
  }
