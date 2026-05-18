package io.github.edadma.nex

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

/** Shared test infrastructure for LLVM codegen tests. The `compile`
  * helper parses + elaborates + lowers a Nex source string and returns
  * the emitted IR text for inspection. We do not shell out to `clang`
  * here — invoking it has a real runtime cost on CI and would couple
  * every check to host toolchain availability. The
  * `examples/compile-hello/` example exercises the full clang
  * round-trip as an integration smoke.
  */
trait NexCodegenTestBase extends Matchers with Assertions:

  protected def compile(src: String): String =
    val ast = new NexParser().parseProgram(src) match
      case Right(p)  => p
      case Left(err) => fail(s"parse error: $err")
    val tp = new NexElaborator().elaborate(ast) match
      case Right(p)   => p
      case Left(errs) => fail(s"elab errors: ${errs.map(_.toString).mkString("; ")}")
    new NexLLVMCodegen().compile(tp)
