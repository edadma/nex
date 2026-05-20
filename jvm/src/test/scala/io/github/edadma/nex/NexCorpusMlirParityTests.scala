package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Cross-backend parity runner for the MLIR-opted-in subset of
  * [[NexProgramCorpus]]. Every case with `mlir = true` is run through
  * the interpreter, the LLVM backend, AND the MLIR backend, with all
  * three stdouts compared byte-for-byte.
  *
  * The number of `mlir = true` cases is the strangler-fig progress
  * bar for the MLIR backend's coverage of the corpus — flipping a
  * case here means MLIR can now lower it without divergence from the
  * spec oracle (the interpreter) or the established AOT path (LLVM).
  *
  * `pending` is honoured: a `pending = Some(reason)` case stays
  * ignored even when `mlir = true`. The MLIR-specific milestone
  * tests in [[NexMLIRParityTests]] continue to cover surface that
  * isn't yet expressible in the parity corpus shape.
  */
class NexCorpusMlirParityTests extends AnyWordSpec with NexParityBase:

  private val mlirCases = NexProgramCorpus.all.filter(_.mlir)

  mlirCases.groupBy(_.category).toSeq.sortBy(_._1).foreach { case (category, cases) =>
    category should {
      cases.foreach { c =>
        c.pending match
          case Some(reason) =>
            c.name ignore parityCheckOn(Seq(LlvmBackend, MlirBackend), c.src, c.expected)
            val _ = reason
          case None =>
            c.name in parityCheckOn(Seq(LlvmBackend, MlirBackend), c.src, c.expected)
      }
    }
  }
