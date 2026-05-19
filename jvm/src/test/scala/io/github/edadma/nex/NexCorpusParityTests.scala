package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Mandatory-parity runner over [[NexProgramCorpus]].
  *
  * Every case in the shared corpus is run through both the interpreter
  * and the AOT compiler, with stdouts compared byte-for-byte via
  * [[NexParityBase.parityCheck]]. JVM-only because the AOT path needs
  * `clang`.
  *
  * Adding a case to the corpus auto-registers a test here — no parity
  * coverage gap is possible by forgetting to mirror an interpreter
  * test. New programs that aren't AOT-compatible should live in
  * `NexInterpreterTests` instead.
  */
class NexCorpusParityTests extends AnyWordSpec with NexParityBase:

  NexProgramCorpus.all.groupBy(_.category).toSeq.sortBy(_._1).foreach { case (category, cases) =>
    category should {
      cases.foreach { c =>
        c.pending match
          case Some(reason) =>
            c.name ignore parityCheck(c.src, c.expected)
            // Note: ignore prints the reason via the test name. The
            // TODO sits in the corpus file so a `grep -n "pending ="`
            // listing is the authoritative open-divergence list.
            // (`reason` itself is referenced here so callers don't see
            // an unused-binding warning.)
            val _ = reason
          case None =>
            c.name in parityCheck(c.src, c.expected)
      }
    }
  }
