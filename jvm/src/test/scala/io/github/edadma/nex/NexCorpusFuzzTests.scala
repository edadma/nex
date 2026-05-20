package io.github.edadma.nex

import org.scalatest.wordspec.AnyWordSpec

/** Mutation-based fuzz harness over [[NexProgramCorpus]].
  *
  * For each (case × no-op mutation), apply the mutation, run the
  * mutated program through the AOT compiler, assert the stdout
  * matches the case's `expected`. Since every mutation is a
  * documented semantic no-op (adds unused globals, unused
  * functions, etc.), divergence here means the codegen for the
  * surrounding scaffolding is leaking into the program's behaviour
  * — exactly the class of bug the divergence audits keep finding
  * one at a time.
  *
  * JVM-only: the AOT path needs `clang`. Pending corpus cases skip
  * (the parity divergence they document is separate from the
  * mutation surface).
  *
  * Cost: each mutation × case is one `clang -O1` invocation. With
  * 150 cases × 4 mutations that's 600 extra shell-outs; for the
  * full sweep budget ~3-5 minutes on top of the regular suite.
  * Adding new mutations multiplies the cost — keep the list small
  * and the rules genuinely no-op.
  */
class NexCorpusFuzzTests extends AnyWordSpec with NexParityBase:

  private val nonPendingCases =
    NexProgramCorpus.all.filter(_.pending.isEmpty)

  NexMutations.all.foreach { m =>
    s"mutation: ${m.name}" should {
      nonPendingCases.foreach { c =>
        s"${c.category} / ${c.name}" in {
          m.apply(c.src) match
            case Some(mutated) =>
              // Mutations preserve expected output by construction —
              // skip the interpreter re-run and compare AOT stdout
              // directly to the corpus case's pre-recorded expected.
              parityCheck(mutated, c.expected)
            case None =>
              cancel(s"mutation ${m.name} didn't fire on this source")
        }
      }
    }
  }
