package io.github.edadma.nex

/** Source-level program mutations for the fuzz runner. Each mutation
  * produces a syntactically distinct program from a corpus source
  * while preserving the expected stdout. The fuzz tests run the
  * mutated program through the AOT compiler and assert its output
  * still matches the corpus case's `expected` — any divergence
  * surfaces a codegen regression that depends on surrounding
  * program shape.
  *
  * Why no-op mutations only: they let the harness reuse each corpus
  * case's pre-recorded `expected` without re-running the interpreter,
  * keeping the test cost to one AOT compile per mutation. Output-
  * changing mutations are possible but would require the runner to
  * shell out twice per case (interpreter + AOT); see
  * `project_nex_parity_corpus.md` for the rationale.
  */
object NexMutations:

  /** A program transformation. `apply` returns `Some(mutated)` when
    * the rule fires for the given source, `None` when it doesn't
    * apply (e.g. the rule looks for a pattern the source doesn't
    * contain). Mutations must be SEMANTIC NO-OPS — the mutated
    * program must produce byte-identical stdout to the original.
    */
  trait Mutation:
    def name: String
    def apply(src: String): Option[String]

  /** Prepend a top-level `val` binding with a unique name. Tests that
    * the codegen for surrounding code is unaffected by additional
    * non-scalar / scalar globals — surfaced divergence #5 in the
    * 2026-05-19 audit (global aggregate init).
    */
  object PrependUnusedGlobalInt extends Mutation:
    val name = "prepend-unused-global-int"
    def apply(src: String): Option[String] =
      Some(s"val __fuzz_unused_int = 42\n\n$src")

  object PrependUnusedGlobalString extends Mutation:
    val name = "prepend-unused-global-string"
    def apply(src: String): Option[String] =
      Some(s"""val __fuzz_unused_str = "fuzz"\n\n$src""")

  /** Prepend a no-op function declaration. Tests that the codegen
    * for the rest of the module isn't sensitive to additional
    * function definitions (function-emission order, symbol-table
    * size, init-function placement).
    */
  object PrependUnusedFunction extends Mutation:
    val name = "prepend-unused-function"
    def apply(src: String): Option[String] =
      Some(s"def __fuzz_unused_fn() = 0\n\n$src")

  /** Append an unused function declaration AFTER `main`. Tests the
    * symmetric ordering case — codegen sometimes emits init code
    * inline-with-main and an additional def after `main` exercises
    * that path differently.
    */
  object AppendUnusedFunction extends Mutation:
    val name = "append-unused-function"
    def apply(src: String): Option[String] =
      Some(s"$src\n\ndef __fuzz_after() = 1\n")

  /** Master mutation list. Adding here auto-registers the mutation
    * across every corpus case in [[NexCorpusFuzzTests]].
    */
  val all: Seq[Mutation] = Seq(
    PrependUnusedGlobalInt,
    PrependUnusedGlobalString,
    PrependUnusedFunction,
    AppendUnusedFunction,
  )
