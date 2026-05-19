package io.github.edadma.nex

import io.github.edadma.cross_platform
import io.github.edadma.path.Path

/** Locates the Nex sysroot's prelude directory at runtime. The prelude is
  * the source-form standard library (see [[project-nex-prelude-and-generics-roadmap]]);
  * Stage 1 ships a placeholder while Stages 2-5 progressively fill it in.
  *
  * Discovery is layered, highest priority first:
  *
  *   1. An explicit override (the `--prelude-path` CLI flag).
  *   2. `NEX_HOME` environment variable: `$NEX_HOME/lib/prelude/`.
  *   3. JVM system property `nex.prelude.path`. Set by `build.sbt` so
  *      `sbt test` runs use the in-repo prelude without per-test wiring.
  *   4. Walk up from the current working directory; at each ancestor,
  *      check for a `prelude/` sibling, then `lib/prelude/`.
  *
  * If none of the four locate a directory, returns `None`. Callers fall
  * back to today's compiler-internal prelude (the source prelude is
  * empty during Stages 1-2, so the silent fallback is intentional).
  */
object NexSysroot:

  /** Find the prelude directory, or `None` if no source prelude is
    * configured on this machine. The result is an absolute path to a
    * directory that exists; callers can list its `.nex` files directly.
    */
  def findPreludeRoot(overridePath: Option[String]): Option[String] =
    overridePath.filter(cross_platform.isDirectory)
      .orElse(envVar("NEX_HOME").map(home => pathJoin(home, "lib", "prelude")).filter(cross_platform.isDirectory))
      .orElse(sysProp("nex.prelude.path").filter(cross_platform.isDirectory))
      .orElse(walkUpForPrelude(cross_platform.getCurrentDirectory))

  /** Read an environment variable across JVM/JS/Native. `sys.env` is
    * supported on all three under the `cross_platform` constraint (used
    * elsewhere in this module — see `Cli.doCompileMlir`).
    */
  private def envVar(name: String): Option[String] =
    sys.env.get(name).filter(_.nonEmpty)

  /** Read a JVM system property; empty / missing returns `None`. */
  private def sysProp(name: String): Option[String] =
    sys.props.get(name).filter(_.nonEmpty)

  /** Walk from `start` up to filesystem root looking for `./prelude/`
    * or `./lib/prelude/`. Stops at the first hit; returns absolute
    * platform path. The `Path` library's `parent` returns `None` at the
    * root, terminating the walk.
    */
  private def walkUpForPrelude(start: String): Option[String] =
    def loop(p: Path): Option[String] =
      val direct = p / Path("prelude")
      if cross_platform.isDirectory(direct.toPlatformString) then
        Some(direct.toPlatformString)
      else
        val nested = p / Path("lib") / Path("prelude")
        if cross_platform.isDirectory(nested.toPlatformString) then
          Some(nested.toPlatformString)
        else
          p.parent match
            case Some(parent) => loop(parent)
            case None         => None
    loop(Path(start))
