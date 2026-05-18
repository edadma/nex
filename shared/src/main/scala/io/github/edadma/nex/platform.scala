package io.github.edadma.nex

import io.github.edadma.cross_platform
import io.github.edadma.path.Path

// Filesystem helpers used by the module loader and the CLI. All six
// delegate to the `cross_platform` library, which has working
// implementations on JVM, JS (Node), and Native — so the same nex
// code runs on every target. Previously these lived as per-platform
// stubs with JS as a throwing placeholder; consolidating here makes
// the whole loader cross-platform.

def readFile(path: String): String =
  cross_platform.readFile(path)

def writeFile(path: String, content: String): Unit =
  cross_platform.writeFile(path, content)

/** List `.nex` files directly inside `dir` (NOT recursive —
  * subdirectories are submodules). Returns absolute paths, sorted
  * for deterministic order.
  */
def listNexFiles(dir: String): List[String] =
  if !cross_platform.isDirectory(dir) then Nil
  else
    cross_platform
      .listFiles(dir)
      .filter(_.endsWith(".nex"))
      .filter(cross_platform.isFile)
      .toList
      .sorted

/** Parent directory of a path; ".' for top-level files. Uses the
  * Path library's segment-aware parent so embedded separators are
  * handled correctly across platforms.
  */
def pathDirname(p: String): String =
  Path(p).parent.map(_.toPlatformString).getOrElse(".")

def pathJoin(parts: String*): String =
  parts.filter(_.nonEmpty).foldLeft(Path("")) { (acc, s) => acc / Path(s) }.toPlatformString

def pathIsDirectory(path: String): Boolean =
  cross_platform.isDirectory(path)
