package io.github.edadma.nex

import java.nio.file.{Files, Paths, Path}
import scala.jdk.CollectionConverters.*

val platform: String = "jvm"

/** Read a file's contents as a UTF-8 string. */
def readFile(path: String): String =
  new String(Files.readAllBytes(Paths.get(path)), java.nio.charset.StandardCharsets.UTF_8)

/** Write `content` to `path` (UTF-8). Overwrites the file if it exists. */
def writeFile(path: String, content: String): Unit =
  Files.writeString(Paths.get(path), content, java.nio.charset.StandardCharsets.UTF_8)

/** List `.nex` files directly inside `dir` (NOT recursive — subdirectories
  * are submodules, loaded separately). Returns absolute paths, sorted for
  * deterministic order. Returns Nil if the directory doesn't exist.
  */
def listNexFiles(dir: String): List[String] =
  val p = Paths.get(dir)
  if !Files.isDirectory(p) then Nil
  else
    val stream = Files.list(p)
    try
      stream.iterator.asScala
        .filter(Files.isRegularFile(_))
        .map(_.toAbsolutePath.toString)
        .filter(_.endsWith(".nex"))
        .toList
        .sorted
    finally stream.close()

/** Directory containing the file at `path`. Returns absolute path. */
def pathDirname(path: String): String =
  val abs = Paths.get(path).toAbsolutePath
  Option(abs.getParent).map(_.toString).getOrElse(".")

/** Join path components using the platform separator. */
def pathJoin(parts: String*): String =
  parts.foldLeft(Paths.get("")) { (acc, s) => acc.resolve(s) }.toString

/** True iff the given path exists and is a directory. */
def pathIsDirectory(path: String): Boolean =
  Files.isDirectory(Paths.get(path))
