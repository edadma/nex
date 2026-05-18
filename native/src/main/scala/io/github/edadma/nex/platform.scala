package io.github.edadma.nex

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

val platform: String = "native"

/** Read a file's contents as a UTF-8 string. */
def readFile(path: String): String =
  new String(Files.readAllBytes(Paths.get(path)), java.nio.charset.StandardCharsets.UTF_8)

/** List `.nex` files directly inside `dir` (NOT recursive — subdirectories
  * are submodules). Returns absolute paths, sorted for deterministic order.
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

def pathDirname(path: String): String =
  val abs = Paths.get(path).toAbsolutePath
  Option(abs.getParent).map(_.toString).getOrElse(".")

def pathJoin(parts: String*): String =
  parts.foldLeft(Paths.get("")) { (acc, s) => acc.resolve(s) }.toString

def pathIsDirectory(path: String): Boolean =
  Files.isDirectory(Paths.get(path))
