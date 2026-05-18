package io.github.edadma.nex

import java.nio.file.{Files, Paths}

val platform: String = "native"

/** Read a file's contents as a UTF-8 string. */
def readFile(path: String): String =
  new String(Files.readAllBytes(Paths.get(path)), java.nio.charset.StandardCharsets.UTF_8)
