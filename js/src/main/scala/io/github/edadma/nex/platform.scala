package io.github.edadma.nex

val platform: String = "js"

/** Read a file's contents — not yet implemented on the JS target.
  * (Future work: Node.js `fs.readFileSync` via Scala.js facade for the
  * server-side build; browser bundles likely won't expose file IO at all.)
  */
def readFile(path: String): String =
  throw new UnsupportedOperationException(
    s"readFile not yet supported on Scala.js (tried to read: $path)"
  )
