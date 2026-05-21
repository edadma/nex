package io.github.edadma.nex

/** Literate Nex (`.lnex`) source preprocessor.
  *
  * A `.lnex` file is Markdown: column-0 lines are prose, lines starting
  * with a tab or four or more spaces are code. The preprocessor strips
  * the prose down to blank lines so the lexer's line numbers continue
  * to match the original source, and dedents code lines by one
  * indentation level so what the lexer sees is ordinary `.nex` source.
  *
  * Fenced code blocks ` ``` ` are treated as non-Nex content (ASCII
  * diagrams, sample output, hex dumps, etc.) and stripped entirely.
  * The fence delimiters and everything between them become blank lines.
  *
  * Indentation rules:
  *   - A leading tab counts as one level (stripped).
  *   - Four leading spaces count as one level (stripped).
  *   - 1-3 leading spaces are prose (list continuation in Markdown).
  *   - Mixed leading whitespace: a tab takes precedence over spaces;
  *     `\t  x` becomes `  x` (the tab is the one indent level).
  */
object NexLiterate:

  /** Transform a `.lnex` source string into the equivalent `.nex`
    * source. Output has exactly as many lines as the input so that
    * error positions reported by the lexer/parser refer to the
    * original file's line numbers.
    */
  def preprocess(source: String): String =
    val out      = new StringBuilder(source.length)
    val lines    = splitKeepNewlines(source)
    var inFence  = false
    var i        = 0
    while i < lines.length do
      val (body, nl) = lines(i)
      val trimmed    = body.stripLeading()

      if inFence then
        if trimmed.startsWith("```") then inFence = false
        out.append(nl)
      else if trimmed.startsWith("```") then
        inFence = true
        out.append(nl)
      else
        dedent(body) match
          case Some(code) => out.append(code).append(nl)
          case None       => out.append(nl)

      i += 1
    out.toString

  /** Split `s` into a list of `(body, newline)` pairs, preserving the
    * original newline characters (`\n`, `\r\n`, or empty for the last
    * line if it lacks a terminator). This lets the output reproduce
    * the source's line endings exactly.
    */
  private def splitKeepNewlines(s: String): Array[(String, String)] =
    val buf = scala.collection.mutable.ArrayBuffer.empty[(String, String)]
    val len = s.length
    var i   = 0
    while i < len do
      var j = i
      while j < len && s.charAt(j) != '\n' && s.charAt(j) != '\r' do j += 1
      val body = s.substring(i, j)
      var nl   = ""
      if j < len then
        if s.charAt(j) == '\r' && j + 1 < len && s.charAt(j + 1) == '\n' then
          nl = "\r\n"; j += 2
        else
          nl = s.substring(j, j + 1); j += 1
      buf += ((body, nl))
      i = j
    buf.toArray

  /** Strip exactly one indentation level from `line` and return the
    * dedented body. Returns `None` for prose lines (column-0 or 1-3
    * leading spaces) and for blank lines that contain only whitespace.
    *
    * A leading tab is one level. Otherwise, four leading spaces are
    * one level. Lines with mixed leading whitespace strip whichever
    * the first leading character is.
    */
  private def dedent(line: String): Option[String] =
    if line.isEmpty then None
    else if line.charAt(0) == '\t' then Some(line.substring(1))
    else if line.startsWith("    ") then Some(line.substring(4))
    else None
