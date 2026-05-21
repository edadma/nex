---
title: Lexical Syntax
summary: Source encoding, identifiers, keywords, literals, comments, indentation, and statement separators.
weight: 20
---

## 2.1 Source encoding

Source files are UTF-8 encoded. Files use the `.nex` extension. Literate sources (Markdown wrappers; see [§2.9](#29-literate-nex)) use `.lnex` and are transparently preprocessed before lexing — every rule below applies to the dedented code inside a `.lnex` file just as it does to a `.nex` file.

## 2.2 Identifiers

Identifiers begin with an ASCII letter or underscore and may contain ASCII letters, digits, and underscores:

```
identifier  ::=  (letter | '_') (letter | digit | '_')*
letter      ::=  'a'..'z' | 'A'..'Z'
digit       ::=  '0'..'9'
```

Unicode identifiers are *deferred*.

## 2.3 Keywords

Reserved words (cannot be used as identifiers):

```nex
and       as        const     def       div       do
else      end       false     for       if        import
in        match     module    mut       not       or
private   return    struct    then      true      val
var       while
```

Words reserved for future use:

```
class     extends   given     implicit  package
pub       trait     type      where     yield
```

## 2.4 Literals

**Boolean literals:** `true`, `false`.

**Integer literals:** decimal, hexadecimal (`0x` prefix), binary (`0b` prefix), or octal (`0o` prefix). Underscores allowed as digit separators.

```nex
42
1_000_000
0xFF
0b1010_1010
0o755
```

Integer literals have type `integer`.

**Real literals:** decimal with required fractional part or exponent.

```nex
3.14
1.0
2.5e-3
6.022e23
1_234.567_89
```

Real literals have type `real`.

**Complex values** are constructed from real values using the prelude constant `i` (the imaginary unit, equal to `(0.0, 1.0)`) together with arithmetic operators. Combined with juxtaposition multiplication (chapter 4), this produces math-flavored notation without a dedicated complex-literal form:

```nex
i                           // imaginary unit: (0.0 + 1.0i)
2.0i                        // (0.0 + 2.0i)  — juxtaposition: 2.0 * i
3.0 + 4.5i                  // (3.0 + 4.5i)
exp(pi * i)                 // (-1.0 + 0.0i) — Euler's identity
```

Complex values have type `complex` (chapter 3). Nex does not provide a dedicated complex-literal token — `2i`, `3 + 4i`, etc. are ordinary expressions parsed under the rules in chapter 4.

(The prelude name `i` shadows freely: a `for i in 0..n` loop variable hides the imaginary unit within the loop body. If you need both in the same scope, pick a different index name — by mathematical convention, `j`, `k`, `m`, or `n`.)

**String literals:** double-quoted, with backslash escapes (`\n`, `\t`, `\r`, `\\`, `\"`, `\x41`, `\u{1F600}`).

```nex
"hello, world"
"line one\nline two"
```

String literals have type `string`.

**Interpolated string literals:** prefixed with `s`. Values are spliced into the string at marker positions:

```nex
s"hello, $name"                    // identifier substitution
s"x + y = ${x + y}"                // arbitrary expression in ${}
s"|z| = ${z.abs()}"                // method/field access in ${}
s"first = ${arr[0]}"               // indexing in ${}
s"price: $$$amount"                // literal $ via doubling
```

Rules:

- `$identifier` substitutes the value of the named binding.
- `${expression}` substitutes the result of an arbitrary expression.
- `$$` produces a literal `$`.
- The form `$ident.field` parses as `${ident}.field` — only the bare identifier is interpolated, and `.field` is literal text. To interpolate a field access, write `${ident.field}`. The same rule applies to `[…]` and `(…)` following an interpolated identifier.

Each interpolated value is converted to its string form (the same conversion `print` uses). Interpolated strings have type `string`.

Format-string interpolation with precision specifiers (`f"x = $x%.3f"`) is *deferred*.

**Unit literal:** `()`. The single value of type `unit`.

## 2.5 Comments

Line comments begin with `//` and extend to the end of the line. Block comments are `/* ... */` and may nest.

```nex
// this is a line comment

/* this is
   a block comment */

/* block /* nested */ comments work */
```

## 2.6 Indentation and structure

Nex uses indentation-based syntax in the Scala 3 family. Indentation introduces a block; dedentation closes it. Blocks are introduced after `=` (in `def` and `val`/`var` bindings), after `then` / `else` (in `if`), after `do` (in `for` / `while`), and after `->` in block-form lambdas.

Indentation may use spaces or tabs but must be consistent within a file. The recommended convention is two spaces.

```nex
def normalize(v: [real]) =
  val mag = sqrt(sum(v * v))
  if mag == 0.0 then
    v
  else
    v / mag
```

## 2.7 End markers

Any indented block may be terminated by an optional `end` marker for clarity. The marker may be bare (`end`) or named with the construct it closes:

```nex
def normalize(v: [real]) =
  val mag = sqrt(sum(v * v))
  if mag == 0.0 then
    v
  else
    v / mag
  end if
end def
```

End markers are particularly useful for long blocks where the closing dedent would otherwise be far from the opening keyword. They are never required and never alter semantics.

## 2.8 Statement separators

A statement ends at the end of a line, unless the line ends with an operator or open delimiter, in which case the statement continues on the next (more-indented) line. Multiple statements on one line may be separated by `;`.

```nex
val x = 1; val y = 2

val long_expression =
  some_function(a, b)
    + another_function(c, d)
    * yet_another(e)
```

## 2.9 Literate Nex

A file with the `.lnex` extension is a literate source: Markdown prose interleaved with indented code blocks. The compiler preprocesses the file before lexing, so the rest of the language sees ordinary Nex source.

The preprocessing rules are:

- **Prose lines** start at column 0 and are stripped — replaced by blank lines so that error positions still refer to the original `.lnex` line numbers. Markdown headings, paragraphs, lists, links, inline math (`$..$`), and display math (`$$..$$`) all fall under this rule.
- **Code lines** start with a tab or at least four spaces. Exactly one indentation level is stripped (the leading tab, or four leading spaces) and the dedented body is fed to the lexer. Any further indentation survives — Nex's own indent-sensitive blocks work normally inside a literate file.
- **Fenced blocks** (` ``` `) are non-Nex content (ASCII diagrams, sample output, hex dumps) and are stripped entirely. The fence delimiters and everything between them become blank lines.
- Lines with 1-3 leading spaces are treated as prose (Markdown list-item continuations).

Example:

```markdown
# Greeting

This file demonstrates literate Nex. The compiler ignores the prose
and reads only the indented blocks below.

    def main() =
        val name = "world"
        print(name)
```

A module may freely mix `.nex` and `.lnex` files; the directory's contents are taken as the union, and the rules of [§9](/spec/09-modules/) apply unchanged.
