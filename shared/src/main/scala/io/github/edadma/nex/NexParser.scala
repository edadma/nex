package io.github.edadma.nex

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharSequenceReader

/** Parser for Nex (Pass 1). Consumes the token stream produced by
  * [[NexLexer]] and builds a [[ProgramAST]].
  *
  * Pass 1 scope:
  *   - all literals (int, real, bool, string, interpolated string, unit)
  *   - identifier references
  *   - binary operators with the full precedence table from spec §4.3
  *   - juxtaposition multiplication (`2x`, `2pi`, `2(x+1)`)
  *   - unary `-` and `not`
  *   - parens for grouping; paren-less tuples
  *   - function calls, indexing, field access, method-call chaining
  *   - rank-1 array literals
  *   - single-expression lambdas: `x -> body`, `(x, y) -> body`,
  *     `(x: T) -> body`
  *   - `val` / `var` / `const` declarations with optional type annotation
  *     and paren-less tuple destructuring on the LHS
  *
  * Deferred to Pass 2:
  *   - `def` function declarations (with parameter modes, return types,
  *     block bodies, recursion)
  *   - `struct` declarations
  *   - `module` / `import`
  *   - `if` / `for` / `while` expressions
  *   - `return`
  *   - attribute annotations (`@test`, `@strict`, `@test module`)
  *   - rank-2 array literals and `@` matmul
  *   - block-form lambda bodies
  *   - `match` expressions (when they arrive in v1)
  */
class NexParser extends StandardTokenParsers with PackratParsers:

  override val lexical: NexLexer = new NexLexer

  import lexical.{Newline, Indent, Dedent}

  // --- Entry points -------------------------------------------------------

  /** Parse a complete program (top-level declarations). */
  def parseProgram(source: String): Either[String, ProgramAST] =
    phrase(program)(lexical.read(new CharSequenceReader(source))) match
      case Success(result, _) => Right(result)
      case ns: NoSuccess      => Left(ns.toString)

  /** Parse a single expression. Useful for tests and a future REPL. */
  def parseExpression(source: String): Either[String, ExprAST] =
    phrase(expr <~ rep(Newline))(lexical.read(new CharSequenceReader(source))) match
      case Success(result, _) => Right(result)
      case ns: NoSuccess      => Left(ns.toString)

  // --- Program -----------------------------------------------------------

  lazy val program: PackratParser[ProgramAST] =
    rep(Newline) ~> repsep(attributedDecl, rep1(Newline)) <~ rep(Newline) ^^ ProgramAST.apply

  // --- Attribute prefix --------------------------------------------------

  /** A declaration optionally preceded by one or more attributes (`@name`).
    * Attributes may live on the same line as the decl or on prior lines.
    */
  lazy val attributedDecl: PackratParser[DeclAST] =
    rep(attribute <~ rep(Newline)) ~ declBare ^^ {
      case attrs ~ d => attachAttrs(d, attrs)
    }

  lazy val attribute: PackratParser[Attribute] =
    "@" ~> ident ^^ Attribute.apply

  private def attachAttrs(d: DeclAST, attrs: List[Attribute]): DeclAST =
    if attrs.isEmpty then d else d match
      case x: ValDeclAST    => x.copy(attributes = attrs)
      case x: VarDeclAST    => x.copy(attributes = attrs)
      case x: ConstDeclAST  => x.copy(attributes = attrs)
      case x: FunDeclAST    => x.copy(attributes = attrs)
      case x: StructDeclAST => x.copy(attributes = attrs)
      case x: ModuleDeclAST =>
        // `@test module foo.bar` — flip isTestOnly when the @test attr is present.
        x.copy(
          attributes = attrs,
          isTestOnly = attrs.exists(_.name == "test") || x.isTestOnly,
        )
      case x: ImportDeclAST => x.copy(attributes = attrs)

  // --- Declarations ------------------------------------------------------

  /** All concrete declaration forms. */
  lazy val declBare: PackratParser[DeclAST] =
    moduleDecl | importDecl | defDecl | structDecl | valDecl | varDecl | constDecl

  /** A declaration (without leading attributes) — used in block contexts. */
  lazy val decl: PackratParser[DeclAST] = attributedDecl

  lazy val valDecl: PackratParser[DeclAST] =
    "val" ~> patternList ~ opt(":" ~> typeExpr) ~ ("=" ~> bindingBody) ^^ {
      case pat ~ tyOpt ~ init => ValDeclAST(pat, tyOpt, init)
    }

  lazy val varDecl: PackratParser[DeclAST] =
    "var" ~> patternList ~ opt(":" ~> typeExpr) ~ ("=" ~> bindingBody) ^^ {
      case pat ~ tyOpt ~ init => VarDeclAST(pat, tyOpt, init)
    }

  lazy val constDecl: PackratParser[DeclAST] =
    "const" ~> patternList ~ opt(":" ~> typeExpr) ~ ("=" ~> bindingBody) ^^ {
      case pat ~ tyOpt ~ init => ConstDeclAST(pat, tyOpt, init)
    }

  /** RHS of a val/var/const binding: same shape as a `def` body — either a
    * single inline expression after `=`, or a Newline-Indent block.
    */
  lazy val bindingBody: PackratParser[ExprAST] =
    blockBody | expr

  // --- def declarations --------------------------------------------------

  lazy val defDecl: PackratParser[DeclAST] =
    opt("private") ~ ("def" ~> ident) ~
      ("(" ~> repsep(funParam, ",") <~ ")") ~
      opt(":" ~> typeExpr) ~
      ("=" ~> funBody) ~ opt(trailingEnd) ^^ {
      case priv ~ name ~ params ~ ret ~ body ~ _ =>
        FunDeclAST(name, params, ret, body, isPrivate = priv.isDefined)
    }

  lazy val funParam: PackratParser[FunParam] =
    ident ~ ":" ~ opt("mut") ~ typeExpr ^^ {
      case n ~ _ ~ mut ~ t =>
        FunParam(n, t, if mut.isDefined then ParamMode.Mut else ParamMode.Read)
    }

  /** Function body: either a single expression on the same line as `=`, or
    * a Newline-Indent-block-Dedent block on the following indented line(s).
    */
  lazy val funBody: PackratParser[ExprAST] =
    blockBody | expr

  // --- struct declarations -----------------------------------------------

  lazy val structDecl: PackratParser[DeclAST] =
    opt("private") ~ ("struct" ~> ident) ~ blockOfFields ~ opt(trailingEnd) ^^ {
      case priv ~ name ~ fields ~ _ =>
        StructDeclAST(name, fields, isPrivate = priv.isDefined)
    }

  lazy val blockOfFields: PackratParser[List[StructField]] =
    Newline ~> Indent ~> repsep(structField, rep1(Newline)) <~ rep(Newline) <~ Dedent

  lazy val structField: PackratParser[StructField] =
    ident ~ (":" ~> typeExpr) ^^ { case n ~ t => StructField(n, t) }

  // --- module / import declarations --------------------------------------

  lazy val moduleDecl: PackratParser[DeclAST] =
    "module" ~> dottedName ^^ { path => ModuleDeclAST(path) }

  lazy val dottedName: PackratParser[List[String]] =
    rep1sep(ident, ".")

  /** `import foo.bar` or `import foo.bar.{x, y as z}`. */
  lazy val importDecl: PackratParser[DeclAST] =
    "import" ~> rep1sep(ident, ".") ~ opt("." ~> selectorBlock) ^^ {
      case path ~ selsOpt =>
        ImportDeclAST(path, selsOpt.getOrElse(Nil))
    }

  lazy val selectorBlock: PackratParser[List[ImportSelector]] =
    "{" ~> repsep(importSelector, ",") <~ "}"

  lazy val importSelector: PackratParser[ImportSelector] =
    ident ~ opt("as" ~> ident) ^^ {
      case n ~ alias => ImportSelector(n, alias)
    }

  // --- end markers -------------------------------------------------------

  /** Optional `end [Name]` or `end [keyword]` trailer after a block. The
    * parser doesn't enforce that the name matches the construct it closes;
    * that's the analyzer's job.
    */
  lazy val endMarker: PackratParser[Any] =
    "end" ~> opt(ident | "if" | "for" | "while" | "do" | "def" | "struct" | "module")

  /** End marker preceded by one or more Newlines. Used after a block whose
    * Dedent already emitted a Newline (since `newlineAfterDedent = true`).
    */
  lazy val trailingEnd: PackratParser[Any] =
    rep1(Newline) ~> endMarker

  // --- Blocks ------------------------------------------------------------

  /** A block body introduced by Newline-Indent. Used as a `def` body, the
    * body of a block-form lambda, the body of a multi-statement `if` /
    * `for` / `while` branch.
    */
  lazy val blockBody: PackratParser[ExprAST] =
    Newline ~> Indent ~> block <~ Dedent

  /** Block: one or more decls / exprs separated by newlines. The last item
    * is the block's result expression. If the block is a single expression
    * with no preceding items, return it directly (no `BlockExpr` wrapper).
    * If the last item is a decl, the implicit result is `unit`.
    */
  lazy val block: PackratParser[ExprAST] =
    rep1sep(blockItem, rep1(Newline)) <~ rep(Newline) ^^ { items =>
      items.last match
        case Right(e) if items.size == 1 => e
        case Right(e) =>
          BlockExpr(items.init.map(toBlockItem), e)
        case Left(_) =>
          BlockExpr(items.map(toBlockItem), UnitLitExpr())
    }

  lazy val blockItem: PackratParser[Either[DeclAST, ExprAST]] =
    declBare ^^ Left.apply |
    assignment ^^ Right.apply |
    exprNoTuple ^^ Right.apply

  /** Assignment statement: `lvalue = rhs`. Only legal at block-item
    * position. The l-value is parsed greedily as a postfix expression
    * (covering bare names, field access, and indexing); the elaborator
    * verifies it's actually assignable.
    */
  lazy val assignment: PackratParser[ExprAST] =
    postfixExpr ~ "=" ~ exprNoTuple ^^ {
      case lhs ~ _ ~ rhs => AssignExpr(lhs, rhs)
    }

  private def toBlockItem(item: Either[DeclAST, ExprAST]): BlockItem = item match
    case Left(d)  => BlockDecl(d)
    case Right(e) => BlockExprItem(e)

  // --- Patterns -----------------------------------------------------------

  /** A pattern list: `a` or `a, b, c`. Returns a `VarPat`/`WildcardPat`
    * directly when single-element, or `TuplePat` for multi-element.
    */
  lazy val patternList: PackratParser[PatternAST] =
    rep1sep(patternAtom, ",") ^^ {
      case p :: Nil => p
      case ps       => TuplePat(ps)
    }

  lazy val patternAtom: PackratParser[PatternAST] =
    ident ^^ { n => if n == "_" then WildcardPat() else VarPat(n) } |
    "(" ~> patternList <~ ")"

  // --- Type expressions ---------------------------------------------------

  /** Function types are right-associative: `A -> B -> C` parses as
    * `A -> (B -> C)`.
    */
  lazy val typeExpr: PackratParser[TypeAST] =
    simpleType ~ opt("->" ~> typeExpr) ^^ {
      case t ~ None     => t
      case t ~ Some(r)  => FuncType(List(t), r)
    } |
    parenTypeList ~ ("->" ~> typeExpr) ^^ {
      case ts ~ r => FuncType(ts, r)
    } |
    parenTypeList ^^ {
      case t :: Nil => t
      case ts       => TupleType(ts)
    } |
    simpleType

  lazy val simpleType: PackratParser[TypeAST] =
    ident ^^ NamedType.apply |
    "[" ~> typeExpr <~ "]" ^^ ArrayType.apply

  lazy val parenTypeList: PackratParser[List[TypeAST]] =
    "(" ~> repsep(typeExpr, ",") <~ ")"

  // --- Expressions --------------------------------------------------------
  //
  // Precedence (tighter → looser) following spec §4.3:
  //   1.  field/method access  (`.`)             — handled in postfix
  //   2.  application/indexing                   — handled in postfix
  //   3.  unary `-`, `not`                        — unaryExpr
  //   4.  `^` (right assoc)                       — powExpr
  //   5.  juxtaposition (numeric × expr)          — juxtExpr
  //   6.  `*`, `/`, `div`, `%`, `@`               — mulExpr
  //   7.  `+`, `-`                                — addExpr
  //   8.  `..`, `..=`                             — rangeExpr
  //   9.  comparison (`==` etc., non-assoc)       — cmpExpr
  //  10.  `and`                                   — andExpr
  //  11.  `or`                                    — orExpr
  //  12.  `->` (lambda)                           — arrowExpr
  //  13.  `,` (tuple)                             — tupleExpr (top-level `expr`)

  /** Top-level expression — may produce a tuple from comma-separated parts. */
  lazy val expr: PackratParser[ExprAST] =
    exprNoTuple ~ rep("," ~> exprNoTuple) ^^ {
      case e ~ Nil  => e
      case e ~ rest => TupleExpr(e :: rest)
    }

  /** An expression at the level where commas are NOT consumed (used inside
    * function arguments, array literals, etc.). Lambdas, `or`, `and`, ...
    * down through application sit below this.
    */
  lazy val exprNoTuple: PackratParser[ExprAST] = arrowExpr

  /** Lambdas — `param-shape -> body`. Right-associative: `x -> y -> z`
    * parses as `x -> (y -> z)`. Body may be a single expression OR a
    * Newline-Indent block.
    */
  lazy val arrowExpr: PackratParser[ExprAST] =
    lambdaParamShape ~ ("->" ~> arrowBody) ^^ {
      case ps ~ body => LambdaExpr(ps, body)
    } |
    orExpr

  lazy val arrowBody: PackratParser[ExprAST] = blockBody | arrowExpr

  /** The "shape" before a lambda arrow: a single bare identifier, or a
    * parenthesized list of named (optionally typed) parameters.
    */
  lazy val lambdaParamShape: PackratParser[List[LambdaParam]] =
    "(" ~> repsep(lambdaParam, ",") <~ ")" |
    ident ^^ { n => List(LambdaParam(n, None)) }

  lazy val lambdaParam: PackratParser[LambdaParam] =
    ident ~ opt(":" ~> typeExpr) ^^ { case n ~ t => LambdaParam(n, t) }

  lazy val orExpr: PackratParser[ExprAST] =
    andExpr ~ rep("or" ~> andExpr) ^^ {
      case f ~ rs => rs.foldLeft(f)((acc, r) => BinOpExpr("or", acc, r))
    }

  lazy val andExpr: PackratParser[ExprAST] =
    cmpExpr ~ rep("and" ~> cmpExpr) ^^ {
      case f ~ rs => rs.foldLeft(f)((acc, r) => BinOpExpr("and", acc, r))
    }

  /** Comparison is non-associative: `a < b < c` is a parse error. */
  lazy val cmpExpr: PackratParser[ExprAST] =
    rangeExpr ~ opt(cmpOp ~ rangeExpr) ^^ {
      case e ~ None           => e
      case e ~ Some(op ~ rhs) => BinOpExpr(op, e, rhs)
    }

  lazy val cmpOp: PackratParser[String] =
    "==" | "!=" | "<=" | ">=" | "<" | ">"

  lazy val rangeExpr: PackratParser[ExprAST] =
    addExpr ~ opt(("..=" | "..") ~ addExpr) ^^ {
      case e ~ None           => e
      case e ~ Some(op ~ rhs) => BinOpExpr(op, e, rhs)
    }

  lazy val addExpr: PackratParser[ExprAST] =
    mulExpr ~ rep(("+" | "-") ~ mulExpr) ^^ {
      case f ~ rs => rs.foldLeft(f) { case (acc, op ~ r) => BinOpExpr(op, acc, r) }
    }

  lazy val mulExpr: PackratParser[ExprAST] =
    juxtExpr ~ rep(mulOp ~ juxtExpr) ^^ {
      case f ~ rs => rs.foldLeft(f) { case (acc, op ~ r) => BinOpExpr(op, acc, r) }
    }

  lazy val mulOp: PackratParser[String] =
    "*" | "/" | "div" | "%" | "@"

  /** Juxtaposition multiplication: a numeric literal followed by an
    * identifier or parenthesized expression binds as `coeff * body`. Falls
    * back to a plain power expression when no numeric prefix is present.
    *
    * Per §4.3 + §4.4, juxtaposition has higher precedence than `*` / `/` but
    * lower than `^`, so `2x^3` = `2 * (x^3)` and `1/2x` = `1 / (2 * x)`.
    *
    * The body is `juxtBody`, not `powExpr`, so the body cannot start with a
    * unary prefix. Otherwise `3 - 4` would parse as `3 * (-4)`: `numericLit`
    * matches `3`, then `powExpr → unaryExpr → "-" ~> postfixExpr` would eat
    * the binary `-` plus `4`.
    *
    * The second alternative — `"-" ~> numericLit ~ juxtBody` — handles a
    * signed coefficient: `-4i` parses as `JuxtaposeExpr(-4, i)`. It only
    * fires when `juxtBody` actually matches, so `-4` standalone (no body)
    * still parses through the `powExpr` fallback as `UnaryOp("-", 4)`, and
    * `-2^2` still parses as `(-2)^2 = 4` (juxtBody can't match `^...` since
    * postfixExpr won't accept a leading `^`).
    */
  lazy val juxtExpr: PackratParser[ExprAST] =
    numericLit ~ juxtBody ^^ { case n ~ b => JuxtaposeExpr(parseNumeric(n), b) } |
    "-" ~> numericLit ~ juxtBody ^^ { case n ~ b => JuxtaposeExpr(negateNumeric(n), b) } |
    powExpr

  /** Power expression with unary prefix forbidden. Used only as the body of
    * juxtaposition — see `juxtExpr`.
    */
  lazy val juxtBody: PackratParser[ExprAST] =
    postfixExpr ~ opt("^" ~> powExpr) ^^ {
      case e ~ None    => e
      case e ~ Some(r) => BinOpExpr("^", e, r)
    }

  /** `^` is right-associative: `a ^ b ^ c` = `a ^ (b ^ c)`. */
  lazy val powExpr: PackratParser[ExprAST] =
    unaryExpr ~ opt("^" ~> powExpr) ^^ {
      case e ~ None    => e
      case e ~ Some(r) => BinOpExpr("^", e, r)
    }

  /** Unary prefix `-` and `not`. */
  lazy val unaryExpr: PackratParser[ExprAST] =
    "-"   ~> postfixExpr ^^ { e => UnaryOpExpr("-", e) }   |
    "not" ~> postfixExpr ^^ { e => UnaryOpExpr("not", e) } |
    postfixExpr

  /** Postfix application: call, index, field/method access — left
    * associative, can chain (`a.b().c[0]`).
    */
  lazy val postfixExpr: PackratParser[ExprAST] =
    primaryExpr ~ rep(postfixOp) ^^ {
      case e ~ ops => ops.foldLeft(e) {
        case (acc, op) => op(acc)
      }
    }

  /** A postfix continuation is a function that takes the receiver
    * expression and wraps it with the appropriate AST node.
    */
  lazy val postfixOp: PackratParser[ExprAST => ExprAST] =
    callTail | indexTail | dotTail

  lazy val callTail: PackratParser[ExprAST => ExprAST] =
    "(" ~> repsep(exprNoTuple, ",") <~ ")" ^^ { args =>
      (recv: ExprAST) => CallExpr(recv, args)
    }

  lazy val indexTail: PackratParser[ExprAST => ExprAST] =
    "[" ~> rep1sep(exprNoTuple, ",") <~ "]" ^^ { ixs =>
      (recv: ExprAST) => IndexExpr(recv, ixs)
    }

  /** `.name` is a field access; `.name(args)` becomes a method-call sugar
    * node so the analyzer can decide field-vs-method per §4.9.
    */
  lazy val dotTail: PackratParser[ExprAST => ExprAST] =
    "." ~> ident ~ opt("(" ~> repsep(exprNoTuple, ",") <~ ")") ^^ {
      case name ~ None       => (recv: ExprAST) => FieldExpr(recv, name)
      case name ~ Some(args) => (recv: ExprAST) => MethodCallExpr(recv, name, args)
    }

  // --- Primary expressions -----------------------------------------------

  lazy val primaryExpr: PackratParser[ExprAST] =
    ifExpr                                                       |
    forExpr                                                      |
    whileExpr                                                    |
    returnExpr                                                   |
    numericLit       ^^ parseNumeric                             |
    interpStringLit                                              |
    stringLit        ^^ StringLitExpr.apply                      |
    "true"           ^^^ BoolLitExpr(true)                       |
    "false"          ^^^ BoolLitExpr(false)                      |
    arrayLit                                                     |
    parenOrTupleOrUnit                                           |
    ident            ^^ VarRefExpr.apply

  // --- Control-flow expressions ------------------------------------------

  /** `if cond then thenBranch [else elseBranch]`. Branches use `exprNoTuple`
    * so a trailing `, x` at the outer level binds the if-expression as the
    * first tuple element rather than extending the else branch (spec §4.3
    * says comma is the loosest operator).
    */
  lazy val ifExpr: PackratParser[ExprAST] =
    ("if" ~> exprNoTuple) ~ ("then" ~> branchBody) ~ opt(elseClause) ~ opt(trailingEnd) ^^ {
      case cond ~ thenB ~ elseB ~ _ => IfExpr(cond, thenB, elseB)
    }

  /** The `else` clause may sit on the same line as the `then` body, or on
    * a new line after the then-block's `Dedent` + trailing `Newline`.
    * `rep(Newline)` matches zero or more — opt backtracks cleanly if no
    * `else` ever shows up.
    */
  lazy val elseClause: PackratParser[ExprAST] =
    rep(Newline) ~> "else" ~> branchBody

  lazy val forExpr: PackratParser[ExprAST] =
    ("for" ~> patternList) ~ ("in" ~> exprNoTuple) ~ ("do" ~> branchBody) ~ opt(trailingEnd) ^^ {
      case pat ~ it ~ body ~ _ => ForExpr(pat, it, body)
    }

  lazy val whileExpr: PackratParser[ExprAST] =
    ("while" ~> exprNoTuple) ~ ("do" ~> branchBody) ~ opt(trailingEnd) ^^ {
      case cond ~ body ~ _ => WhileExpr(cond, body)
    }

  lazy val returnExpr: PackratParser[ExprAST] =
    "return" ~> opt(exprNoTuple) ^^ ReturnExpr.apply

  /** Branch body — used for `then` / `else` of `if` and `do` of `for` /
    * `while`. Either a single inline expression or a Newline-Indent block.
    */
  lazy val branchBody: PackratParser[ExprAST] =
    blockBody | exprNoTuple

  /** Match the lexer's pre-split [[NexLexer.InterpStringTok]] and convert
    * its parts to AST nodes. The `${...}` body strings are re-parsed using
    * the standard [[parseExpression]] entry point — no manual char-walking
    * anywhere on this code path.
    */
  lazy val interpStringLit: PackratParser[ExprAST] =
    acceptMatch("interpolated string", { case t: lexical.InterpStringTok => t }) ^^ { tok =>
      InterpStringLitExpr(tok.parts.map {
        case lexical.IText(s)   => InterpText(s)
        case lexical.IIdent(n)  => InterpVar(n)
        case lexical.IExpr(raw) => InterpExprPart(raw)
      })
    }

  /** `[e1, e2, ...]` — rank-1 array literal. Empty `[]` is accepted but
    * needs a type annotation at the binding site (`val a: [real] = []`)
    * to disambiguate the element type — that's an elaborator concern.
    */
  lazy val arrayLit: PackratParser[ExprAST] =
    "[" ~> repsep(exprNoTuple, ",") <~ "]" ^^ { elems =>
      // Wrap in a Call to a synthetic `__array` for now — the elaborator
      // will lower this to a typed array constructor once types arrive.
      CallExpr(VarRefExpr("__array"), elems)
    }

  /** Parens may wrap a unit literal `()`, a single expression (grouping),
    * or a tuple. Tuples in primary position with parens are equivalent to
    * the paren-less form via the top-level `expr` parser; the paren form
    * is needed when commas would otherwise be parsed as something else
    * (function args, array elements).
    */
  lazy val parenOrTupleOrUnit: PackratParser[ExprAST] =
    "(" ~ ")" ^^^ UnitLitExpr() |
    "(" ~> expr <~ ")"

  // --- Helpers: convert lexical strings to AST -----------------------------

  /** Convert a NumericLit's raw text to either an `IntLitExpr` or a
    * `RealLitExpr`. The lexer strips `_` separators already; base prefixes
    * (`0x`, `0b`, `0o`) survive in the raw text.
    */
  private def parseNumeric(raw: String): ExprAST =
    if raw.startsWith("0x") || raw.startsWith("0X") then
      IntLitExpr(java.lang.Long.parseUnsignedLong(raw.substring(2), 16))
    else if raw.startsWith("0b") || raw.startsWith("0B") then
      IntLitExpr(java.lang.Long.parseUnsignedLong(raw.substring(2), 2))
    else if raw.startsWith("0o") || raw.startsWith("0O") then
      IntLitExpr(java.lang.Long.parseUnsignedLong(raw.substring(2), 8))
    else if raw.contains('.') || raw.contains('e') || raw.contains('E') then
      RealLitExpr(raw.toDouble)
    else
      IntLitExpr(java.lang.Long.parseLong(raw))

  /** Same as `parseNumeric` but fronts the literal with a unary minus. Used
    * by the signed-coefficient juxt alternative so `-4i` folds to a single
    * `IntLitExpr(-4)` coefficient instead of `UnaryOp("-", IntLit(4))`.
    */
  private def negateNumeric(raw: String): ExprAST =
    parseNumeric(raw) match
      case IntLitExpr(n)  => IntLitExpr(-n)
      case RealLitExpr(r) => RealLitExpr(-r)
      case other          => UnaryOpExpr("-", other)

  // The interpolated-string body is split inside the lexer using parser
  // combinators (see NexLexer.interpBodyChunk); the parser just consumes
  // the resulting InterpStringTok directly via the `interpStringLit`
  // parser above. No hand-coded body walking on this code path.
