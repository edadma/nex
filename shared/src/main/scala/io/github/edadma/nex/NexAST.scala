package io.github.edadma.nex

import scala.util.parsing.input.Positional

// ============================================================================
// Top-level program structure
// ============================================================================

/** A parsed program: a list of top-level declarations. */
case class ProgramAST(decls: List[DeclAST]) extends Positional

// ============================================================================
// Declarations
// ============================================================================

sealed trait DeclAST extends Positional:
  /** Attributes (`@test`, `@strict`, ...) attached to this declaration. */
  def attributes: List[Attribute]

/** `val pattern [: type] = expr`. */
case class ValDeclAST(
    pat: PatternAST, typ: Option[TypeAST], init: ExprAST,
    isPrivate: Boolean = false,
    attributes: List[Attribute] = Nil,
) extends DeclAST

/** `var pattern [: type] = expr`. */
case class VarDeclAST(
    pat: PatternAST, typ: Option[TypeAST], init: ExprAST,
    isPrivate: Boolean = false,
    attributes: List[Attribute] = Nil,
) extends DeclAST

/** `const NAME [: type] = constExpr`. */
case class ConstDeclAST(
    pat: PatternAST, typ: Option[TypeAST], init: ExprAST,
    isPrivate: Boolean = false,
    attributes: List[Attribute] = Nil,
) extends DeclAST

/** `def name(params) [: returnType] = body`. The body is `None` when the
  * declaration carries an `@intrinsic("opId")` attribute — the compiler
  * supplies the implementation via per-backend dispatch tables.
  */
case class FunDeclAST(
    name:       String,
    params:     List[FunParam],
    returnType: Option[TypeAST],
    body:       Option[ExprAST],
    isPrivate:  Boolean = false,
    attributes: List[Attribute] = Nil,
) extends DeclAST

/** A function-declaration parameter. `mode` is `read` (inferred) or `mut`
  * (declared) per spec §6.4.
  */
case class FunParam(name: String, typ: TypeAST, mode: ParamMode)

enum ParamMode:
  case Read   // inferred default
  case Mut    // explicit `mut`

/** `struct Name; field: T; ...; end [Name]`. */
case class StructDeclAST(
    name:       String,
    fields:     List[StructField],
    isPrivate:  Boolean = false,
    attributes: List[Attribute] = Nil,
) extends DeclAST

case class StructField(name: String, typ: TypeAST) extends Positional

/** `module foo.bar`. The path is the dotted-name sequence. */
case class ModuleDeclAST(
    path:       List[String],
    isTestOnly: Boolean = false,
    attributes: List[Attribute] = Nil,
) extends DeclAST

/** `import foo.bar.{x, y as z}`. */
case class ImportDeclAST(
    path:       List[String],
    selectors:  List[ImportSelector],
    attributes: List[Attribute] = Nil,
) extends DeclAST

case class ImportSelector(name: String, alias: Option[String] = None)

/** Attribute, e.g. `@test`, `@strict`, `@intrinsic("libm.sqrt")`. The
  * `args` list is empty for argument-less attributes and otherwise holds
  * the literal string arguments inside the parentheses.
  */
case class Attribute(name: String, args: List[String] = Nil) extends Positional

// ============================================================================
// Patterns (the left-hand side of a binding)
// ============================================================================

sealed trait PatternAST extends Positional

/** A name pattern: `x` binds `x` to the value. */
case class VarPat(name: String) extends PatternAST

/** Wildcard: `_` accepts a value and discards it. */
case class WildcardPat() extends PatternAST

/** Tuple destructuring: `a, b, c` (or `(a, b, c)`). Must have ≥ 2 elements;
  * single-element patterns become the inner pattern.
  */
case class TuplePat(elems: List[PatternAST]) extends PatternAST

// ============================================================================
// Types (Pass 1: enough for binding annotations)
// ============================================================================

sealed trait TypeAST extends Positional

/** Named type: `real`, `integer`, `bool`, `complex`, `unit`, `string`,
  * `Point`, etc.
  */
case class NamedType(name: String) extends TypeAST

/** `[T]` — rank-1 array. */
case class ArrayType(elem: TypeAST) extends TypeAST

/** `(T1, T2, ...)` or paren-less `T1, T2, ...` — heterogeneous product. */
case class TupleType(elems: List[TypeAST]) extends TypeAST

/** Function type: `(P1, P2, ...) -> R` or single-arg `P -> R`. */
case class FuncType(params: List[TypeAST], ret: TypeAST) extends TypeAST

// ============================================================================
// Expressions
// ============================================================================

sealed trait ExprAST extends Positional

// -- Literals --------------------------------------------------------------

case class IntLitExpr(value: Long) extends ExprAST
case class RealLitExpr(value: Double) extends ExprAST
case class BoolLitExpr(value: Boolean) extends ExprAST
case class StringLitExpr(value: String) extends ExprAST

/** Body of an interpolated string, split along `$` boundaries. */
case class InterpStringLitExpr(parts: List[InterpPart]) extends ExprAST

sealed trait InterpPart
/** Literal text between interpolations. */
case class InterpText(text: String) extends InterpPart
/** `$name` — substitute a binding. */
case class InterpVar(name: String) extends InterpPart
/** `${expr}` — the raw source text between the braces, to be re-parsed
  * by the elaborator. (Doing the re-parse at parse time would require
  * a fresh lexer instance per nesting level — easier to defer.)
  */
case class InterpExprPart(rawExpr: String) extends InterpPart

/** `()` — the unit value. */
case class UnitLitExpr() extends ExprAST

// -- References ------------------------------------------------------------

/** Bare identifier reference: `x`, `pi`, `i`. */
case class VarRefExpr(name: String) extends ExprAST

// -- Operators -------------------------------------------------------------

/** Binary operator. `op` is the lexical string (`+`, `*`, `==`, `and`,
  * `..`, `->`, ...).
  */
case class BinOpExpr(op: String, lhs: ExprAST, rhs: ExprAST) extends ExprAST

/** Unary prefix operator. `op` is `-` or `not`. */
case class UnaryOpExpr(op: String, operand: ExprAST) extends ExprAST

/** Juxtaposition multiplication: `2x`, `2pi`, `2(x+1)`. Stored as its own
  * node so the AST distinguishes it from explicit `2 * x`; the elaborator
  * can lower both to the same typed-AST node.
  */
case class JuxtaposeExpr(coeff: ExprAST, body: ExprAST) extends ExprAST

// -- Application / projection ----------------------------------------------

/** Function call: `f(arg, arg, ...)`. */
case class CallExpr(callee: ExprAST, args: List[ExprAST]) extends ExprAST

/** Indexing: `a[i]` (single) or `a[i, j]` (rank-2; Pass 2). */
case class IndexExpr(arr: ExprAST, indices: List[ExprAST]) extends ExprAST

/** The `:` axis selector — only legal inside an `IndexExpr`'s index list,
  * marks "all of this axis" for rank-2 slicing (spec §4.14). The parser
  * accepts it exclusively in that position; using `:` elsewhere is a
  * parse error.
  */
case class AxisAllExpr() extends ExprAST

/** Field access / method call sugar: `a.b` (field) and `a.b(args)` (sugar
  * for `b(a, args)` per §4.9). The parser produces `Field` for the access
  * form and `Call(Field(a, b), args)` is rewritten to `MethodCall(a, b,
  * args)` by the parser when it sees the chained call shape.
  */
case class FieldExpr(receiver: ExprAST, field: String) extends ExprAST

/** Method-call sugar: `receiver.name(args...)` — the analyzer decides
  * between field-access-then-call and function-call sugar.
  */
case class MethodCallExpr(receiver: ExprAST, name: String, args: List[ExprAST]) extends ExprAST

// -- Lambdas ---------------------------------------------------------------

/** Lambda parameter — name with optional type annotation. */
case class LambdaParam(name: String, typ: Option[TypeAST])

/** Lambda expression: single-arg `x -> body`, multi-arg `(x, y) -> body`,
  * or block-form `x ->\n  ...`.
  */
case class LambdaExpr(params: List[LambdaParam], body: ExprAST) extends ExprAST

// -- Tuples ----------------------------------------------------------------

/** Tuple expression: `a, b, c` or `(a, b, c)`. Must have ≥ 2 elements. */
case class TupleExpr(elems: List[ExprAST]) extends ExprAST

// -- Control flow ----------------------------------------------------------

/** `if cond then thenBranch [else elseBranch]`. If `elseBranch` is `None`,
  * the expression is `unit`-typed.
  */
case class IfExpr(cond: ExprAST, thenBranch: ExprAST, elseBranch: Option[ExprAST]) extends ExprAST

/** `for pat in iterable do body`. */
case class ForExpr(pat: PatternAST, iterable: ExprAST, body: ExprAST) extends ExprAST

/** `while cond do body`. */
case class WhileExpr(cond: ExprAST, body: ExprAST) extends ExprAST

/** `return [expr]` — early exit from the enclosing function. */
case class ReturnExpr(value: Option[ExprAST]) extends ExprAST

/** Assignment statement (modelled as a unit-typed expression for AST
  * uniformity): `target = value`, where `target` is an l-value
  * (VarRefExpr, FieldExpr, or IndexExpr — validated at elaboration time).
  * Only legal at block-item position.
  */
case class AssignExpr(target: ExprAST, value: ExprAST) extends ExprAST

// -- Block ----------------------------------------------------------------

/** Block expression: a sequence of declarations / statements followed by a
  * final expression whose value the block produces. Used as a function
  * body, lambda block-body, or RHS of `val x = <block>`.
  */
case class BlockExpr(items: List[BlockItem], result: ExprAST) extends ExprAST

sealed trait BlockItem extends Positional
case class BlockDecl(decl: DeclAST) extends BlockItem
case class BlockExprItem(expr: ExprAST) extends BlockItem
