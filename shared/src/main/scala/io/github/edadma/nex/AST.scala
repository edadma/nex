package io.github.edadma.nex

import scala.util.parsing.input.Positional

sealed trait Expr extends Positional

case class Num(value: String) extends Expr
case class Str(value: String) extends Expr
case class Var(name: String) extends Expr
case class Placeholder() extends Expr
case class Lambda(param: String, body: Expr) extends Expr
case class ArrayLit(elements: List[Expr]) extends Expr
case class BinOp(op: String, left: Expr, right: Expr) extends Expr
case class UnaryOp(op: String, expr: Expr) extends Expr
case class Call(name: String, args: List[Expr]) extends Expr
case class Assign(name: String, expr: Expr) extends Expr
case class Pipe(value: Expr, fn: Expr) extends Expr
case class Compose(f: Expr, g: Expr) extends Expr
