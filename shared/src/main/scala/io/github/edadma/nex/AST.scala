package io.github.edadma.nex

import scala.util.parsing.input.Positional

sealed trait Expr extends Positional

case class NumberExpr(value: String) extends Expr
case class StringExpr(value: String) extends Expr
case class VarExpr(name: String) extends Expr
case class PlaceholderExpr() extends Expr
case class FunctionExpr(params: List[String], body: Expr) extends Expr
case class ArrayExpr(elements: List[Expr]) extends Expr
case class BinaryExpr(op: String, left: Expr, right: Expr) extends Expr
case class UnaryExpr(op: String, expr: Expr) extends Expr
case class ApplyExpr(fn: Expr, args: List[Expr]) extends Expr
case class AssignExpr(name: String, expr: Expr) extends Expr
case class PipeExpr(value: Expr, fn: Expr) extends Expr
case class ComposeExpr(f: Expr, g: Expr) extends Expr
