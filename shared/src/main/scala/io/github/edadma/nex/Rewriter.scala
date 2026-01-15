package io.github.edadma.nex

object Rewriter:
  private val placeholderParam = "_arg"

  def rewrite(expr: Expr): Expr =
    expr match
      // Don't wrap these in FunctionExpr - just rewrite their children
      case ApplyExpr(fn, args) => ApplyExpr(rewrite(fn), args.map(rewrite)).setPos(expr.pos)
      case ArrayExpr(elems) => ArrayExpr(elems.map(rewrite)).setPos(expr.pos)
      case AssignExpr(name, e) => AssignExpr(name, rewrite(e)).setPos(expr.pos)
      case FunctionExpr(params, body) => FunctionExpr(params, rewrite(body)).setPos(expr.pos)
      case PipeExpr(value, fn) => PipeExpr(rewrite(value), rewrite(fn)).setPos(expr.pos)
      case ComposeExpr(f, g) => ComposeExpr(rewrite(f), rewrite(g)).setPos(expr.pos)
      // Wrap these in FunctionExpr if they contain placeholders
      case _ if containsPlaceholder(expr) =>
        FunctionExpr(List(placeholderParam), replacePlaceholder(expr)).setPos(expr.pos)
      case _ => expr

  private def containsPlaceholder(expr: Expr): Boolean =
    expr match
      case PlaceholderExpr() => true
      case NumberExpr(_) | StringExpr(_) | VarExpr(_) => false
      case FunctionExpr(_, body) => containsPlaceholder(body)
      case ArrayExpr(elems) => elems.exists(containsPlaceholder)
      case BinaryExpr(_, left, right) => containsPlaceholder(left) || containsPlaceholder(right)
      case UnaryExpr(_, e) => containsPlaceholder(e)
      case ApplyExpr(fn, args) => containsPlaceholder(fn) || args.exists(containsPlaceholder)
      case AssignExpr(_, e) => containsPlaceholder(e)
      case PipeExpr(value, fn) => containsPlaceholder(value) || containsPlaceholder(fn)
      case ComposeExpr(f, g) => containsPlaceholder(f) || containsPlaceholder(g)

  private def replacePlaceholder(expr: Expr): Expr =
    expr match
      case PlaceholderExpr() => VarExpr(placeholderParam).setPos(expr.pos)
      case NumberExpr(_) | StringExpr(_) | VarExpr(_) => expr
      case FunctionExpr(params, body) => FunctionExpr(params, replacePlaceholder(body)).setPos(expr.pos)
      case ArrayExpr(elems) => ArrayExpr(elems.map(replacePlaceholder)).setPos(expr.pos)
      case BinaryExpr(op, left, right) =>
        BinaryExpr(op, replacePlaceholder(left), replacePlaceholder(right)).setPos(expr.pos)
      case UnaryExpr(op, e) => UnaryExpr(op, replacePlaceholder(e)).setPos(expr.pos)
      case ApplyExpr(fn, args) => ApplyExpr(replacePlaceholder(fn), args.map(replacePlaceholder)).setPos(expr.pos)
      case AssignExpr(name, e) => AssignExpr(name, replacePlaceholder(e)).setPos(expr.pos)
      case PipeExpr(value, fn) => PipeExpr(replacePlaceholder(value), replacePlaceholder(fn)).setPos(expr.pos)
      case ComposeExpr(f, g) => ComposeExpr(replacePlaceholder(f), replacePlaceholder(g)).setPos(expr.pos)
