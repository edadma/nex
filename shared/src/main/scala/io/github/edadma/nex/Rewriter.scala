package io.github.edadma.nex

object Rewriter:
  private val placeholderParam = "_arg"

  def rewrite(expr: Expr): Expr =
    expr match
      // Don't wrap these in Lambda - just rewrite their children
      case Apply(fn, args) => Apply(rewrite(fn), args.map(rewrite)).setPos(expr.pos)
      case ArrayLit(elems) => ArrayLit(elems.map(rewrite)).setPos(expr.pos)
      case Assign(name, e) => Assign(name, rewrite(e)).setPos(expr.pos)
      case Lambda(p, body) => Lambda(p, rewrite(body)).setPos(expr.pos)
      case Pipe(value, fn) => Pipe(rewrite(value), rewrite(fn)).setPos(expr.pos)
      case Compose(f, g) => Compose(rewrite(f), rewrite(g)).setPos(expr.pos)
      // Wrap these in Lambda if they contain placeholders
      case _ if containsPlaceholder(expr) =>
        Lambda(placeholderParam, replacePlaceholder(expr)).setPos(expr.pos)
      case _ => expr

  private def containsPlaceholder(expr: Expr): Boolean =
    expr match
      case Placeholder() => true
      case Num(_) | Str(_) | Var(_) => false
      case Lambda(_, body) => containsPlaceholder(body)
      case ArrayLit(elems) => elems.exists(containsPlaceholder)
      case BinOp(_, left, right) => containsPlaceholder(left) || containsPlaceholder(right)
      case UnaryOp(_, e) => containsPlaceholder(e)
      case Apply(fn, args) => containsPlaceholder(fn) || args.exists(containsPlaceholder)
      case Assign(_, e) => containsPlaceholder(e)
      case Pipe(value, fn) => containsPlaceholder(value) || containsPlaceholder(fn)
      case Compose(f, g) => containsPlaceholder(f) || containsPlaceholder(g)

  private def replacePlaceholder(expr: Expr): Expr =
    expr match
      case Placeholder() => Var(placeholderParam).setPos(expr.pos)
      case Num(_) | Str(_) | Var(_) => expr
      case Lambda(p, body) => Lambda(p, replacePlaceholder(body)).setPos(expr.pos)
      case ArrayLit(elems) => ArrayLit(elems.map(replacePlaceholder)).setPos(expr.pos)
      case BinOp(op, left, right) =>
        BinOp(op, replacePlaceholder(left), replacePlaceholder(right)).setPos(expr.pos)
      case UnaryOp(op, e) => UnaryOp(op, replacePlaceholder(e)).setPos(expr.pos)
      case Apply(fn, args) => Apply(replacePlaceholder(fn), args.map(replacePlaceholder)).setPos(expr.pos)
      case Assign(name, e) => Assign(name, replacePlaceholder(e)).setPos(expr.pos)
      case Pipe(value, fn) => Pipe(replacePlaceholder(value), replacePlaceholder(fn)).setPos(expr.pos)
      case Compose(f, g) => Compose(replacePlaceholder(f), replacePlaceholder(g)).setPos(expr.pos)
