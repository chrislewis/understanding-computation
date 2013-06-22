package understandingcomputation.simple

/* A semantics implements an interpretation strategy. */
sealed trait Semantics {
  def reduceExpression(expr: Expression, env: Environment): Expression
  def reduceStatement(stmt: Statement, env: Environment): (Statement, Environment)
}

object SmallStep extends Semantics {

  override def reduceExpression(expr: Expression, env: Environment): Expression =
    expr match {
      case n @ Number(_) => n

      case b @ Bool(_) => b

      case Variable(n) => env(n)

      case Add(Reducible(l), r) => Add(reduceExpression(l, env), r)
      case Add(l, Reducible(r)) => Add(l, reduceExpression(r, env))
      case Add(Number(l), Number(r)) => Number(l + r)

      case Multiply(Reducible(l), r) => Multiply(reduceExpression(l, env), r)
      case Multiply(l, Reducible(r)) => Multiply(l, reduceExpression(r, env))
      case Multiply(Number(l), Number(r)) => Number(l * r)

      case LessThan(Reducible(l), r) => LessThan(reduceExpression(l, env), r)
      case LessThan(l, Reducible(r)) => LessThan(l, reduceExpression(r, env))
      case LessThan(Number(l), Number(r)) => Bool(l < r)
    }

  override def reduceStatement(stmt: Statement, env: Environment): (Statement, Environment) =
    stmt match {
      case Assign(n, Reducible(expr)) => Assign(n, reduceExpression(expr, env)) -> env
      case Assign(n, expr) => DoNothing -> (env + (n -> expr))

      case If(Reducible(expr), c, a) => reduceStatement(If(reduceExpression(expr, env), c, a), env)
      case If(Bool(true), c, _) => c -> env
      case If(Bool(false), _, a) => a -> env

      case Sequence(DoNothing, s) => (s, env)
      case Sequence(f, s) =>
        val (rf, renv) = reduceStatement(f, env)
        (Sequence(rf, s), renv)

      case While(e, s) => reduceStatement(If(e, Sequence(s, While(e, s)), DoNothing), env)

      case _ => (stmt, env)
    }
}

object BigStep extends Semantics {

  /* Down cast the result of a reduction to a specific expression type. */
  private def evalExprAs[E <: Expression](expr: Expression, env: Environment): E =
    reduceExpression(expr, env).asInstanceOf[E]

  override def reduceExpression(expr: Expression, env: Environment): Expression =
    expr match {
      case b @ Bool(_) => b

      case n @ Number(_) => n

      case Variable(n) => env(n)

      case Add(l, r) => Number(evalExprAs[Number](l, env).value + evalExprAs[Number](r, env).value)

      case Multiply(l, r) => Number(evalExprAs[Number](l, env).value * evalExprAs[Number](r, env).value)

      case LessThan(l, r) => Bool(evalExprAs[Number](l, env).value < evalExprAs[Number](r, env).value)

    }

  override def reduceStatement(stmt: Statement, env: Environment): (Statement, Environment) =
    stmt match {
      case Assign(n, expr) => DoNothing -> (env + (n -> reduceExpression(expr, env)))

      case If(cond, c, a) =>
        if(reduceExpression(cond, env) == Bool(true)) reduceStatement(c, env)
        else reduceStatement(a, env)

      case Sequence(f, s) =>
        val (rf, renv) = reduceStatement(f, env)
        reduceStatement(s, renv)

      case While(c, s) =>
        if(reduceExpression(c, env) == Bool(true)) {
          val (_, renv) = reduceStatement(s, env)
          reduceStatement(While(c, s), renv)
        } else s -> env

      case DoNothing => DoNothing -> env
    }

}

