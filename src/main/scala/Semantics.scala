package understandingcomputation.simple

/* A semantics implements an interpretation strategy. */
sealed trait Semantics {

  /** Evaluate an expression to completion. */
  def evaluateExpression(expr: Expression, env: Environment): Expression

  /** Evaluate a statement to completion. */
  def evaluateStatement(stmt: Statement, env: Environment): (Statement, Environment)

}

object SmallStep extends Semantics {

  override def evaluateExpression(expr: Expression, env: Environment): Expression =
    exprReduction(expr, env).head

  override def evaluateStatement(stmt: Statement, env: Environment): (Statement, Environment) =
    stmtReduction(stmt, env).head

  val exprReduction: (Expression, Environment) => List[Expression] =
    foldExpression(_, _)(List[Expression]())(_ :: _)

  val stmtReduction: (Statement, Environment) => List[(Statement, Environment)] =
    foldStatement(_, _)(List[(Statement, Environment)]())(_ :: _)

  def foldExpression[A](expr: Expression, env: Environment)(zero: A)(op: (Expression, A) => A): A =
    expr match {
      case Reducible(ee) =>
        val rexpr = reduceExpression(ee, env)
        foldExpression(rexpr, env)(op(rexpr, zero))(op)
      case _ => zero
    }

  def foldStatement[A](stmt: Statement, env: Environment)(zero: A)(op: ((Statement, Environment), A) => A): A =
    stmt match {
      case DoNothing => zero
      case s =>
        val (rs, renv) = reduceStatement(s, env)
        foldStatement(rs, renv)(op((rs, renv), zero))(op)
    }

  def reduceExpression(expr: Expression, env: Environment): Expression =
    expr match {
      case n @ Number(_) => n

      case b @ Bool(_) => b

      case Variable(n) => env(n)

      case Add(Reducible(l), r) => Add(evaluateExpression(l, env), r)
      case Add(l, Reducible(r)) => Add(l, evaluateExpression(r, env))
      case Add(Number(l), Number(r)) => Number(l + r)

      case Multiply(Reducible(l), r) => Multiply(evaluateExpression(l, env), r)
      case Multiply(l, Reducible(r)) => Multiply(l, evaluateExpression(r, env))
      case Multiply(Number(l), Number(r)) => Number(l * r)

      case LessThan(Reducible(l), r) => LessThan(evaluateExpression(l, env), r)
      case LessThan(l, Reducible(r)) => LessThan(l, evaluateExpression(r, env))
      case LessThan(Number(l), Number(r)) => Bool(l < r)
    }

  def reduceStatement(stmt: Statement, env: Environment): (Statement, Environment) =
    stmt match {
      case Assign(n, Reducible(expr)) => Assign(n, evaluateExpression(expr, env)) -> env
      case Assign(n, expr) => DoNothing -> (env + (n -> expr))

      case If(Reducible(expr), c, a) => evaluateStatement(If(evaluateExpression(expr, env), c, a), env)
      case If(Bool(true), c, _) => c -> env
      case If(Bool(false), _, a) => a -> env

      case Sequence(DoNothing, s) => (s, env)
      case Sequence(f, s) =>
        val (rf, renv) = evaluateStatement(f, env)
        (Sequence(rf, s), renv)

      case While(e, s) => evaluateStatement(If(e, Sequence(s, While(e, s)), DoNothing), env)

      case _ => (stmt, env)
    }
}

object BigStep extends Semantics {

  /* Down cast the result of a reduction to a specific expression type. */
  private def evalExprAs[E <: Expression](expr: Expression, env: Environment): E =
    evaluateExpression(expr, env).asInstanceOf[E]

  override def evaluateExpression(expr: Expression, env: Environment): Expression =
    expr match {
      case b @ Bool(_) => b

      case n @ Number(_) => n

      case Variable(n) => env(n)

      case Add(l, r) => Number(evalExprAs[Number](l, env).value + evalExprAs[Number](r, env).value)

      case Multiply(l, r) => Number(evalExprAs[Number](l, env).value * evalExprAs[Number](r, env).value)

      case LessThan(l, r) => Bool(evalExprAs[Number](l, env).value < evalExprAs[Number](r, env).value)

    }

  override def evaluateStatement(stmt: Statement, env: Environment): (Statement, Environment) =
    stmt match {
      case Assign(n, expr) => DoNothing -> (env + (n -> evaluateExpression(expr, env)))

      case If(cond, c, a) =>
        if(evaluateExpression(cond, env) == Bool(true)) evaluateStatement(c, env)
        else evaluateStatement(a, env)

      case Sequence(f, s) =>
        val (rf, renv) = evaluateStatement(f, env)
        evaluateStatement(s, renv)

      case While(c, s) =>
        if(evaluateExpression(c, env) == Bool(true)) {
          val (_, renv) = evaluateStatement(s, env)
          evaluateStatement(While(c, s), renv)
        } else s -> env

      case DoNothing => DoNothing -> env
    }

}
