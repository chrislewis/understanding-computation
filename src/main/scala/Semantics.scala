package understandingcomputation.simple

/* A semantics implements an interpretation strategy. Evaluation returns a list
 * of the intermediate reductions taken by the Semantics in reverse order such
 * that the final reduction is the head.
 */
sealed trait Semantics {

  /** Evaluate an expression to completion. */
  def evaluateExpression(expr: Expression, env: Environment): List[Expression]

  /** Evaluate a statement to completion. */
  def evaluateStatement(stmt: Statement, env: Environment): List[(Statement, Environment)]

}

object SmallStep extends Semantics {

  override def evaluateExpression(expr: Expression, env: Environment): List[Expression] =
    foldExpression(expr, env)(List[Expression]())(_ :: _)

  override def evaluateStatement(stmt: Statement, env: Environment): List[(Statement, Environment)] =
    foldStatement(stmt, env)(List[(Statement, Environment)]())(_ :: _)

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

  def reduceStatement(stmt: Statement, env: Environment): (Statement, Environment) =
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
    evaluateExpression(expr, env).asInstanceOf[E]

  override def evaluateExpression(expr: Expression, env: Environment): List[Expression] =
    List(expr match {
      case b @ Bool(_) => b

      case n @ Number(_) => n

      case Variable(n) => env(n)

      case Add(l, r) => Number(evalExprAs[Number](l, env).value + evalExprAs[Number](r, env).value)

      case Multiply(l, r) => Number(evalExprAs[Number](l, env).value * evalExprAs[Number](r, env).value)

      case LessThan(l, r) => Bool(evalExprAs[Number](l, env).value < evalExprAs[Number](r, env).value)
    })

  override def evaluateStatement(stmt: Statement, env: Environment): List[(Statement, Environment)] =
    reduceStatement(stmt, env) :: Nil

  def reduceStatement(stmt: Statement, env: Environment): (Statement, Environment) =
    stmt match {
      case Assign(n, expr) => DoNothing -> (env + (n -> evaluateExpression(expr, env).head))

      case If(cond, c, a) =>
        if(evaluateExpression(cond, env) == Bool(true)) reduceStatement(c, env)
        else reduceStatement(a, env)

      case Sequence(f, s) =>
        val (rf, renv) = reduceStatement(f, env)
        reduceStatement(s, renv)

      case While(c, s) =>
        if(evaluateExpression(c, env) == Bool(true)) {
          val (_, renv) = reduceStatement(s, env)
          reduceStatement(While(c, s), renv)
        } else s -> env

      case DoNothing => DoNothing -> env
    }

}
