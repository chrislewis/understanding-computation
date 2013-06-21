package understandingcomputation

object Reducible {
  def unapply(e: Expression) =
    e match {
      case Bool(_) | Number(_) => None
      case _ => Some(e)
    }
}

object Machine {

  type Environment = Map[String, Expression] // TODO expr means variables can bind to expressions

  def reduceExpression(expr: Expression, env: Environment): Expression =
    expr match {
      case n @ Number(_) => n

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

      case _ => (stmt, env)
    }

  def runExpression(expr: Expression, env: Environment) {
    println(expr)
    expr match {
      case Reducible(ee) => runExpression(reduceExpression(ee, env), env)
      case _ => ()
    }
  }

  def runStatement(stmt: Statement, env: Environment) {
    println(stmt + " ---- " + env)
    stmt match {
      case DoNothing => ()
      case s =>
        val (rs, renv) = reduceStatement(s, env)
        runStatement(rs, renv)
    }
  }

}