package understandingcomputation.simple

object `package` {
    /* An environment is a table of identifiers to expressions. As there is no
     expression type to represent irreducible values, an identifier can bind to
     a reducible expression or a fully reduced primitive. */
  type Environment = Map[Symbol, Expression]

  /* A Convenient extractor to test for reducible expressions. */
  object Reducible {
    def unapply(e: Expression) =
      e match {
        case Bool(_) | Number(_) => None
        case _ => Some(e)
      }
  }
}

object Machine {

  def foldExpression[A](sem: Semantics, expr: Expression, env: Environment)(zero: A)(op: (Expression, A) => A): A =
    expr match {
      case Reducible(ee) =>
        val rexpr = sem.reduceExpression(ee, env)
        foldExpression(sem, rexpr, env)(op(rexpr, zero))(op)
      case _ => zero
    }

  def foldStatement[A](sem: Semantics, stmt: Statement, env: Environment)(zero: A)(op: (Statement, Environment, A) => A): A =
    stmt match {
      case DoNothing => zero
      case s =>
        val (rs, renv) = sem.reduceStatement(s, env)
        foldStatement(sem, rs, renv)(op(rs, renv, zero))(op) // TODO will miss the first
    }

  def runExpression(sem: Semantics, expr: Expression, env: Environment) =
    foldExpression(sem, expr, env)(()) { (e, _) => println(e) }

  def runStatement(sem: Semantics, stmt: Statement, env: Environment) =
    foldStatement(sem, stmt, env)(()) { (stmt, env, _) => println(stmt + " ---- " + env) }

}
