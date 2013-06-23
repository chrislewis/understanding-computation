package understandingcomputation.simple

class Machine(sem: Semantics) {

  def runExpression(expr: Expression, env: Environment) =
    Machine.runExpression(sem, expr, env)

  def runStatement(stmt: Statement, env: Environment) =
    Machine.runStatement(sem, stmt, env)

}

object Machine {

  def apply(sem: Semantics) = new Machine(sem)
  
  def runExpression(sem: Semantics, expr: Expression, env: Environment) =
    println(sem.evaluateExpression(expr, env).head)

  def runStatement(sem: Semantics, stmt: Statement, env: Environment) = {
    val (rs, renv) = sem.evaluateStatement(stmt, env).head
    println(rs + " ---- " + renv)
  }

}
