package understandingcomputation.simple

/* Simple allows mutations in the environment through statements. Statements
   could be forced under the Expression tree with the introduction of a Unit
   primative, but for now we'll keep with the book and keep them separated. */
sealed trait Statement
case object DoNothing extends Statement
case class Assign(name: String, expr: Expression) extends Statement
case class If(condition: Expression, consequence: Statement, alternative: Statement) extends Statement