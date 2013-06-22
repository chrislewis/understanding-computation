package understandingcomputation.simple

/* Simple allows mutations in the environment through statements. Statements
   could be forced under the Expression tree with the introduction of a Unit
   primative, but for now we'll keep with the book and keep them separated. */
sealed trait Statement

/* A pair of statements. */
final case class Sequence(first: Statement, second: Statement) extends Statement

/* An assigmnet binds an identifier to an expression when evaluated in an environment. */
final case class Assign(name: Symbol, expr: Expression) extends Statement

/* Constrol structures. */
final case class If(condition: Expression, consequence: Statement, alternative: Statement) extends Statement
final case class While(condition: Expression, stmt: Statement) extends Statement

/* A bottom value for statements indicating no further evaluation is possible. */
case object DoNothing extends Statement