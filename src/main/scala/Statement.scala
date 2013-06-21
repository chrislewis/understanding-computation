package understandingcomputation

sealed trait Statement
case object DoNothing extends Statement
case class Assign(name: String, expr: Expression) extends Statement
case class If(condition: Expression, consequence: Statement, alternative: Statement) extends Statement