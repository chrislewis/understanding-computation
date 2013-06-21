package understandingcomputation

sealed trait Expression
case class Number(value: Int) extends Expression
case class Add(left: Expression, right: Expression) extends Expression
case class Multiply(left: Expression, right: Expression) extends Expression

case class Bool(value: Boolean) extends Expression
case class LessThan(left: Expression, right: Expression) extends Expression

case class Variable(name: String) extends Expression