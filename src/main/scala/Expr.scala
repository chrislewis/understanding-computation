package understandingcomputation.simple

/* The root of the expression tree. */
sealed trait Expression

/* Primitive values. */
case class Number(value: Int) extends Expression
case class Bool(value: Boolean) extends Expression

/* Numeric operations. */
case class Add(left: Expression, right: Expression) extends Expression
case class Multiply(left: Expression, right: Expression) extends Expression

/* Logical expressions. */
case class LessThan(left: Expression, right: Expression) extends Expression

/* A binding to an expression in some environment. */
case class Variable(name: String) extends Expression