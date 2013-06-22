package understandingcomputation.simple

/* The root of the expression tree. */
sealed trait Expression

/* Primitive values. */
final case class Number(value: Int) extends Expression
final case class Bool(value: Boolean) extends Expression

/* Numeric operations. */
final case class Add(left: Expression, right: Expression) extends Expression
final case class Multiply(left: Expression, right: Expression) extends Expression

/* Logical expressions. */
final case class LessThan(left: Expression, right: Expression) extends Expression

/* A binding to an expression in some environment. */
final case class Variable(name: Symbol) extends Expression