package understandingcomputation

package object simple {
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