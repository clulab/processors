package org.clulab.odin.impl

/**
  * Represents a regex quantifier
  */
trait Quantifier {
  // whether or not the quantifier is greedy (true) or lazy (i.e., greedy = false)
  val greedy: Boolean = true
}

/** No quantifier */
case class NullQuantifier() extends Quantifier

/** Equiv. to regex ? */
case class OptionalQuantifier() extends Quantifier { override val greedy: Boolean = false }

/** Encodes regex * and *? */
case class KleeneStar(override val greedy: Boolean) extends Quantifier

/** Encodes regex + and +? */
case class OneOrMore(override val greedy: Boolean) extends Quantifier

/** Encodes regex {n} */
case class ExactQuantifier(repeat: Int) extends Quantifier {
  require(repeat > 0, "ExactQuantifier must be a positive number")
}
/** Encodes regex {,e} {s,} {s,e} and lazy variants */
case class RangedQuantifier(
  minRepeat: Option[Int] = None,
  maxRepeat: Option[Int] = None,
  override val greedy: Boolean = true
) extends Quantifier {
  require(minRepeat.nonEmpty || maxRepeat.nonEmpty, "RangedQuantifier must have either either a minRepeat or maxRepeat")
  require(if (minRepeat.nonEmpty) minRepeat.get >= 0 else true, "minRepeat for RangedQuantifier cannot be negative")
  require(if (maxRepeat.nonEmpty) maxRepeat.get >= 0 else true, "maxRepeat for RangedQuantifier cannot be negative")
}