package org.clulab.odin.impl

/**
  * Represents a regex quantifier
  * Note that graph pattern argument quantifiers are never lazy
  */
trait ArgumentQuantifier

/** No quantifier */
case object NullQuantifier extends ArgumentQuantifier

/** Equiv. to regex ? */
case object OptionalQuantifier extends ArgumentQuantifier

/** Encodes regex * and *? */
case object KleeneStar extends ArgumentQuantifier

/** Encodes regex + and +? */
case object OneOrMore extends ArgumentQuantifier

/** Encodes regex {n} */
case class ExactQuantifier(repeat: Int) extends ArgumentQuantifier {
  require(repeat > 0, "ExactQuantifier must be a positive number")
}
/**
  * Encodes regex {,e} {s,} {s,e} and lazy variants
  */
case class RangedQuantifier(
  minRepeat: Option[Int] = None,
  maxRepeat: Option[Int] = None
) extends ArgumentQuantifier {
  require(minRepeat.nonEmpty || maxRepeat.nonEmpty, "RangedQuantifier must have either either a minRepeat or maxRepeat")
  require(if (minRepeat.nonEmpty) minRepeat.get >= 0 else true, "minRepeat for RangedQuantifier cannot be negative")
  require(if (maxRepeat.nonEmpty) maxRepeat.get >= 0 else true, "maxRepeat for RangedQuantifier cannot be negative")
}