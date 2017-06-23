package org.clulab.odin.impl


/**
  * Quantifiers for graph pattern arguments.
  * Note that graph pattern argument quantifiers are never lazy
  */
trait ArgumentQuantifier

case object NullQuantifier extends ArgumentQuantifier

case class ExactQuantifier(reps: Int) extends ArgumentQuantifier

/**
  * Encodes regex *, +, {,e} {s,} {s,e} and null quantifier (represented as {1,1})
  */
case class RangedQuantifier(
  minRepeat: Option[Int] = None,
  maxRepeat: Option[Int] = None
) extends ArgumentQuantifier {
  require(if (minRepeat.nonEmpty) minRepeat.get >= 0 else true, "minRepeat for RangedQuantifier cannot be negative")
  require(if (maxRepeat.nonEmpty) maxRepeat.get >= 0 else true, "maxRepeat for RangedQuantifier cannot be negative")
  require(if (minRepeat.isDefined && maxRepeat.isDefined) minRepeat.get < maxRepeat.get else true, "minRepeat for RangedQuantifier must be less than maxRepeat")
}