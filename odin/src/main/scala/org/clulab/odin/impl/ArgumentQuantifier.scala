package org.clulab.odin.impl

/**
  * Encodes regex *, +, {e}, {,e} {s,} {s,e} and null quantifier (represented as {1,1})
  * Note that graph pattern argument quantifiers are never lazy
  */
case class ArgumentQuantifier(
  minRepeat: Option[Int] = None,
  maxRepeat: Option[Int] = None
) {
  require(if (minRepeat.nonEmpty) minRepeat.get >= 0 else true, "minRepeat for RangedQuantifier cannot be negative")
  require(if (maxRepeat.nonEmpty) maxRepeat.get >= 0 else true, "maxRepeat for RangedQuantifier cannot be negative")
}