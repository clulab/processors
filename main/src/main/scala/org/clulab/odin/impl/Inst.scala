package org.clulab.odin.impl

// instruction
sealed trait Inst {
  var posId: Int = 0
  var next: Inst = null
  def dup(): Inst
  def deepcopy(): Inst = {
    val inst = dup()
    if (next != null) inst.next = next.deepcopy()
    inst
  }
}

// the pattern matched succesfully
case object Done extends Inst {
  def dup() = this
}

// no operation
case class Pass() extends Inst {
  def dup() = copy()
}

// split execution
case class Split(lhs: Inst, rhs: Inst) extends Inst {
  def dup() = Split(lhs.deepcopy(), rhs.deepcopy())
  override def hashCode: Int = (lhs, rhs, posId).##
}

// start capturing tokens
case class SaveStart(name: String) extends Inst {
  def dup() = copy()
  override def hashCode: Int = ("start", name, posId).##
}

// end capturing tokens
case class SaveEnd(name: String) extends Inst {
  def dup() = copy()
  override def hashCode: Int = ("end", name, posId).##
}

// matches token using token constraint
case class MatchToken(c: TokenConstraint) extends Inst {
  def dup() = copy()
  override def hashCode: Int = (c, posId).##
}

// matches mention by label using string matcher
case class MatchMention(
                         m: StringMatcher,
                         name: Option[String],
                         arg: Option[String]
                       ) extends Inst {
  def dup() = copy()
  override def hashCode: Int = (m, name, arg, posId).##
}

// matches sentence start
case class MatchSentenceStart() extends Inst {
  def dup() = copy()
}

// matches sentence end
case class MatchSentenceEnd() extends Inst {
  def dup() = copy()
}

// zero-width look-ahead assertion
case class MatchLookAhead(start: Inst, negative: Boolean) extends Inst {
  def dup() = MatchLookAhead(start.deepcopy(), negative)
  override def hashCode: Int = ("lookahead", start, negative).##
}

// zero-width look-behind assertion
case class MatchLookBehind(start: Inst, negative: Boolean) extends Inst {
  def dup() = MatchLookBehind(start.deepcopy(), negative)
  override def hashCode: Int = ("lookbehind", start, negative).##
}
