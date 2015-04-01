package edu.arizona.sista.odin

/** Inherit from this class to implement your custom actions.
  *
  * An action is a method of the form:
  * {{{
  * def customAction(mentions: Seq[Mention], state: State): Seq[Mention]
  * }}}
  */
class Actions {
  /** Returns the mentions untransformed */
  def identity(mentions: Seq[Mention], state: State): Seq[Mention] = mentions
}
