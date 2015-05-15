package edu.arizona.sista.odin

/** Inherit from this class to implement your custom actions.
  *
  * An action is a method of the form:
  * {{{
  * def customAction(mentions: Seq[Mention], state: State): Seq[Mention]
  * }}}
  */
class Actions {
  /** The default action. Set to the identityAction. */
  val default: Action = identityAction
}
