package org.clulab.odin.debugger.debug.matches

case class MentionMatch(matches: Boolean, reason: String)

object MentionMatch {
  val stateMismatch = MentionMatch(false, "State mismatch")
  val intervalMismatch = MentionMatch(false, "Interval mismatch")
  val labelMismatch = MentionMatch(false, "Label mismatch")
  val typeMismatch = MentionMatch(false, "Type mismatch")
  val mentionMatch = MentionMatch(true, "Mention match")
}
