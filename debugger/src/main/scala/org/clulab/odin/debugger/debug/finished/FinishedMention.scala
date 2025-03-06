package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.context.ImmutableDebuggerContext
import org.clulab.odin.debugger.debug.matches.MentionMatch

class FinishedMention(
  debuggerContext: ImmutableDebuggerContext,
  val mention: Mention,
  val stateMentions: Seq[Mention],
  val mentionMatches: Seq[MentionMatch]
) extends Finished(debuggerContext)
