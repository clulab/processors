package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.{MentionMatch, ImmutableDebuggerContext}

class FinishedMention(
  debuggerContext: ImmutableDebuggerContext,
  val mention: Mention,
  val stateMentions: Seq[Mention],
  val mentionMatches: Seq[MentionMatch]
) extends Finished(debuggerContext)
