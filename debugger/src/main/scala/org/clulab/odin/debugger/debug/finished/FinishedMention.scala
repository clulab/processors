package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.{MentionMatch, StaticDebuggerContext}

class FinishedMention(
  debuggerContext: StaticDebuggerContext,
  val mention: Mention,
  val stateMentions: Seq[Mention],
  val mentionMatches: Seq[MentionMatch]
) extends Finished(debuggerContext)
