package org.clulab.odin.debugger.debug

import org.clulab.odin.Mention

case class FinishedMention( // TODO: May be different for Cross and Graph
  mention: Mention,
  stateMentions: Seq[Mention],
  mentionMatches: Seq[MentionMatch],
  debuggerRecord: DebuggerRecordForMention
)
