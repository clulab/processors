package org.clulab.odin.debugger.debug

import org.clulab.odin.Mention

case class FinishedLocalAction(
  inMentions: Seq[Mention],
  outMentions: Seq[Mention],
  debuggerRecord: DebuggerRecordForLocalAction
)

case class FinishedGlobalAction(
  inMentions: Seq[Mention],
  outMentions: Seq[Mention],
  debuggerRecord: DebuggerRecordForGlobalAction
)
