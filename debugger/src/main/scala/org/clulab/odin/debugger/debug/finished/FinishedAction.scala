package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.ImmutableDebuggerContext

class FinishedLocalAction(
  debuggerContext: ImmutableDebuggerContext,
  val inMentions: Seq[Mention],
  val outMentions: Seq[Mention]
) extends Finished(debuggerContext)

class FinishedGlobalAction(
  debuggerContext: ImmutableDebuggerContext,
  val inMentions: Seq[Mention],
  val outMentions: Seq[Mention],
) extends Finished(debuggerContext)
