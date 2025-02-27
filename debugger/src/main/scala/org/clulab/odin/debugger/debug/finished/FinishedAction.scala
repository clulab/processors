package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.StaticDebuggerContext

class FinishedLocalAction(
  debuggerContext: StaticDebuggerContext,
  val inMentions: Seq[Mention],
  val outMentions: Seq[Mention]
) extends Finished(debuggerContext)

class FinishedGlobalAction(
  debuggerContext: StaticDebuggerContext,
  val inMentions: Seq[Mention],
  val outMentions: Seq[Mention],
) extends Finished(debuggerContext)
