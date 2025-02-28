package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.debugger.debug.ThreadMatch
import org.clulab.odin.debugger.debug.context.ImmutableDebuggerContext
import org.clulab.odin.impl.ThompsonVM.SingleThread

class FinishedThread(
  debuggerContext: ImmutableDebuggerContext,
  val thread: SingleThread,
  val instMatch: Boolean,
  val threadMatch: ThreadMatch
) extends Finished(debuggerContext)
