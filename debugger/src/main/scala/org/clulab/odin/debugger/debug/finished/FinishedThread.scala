package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.debugger.debug.{ImmutableDebuggerContext, ThreadMatch}
import org.clulab.odin.impl.ThompsonVM.SingleThread

class FinishedThread(
  debuggerContext: ImmutableDebuggerContext,
  val thread: SingleThread,
  val threadMatch: ThreadMatch
) extends Finished(debuggerContext)
