package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.debugger.debug.{StaticDebuggerContext, ThreadMatch}
import org.clulab.odin.impl.ThompsonVM.SingleThread

class FinishedThread(
  debuggerContext: StaticDebuggerContext,
  val thread: SingleThread,
  val threadMatch: ThreadMatch
) extends Finished(debuggerContext)
