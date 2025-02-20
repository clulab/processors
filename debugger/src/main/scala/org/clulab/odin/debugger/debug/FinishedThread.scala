package org.clulab.odin.debugger.debug

import org.clulab.odin.impl.ThompsonVM.SingleThread

case class FinishedThread(
  thread: SingleThread,
  instMatch: Boolean,
  threadMatch: ThreadMatch,
  debuggerRecord: DebuggerRecord
)
