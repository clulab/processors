package org.clulab.odin.debugger.debug

import org.clulab.odin.impl.Inst

case class FinishedInst(
  inst: Inst,
  instMatch: Boolean,
  debuggerRecord: DebuggerRecord
)
