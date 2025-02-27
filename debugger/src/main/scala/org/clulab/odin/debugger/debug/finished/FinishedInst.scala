package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.debugger.debug.StaticDebuggerContext
import org.clulab.odin.impl.Inst

class FinishedInst(
  debuggerContext: StaticDebuggerContext,
  val inst: Inst,
  val instMatch: Boolean,
) extends Finished(debuggerContext)
