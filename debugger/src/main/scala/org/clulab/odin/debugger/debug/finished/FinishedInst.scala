package org.clulab.odin.debugger.debug.finished

import org.clulab.odin.debugger.debug.ImmutableDebuggerContext
import org.clulab.odin.impl.Inst

class FinishedInst(
  debuggerContext: ImmutableDebuggerContext,
  val inst: Inst,
  val instMatch: Boolean
) extends Finished(debuggerContext)
