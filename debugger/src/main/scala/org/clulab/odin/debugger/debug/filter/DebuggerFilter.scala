package org.clulab.odin.debugger.debug.filter

trait DebuggerFilter[ArgumentType] {
  def apply(argument: ArgumentType): Boolean
}
