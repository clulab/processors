package org.clulab.odin.debugger.debug.filter

trait Filter[ArgumentType] {
  def apply(argument: ArgumentType): Boolean
}
