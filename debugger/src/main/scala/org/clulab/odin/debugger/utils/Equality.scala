package org.clulab.odin.debugger.utils

trait Equality {
  def hashCode(): Int
  def equals(other: Any): Boolean
}
