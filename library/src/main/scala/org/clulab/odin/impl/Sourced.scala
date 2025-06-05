package org.clulab.odin.impl

trait Sourced[T] {
  def copyWithSource(source: String): T
}
