package org.clulab.odin.impl

trait Sourced[T] {
  val sourceOpt: Option[String]
  def copyWithSource(source: String): T
}
