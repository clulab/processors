package org.clulab.scaladynet.vectors

abstract class Vector[T]() {

  def length: Int

  def size: Int = length

  def nonEmpty: Boolean = length > 0
}
