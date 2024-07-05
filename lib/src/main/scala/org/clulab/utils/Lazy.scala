package org.clulab.utils

// This works around the impossibility of Array[(=> T)].
// Use instead Array[Lazy[T]].
class Lazy[+T](_value: => T) {

  lazy val value: T = _value
}

object Lazy {

  def apply[T](value: => T): Lazy[T] = new Lazy(value)
}
