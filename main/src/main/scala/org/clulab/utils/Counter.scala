package org.clulab.utils

class Counter(protected var value: Int, step: Int) {

  def inc(increment: Int = 1) = {
    value += (increment * step)
    value
  }

  def get: Int = value
}

object Counter {

  def apply(start: Int = 0, step: Int = 1): Counter = new Counter(start, step)
}
