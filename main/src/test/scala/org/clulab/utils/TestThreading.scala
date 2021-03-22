package org.clulab.utils

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestThreading extends FlatSpec with Matchers {
  val threads = 26
  val numbers = 0.until(threads)

  {
    val parNumbers = ThreadUtils.parallelize(numbers, threads)

    parNumbers.foreach { number =>
      println(number)
    }
  }
}
