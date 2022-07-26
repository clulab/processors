package org.clulab.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestThreading extends AnyFlatSpec with Matchers {
  val threads = 26
  val numbers = 0.until(threads)

  {
    val parNumbers = ThreadUtils.parallelize(numbers, threads)

    parNumbers.foreach { number =>
      println(number)
    }
  }
}
