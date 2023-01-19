package org.clulab.utils

class TestThreading extends Test {
  val threads = 26
  val numbers = 0.until(threads)

  {
    val parNumbers = ThreadUtils.parallelize(numbers, threads)

    parNumbers.foreach { number =>
      println(number)
    }
  }
}
