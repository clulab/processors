package org.clulab.utils

import scala.util.Using

class TestThreading extends Test {
  val threads = 26
  val numbers = 0.until(threads)

  {
    val parNumbers = Parallelizer.parallelize(numbers)

    parNumbers.foreach { number =>
      println(number)
    }
  }

  class InspectableParallelizer[T](iterable: Iterable[T], threadLimit: Int)
      extends Parallelizer[T](iterable, threadLimit) {
    def isShutdown: Boolean = forkJoinPool.isShutdown
  }

  behavior of "Parallelizer"

  it should "shut down its thread pool when closed" in {
    val parallelizer = new InspectableParallelizer(numbers, threads)

    Using.resource(parallelizer) { resource =>
      resource.par.sum should be(numbers.sum)
    }

    parallelizer.isShutdown should be(true)
  }

  it should "parallelize sets and shut down their thread pool" in {
    val parallelizer = new InspectableParallelizer(numbers.toSet, threads)

    val doubled = Using.resource(parallelizer) { resource =>
      resource.par.map(_ * 2).seq.toSet
    }

    doubled should be(numbers.map(_ * 2).toSet)
    parallelizer.isShutdown should be(true)
  }
}
