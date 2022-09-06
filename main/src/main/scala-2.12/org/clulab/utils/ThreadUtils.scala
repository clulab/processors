package org.clulab.utils

import java.util.concurrent.{ForkJoinPool => JavaForkJoinPool} // for Scala 2.12
//import scala.concurrent.forkjoin.{ForkJoinPool => ScalaForkJoinPool} // for Scala 2.11
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.ParIterable

object ThreadUtils extends ThreadUtilsShared() {

  def parallelize[T](parIterable: ParIterable[T], threads: Int): ParIterable[T] = {
    // There seems to be no way other than code generation to avoid the deprecation warning.
    // At least it is limited to one location by being contained in a library method.
    val forkJoinPool = new JavaForkJoinPool(threads) // for Scala 2.12
    // val forkJoinPool = new ScalaForkJoinPool(threads) // for Scala 2.11
    val forkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)

    parIterable.tasksupport = forkJoinTaskSupport
    parIterable
  }
}
