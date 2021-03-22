package org.clulab.utils

//import java.util.concurrent.{ForkJoinPool => JavaForkJoinPool}
import scala.concurrent.forkjoin.{ForkJoinPool => ScalaForkJoinPool}
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.ParIterable
import scala.collection.parallel.ParSeq
import scala.collection.parallel.ParSet

object ThreadUtils {

  def parallelize[T](parIterable: ParIterable[T], threads: Int): ParIterable[T] = {
    // There seems to be no way other than code generation to avoid the deprecation warning.
    // At least it is limited to one location by being contained in a library method.
    // val forkJoinPool = new JavaForkJoinPool(threads) // For Scala 2.12
    val forkJoinPool = new ScalaForkJoinPool(threads)
    val forkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)

    parIterable.tasksupport = forkJoinTaskSupport
    parIterable
  }

  def parallelize[T](seq: Seq[T], threads: Int): ParSeq[T] = {
    val parSeq = seq.par
    parallelize(parSeq, threads)
    parSeq
  }

  def parallelize[T](set: Set[T], threads: Int): ParSet[T] = {
    val parSet = set.par
    parallelize(parSet, threads)
    parSet
  }
}
