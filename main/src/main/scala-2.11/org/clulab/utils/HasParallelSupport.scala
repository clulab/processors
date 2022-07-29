package org.clulab.utils

import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel.{ForkJoinTaskSupport, ParSeq, ParSet}
import scala.concurrent.forkjoin.ForkJoinPool

trait HasParallelSupport {
  def toParSeq[T](seq: Seq[T]): ParSeq[T] = seq.par
  def toParSet[T](set: Set[T]): ParSet[T] = set.par

  def toParArray[T](array: Array[T]): ParArray[T] = array.par

  def forkJoinTaskSupport(threads: Int): ForkJoinTaskSupport = {
    val forkJoinPool = new ForkJoinPool(threads)
    new ForkJoinTaskSupport(forkJoinPool)
  }
}