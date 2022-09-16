package org.clulab.utils

import scala.collection.parallel.ParIterable
import scala.collection.parallel.ParSeq
import scala.collection.parallel.ParSet

abstract class ThreadUtilsShared() {

  def parallelize[T](parIterable: ParIterable[T], threads: Int): ParIterable[T]

  def parallelize[T](seq: Seq[T], threads: Int): ParSeq[T] = {
    val parSeq = seq.par
    parallelize(parSeq, threads)
    parSeq
  }

  def parallelize[T](seq: Seq[T]): ParSeq[T] = seq.par

  def parallelize[T](set: Set[T], threads: Int): ParSet[T] = {
    val parSet = set.par
    parallelize(parSet, threads)
    parSet
  }

  def parallelize[T](set: Set[T]): ParSet[T] = set.par
}
