package org.clulab.utils

import scala.collection.parallel.ParSeq
import scala.collection.parallel.ParSet

abstract class ParallelizerShared() {

  def parallelize[T](seq: Seq[T]): ParSeq[T] = seq.par

  def parallelize[T](set: Set[T]): ParSet[T] = set.par
}
