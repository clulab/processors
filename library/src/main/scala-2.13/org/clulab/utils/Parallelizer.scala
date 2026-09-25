package org.clulab.utils

import java.util.concurrent.{ForkJoinPool => JavaForkJoinPool} // for Scala 2.12
// import scala.concurrent.forkjoin.{ForkJoinPool => ScalaForkJoinPool} // for Scala 2.11
import scala.collection.parallel.CollectionConverters._ // Scala 2.13+
import scala.collection.parallel.ExecutionContextTaskSupport
import scala.collection.parallel.ParIterable
import scala.collection.parallel.ParSeq
import scala.collection.parallel.ParSet
import scala.concurrent.ExecutionContext

class Parallelizer[T](iterable: Iterable[T], threadLimit: Int) extends AutoCloseable {
  protected val forkJoinPool = new JavaForkJoinPool(threadLimit)
  val par: ParIterable[T] = {
    val executionContext = ExecutionContext.fromExecutor(forkJoinPool)
    val taskSupport = new ExecutionContextTaskSupport(executionContext)
    val parIterable = iterable.par

    parIterable.tasksupport = taskSupport
    parIterable
  }

  override def close(): Unit = forkJoinPool.shutdown()
}

object Parallelizer {

  def parallelize[T](seq: Seq[T]): ParSeq[T] = seq.par

  def parallelize[T](set: Set[T]): ParSet[T] = set.par
}
