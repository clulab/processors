package org.clulab.utils

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.{ForkJoinTaskSupport, ParIterable, ParSeq, ParSet}
import scala.concurrent.{ExecutionContext, Future}

object ThreadUtils extends HasParallelSupport {

  def parallelize[T](parIterable: ParIterable[T], threads: Int): ParIterable[T] = {
    parIterable.tasksupport = forkJoinTaskSupport(threads)
    parIterable
  }

  def parallelize[T](seq: Seq[T], threads: Int): ParSeq[T] = {
    val parSeq = toParSeq(seq)
    parallelize(parSeq, threads)
    parSeq
  }

  def parallelize[T](set: Set[T], threads: Int): ParSet[T] = {
    val parSet = toParSet(set)
    parallelize(parSet, threads)
    parSet
  }

  object NamedFuture {

    def apply[T](name: String)(body: => T)(implicit @deprecatedName('execctx) executor: ExecutionContext): Future[T] = {
      Future {
        val thread = Thread.currentThread
        val oldName = thread.getName
        val result = try {
          thread.setName(s"$oldName ($name)")
          body
        }
        finally {
          thread.setName(oldName)
        }

        result
      }(executor)
    }
  }
}
