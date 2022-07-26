package org.clulab.utils

import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.ParIterable
import scala.collection.parallel.ParSeq
import scala.collection.parallel.ParSet
import scala.concurrent.{ExecutionContext, Future}

object ThreadUtils {
  import scala.collection.parallel.CollectionConverters._

  def parallelize[T](parIterable: ParIterable[T], threads: Int): ParIterable[T] = {
    // There seems to be no way other than code generation to avoid the deprecation warning.
    // At least it is limited to one location by being contained in a library method.
    // val forkJoinPool = new JavaForkJoinPool(threads) // For Scala 2.12
    val forkJoinPool = new ForkJoinPool(threads)
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

  object NamedFuture {

    def apply[T](name: String)(body: => T)(implicit @deprecatedName("execctx") executor: ExecutionContext): Future[T] = {
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
