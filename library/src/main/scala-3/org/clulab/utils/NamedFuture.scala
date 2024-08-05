package org.clulab.utils

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

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
