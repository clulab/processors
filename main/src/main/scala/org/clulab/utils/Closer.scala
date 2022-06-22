package org.clulab.utils

import scala.io.{BufferedSource, Source}
import scala.util.control.NonFatal

object Closer {

  def close(resource: => AutoCloseable): Unit = resource.close()

  // This is so that exceptions caused during close are caught, but don't
  // prevent the registration of any previous exception.
  // See also https://medium.com/@dkomanov/scala-try-with-resources-735baad0fd7d.
  // Others have resource: => Closeable, but I want the resource evaluated beforehand
  // so that it doesn't throw an exception before there is anything to close.
  def autoClose2[Resource, Result](resource: Resource)(closer: () => Unit)(function: Resource => Result): Result = {

    val (result: Option[Result], exception: Option[Throwable]) = try {
      (Some(function(resource)), None)
    }
    catch {
      case exception: Throwable => (None, Some(exception))
    }

    val closeException: Option[Throwable] = Option(resource).flatMap { resource =>
      try {
        closer()
        None
      }
      catch {
        case exception: Throwable => Some(exception)
      }
    }

    (exception, closeException) match {
      case (None, None) => result.get
      case (Some(ex), None) => throw ex
      case (None, Some(ex)) => throw ex
      case (Some(ex), Some(closeEx)) => (ex, closeEx) match {
        case (e, NonFatal(nonfatal)) =>
          // Put the potentially fatal one first.
          e.addSuppressed(nonfatal)
          throw e
        case (NonFatal(nonfatal), e) =>
          // Put the potentially fatal one first.
          e.addSuppressed(nonfatal)
          throw e
        case (e, closeE) =>
          // On tie, put exception before closeException.
          e.addSuppressed(closeE)
          throw e
      }
    }
  }

  // Two arguments can be used generically if the Resource has inherited from Closeable.
  def autoClose[Resource <: AutoCloseable, Result](resource: Resource)(function: Resource => Result): Result =
      Closer.autoClose2(resource)(() => resource.close())(function)

  // In Scala 2.11, Source does not inherit from Closeable, so one has to tell Closer how to close() it.
  implicit class AutoSource(resource: Source) {

    def autoClose[Result](function: Source => Result) =
        Closer.autoClose2(resource)(() => resource.close())(function)
  }

  // In Scala 2.11, Source does not inherit from Closeable, so one has to tell Closer how to close() it.
  implicit class AutoBufferedSource(resource: BufferedSource) {

    def autoClose[Result](function: Source => Result) =
      Closer.autoClose2(resource)(() => resource.close())(function)
  }

  // Allow for alternative syntax closeable.autoClose { closeable => ... }
  implicit class AutoCloser[Resource <: AutoCloseable](resource: Resource) {

    def autoClose[Result](function: Resource => Result): Result =
        Closer.autoClose(resource)(function)
  }
}
