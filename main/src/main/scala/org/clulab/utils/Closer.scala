package org.clulab.utils

import scala.io.Source
import scala.language.implicitConversions
import scala.util.control.NonFatal

object Closer {

  trait Releasable[Resource] {
    def release(resource: Resource): Unit
  }

  object Releasable {

    implicit def releasableAutoCloseable[Resource <: AutoCloseable]: Releasable[Resource] = {
      new Releasable[Resource] {
        def release(resource: Resource): Unit = resource.close()
      }
    }

    // In Scala 2.11, Source does not inherit from Closeable, so one has to tell Closer how to close() it.
    implicit def releasableSource[Resource <: Source]: Releasable[Resource] = {
      new Releasable[Resource] {
        def release(resource: Resource): Unit = resource.close()
      }
    }
  }

  def close[Resource: Releasable](resource: => Resource): Unit =
      implicitly[Releasable[Resource]].release(resource)

  // This is so that exceptions caused during close are caught, but don't
  // prevent the registration of any previous exception.
  // See also https://medium.com/@dkomanov/scala-try-with-resources-735baad0fd7d.
  // Others have resource: => Closeable, but I want the resource evaluated beforehand
  // so that it doesn't throw an exception before there is anything to close.
  // 3 here is for the number of arguments.  Operator overloading doesn't handle it.
  protected def autoClose3[Resource, Result](resource: Resource)(closer: () => Unit)(function: Resource => Result): Result = {

    val (result: Option[Result], exception: Option[Throwable]) = try {
      (Some(function(resource)), None)
    }
    catch {
      case exception: Throwable => (None, Some(exception))
    }

    val closeException: Option[Throwable] = Option(resource).flatMap { _ =>
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

  def autoClose[Resource: Releasable, Result](resource: Resource)(function: Resource => Result): Result =
      autoClose3(resource)(() => implicitly[Releasable[Resource]].release(resource))(function)

  implicit class AutoCloser[Resource: Releasable](resource: Resource) {

    def autoClose[Result](function: Resource => Result): Result =
        Closer.autoClose(resource)(function)
  }
}
