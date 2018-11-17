package org.clulab.utils

import scala.util.control.NonFatal

object Closer {

  def close[Closeable <: {def close() : Unit}](resource: => Closeable): Unit = resource.close()

  // This is so that exceptions caused during close are caught, but don't
  // prevent the registration of any previous exception.
  // See also https://medium.com/@dkomanov/scala-try-with-resources-735baad0fd7d.
  // Others have resource: => Closeable, but I want the resource evaluated beforehand
  // so that it doesn't throw an exception before there is anything to close.
  def autoClose[Closeable <: {def close() : Unit}, Result](resource: Closeable)(function: Closeable => Result): Result = {

    val (result: Option[Result], exception: Option[Throwable]) = try {
      (Some(function(resource)), None)
    }
    catch {
      case exception: Throwable => (None, Some(exception))
    }

    val closeException: Option[Throwable] = Option(resource).map { resource =>
      try {
        resource.close()
        None
      }
      catch {
        case exception: Throwable => Some(exception)
      }
    }.flatten

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
}
