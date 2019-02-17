package org.clulab.fatdynet.utils

import scala.util.control.NonFatal

object Deleter {

  protected type Deleteable = {def delete(): Boolean}

  def delete[Resource <: Deleteable](resource: => Resource): Unit = resource.delete()

  // See also Closer
  def autoDelete[Resource <: Deleteable, Result](resource: Resource)(function: Resource => Result): Result = {

    val (result: Option[Result], exception: Option[Throwable]) = try {
      (Some(function(resource)), None)
    }
    catch {
      case exception: Throwable => (None, Some(exception))
    }

    val deleteException: Option[Throwable] = Option(resource).flatMap { resource =>
      try {
        resource.delete()
        None
      }
      catch {
        case exception: Throwable => Some(exception)
      }
    }

    (exception, deleteException) match {
      case (None, None) => result.get
      case (Some(ex), None) => throw ex
      case (None, Some(ex)) => throw ex
      case (Some(ex), Some(deleteEx)) => (ex, deleteEx) match {
        case (e, NonFatal(nonfatal)) =>
          // Put the potentially fatal one first.
          e.addSuppressed(nonfatal)
          throw e
        case (NonFatal(nonfatal), e) =>
          // Put the potentially fatal one first.
          e.addSuppressed(nonfatal)
          throw e
        case (e, deleteE) =>
          // On tie, put exception before deleteException.
          e.addSuppressed(deleteE)
          throw e
      }
    }
  }

  // Allow for alternative syntax deleteable.autoDelete { deleteable => ... }
  implicit class AutoDeleter[Resource <: Deleter.Deleteable](resource: Resource) {

    def autoDelete[Result](function: Resource => Result): Result = Deleter.autoDelete(resource)(function)
  }
}
