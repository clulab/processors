package org.clulab.odin.debugger.utils

import org.clulab.odin.debugger.debug.filter.DynamicDebuggerFilter
import org.clulab.odin.debugger.debug.finished.Finished

import scala.collection.mutable

class Transcript[T <: Finished](val values: mutable.Buffer[T]) {

  def clear: Unit = values.clear

  def append(value: T): Unit = values += value

  def appendOpt(valueOpt: Option[T]): Unit = valueOpt.foreach(append)

  def appendAll(transcript: Transcript[T]): Unit = values.appendAll(transcript.values)

  def filter(f: DynamicDebuggerFilter): Transcript[T] = {
    val newValues = values.filter { value =>
      f(value.debuggerContext)
    }

    new Transcript(newValues)
  }

  def map[A](f: T => A): Seq[A] = values.map(f)
}

object Transcript {

  def apply[T <: Finished](): Transcript[T] = empty

  def empty[T <: Finished]: Transcript[T] = new Transcript[T](mutable.Buffer.empty[T])
}
