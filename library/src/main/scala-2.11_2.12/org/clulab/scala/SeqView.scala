package org.clulab.scala

object SeqView {
  type Type[T] = scala.collection.SeqView[T, Seq[T]]
}
