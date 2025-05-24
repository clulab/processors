package org.clulab.scala

object SeqView {
  type Immutable[T] = scala.collection.SeqView[T, Seq[T]]
}
