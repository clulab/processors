package org.clulab

package object scala {
  type BufferedIterator[T] = _root_.scala.BufferedIterator[T]

  type LazyList[T] = _root_.scala.collection.immutable.Stream[T]
  val LazyList = _root_.scala.collection.immutable.Stream
}
