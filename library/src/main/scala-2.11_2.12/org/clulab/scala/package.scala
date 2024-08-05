package org.clulab

import _root_.scala.{BufferedIterator => GenericBufferedIterator}
import _root_.scala.collection.immutable.{Stream => ImmutableStream}

package object scala {
  type BufferedIterator[T] = GenericBufferedIterator[T]

  type LazyList[T] = ImmutableStream[T]
  val LazyList = ImmutableStream
}
