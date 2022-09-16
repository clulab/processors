package org.clulab

import _root_.scala.collection.{BufferedIterator => GenericBufferedIterator}
import _root_.scala.collection.immutable.{LazyList => ImmutableLazyList}

package object scala {
  type BufferedIterator[T] = GenericBufferedIterator[T]

  type LazyList[T] = ImmutableLazyList[T]
  val LazyList = ImmutableLazyList
}
