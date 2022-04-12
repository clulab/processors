package org.clulab.utils

import scala.collection.mutable

// This AnyRef thing is not absolutely complete.
// See https://stackoverflow.com/questions/2440134/is-this-the-proper-way-to-initialize-null-references-in-scala.
class SeqOdometer[T <: Any](val sequences: Array[Seq[T]]) extends Iterator[mutable.ArraySeq[T]] {
  require(sequences.nonEmpty)
  require(sequences.forall(_.nonEmpty))

  val indices = sequences.indices
  val iterators: Array[Iterator[T]] = sequences.map(_.iterator)
  val values: mutable.ArraySeq[T] = iterators.zipWithIndex.map { case (iterator: Iterator[T], index: Int) =>
    // Leave room for the very first next().
    if (index == 0) null.asInstanceOf[T]
    else iterator.next()
  }

  override def hasNext: Boolean = iterators.exists(_.hasNext)

  override def next(): mutable.ArraySeq[T] = {
    indices.find { index =>
      val oldIterator = iterators(index)
      val hasNext = oldIterator.hasNext
      val newIterator =
        if (hasNext)
          oldIterator
        else {
          val newIterator = sequences(index).iterator
          iterators(index) = newIterator
          newIterator
        }
      values(index) = newIterator.next()
      hasNext
    }
    values
  }
}

class RangeOdometer(val ranges: Array[Range]) extends Iterator[mutable.ArraySeq[Int]] {
  require(ranges.nonEmpty)
  require(ranges.forall(_.nonEmpty))

  val indices = ranges.indices
  val iterators: Array[Iterator[Int]] = ranges.map(_.iterator)
  val values: mutable.ArraySeq[Int] = iterators.zipWithIndex.map { case (iterator: Iterator[Int], index: Int) =>
    // Leave room for the very first next().
    if (index == 0) -1
    else iterator.next()
  }

  override def hasNext: Boolean = iterators.exists(_.hasNext)

  override def next(): mutable.ArraySeq[Int] = {
    indices.find { index =>
      val oldIterator = iterators(index)
      val hasNext = oldIterator.hasNext
      val newIterator =
        if (hasNext)
          oldIterator
        else {
          val newIterator = ranges(index).iterator
          iterators(index) = newIterator
          newIterator
        }
      values(index) = newIterator.next()
      hasNext
    }
    values
  }
}
