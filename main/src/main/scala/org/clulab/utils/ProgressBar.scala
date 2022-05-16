package org.clulab.utils

import me.tongfei.progressbar.{ProgressBar => JProgressBar}

class ProgressBar[T](text: String, outerIterator: Iterator[T]) extends Iterable[T] {
  val (jProgressBar, innerIterator) = {
    val (leftIterator, rightIterator) = outerIterator.duplicate
    val jProgressBar = new JProgressBar(text, leftIterator.length)

    (jProgressBar, new ProgressBarIterator(jProgressBar, rightIterator))
  }

  override def iterator: Iterator[T] = innerIterator

  // This convenience method unfortunately limits the progress bar to one traversal.
  def setExtraMessage(message: String): Unit = jProgressBar.setExtraMessage(message)
}

object ProgressBar {

  def apply[T](text: String, iterator: Iterator[T]): ProgressBar[T] =
      new ProgressBar(text, iterator)

  def apply[T](text: String, iterable: Iterable[T]): ProgressBar[T] =
      apply(text, iterable.iterator)
}

class ProgressBarIterator[T](jProgressBar: JProgressBar, iterator: Iterator[T]) extends Iterator[T] {

  override def hasNext: Boolean = {
    val result = iterator.hasNext

    if (!result)
      jProgressBar.close()
    result
  }

  override def next(): T = {
    jProgressBar.step()
    iterator.next()
  }
}
