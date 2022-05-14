package org.clulab.utils

import me.tongfei.progressbar.{ProgressBar => JProgressBar}

class ProgressBar[T](text: String, innerIterator: Iterator[T]) extends Iterable[T] {

  override def iterator: Iterator[T] = {
    val jProgressBar = new JProgressBar(text, innerIterator.length)

    new ProgressBarIterator(innerIterator, jProgressBar)
  }
}

object ProgressBar {

  def apply[T](text: String, iterator: Iterator[T]): ProgressBar[T] =
      new ProgressBar(text, iterator)

  def apply[T](text: String, iterable: Iterable[T]): ProgressBar[T] =
      apply(text, iterable.iterator)
}

class ProgressBarIterator[T](iterator: Iterator[T], jProgressBar: JProgressBar) extends Iterator[T] {

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
