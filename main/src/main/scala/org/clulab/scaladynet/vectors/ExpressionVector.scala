package org.clulab.scaladynet.vectors

import org.clulab.scaladynet.expressions.Expression

import scala.collection.mutable
import scala.language.implicitConversions

class ExpressionVector(values: mutable.IndexedSeq[Expression] = mutable.IndexedSeq.empty[Expression])
    extends MutableVector[Expression](values) with mutable.IndexedSeq[Expression] {

  def this(values: Seq[Expression]) = this(mutable.IndexedSeq(values: _*))

  def apply(idx: Int): Expression = values(idx)

  override def reverse: ExpressionVector = new ExpressionVector(values.reverse)

  override def indices: Range = values.indices

  override def nonEmpty: Boolean = values.nonEmpty

  override def size(): Int = values.size

  def sum(ev: ExpressionVector): Expression = ???
  def add(v: Expression): Unit = ???

  override def iterator: Iterator[Expression] = values.iterator
}

object ExpressionVector {

  implicit def MutableSeq2ExpressionVector(x: mutable.IndexedSeq[Expression]): ExpressionVector =
    new ExpressionVector(x)

  implicit def ImmutableSeq2ExpressionVector(x: IndexedSeq[Expression]): ExpressionVector = {
    new ExpressionVector(mutable.IndexedSeq(x: _*))
  }
}
