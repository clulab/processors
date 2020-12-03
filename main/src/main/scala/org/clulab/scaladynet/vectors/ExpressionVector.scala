package org.clulab.scaladynet.vectors

import org.clulab.scaladynet.expressions.Expression
import org.clulab.scaladynet.utils.Tensor

import scala.language.implicitConversions

class ExpressionVector() extends scala.collection.mutable.IndexedSeq[Expression] {
  // Needs internal representation then
  def this(values: Seq[Expression] = Seq.empty) = this()

  override def size(): Int = ???
  override def nonEmpty: Boolean = ???
  override def indices: Range = ???
  override def reverse: ExpressionVector = ???

  def sum(ev: ExpressionVector): Expression = ???
  def add(v: Expression): Unit = ???
  def length: Int = ???
  def apply(i: Int): Expression = ???
  def value(): Tensor = ???

  override def update(idx: Int, elem: Expression): Unit = ???
}

object ExpressionVector {
  implicit def Seq2ExpressionVector(x: Seq[Expression]) =
    new ExpressionVector(x)
}
