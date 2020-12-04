package org.clulab.scaladynet.expressions

import org.clulab.scaladynet.ComputationGraph
import org.clulab.scaladynet.parameters.LookupParameter
import org.clulab.scaladynet.parameters.Parameter
import org.clulab.scaladynet.utils.Dim
import org.clulab.scaladynet.utils.Tensor
import org.clulab.scaladynet.vectors.ExpressionVector
import org.clulab.scaladynet.vectors.UnsignedVector

class Expression {
  def concatenate: Expression = ???
  def input: Expression = ???
  def logSumExp: Expression = ???
  def lookup: Expression = ???
  def pick: Expression = ???
  def pickNegLogSoftmax: Expression = ???
  def randomNormal: Expression = ???
  def sum(ev: ExpressionVector): Expression = ???
  def value(): Tensor = ???
  def *(e2: Expression): Expression = ???
  def +(e2: Expression): Expression = ???
  def -(e2: Expression): Expression = ???
  def dim(): Dim = ???
}

object Expression {
  def concatenate(ev: ExpressionVector): Expression = ???
  def concatenate(exprs: Expression*): Expression = ???
  def input(s: Float): Expression = ???
  def logSumExp(ev: ExpressionVector): Expression = ???
  def pick(e: Expression, v: Int): Expression = ???
  def pickNegLogSoftmax(e: Expression, v: Long): Expression = ???
  def sum(ev: ExpressionVector): Expression = ???
  def lookup(p: LookupParameter, index: Int): Expression = ???
  def concatenateCols(exprs: Expression*): Expression = ???
  def conv2d(a: Expression, b: Expression, c: Expression, stride: UnsignedVector): Expression = ???
  def parameter(p: Parameter): Expression = ???
  def parameter(a: Expression, b: Expression, stride: UnsignedVector): Expression = ???
  def constLookup(p: LookupParameter, index: Long): Expression = ???
  def tanh(e: Expression): Expression = ???
  def rectify(e: Expression): Expression = ???
  def randomNormal(d: Dim): Expression = ???
  def dropout(e: Expression, prob: Float): Expression = ???

  // Internal
  def const_parameter(g: ComputationGraph, p: Parameter): Expression = ???
}
