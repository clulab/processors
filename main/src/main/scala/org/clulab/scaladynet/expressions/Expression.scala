package org.clulab.scaladynet.expressions

import org.clulab.scaladynet.ComputationGraph
import org.clulab.scaladynet.Real
import org.clulab.scaladynet.nodes.CwiseSum
import org.clulab.scaladynet.nodes.Dropout
import org.clulab.scaladynet.nodes.LogSumExp
import org.clulab.scaladynet.nodes.MatrixMultiply
import org.clulab.scaladynet.nodes.Negate
import org.clulab.scaladynet.nodes.Node
import org.clulab.scaladynet.nodes.Rectify
import org.clulab.scaladynet.nodes.Tanh
import org.clulab.scaladynet.parameters.LookupParameter
import org.clulab.scaladynet.parameters.Parameter
import org.clulab.scaladynet.utils.Dim
import org.clulab.scaladynet.utils.Tensor
import org.clulab.scaladynet.vectors.ExpressionVector
import org.clulab.scaladynet.vectors.UnsignedVector

class Expression(var cgOpt: Option[ComputationGraph] = None, var i: Int = 0, var graph_id: Int = 0) {

  protected def newExpression(node: Node): Expression = Expression.applyCg(cgOpt, cgOpt.get.add_function_node(node))

  def -(): Expression = newExpression(Negate(i))

  def +(e2: Expression): Expression = newExpression(CwiseSum(i, e2.i))

  def -(e2: Expression): Expression = newExpression(CwiseSum(i, -e2.i))

  def *(e2: Expression): Expression = newExpression(MatrixMultiply(i, e2.i))


  def dim(): Dim = cgOpt.get.get_dimension(i)

  def value(): Tensor = cgOpt.get.get_value(i)

  def is_stale: Boolean = {
    // return (get_number_of_active_graphs() != 1 || graph_id != get_current_graph_id());
    false
  }
}

object Expression {

  class Detail(e: Expression)

//  val cg = new ComputationGraph()

  def apply(i: Int = 0, graph_id: Int = 0): Expression = new Expression(i = i, graph_id = graph_id)

  def applyCg(cgOpt: Option[ComputationGraph], i: Int = 0, graph_id: Int = 0): Expression = new Expression(cgOpt, i, graph_id)

  def newExpression(oldExpression: Expression, node: Node): Expression = {
    val cgOpt = oldExpression.cgOpt
    applyCg(cgOpt, cgOpt.get.add_function_node(node))
  }

  def concatenate(ev: ExpressionVector): Expression = ???

  def concatenate(exprs: Expression*): Expression = ???

  def concatenateCols(exprs: Expression*): Expression = ???

  def constLookup(p: LookupParameter, index: Int): Expression = ???

  // Where does the cg come from?
  def constLookup(g: ComputationGraph, p: LookupParameter, index: Int): Expression = Expression.applyCg(Some(g), g.add_lookup(p, index))

  def conv2d(a: Expression, b: Expression, c: Expression, stride: UnsignedVector): Expression = ???

  def dropout(x: Expression, p: Real): Expression = newExpression(x, Dropout(x.i, p))

  def input(s: Float): Expression = ???

  def logSumExp(xs: ExpressionVector): Expression = null // Detail(newExpression(LogSumExp(xs)) // what in the world?

  def lookup(p: LookupParameter, index: Int): Expression = ???

  def parameter(p: Parameter): Expression = ???

  def parameter(a: Expression, b: Expression, stride: UnsignedVector): Expression = ???

  def pick(e: Expression, v: Int): Expression = ???

  def pickNegLogSoftmax(e: Expression, v: Long): Expression = ???

  def randomNormal(d: Dim): Expression = ???

  def rectify(x: Expression): Expression = newExpression(x, Rectify(x.i))

  def sum(ev: ExpressionVector): Expression = ???

  def tanh(x: Expression): Expression = newExpression(x, Tanh(x.i))

  // Internal
  def const_parameter(g: ComputationGraph, p: Parameter): Expression = ???
}
