package org.clulab.scaladynet

import org.clulab.scaladynet.expressions.Expression
import org.clulab.scaladynet.nodes.Node
import org.clulab.scaladynet.parameters.LookupParameter
import org.clulab.scaladynet.utils.Dim
import org.clulab.scaladynet.utils.Tensor

class ComputationGraph {

  def get_value(i: Int): Tensor = ???

  def get_dimension(i: Int): Dim = ???

  def add_function_node(node: Node): Int = 7

  def add_lookup(p: LookupParameter, index: Int): Int = 8
}

object ComputationGraph {
  def renew(): Unit = ???
  def backward(last: Expression): Unit = ???
  def version: Int = ???
  def clear(): Unit = ???

  // internal
  def current: ComputationGraph = null
}

