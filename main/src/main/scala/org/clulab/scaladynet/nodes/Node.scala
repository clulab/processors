package org.clulab.scaladynet.nodes

import org.clulab.scaladynet.ComputationGraph
import org.clulab.scaladynet.utils.Dim

abstract class Node {
  val dim_forward: Seq[Dim] = Seq.empty
  def forward()
  def arity: Int
  def setCg(cg: ComputationGraph): Unit
  def getCg: ComputationGraph
}
