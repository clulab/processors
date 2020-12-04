package org.clulab.scaladynet.builders

import org.clulab.scaladynet.ComputationGraph
import org.clulab.scaladynet.expressions.Expression

abstract class RnnBuilder extends Builder {

  def newGraph(update: Boolean = true): Unit = {
    assert(!update)
    // Maybe update to a new graph.  Throw the old one away?
    // It gets a different version number
    new_graph(ComputationGraph.current, update)
  }

  def startNewSequence(): Unit = start_new_sequence()

  def addInput(x: Expression): Expression = ???

  def setDropout(d: Float): Unit = set_dropout(d)

  def disableDropout(): Unit = disable_dropout()

  // Internal

  var dropout_rate: Float = 0f
  var _cg: Option[ComputationGraph] = None
  val sm: Int = 7

  protected def new_graph(cg: ComputationGraph, update: Boolean = true): Unit = {
    // sm.transition(RNNOp.start_new_sequence)
    new_graph_impl(cg, update)
  }

  protected def start_new_sequence(h_0: Seq[Expression] = Seq.empty) {
    assert(h_0.isEmpty)
    //sm.transition(RNNOp.start_new_sequence)
    // cur = RNNPointer(-1)
    // head.clear
    start_new_sequence_impl(h_0)
  }

  protected def new_graph_impl(cg: ComputationGraph, update: Boolean)

  protected def start_new_sequence_impl(h_0: Seq[Expression]): Unit

  protected def set_dropout(d: Float): Unit = dropout_rate = d

  protected def disable_dropout(): Unit = dropout_rate = 0f
}
