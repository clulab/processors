package org.clulab.dynet

import edu.cmu.dynet.{ComputationGraph, ExpressionVector}

/**
 * Intermediate layer in a sequence modeling architecture: goes from ExpressionVector to ExpressionVector
 */
trait IntermediateLayer extends Saveable {
  def forward(inputExpressions: ExpressionVector,
              doDropout: Boolean)(implicit cg: ComputationGraph): ExpressionVector

  def inDim: Int
  def outDim: Int
}
