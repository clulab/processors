package org.clulab.dynet

import edu.cmu.dynet.ExpressionVector

/**
 * Intermediate layer in a sequence modeling architecture: goes from ExpressionVector to ExpressionVector
 */
trait IntermediateLayer {
  def mkEmbeddings(inputExpressions: ExpressionVector,
                   doDropout: Boolean): ExpressionVector

  def inDim: Int
  def outDim: Int
}
