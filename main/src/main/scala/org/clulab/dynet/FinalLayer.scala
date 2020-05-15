package org.clulab.dynet

import edu.cmu.dynet.{Expression, ExpressionVector}

trait FinalLayer {
  def forward(inputExpressions: ExpressionVector,
              predicatePositionOpt: Option[Int],
              doDropout: Boolean): ExpressionVector

  def inDim: Int
  def outDim: Int

  def loss(finalStates: ExpressionVector, goldLabels: IndexedSeq[String]): Expression
}
