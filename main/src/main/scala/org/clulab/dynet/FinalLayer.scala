package org.clulab.dynet

import edu.cmu.dynet.{Expression, ExpressionVector}

trait FinalLayer extends Saveable {
  def forward(inputExpressions: ExpressionVector,
              predicatePositionOpt: Option[Int],
              doDropout: Boolean): ExpressionVector

  def inDim: Int
  def outDim: Int

  def loss(emissionScoresAsExpression: ExpressionVector, goldLabels: IndexedSeq[String]): Expression

  def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String]
}
