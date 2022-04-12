package org.clulab.dynet

import edu.cmu.dynet.{Expression, ExpressionVector}

trait FinalLayer extends Saveable {
  def forward(inputExpressions: ExpressionVector,
              modifierHeadPairs: Option[IndexedSeq[ModifierHeadPair]],
              doDropout: Boolean): ExpressionVector

  def inDim: Int
  def outDim: Int

  def loss(emissionScoresAsExpression: ExpressionVector, goldLabels: IndexedSeq[Label]): Expression

  def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String]

  def inferenceWithScores(emissionScores: Array[Array[Float]]): IndexedSeq[IndexedSeq[(String, Float)]]
}
