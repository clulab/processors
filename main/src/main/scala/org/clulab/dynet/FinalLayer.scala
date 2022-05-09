package org.clulab.dynet

import edu.cmu.dynet.{ComputationGraph, Expression, ExpressionVector}

trait FinalLayer extends Saveable {
  def forward(inputExpressions: ExpressionVector,
              modifierHeadPairs: Option[IndexedSeq[ModifierHeadPair]],
              doDropout: Boolean)(implicit cg: ComputationGraph): ExpressionVector

  def inDim: Int
  def outDim: Int

  def loss(emissionScoresAsExpression: ExpressionVector, goldLabels: IndexedSeq[Label])(implicit cg: ComputationGraph): Expression

  def inference(emissionScores: Array[Array[Float]])(implicit cg: ComputationGraph): IndexedSeq[String]

  def inferenceWithScores(emissionScores: Array[Array[Float]])(implicit cg: ComputationGraph): IndexedSeq[IndexedSeq[(String, Float)]]
}
