package org.clulab.dynet

import edu.cmu.dynet.{Expression, ExpressionVector}
import org.clulab.struct.DirectedGraph

trait FinalLayer extends Saveable {
  def forward(inputExpressions: ExpressionVector,
              headPositionsOpt: Option[IndexedSeq[Int]],
              doDropout: Boolean): ExpressionVector

  def graphForward(inputExpressions: ExpressionVector,                   
                   doDropout: Boolean): DirectedGraph[Expression]

  def inDim: Int
  def outDim: Int

  def loss(emissionScoresAsExpression: ExpressionVector, goldLabels: IndexedSeq[String]): Expression

  def graphLoss(predictedGraph: DirectedGraph[Expression], goldGraph: DirectedGraph[String]): Expression

  def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String]

  def inferenceWithScores(emissionScores: Array[Array[Float]]): IndexedSeq[IndexedSeq[(String, Float)]]
}
