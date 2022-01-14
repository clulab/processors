package org.clulab.dynet

import edu.cmu.dynet.{Expression, ExpressionVector}
import org.clulab.struct.EdgeMap

trait FinalLayer extends Saveable {
  def forward(inputExpressions: ExpressionVector,
              headPositionsOpt: Option[IndexedSeq[Int]],
              doDropout: Boolean): ExpressionVector

  def graphForward(inputExpressions: ExpressionVector, 
                   doDropout: Boolean): EdgeMap[Expression]

  def inDim: Int
  def outDim: Int

  def loss(emissionScoresAsExpression: ExpressionVector, goldLabels: IndexedSeq[String]): Expression

  def graphLoss(predictedGraph: EdgeMap[Expression], goldGraph: EdgeMap[String]): Expression

  def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String]

  def graphInference(emissionScores: EdgeMap[Expression]): EdgeMap[String]

  def inferenceWithScores(emissionScores: Array[Array[Float]]): IndexedSeq[IndexedSeq[(String, Float)]]
}
