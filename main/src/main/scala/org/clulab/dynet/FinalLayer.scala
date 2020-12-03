package org.clulab.dynet

//import edu.cmu.dynet.{Expression, ExpressionVector}
import org.clulab.scaladynet.expressions.Expression
import org.clulab.scaladynet.vectors.ExpressionVector

trait FinalLayer extends Saveable {
  def forward(inputExpressions: ExpressionVector,
              headPositionsOpt: Option[IndexedSeq[Int]],
              doDropout: Boolean): ExpressionVector

  def inDim: Int
  def outDim: Int

  def loss(emissionScoresAsExpression: ExpressionVector, goldLabels: IndexedSeq[String]): Expression

  def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String]

  def inferenceWithScores(emissionScores: Array[Array[Float]]): IndexedSeq[IndexedSeq[(String, Float)]]
}
