package org.clulab.dynet

//import edu.cmu.dynet.ExpressionVector
import org.clulab.scaladynet.vectors.ExpressionVector

/**
 * First layer that occurs in a sequence modeling architecture: goes from words to Expressions
 */
trait InitialLayer extends Saveable {
  def forward(sentence: AnnotatedSentence,
              doDropout: Boolean): ExpressionVector

  def outDim:Int // output dimension
}
