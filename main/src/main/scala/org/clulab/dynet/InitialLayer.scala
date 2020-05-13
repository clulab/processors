package org.clulab.dynet

import edu.cmu.dynet.ExpressionVector

/**
 * First layer that occurs in a sequence modeling architecture: goes from words to Expressions
 */
trait InitialLayer {
  def forward(words: IndexedSeq[String],
              doDropout: Boolean): ExpressionVector = {
    forward(words, None, None, doDropout)
  }

  def forward(words: IndexedSeq[String],
              tags: Option[IndexedSeq[String]],
              predicatePosition: Option[Int],
              doDropout: Boolean): ExpressionVector

  def outDim:Int // output dimension
}
