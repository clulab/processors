package org.clulab.dynet

import edu.cmu.dynet.ExpressionVector

/**
 * First layer that occurs in a sequence modeling architecture: goes from words to Expressions
 */
trait InitialLayer {
  def mkEmbeddings(words: IndexedSeq[String],
                   doDropout: Boolean): ExpressionVector = {
    mkEmbeddings(words, None, None, doDropout)
  }

  def mkEmbeddings(words: IndexedSeq[String],
                   tags: Option[IndexedSeq[String]],
                   predicatePosition: Option[Int],
                   doDropout: Boolean): ExpressionVector

  def dim:Int // output dimension
}
