package org.clulab.dynet

import edu.cmu.dynet.ExpressionVector

/**
 * This layer takes a sequence of words and produces a sequence of Expression that stores the complete word embeddings
 * @author Mihai
 */
class EmbeddingLayer {

  lazy val constEmbedder = ConstEmbeddingsGlove()

  def mkEmbeddings(words: IndexedSeq[String],
                   doDropout: Boolean): ExpressionVector = {
    mkEmbeddings(words, None, None, doDropout)
  }

  def mkEmbeddings(words: IndexedSeq[String],
                   tags: Option[IndexedSeq[String]],
                   predicatePosition: Option[Int],
                   doDropout: Boolean): ExpressionVector = {

    // const word embeddings such as GloVe
    val constEmbeddings = constEmbedder.mkEmbeddings(words)


  }
}
