package org.clulab.odin.impl

import java.io.InputStream

import org.clulab.embeddings.SanitizedWordEmbeddingMap


trait OdinResource

// for distributional similarity comparisons
// Uses Word2Vec class as its backend
class EmbeddingsResource(is: InputStream) extends SanitizedWordEmbeddingMap(is, None, false) with OdinResource {
  override def similarity(w1: String, w2: String): Double = {
    val score = super.similarity(w1, w2)
      //println(s"score is $score")
      score
  }
}