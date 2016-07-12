package org.clulab.odin.impl

import org.clulab.embeddings.word2vec.Word2Vec
import java.io.InputStream


trait OdinResource

// for distributional similarity comparisons
// Uses Word2Vec class as its backend
class EmbeddingsResource(is: InputStream) extends Word2Vec(is, None) with OdinResource {
  override def similarity(w1: String, w2: String): Double = {
    val score = super.similarity(w1, w2)
      //println(s"score is $score")
      score
  }
}