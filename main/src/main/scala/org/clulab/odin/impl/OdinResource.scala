package org.clulab.odin.impl

import java.io.InputStream

import org.clulab.embeddings.SanitizedWordEmbeddingMap
import org.clulab.scala.WrappedArray._


trait OdinResource

// for distributional similarity comparisons
// Uses Word2Vec class as its backend
@annotation.nowarn("cat=deprecation")
class EmbeddingsResource(is: InputStream, val p: String) extends SanitizedWordEmbeddingMap(is, None, false) with OdinResource {
  override def similarity(w1: String, w2: String): Double = {
    val score = super.similarity(w1, w2)
      //println(s"score is $score")
      score
  }
}