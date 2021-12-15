package org.clulab.processors.clu

import org.clulab.dynet.ConstEmbeddingsGlove
import org.clulab.embeddings.WordEmbeddingMapPool

object GetWordEmbeddings extends App {
  val constEmbeddingsGlove = ConstEmbeddingsGlove // Make sure that the embeddings have been loaded.
  val wordEmbeddingMap = WordEmbeddingMapPool.get("glove.840B.300d.10f", compact = true).get
  val embedding = wordEmbeddingMap.get("this").get

  println(embedding.mkString(" "))
}