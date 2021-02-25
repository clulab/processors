package org.clulab.embeddings

import org.clulab.embeddings.WordEmbeddingMap.ArrayType
import org.clulab.embeddings.WordEmbeddingMap.ValueType

/**
 * Implements an word embedding map, where each embedding is stored as a distinct array
 */
class ExplicitWordEmbeddingMap extends WordEmbeddingMap {
  /** Retrieves the embedding for this word, if it exists in the map */
  override def get(word: String): Option[ArrayType] = None // TODO

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  override def getOrElseUnknown(word: String): ArrayType = {
    val emb = get(word)
    if(emb.isEmpty) {
      val unkEmb = get(ExplicitWordEmbeddingMap.UNK)
      if(unkEmb.isEmpty) {
        throw new RuntimeException("ERROR: can't find embedding for the unknown token!")
      }
      unkEmb.get
    } else {
      emb.get
    }
  }

  /** The dimension of an embedding vector */
  override def dim: Int = 0 // TODO

  def isOutOfVocabulary(word: String): Boolean = {
    ???
  }

  def makeCompositeVector(text: Iterable[String]): ArrayType = {
    ???
  }

  def makeCompositeVectorWeighted(text: Iterable[String], weights: Iterable[Float]): ArrayType = {
    ???
  }

  def avgSimilarity(text1: Iterable[String], text2: Iterable[String]): ValueType = {
    ???
  }
}

object ExplicitWordEmbeddingMap {
  protected val UNK = "" // token used for unknowns

  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): ExplicitWordEmbeddingMap = {
    null // TODO
  }
}
