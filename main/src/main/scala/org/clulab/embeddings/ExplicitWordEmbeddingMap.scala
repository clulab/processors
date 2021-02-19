package org.clulab.embeddings
import org.clulab.embeddings.WordEmbeddingMap.ArrayType

/**
 * Implements an word embedding map, where each embedding is stored as a distinct array
 */
class ExplicitWordEmbeddingMap extends WordEmbeddingMap {
  /** Retrieves the embedding for this word, if it exists in the map */
  override def get(word: String): Option[ArrayType] = ???

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  override def getOrElseUnknown(word: String): ArrayType = ???

  /** The dimension of an embedding vector */
  override def dim: Float = ???
}

object ExplicitWordEmbeddingMap {
  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): ExplicitWordEmbeddingMap = {
    null // TODO
  }
}
