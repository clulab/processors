package org.clulab.embeddings

import org.clulab.embeddings.WordEmbeddingMap._

/**
 * Basic functionality required by all implementations of word embeddings
 */
trait WordEmbeddingMap {
  /** Retrieves the embedding for this word, if it exists in the map */
  def get(word: String): Option[ArrayType]

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  def getOrElseUnknown(word: String): ArrayType

  /** The dimension of an embedding vector */
  def dim: Float

  /** Normalize this vector to length 1, in place. (If the length is zero, do nothing.) */
  def norm(array: ArrayType): ArrayType = {
    var len = 0.asInstanceOf[ValueType] // optimization
    var i = 0 // optimization

    while (i < array.length) {
      len += array(i) * array(i)
      i += 1
    }
    len = math.sqrt(len).asInstanceOf[ValueType]

    if (len != 0) {
      i = 0
      while (i < array.length) {
        array(i) /= len
        i += 1
      }
    }
    array
  }
}

object WordEmbeddingMap {
  type ValueType = Float
  type ArrayType = Array[ValueType]
}