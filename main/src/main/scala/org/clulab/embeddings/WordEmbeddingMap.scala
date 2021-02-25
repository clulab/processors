package org.clulab.embeddings

import org.clulab.embeddings.WordEmbeddingMap._

/**
 * Basic functionality required by all implementations of word embeddings
 */
trait WordEmbeddingMap {
  val wordSanitizer: WordSanitizing

  /** Retrieves the embedding for this word, if it exists in the map */
  def get(word: String): Option[ArrayType]

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  def getOrElseUnknown(word: String): ArrayType

  /** The dimension of an embedding vector */
  def dim: Int

  /** Normalize this vector to length 1, in place, if possible. */
  def norm(array: ArrayType): Option[ArrayType] = {

    def calcLength(): ValueType = {
      var len = 0.asInstanceOf[ValueType] // optimization
      var i = 0 // optimization

      while (i < array.length) {
        len += array(i) * array(i)
        i += 1
      }
      math.sqrt(len).asInstanceOf[ValueType]
    }

    def divide(divisor: ValueType): ArrayType = {
      var i = 0 // optimization

      while (i < array.length) {
        array(i) /= divisor
        i += 1
      }
      array
    }

    val length = calcLength()

    if (length != 0)  Some(divide(length))
    else None
  }

  def isOutOfVocabulary(word: String): Boolean

  def makeCompositeVector(text: Iterable[String]): Option[ArrayType]

  def makeCompositeVectorWeighted(text: Iterable[String], weights: Iterable[Float]): Option[ArrayType]

  protected def sanitizedAvgSimilarity(text1: Iterable[String], text2: Iterable[String]): Option[ValueType]

  // Find the average embedding similarity between any two words in these two texts.
  // IMPORTANT: words here must be words not lemmas!
  def avgSimilarity(text1: Iterable[String], text2: Iterable[String]): ValueType = {
    val sanitizedText1 = text1.map(wordSanitizer.sanitizeWord(_))
    val sanitizedText2 = text2.map(wordSanitizer.sanitizeWord(_))

    sanitizedAvgSimilarity(sanitizedText1, sanitizedText2)
  }
}

object WordEmbeddingMap {
  type ValueType = Float
  type ArrayType = Array[ValueType]
}
