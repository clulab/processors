package org.clulab.embeddings

import org.clulab.embeddings.WordEmbeddingMap._

import scala.collection.mutable.{IndexedSeq => MutableIndexedSeq}

/**
 * Basic functionality required by all implementations of word embeddings
 */
trait WordEmbeddingMap {

  /** The dimension of an embedding vector */
  def dim: Int

  /** Retrieves the embedding for this word, if it exists in the map */
  def get(word: String): Option[SeqType]

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  def getOrElseUnknown(word: String): SeqType

  def isOutOfVocabulary(word: String): Boolean

  def makeCompositeVector(text: Iterable[String]): SeqType

  def makeCompositeVectorWeighted(text: Iterable[String], weights: Iterable[Float]): SeqType

  protected def sanitizedAvgSimilarity(text1: Iterable[String], text2: Iterable[String]): ValueType

  // Find the average embedding similarity between any two words in these two texts.
  // IMPORTANT: words here must be words not lemmas!
  def avgSimilarity(text1: Iterable[String], text2: Iterable[String],
      wordSanitizer: WordSanitizing = WordEmbeddingMap.defaultWordSanitizer): ValueType = {
    val sanitizedText1 = text1.map(wordSanitizer.sanitizeWord(_))
    val sanitizedText2 = text2.map(wordSanitizer.sanitizeWord(_))

    sanitizedAvgSimilarity(sanitizedText1, sanitizedText2)
  }

  /** Save this object in binary format. */
  def save(filename: String): Unit
}

object WordEmbeddingMap {
  type ValueType = Float
  type ArrayType = Array[ValueType] // MutableIndexedSeq[ValueType] // Check to see if this still compiles.
  type SeqType = Array[ValueType] // IndexedSeq[ValueType] // These are not supposed to be writable and used for results of calculations.

  lazy val defaultWordSanitizer = new DefaultWordSanitizer()

  /** Normalize this vector to length 1, in place, if possible. */
  def norm(array: MutableIndexedSeq[ValueType]): Unit = {

    def calcLength(): ValueType = {
      var len = 0.asInstanceOf[ValueType] // optimization
      var i = 0 // optimization

      while (i < array.length) {
        len += array(i) * array(i)
        i += 1
      }
      math.sqrt(len.toDouble).asInstanceOf[ValueType]
    }

    def divide(divisor: ValueType): Unit = {
      var i = 0 // optimization

      while (i < array.length) {
        array(i) /= divisor
        i += 1
      }
    }

    val length = calcLength()

    if (length != 0)  divide(length)
  }

  def dotProduct(v1: SeqType, v2: SeqType): ValueType = {
    require(v1.length == v2.length) //should we always assume that v2 is longer? perhaps set shorter to length of longer...
    // This would be way prettier, but it is ~20 times slower
    // v1.indices.foldRight(0.0f)((i, sum) => sum + v1(i) * v2(i))
    var sum = 0.asInstanceOf[ValueType] // optimization
    var i = 0 // optimization
    while (i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }
}
