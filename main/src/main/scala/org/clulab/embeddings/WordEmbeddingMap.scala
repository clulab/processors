package org.clulab.embeddings

import org.clulab.scala.WrappedArray._
import org.clulab.utils.MathUtils

import scala.collection.mutable.{IndexedSeq => MutableIndexedSeq}

/**
 * Basic functionality required by all implementations of word embeddings
 */
trait WordEmbeddingMap {

  /** The dimension (width) of an embedding vector */
  def dim: Int

  def isOutOfVocabulary(word: String): Boolean

  // These two methods provide access to embeddings primarily for neural networks.  The values
  // returned should not be mutated, but the individual Float values can be read by index.

  /** Retrieves the embedding for this word, if it exists in the map */
  def get(word: String): Option[IndexedSeq[Float]]

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the unknown
    * embedding instead.  That embedding is defined in the vector file.  If it is needed,
    * but doesn't exist, a RuntimeException is thrown.
    */
  def getOrElseUnknown(word: String): IndexedSeq[Float]

  /** Returns all keys presented in the map, excluding the key for the unknown token */
  def keys: Set[String]

  /** The embedding corresponding to the unknown token */
  def unknownEmbedding: IndexedSeq[Float]

  // These three methods are primarily for use with grounding.  The returned array is a copy and
  // it's probably not a good idea to edit it, but doing so will not affect other embeddings.

  def makeCompositeVector(text: Iterable[String]): Array[Float]

  def makeCompositeVectorWeighted(text: Iterable[String], weights: Iterable[Float]): Array[Float]

  def avgSimilarity(text1: Iterable[String], text2: Iterable[String]): Float

  /** Save this object in binary format. */
  def save(filename: String): Unit

  /** filterPredicate: if passed, only returns words that match the predicate */
  def mostSimilarWords(vector: Array[Float], howMany: Int, filterPredicateOpt: Option[String => Boolean]): Seq[(String, Double)] = {
    val unfilteredKeys = keys
    val filteredKeys = filterPredicateOpt.map(unfilteredKeys.filter).getOrElse(unfilteredKeys)
    val result = MathUtils.nBest[String](word => WordEmbeddingMap.dotProduct(vector, getOrElseUnknown(word)).toDouble)(filteredKeys, howMany)

    result
  }

  /**
    * Finds the words most similar to this set of inputs
    *
    * IMPORTANT: Words here must already be normalized to match how they are stored in the map.
    *
    * This method is included to support the interface of the deprecated [[org.clulab.embeddings.SanitizedWordEmbeddingMap SanitizedWordEmbeddingMap]].
    * Unknown words may be skipped in calculating the composite or the unknown vector might be
    * used.  That is decided by the subclass.  This method calls only public member functions,
    * so reimplement or subclass for alternative behavior.
    */
  def mostSimilarWords(words: Set[String], howMany: Int): Seq[(String, Double)] = {
    val compositeVector = makeCompositeVector(words)
    mostSimilarWords(compositeVector, howMany, None)
  }
}

object WordEmbeddingMap {

  lazy val defaultWordSanitizer = new DefaultWordSanitizer()

  /** Normalize this vector to length 1, in place, if possible. */
  def norm(array: MutableIndexedSeq[Float]): Unit = {

    def calcLength(): Float = {
      var len = 0f // optimization
      var i = 0 // optimization

      while (i < array.length) {
        len += array(i) * array(i)
        i += 1
      }
      math.sqrt(len.toDouble).toFloat
    }

    def divide(divisor: Float): Unit = {
      var i = 0 // optimization

      while (i < array.length) {
        array(i) /= divisor
        i += 1
      }
    }

    val length = calcLength()

    if (length != 0)  divide(length)
  }

  def dotProduct(v1: IndexedSeq[Float], v2: IndexedSeq[Float]): Float = {
    require(v1.length == v2.length) //should we always assume that v2 is longer? perhaps set shorter to length of longer...
    // This would be way prettier, but it is ~20 times slower
    // v1.indices.foldRight(0.0f)((i, sum) => sum + v1(i) * v2(i))
    var sum = 0f // optimization
    var i = 0 // optimization
    while (i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }
}
