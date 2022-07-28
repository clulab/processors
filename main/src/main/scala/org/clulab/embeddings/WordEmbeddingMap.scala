package org.clulab.embeddings

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
}

object WordEmbeddingMap {

  lazy val defaultWordSanitizer = new DefaultWordSanitizer()


  /** Normalize this vector to length 1, in place, if possible.
    *
    * NOTE:   Scala 2.13 has changed Array.view to be implemented by Array.slice, which creates a copy. The most optimal
    * way to work with this is to implement the view / slice functionality in the `norm` method, as shown below.
    *
    */

  def norm(array: MutableIndexedSeq[Float], from: Int, until: Int): Unit = {
    require((array.isEmpty && from == 0 && until == 0) || (from >= 0 && from < array.length && until >= from && until <= array.length))

    def calcLength(): Float = {
      var len = 0f // optimization
      var i = from // optimization

      while (i < until) {
        len += array(i) * array(i)
        i += 1
      }
      math.sqrt(len.toDouble).toFloat
    }

    def divide(divisor: Float): Unit = {
      var i = from// optimization

      while (i < until) {
        array(i) /= divisor
        i += 1
      }
    }

    val length = calcLength()

    if (length != 0)  divide(length)
  }

  /** Normalize this vector to length 1, in place, if possible. */
  def norm(array: MutableIndexedSeq[Float]): Unit = norm(array,0,array.length)

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
