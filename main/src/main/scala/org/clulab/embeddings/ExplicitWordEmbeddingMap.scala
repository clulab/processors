package org.clulab.embeddings

import org.clulab.embeddings.WordEmbeddingMap._

/**
 * Implements an word embedding map, where each embedding is stored as a distinct array
 */
class ExplicitWordEmbeddingMap(matrixConstructor: => Map[String, ArrayType], val wordSanitizer: WordSanitizing = new DefaultWordSanitizer) extends WordEmbeddingMap {
  // Laziness here causes problems with InputStream-based alternate constructor.
  val matrix: Map[String, ArrayType] = matrixConstructor
  val unkEmbeddingOpt: Option[ArrayType] = get(ExplicitWordEmbeddingMap.UNK)

  /** The dimension of an embedding vector */
  override val dim: Int = matrix.values.head.length

  /** Retrieves the embedding for this word, if it exists in the map */
  override def get(word: String): Option[ArrayType] = matrix.get(word)

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  override def getOrElseUnknown(word: String): ArrayType = {
    get(word).getOrElse(
      unkEmbeddingOpt.getOrElse(
        throw new RuntimeException("ERROR: can't find embedding for the unknown token!")
      )
    )
  }

  def isOutOfVocabulary(word: String): Boolean =
    word == ExplicitWordEmbeddingMap.UNK || !matrix.contains(wordSanitizer.sanitizeWord(word))

  def makeCompositeVector(text: Iterable[String]): ArrayType = {
    val total = new ArrayType(dim)

    text.foreach { word =>
      matrix.get(word).foreach { addend => add(total, addend) }
    }
    norm(total)
  }

  def makeCompositeVectorWeighted(text: Iterable[String], weights: Iterable[Float]): ArrayType = {
    val total = new ArrayType(dim) // automatically initialized to zero

    (text, weights).zipped.foreach { (word, weight) =>
      // This therefore skips the unknown words, which may not be the right strategy.
      matrix.get(word).foreach { index => addWeighted(total, index, weight) }
    }
    norm(total)
  }

  protected def add(dest: ArrayType, src: ArrayType): Unit = {
    var i = 0

    while (i < dim) {
      dest(i) += src(i)
      i += 1
    }
  }

  protected def addWeighted(dest: ArrayType, src: ArrayType, weight: Float): Unit = {
    var i = 0

    while (i < dim) {
      dest(i) += src(i) * weight
      i += 1
    }
  }

  override protected def sanitizedAvgSimilarity(texts1: Iterable[String], texts2: Iterable[String]): ValueType = {
    var sum = 0.asInstanceOf[ValueType] // optimization
    var count = 0 // optimization

    texts1.foreach { text1 =>
      val row1Opt = matrix.get(text1)

      if (row1Opt.isDefined) {
        texts2.foreach { text2 =>
          val row2Opt = matrix.get(text2)

          if (row2Opt.isDefined) {
            sum += dotProduct(row1Opt.get, row2Opt.get)
            count += 1
          }
        }
      }
    }
    if (count != 0) sum / count
    else 0
  }

  protected def dotProduct(v1: ArrayType, v2: ArrayType): ValueType = {
    require(v1.length == v2.length)
    var sum = 0.asInstanceOf[ValueType] // optimization
    var i = 0 // optimization

    while (i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }
}

object ExplicitWordEmbeddingMap {
  protected val UNK = "" // token used for unknowns

  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): ExplicitWordEmbeddingMap = {
    null // TODO
  }
}
