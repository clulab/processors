package org.clulab.embeddings

import java.io._
import org.clulab.embeddings.WordEmbeddingMap._
import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Logging
import org.clulab.utils.Sourcer

import scala.collection.mutable.{HashMap => MutableHashMap, Map => MutableMap}

/**
 * Implements an word embedding map, where each embedding is stored as a distinct array
 */
class ExplicitWordEmbeddingMap(buildType: ExplicitWordEmbeddingMap.BuildType, val wordSanitizer: WordSanitizing = new DefaultWordSanitizer) extends WordEmbeddingMap {
  val map: ExplicitWordEmbeddingMap.MapType = buildType
  val unkEmbeddingOpt: Option[ArrayType] = get(ExplicitWordEmbeddingMap.UNK)

  /** The dimension of an embedding vector */
  override val dim: Int = map.values.head.length

  /** Retrieves the embedding for this word, if it exists in the map */
  override def get(word: String): Option[ArrayType] = map.get(word)

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  override def getOrElseUnknown(word: String): ArrayType = {
    get(word).getOrElse(
      unkEmbeddingOpt.getOrElse(
        throw new RuntimeException("ERROR: can't find embedding for the unknown token!")
      )
    )
  }

  def isOutOfVocabulary(word: String): Boolean =
    word == ExplicitWordEmbeddingMap.UNK || !map.contains(wordSanitizer.sanitizeWord(word))

  def makeCompositeVector(text: Iterable[String]): ArrayType = {
    val total = new ArrayType(dim)

    text.foreach { word =>
      map.get(word).foreach { addend => add(total, addend) }
    }
    norm(total)
    total
  }

  def makeCompositeVectorWeighted(text: Iterable[String], weights: Iterable[Float]): ArrayType = {
    val total = new ArrayType(dim) // automatically initialized to zero

    (text, weights).zipped.foreach { (word, weight) =>
      // This therefore skips the unknown words, which may not be the right strategy.
      map.get(word).foreach { index => addWeighted(total, index, weight) }
    }
    norm(total)
    total
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
      val row1Opt = map.get(text1)

      if (row1Opt.isDefined) {
        texts2.foreach { text2 =>
          val row2Opt = map.get(text2)

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

  def save(filename: String): Unit = {
    new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(filename))).autoClose { objectOutputStream =>
      // Writing is performed in two steps so that the parts can be
      // processed separately when read back in.
      objectOutputStream.writeObject(map)
    }
  }
}

object ExplicitWordEmbeddingMap extends Logging {
  protected type MutableMapType = MutableHashMap[String, ArrayType]

  type MapType = MutableMap[String, ArrayType]

  protected type BuildType = MapType

  protected val UNK = "" // token used for unknowns

  // TODO need a sanitizer here
  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): ExplicitWordEmbeddingMap = {
    logger.trace("Started to load embedding matrix from file " + filename + "...")
    val buildType =
      if (cached) loadBin(filename)
      else loadTxt(filename, resource)
    logger.trace("Completed embedding matrix loading.")
    new ExplicitWordEmbeddingMap(buildType)
  }

  protected def loadTxt(filename: String, resource: Boolean): BuildType = {
    (
        if (resource) Sourcer.sourceFromResource(filename)
        else Sourcer.sourceFromFile(filename)
        ).autoClose { source =>
      val lines = source.getLines()

      buildMatrix(lines)
    }
  }

  protected def loadBin(filename: String): BuildType = {
    new ClassLoaderObjectInputStream(this.getClass.getClassLoader, new BufferedInputStream(new FileInputStream(filename))).autoClose { objectInputStream =>
      objectInputStream.readObject().asInstanceOf[BuildType]
    }
  }

  private def buildMatrix(lines: Iterator[String]): BuildType = {

    def norm(array: ArrayType): Unit = WordEmbeddingMap.norm(array)

    val map = new MutableMapType()
    var first = true
    var dims = 0
    var total = 0
    var kept = 0

    lines.zipWithIndex.foreach { case (line, index) =>
      total += 1
      val bits = line.split("\\s+")
      if (first) {
        dims = bits(1).toInt
        first = false
      }
      else {
        if (bits.length != dims + 1) {
          println(s"${bits.length} != ${dims + 1} found on line ${index + 1}")
        }
        assert(bits.length == dims + 1)
        val w = bits(0)
        if (true) {
          kept += 1
          val weights = new ArrayType(dims)
          var i = 0
          while (i < dims) {
            weights(i) = bits(i + 1).asInstanceOf[ValueType]
            i += 1
          }
          norm(weights)
          map.put(w, weights)
        }
      }
    }
    logger.debug(s"Completed matrix loading. Kept $kept words out of a total of $total words.")
    map.toMap.asInstanceOf[BuildType]
  }
}
