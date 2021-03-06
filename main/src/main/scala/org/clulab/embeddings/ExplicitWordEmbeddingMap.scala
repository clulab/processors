package org.clulab.embeddings

import java.io._
import org.clulab.embeddings.WordEmbeddingMap._
import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Logging
import org.clulab.utils.Sourcer

import scala.collection.mutable.{HashMap => MutableHashMap, Map => MutableMap}
import scala.io.Source

/**
 * Implements an word embedding map, where each embedding is stored as a distinct array
 */
class ExplicitWordEmbeddingMap(buildType: ExplicitWordEmbeddingMap.BuildType) extends WordEmbeddingMap {
  val map: ExplicitWordEmbeddingMap.MapType = buildType._1
  val unkEmbeddingOpt: Option[SeqType] = buildType._2

  /** The dimension of an embedding vector */
  override val dim: Int = map.values.head.length

  def compare(lefts: SeqType, rights: SeqType): Boolean = {
    lefts.length == rights.length &&
        lefts.zip(rights).forall { case (left, right) =>
          left - right < 0.0001f
        }
  }

  def compare(left: ExplicitWordEmbeddingMap.MapType, right: ExplicitWordEmbeddingMap.MapType): Boolean = {
    left.keySet == right.keySet && {
      left.keySet.forall { key =>
        compare(left(key), right(key))
      }
    }
  }

  def compare(left: Option[SeqType], right: Option[SeqType]): Boolean = {
    (left, right) match {
      case (None, None) => true
      case (Some(left), Some(right)) => compare(left, right)
      case _ => false
    }
  }

  override def equals(other: Any): Boolean = {
    other.isInstanceOf[ExplicitWordEmbeddingMap] && {
      val that = other.asInstanceOf[ExplicitWordEmbeddingMap]

      this.dim == that.dim &&
          compare(this.map, that.map) &&
          compare(this.unkEmbeddingOpt, that.unkEmbeddingOpt)
    }
  }

  override def hashCode(): Int = 0

  /** Retrieves the embedding for this word, if it exists in the map */
  override def get(word: String): Option[SeqType] = map.get(word).map(_.toSeq)

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  override def getOrElseUnknown(word: String): SeqType = {
    get(word).getOrElse(
      unkEmbeddingOpt.getOrElse(
        throw new RuntimeException("ERROR: can't find embedding for the unknown token!")
      )
    )
  }

  // Be careful because this word may not be sanitized!
  def isOutOfVocabulary(word: String): Boolean = !map.contains(word)

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

  protected def add(dest: ArrayType, src: SeqType): Unit = {
    var i = 0

    while (i < dim) {
      dest(i) += src(i)
      i += 1
    }
  }

  protected def addWeighted(dest: ArrayType, src: SeqType, weight: Float): Unit = {
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

  protected def dotProduct(v1: ArrayType, v2: SeqType): ValueType = {
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
      objectOutputStream.writeObject(map)
      objectOutputStream.writeObject(unkEmbeddingOpt)
    }
  }
}

object ExplicitWordEmbeddingMap extends Logging {
  protected type MutableMapType = MutableHashMap[String, ArrayType]

  type MapType = MutableMap[String, ArrayType]

  protected type BuildType = (MapType, Option[SeqType])

  val UNK = "" // token used for unknowns

  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): ExplicitWordEmbeddingMap = {
    logger.trace("Started to load embedding matrix from file " + filename + "...")
    val buildType =
      if (cached) loadBin(filename)
      else loadTxt(filename, resource)
    logger.trace("Completed embedding matrix loading.")
    new ExplicitWordEmbeddingMap(buildType)
  }

  def apply(inputStream: InputStream, binary: Boolean): ExplicitWordEmbeddingMap = {
    val buildType = if (binary) {
      val objectInputStream = new ClassLoaderObjectInputStream(this.getClass.getClassLoader, inputStream)
      loadBin(objectInputStream)
    }
    else {
      val source = Source.fromInputStream(inputStream)
      val lines = source.getLines()

      buildMatrix(lines)
    }

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
      loadBin(objectInputStream)
    }
  }

  protected def loadBin(objectInputStream: ClassLoaderObjectInputStream): BuildType = {
    val map = objectInputStream.readObject().asInstanceOf[MapType]
    val unkEmbeddingOpt = objectInputStream.readObject().asInstanceOf[Option[SeqType]]

    (map, unkEmbeddingOpt)
  }

  protected def getWordCountOptAndColumns(linesAndIndices: BufferedIterator[(String, Int)]): (Option[Int], Int) = {
    val (line, _) = linesAndIndices.head
    val bits = line.split(' ')

    require(bits.length >= 2, "A glove file must have at least two columns everywhere.")
    if (bits.length == 2) {
      linesAndIndices.next() // Go ahead and consume the first Line.
      (Some(bits(0).toInt), bits(1).toInt)
    }
    else
      (None, bits.length - 1)
  }

  private def buildMatrix(lines: Iterator[String]): BuildType = {
    val linesAndIndices = lines.zipWithIndex.buffered
    val map = new MutableMapType()
    val (wordCountOpt, columns) = getWordCountOptAndColumns(linesAndIndices)
    var unknownWeightsOpt: Option[SeqType] = None
    var total = 0

    linesAndIndices.foreach { case (line, index) =>
      total += 1
      val bits = line.split("\\s+")
      require(bits.length == columns + 1, s"${bits.length} != ${columns + 1} found on line ${index + 1}")
      val word = bits(0)
      val weights = {
        val weights = new ArrayType(columns)
        var i = 0
        while (i < columns) {
          weights(i) = bits(i + 1).toDouble.asInstanceOf[ValueType]
          i += 1
        }
        WordEmbeddingMap.norm(weights)
        weights
      }
      if (word == UNK)
        unknownWeightsOpt = Some(weights.toSeq)
      else
        map.put(word, weights)
    }
    logger.info(s"Completed matrix loading. Kept ${map.size} words out of a total of $total.")
    if (wordCountOpt.isDefined)
      require(wordCountOpt.get == total, s"The matrix file should have had ${wordCountOpt.get} lines of words.")
    (map, unknownWeightsOpt)
  }
}
