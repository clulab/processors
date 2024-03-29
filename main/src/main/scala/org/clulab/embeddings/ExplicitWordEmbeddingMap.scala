package org.clulab.embeddings

import org.clulab.scala.BufferedIterator
import org.clulab.scala.WrappedArray._
import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.utils.Logging
import org.clulab.utils.Sourcer

import java.nio.charset.StandardCharsets
import java.io._
import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.io.Source
import scala.util.Using

/**
 * Implements a word embedding map where each embedding is stored as a distinct array.
 *
 * This class accommodates glove embedding files, either with or without the header line
 * that has sometimes been inserted into files to indicate the number of rows and columns
 * of vector values and with an optional vector for unknown words.  An assortment of glove
 * files packaged into jars is available from [[https://artifactory.clulab.org CLU Lab's Artifactory server]]
 * and more can be downloaded in text format from the [[https://nlp.stanford.edu/projects/glove/ GloVe website]].
 *
 * The jarred variants make it possible to include word embeddings as a library dependency
 * and to read the files as resources.  A resource flag is included in several methods for
 * this eventuality.  The original text files can be extracted manually from the jars if
 * need be.  Embeddings are read from the filesystem when resource = false, which is the
 * default.  Some CLU Lab glove files in circulation have an empty word (blank string)
 * inserted, usually as the first word in the file.  The associated vector can be used for
 * unknown words in place of a zero or random vector and instead of leaving out words.
 * The words in a glove file have (usually) had their case preserved, so for most accurate
 * results, treat other words the same.
 *
 * A simple example is included in [[org.clulab.embeddings.ExplicitWordEmbeddingMap\$.main main]].
 */
class ExplicitWordEmbeddingMap(protected val buildType: ExplicitWordEmbeddingMap.BuildType) extends WordEmbeddingMap {
  val map: ExplicitWordEmbeddingMap.ImplMapType = buildType.map
  val unkEmbeddingOpt: Option[IndexedSeq[Float]] = buildType.unknownArray.map(_.toIndexedSeq)

  /** The dimension of an embedding vector */
  override val dim: Int = map.values.head.length

  // Be careful because this word may not be sanitized!
  def isOutOfVocabulary(word: String): Boolean = !map.contains(word)

  def compare(lefts: IndexedSeq[Float], rights: IndexedSeq[Float]): Boolean = {
    lefts.length == rights.length &&
        lefts.zip(rights).forall { case (left, right) =>
          left - right < 0.0001f
        }
  }

  def compare(left: ExplicitWordEmbeddingMap.ImplMapType, right: ExplicitWordEmbeddingMap.ImplMapType): Boolean = {
    left.keySet == right.keySet && {
      left.keySet.forall { key =>
        compare(left(key), right(key))
      }
    }
  }

  def compare(left: Option[IndexedSeq[Float]], right: Option[IndexedSeq[Float]]): Boolean = {
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

  override def hashCode(): Int = 0 // Don't even try.

  /** Retrieves the embedding for this word, if it exists in the map */
  override def get(word: String): Option[IndexedSeq[Float]] = map.get(word).map(_.toIndexedSeq)

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  override def getOrElseUnknown(word: String): IndexedSeq[Float] = {
    get(word).getOrElse(
      unkEmbeddingOpt.getOrElse(
        throw new RuntimeException("ERROR: can't find embedding for the unknown token!")
      )
    )
  }


  def makeCompositeVector(text: Iterable[String]): Array[Float] = {
    val total = new Array[Float](dim) // automatically initialized to zero

    text.foreach { word =>
      map.get(word).foreach { addend => add(total, addend) }
    }
    WordEmbeddingMap.norm(total)
    total
  }

  def makeCompositeVectorWeighted(text: Iterable[String], weights: Iterable[Float]): Array[Float] = {
    val total = new Array[Float](dim) // automatically initialized to zero

    text.zip(weights).foreach { case (word, weight) =>
      // This therefore skips the unknown words, which may not be the right strategy.
      map.get(word).foreach { index => addWeighted(total, index, weight) }
    }
    WordEmbeddingMap.norm(total)
    total
  }

  protected def add(dest: Array[Float], src: IndexedSeq[Float]): Unit = {
    var i = 0

    while (i < dim) {
      dest(i) += src(i)
      i += 1
    }
  }

  protected def addWeighted(dest: Array[Float], src: IndexedSeq[Float], weight: Float): Unit = {
    var i = 0

    while (i < dim) {
      dest(i) += src(i) * weight
      i += 1
    }
  }

  def avgSimilarity(texts1: Iterable[String], texts2: Iterable[String]): Float = {
    var sum = 0f // optimization
    var count = 0 // optimization

    texts1.foreach { text1 =>
      val row1Opt = map.get(text1)

      if (row1Opt.isDefined) {
        texts2.foreach { text2 =>
          val row2Opt = map.get(text2)

          if (row2Opt.isDefined) {
            sum += WordEmbeddingMap.dotProduct(row1Opt.get, row2Opt.get)
            count += 1
          }
        }
      }
    }
    if (count != 0) sum / count
    else 0
  }

  def save(filename: String): Unit = {
    Using.resource(new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(filename)))) { objectOutputStream =>
      objectOutputStream.writeObject(map)
      objectOutputStream.writeObject(buildType.unknownArray)
    }
  }

  /** Returns all keys presented in the map, excluding the key for the unknown token */
  override def keys: Set[String] = map.keys.toSet // + ExplicitWordEmbeddingMap.UNK

  override def unknownEmbedding: IndexedSeq[Float] = unkEmbeddingOpt.get
}

object ExplicitWordEmbeddingMap extends Logging {
  protected type ImplMapType = MutableHashMap[String, Array[Float]]

  case class BuildType(map: ImplMapType, unknownArray: Option[Array[Float]])

  val UNK = "" // token for unknowns

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
      val source = Source.fromInputStream(inputStream, StandardCharsets.ISO_8859_1.toString)
      val lines = source.getLines()

      buildMatrix(lines)
    }

    new ExplicitWordEmbeddingMap(buildType)
  }

  protected def loadTxt(filename: String, resource: Boolean): BuildType = {
    Using.resource(
      if (resource) Sourcer.sourceFromResource(filename, StandardCharsets.ISO_8859_1.toString)
      else Sourcer.sourceFromFilename(filename, StandardCharsets.ISO_8859_1.toString)
    ) { source =>
      val lines = source.getLines()

      buildMatrix(lines)
    }
  }

  protected def loadBin(filename: String): BuildType = {
    Using.resource(new ClassLoaderObjectInputStream(this.getClass.getClassLoader, new BufferedInputStream(new FileInputStream(filename)))) { objectInputStream =>
      loadBin(objectInputStream)
    }
  }

  protected def loadBin(objectInputStream: ClassLoaderObjectInputStream): BuildType = {
    val map = objectInputStream.readObject().asInstanceOf[ImplMapType]
    val unkEmbeddingOpt = objectInputStream.readObject().asInstanceOf[Option[Array[Float]]]

    BuildType(map, unkEmbeddingOpt)
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
    val map = new ImplMapType()
    val (wordCountOpt, columns) = getWordCountOptAndColumns(linesAndIndices)
    var unknownWeightsOpt: Option[Array[Float]] = None
    var total = 0

    linesAndIndices.foreach { case (line, index) =>
      total += 1
      val bits = line.split("\\s+")
      require(bits.length == columns + 1, s"${bits.length} != ${columns + 1} found on line ${index + 1}")
      val word = bits(0)
      val weights = {
        val weights = new Array[Float](columns)
        var i = 0
        while (i < columns) {
          weights(i) = bits(i + 1).toFloat
          i += 1
        }
        WordEmbeddingMap.norm(weights)
        weights
      }
      if (word == UNK)
        unknownWeightsOpt = Some(weights)
      else
        map.put(word, weights)
    }
    logger.info(s"Completed matrix loading. Kept ${map.size} words out of a total of $total.")
    if (wordCountOpt.isDefined)
      require(wordCountOpt.get == total, s"The matrix file should have had ${wordCountOpt.get} lines of words.")
    BuildType(map, unknownWeightsOpt)
  }

  def main(args: Array[String]): Unit = {
    println("Syntax: <filename> <count> <word1 ...>")
    val filename = args.lift(0).getOrElse("glove.840B.300d.10f.txt")
    val count = args.lift(1).getOrElse("10").toInt
    val argsWords = args.slice(2, args.length).toSet
    val words = if (argsWords.isEmpty) Set("house") else argsWords
    val wordEmbeddingMap = ExplicitWordEmbeddingMap(filename, resource = false)
    val mostSimilarWords = wordEmbeddingMap.mostSimilarWords(words, count)

    mostSimilarWords.zipWithIndex.foreach { case ((word, similarity), index) =>
      println(s"$index $word $similarity")
    }
  }
}
