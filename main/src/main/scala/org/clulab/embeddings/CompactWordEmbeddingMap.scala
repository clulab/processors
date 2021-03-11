package org.clulab.embeddings

import java.io._

import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Logging
import org.clulab.utils.Sourcer

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.io.Source

/**
  * This class and its companion object have been backported from Eidos.  There it is/was an optional
  * replacement for WordEmbeddingMap used for performance reasons.  It loads data faster from disk and stores it
  * more compactly in memory.  It does not, however, include all the operations of processer's Word2Vec.
  * For instance, logMultiplicativeTextSimilarity is not included, but could probably be added.  Other
  * methods like getWordVector, which in Word2Vec returns an Array[Double], would be inefficient to
  * include because the arrays of doubles (or floats) are no longer part of the design.  For more
  * documentation other than that immediately below, both the companion object and the related test case
  * (org.clulab.embeddings.TestCompactWord2Vec) may be helpful.
  *
  * The class is typically instantiated by the apply method of the companion object which takes as
  * arguments a filename and then two booleans: "resource", which specifies whether the named file
  * exists as a resource or is alternatively stored on the broader filesystem, and "cached", which
  * specifies that the data consists of Java-serialized objects (see the save method) or, alternatively,
  * the standard vector text format.  The apply method arranges for the file to be read in the appropriate
  * way and converted into a map with the words being keys with values being the row numbers in an implied
  * 2-dimentional matrix of the all vector values, also included in the constructor.  So, rather than each
  * word being mapped to an independent, mini array as in Word2Vec, they are mapped to an integer row
  * number of a single, larger matrix/array.
  *
  * To take advantage of the faster load times, the vector data file needs to be converted from text
  * format into a binary (Java serialized objects) for loadBin below.  The test case includes an example.
  * In some preprocessing phase, call CompactWord2Vec(filename, resource = false, cached = false)
  * on the file containing the vectors in text format, such as glove.840B.300d.txt.  "resource" is
  * usually false because it can be a very large file, too large to include as a resource.  On the
  * resulting return value, call save(compactFilename).  Thereafter, for normal, speedy processing,
  * use CompactWord2Vec(compactFilename, resource = false, cached = true).
  */
class CompactWordEmbeddingMap(protected val buildType: CompactWordEmbeddingMap.BuildType)
    extends WordEmbeddingMap {
  protected val map: CompactWordEmbeddingMap.ImplMapType = buildType.map // (word -> row)
  protected val array: Array[Float] = buildType.array // flattened matrix
  val columns: Int = array.length / map.size
  val rows: Int = array.length / columns
  val unkEmbeddingOpt: Option[IndexedSeq[Float]] = buildType.unknownArray.map(_.toIndexedSeq)

  /** The dimension of an embedding vector */
  override val dim: Int = columns

  // Be careful because this word may not be sanitized!
  override def isOutOfVocabulary(word: String): Boolean = !map.contains(word)

  def compare(lefts: IndexedSeq[Float], rights: IndexedSeq[Float]): Boolean = {
    lefts.length == rights.length &&
        lefts.zip(rights).forall { case (left, right) =>
          left - right < 0.0001f
        }
  }

  def compare(left: CompactWordEmbeddingMap.ImplMapType, right: CompactWordEmbeddingMap.ImplMapType): Boolean = {
    left.keySet == right.keySet && {
      left.keySet.forall { key =>
        left(key) == right(key)
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
    other.isInstanceOf[CompactWordEmbeddingMap] && {
      val that = other.asInstanceOf[CompactWordEmbeddingMap]

      this.dim == that.dim &&
          this.columns == that.columns &&
          this.rows == that.rows &&
          compare(this.array, that.array) &&
          compare(this.map, that.map) &&
          compare(this.unkEmbeddingOpt, that.unkEmbeddingOpt)
    }
  }

  override def hashCode(): Int = 0 // Don't even try.

  def get(word: String): Option[IndexedSeq[Float]] = {
    map.get(word).map { row =>
      val offset = row * columns
      array.view(offset, offset + columns)
    }
  }

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the Unknown token instead */
  override def getOrElseUnknown(word: String): IndexedSeq[Float] = {
    get(word).getOrElse(
      unkEmbeddingOpt.getOrElse(
        throw new RuntimeException("ERROR: can't find embedding for the unknown token!")
      )
    )
  }

  /** Computes the embedding of a text, as an unweighted average of all words */
  override def makeCompositeVector(text: Iterable[String]): Array[Float] = {
    val total = new Array[Float](columns) // automatically initialized to zero

    text.foreach { word =>
      // This therefore skips the unknown words, which may not be the right strategy.
      map.get(word).foreach { index => add(total, index) }
    }
    WordEmbeddingMap.norm(total)
    total
  }

  override def makeCompositeVectorWeighted(text: Iterable[String], weights: Iterable[Float]): Array[Float] = {
    val total = new Array[Float](columns) // automatically initialized to zero

    (text, weights).zipped.foreach { (word, weight) =>
      // This therefore skips the unknown words, which may not be the right strategy.
      map.get(word).foreach { index => addWeighted(total, index, weight) }
    }
    WordEmbeddingMap.norm(total)
    total
  }

  def keys: Iterable[String] = map.keys // debug use only

  protected def add(dest: Array[Float], srcRow: Int): Unit = {
    val srcOffset = srcRow * columns
    var i = 0 // optimization

    while (i < columns) {
      dest(i) += array(srcOffset + i)
      i += 1
    }
  }

  protected def addWeighted(dest: Array[Float], srcRow: Int, weight: Float): Unit = {
    val srcOffset = srcRow * columns
    var i = 0 // optimization

    while (i < columns) {
      dest(i) += array(srcOffset + i) * weight
      i += 1
    }
  }

  // Find the average embedding similarity between any two words in these two texts.
  override def avgSimilarity(texts1: Iterable[String], texts2: Iterable[String]): Float = {
    var sum = 0f // optimization
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
    else 0f
  }

  def dotProduct(row1: Int, row2: Int): Float = {
    val offset1 = row1 * columns
    val offset2 = row2 * columns
    var sum = 0f
    var i = 0 // optimization

    while (i < columns) {
      sum += array(offset1 + i) * array(offset2 + i)
      i += 1
    }
    sum
  }

  def save(filename: String): Unit = {
    // Sort the map entries (word -> row) by row and then keep just the word.
    // This should put "", the unknown word, first.
    val words = map.toArray.sortBy(_._2).map(_._1).mkString("\n")

    new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(filename))).autoClose { objectOutputStream =>
      objectOutputStream.writeObject(words)
      objectOutputStream.writeObject(array)
      objectOutputStream.writeObject(buildType.unknownArray)
    }
  }
}

object CompactWordEmbeddingMap extends Logging {
  protected type ImplMapType = MutableHashMap[String, Int]

  case class BuildType(map: ImplMapType, array: Array[Float], unknownArray: Option[Array[Float]])

  val UNK = "" // token for unknowns

  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): CompactWordEmbeddingMap = {
    logger.trace("Started to load embedding matrix from file " + filename + "...")
    val buildType =
      if (cached) loadBin(filename)
      else loadTxt(filename, resource)
    logger.trace("Completed embedding matrix loading.")
    new CompactWordEmbeddingMap(buildType)
  }

  def apply(inputStream: InputStream, binary: Boolean): CompactWordEmbeddingMap = {
    val buildType = if (binary) {
      val objectInputStream = new ClassLoaderObjectInputStream(this.getClass.getClassLoader, inputStream)
      loadBin(objectInputStream)
    }
    else {
      val source = Source.fromInputStream(inputStream)
      val lines = source.getLines()

      buildMatrix(lines)
    }

    new CompactWordEmbeddingMap(buildType)
  }

  protected def loadTxt(filename: String, resource: Boolean): BuildType = {
    (
      // Check first line for two columns, otherwise calculate it.
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

  protected def loadBin(objectInputStream: ObjectInputStream): BuildType = {
    val map = {
      val words = objectInputStream.readObject().asInstanceOf[String].split('\n')
      // Were it not for MapType, the following could be Map(words.zipWithIndex: _*)
      val map: ImplMapType = new ImplMapType()
      words.foreach { word =>
        map += word -> map.size
      }
      map
    }
    val array = objectInputStream.readObject().asInstanceOf[Array[Float]]
    val unknownArrayOpt = objectInputStream.readObject().asInstanceOf[Option[Array[Float]]]

    BuildType(map, array, unknownArrayOpt)
  }

  protected def norm(arrayBuffer: ArrayBuffer[Float], rowIndex: Int, columns: Int): Unit = {
    val offset = rowIndex * columns

    WordEmbeddingMap.norm(arrayBuffer.view(offset, offset + columns))
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

  protected def buildMatrix(lines: Iterator[String]): BuildType = {
    val linesAndIndices = lines.zipWithIndex.buffered
    val map = new ImplMapType()
    val (wordCountOpt, columns) = getWordCountOptAndColumns(linesAndIndices)
    val dim = wordCountOpt.map(_ * columns).getOrElse(1000 * columns)
    val arrayBuffer = new ArrayBuffer[Float](dim)
    var unknownArrayBufferOpt: Option[ArrayBuffer[Float]] = None
    var total = 0

    linesAndIndices.foreach { case (line, index) =>
      if (index % 1000 == 0)
        println(index)
      total += 1
      val bits = line.split(' ')
      require(bits.length == columns + 1, s"${bits.length} != ${columns + 1} found on line ${index + 1}")
      val word = bits(0)
      if (map.contains(word))
        logger.warn(s"The word '$word' is duplicated in the vector file on line ${index + 1} and this later instance is skipped.")
      else if (word == UNK && unknownArrayBufferOpt.isDefined)
        logger.warn(s"The unknown vector is duplicated in the vector file on line ${index + 1} and this later instance is skipped.")
      else {
        val buffer =
          if (word == UNK) {
            val unknownArrayBuffer = new ArrayBuffer[Float](columns)
            unknownArrayBufferOpt = Some(unknownArrayBuffer)
            unknownArrayBuffer
          }
          else
            arrayBuffer
        var i = 0 // optimization
        while (i < columns) {
          buffer += bits(i + 1).toFloat
          i += 1
        }
        if (word == UNK)
          norm(buffer, 0, columns)
        else {
          val row = map.size
          norm(buffer, row, columns)
          map += (word -> row)
        }
      }
    }
    logger.info(s"Completed matrix loading. Kept ${map.size} words from $total lines of words.")
    if (unknownArrayBufferOpt.isDefined)
      logger.info(s"An unknown vector is defined for the matrix.")
    if (wordCountOpt.isDefined)
      require(wordCountOpt.get == total, s"The matrix file should have had ${wordCountOpt.get} lines of words.")
    BuildType(map, arrayBuffer.toArray, unknownArrayBufferOpt.map(_.toArray))
  }
}
