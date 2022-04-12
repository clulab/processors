package org.clulab.embeddings

import java.io._
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.{ClassLoaderObjectInputStream, Sourcer}
import org.slf4j.{Logger, LoggerFactory}

import java.nio.charset.StandardCharsets
import scala.collection.immutable.HashMap
import scala.collection.mutable.{HashMap => MutableHashMap, Map => MutableMap}

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
class OldCompactWordEmbeddingMap(buildType: OldCompactWordEmbeddingMap.BuildType) extends WordEmbeddingMap {
  protected val map: OldCompactWordEmbeddingMap.MapType = buildType._1 // (word -> row)
  protected val array: OldCompactWordEmbeddingMap.ArrayType = buildType._2 // flattened matrix
  val columns: Int = array.length / map.size
  val rows: Int = array.length / columns

  def oldGet(word: String): Option[OldCompactWordEmbeddingMap.ArrayType] = { // debug use only
    map.get(word).map { row =>
      val offset = row * columns

      array.slice(offset, offset + columns)
    }
  }

  def knownKeys: Iterable[String] = map.keys // debug use only

  override def keys: Set[String] = map.keys.toSet // debug use only

  override def unknownEmbedding: IndexedSeq[Float] = {
    throw new RuntimeException("ERROR: unknownEmbedding not supported in this class!")
  }

  def save(filename: String): Unit = {
    // Sort the map entries (word -> row) by row and then keep just the word.
    val words = map.toArray.sortBy(_._2).map(_._1).mkString("\n")

    new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(filename))).autoClose { objectOutputStream =>
      // Writing is performed in two steps so that the parts can be
      // processed separately when read back in.
      objectOutputStream.writeObject(words)
      objectOutputStream.writeObject(array)
    }
  }

  def dotProduct(row1: Int, row2: Int): OldCompactWordEmbeddingMap.ValueType = {
    val offset1 = row1 * columns
    val offset2 = row2 * columns
    var sum = 0.asInstanceOf[OldCompactWordEmbeddingMap.ValueType] // optimization
    var i = 0 // optimization

    while (i < columns) {
      sum += array(offset1 + i) * array(offset2 + i)
      i += 1
    }
    sum
  }

  protected def add(dest: OldCompactWordEmbeddingMap.ArrayType, srcRow: Int): Unit = {
    val srcOffset = srcRow * columns
    var i = 0 // optimization

    while (i < columns) {
      dest(i) += array(srcOffset + i)
      i += 1
    }
  }

  protected def addWeighted(dest: OldCompactWordEmbeddingMap.ArrayType, srcRow: Int, weight:Float): Unit = {
    val srcOffset = srcRow * columns
    var i = 0 // optimization

    while (i < columns) {
      dest(i) += array(srcOffset + i)*weight
      i += 1
    }
  }

  def isOutOfVocabulary(word: String): Boolean = !map.contains(EmbeddingUtils.sanitizeWord(word))

  // Normalize this vector to length 1, in place.
  // (If the length is zero, do nothing.)
  protected def norm(array: OldCompactWordEmbeddingMap.ArrayType): OldCompactWordEmbeddingMap.ArrayType = {
    var len = 0.asInstanceOf[OldCompactWordEmbeddingMap.ValueType] // optimization
    var i = 0 // optimization

    while (i < array.length) {
      len += array(i) * array(i)
      i += 1
    }
    len = math.sqrt(len).asInstanceOf[OldCompactWordEmbeddingMap.ValueType]

    if (len != 0) {
      i = 0
      while (i < array.length) {
        array(i) /= len
        i += 1
      }
    }
    array
  }

  def makeCompositeVector(text: Iterable[String]): OldCompactWordEmbeddingMap.ArrayType = {
    val total = new OldCompactWordEmbeddingMap.ArrayType(columns)

    text.foreach { word =>
      map.get(word).foreach { index => add(total, index) }
    }
    norm(total)
  }

  def makeCompositeVectorWeighted(text: Iterable[String], weights:Iterable[Float]): OldCompactWordEmbeddingMap.ArrayType = {
    val total = new OldCompactWordEmbeddingMap.ArrayType(columns)

    (text, weights).zipped.foreach { (word, weight) =>
      map.get(word).foreach { index => addWeighted(total, index, weight) }
    }

    norm(total)
  }

  // Find the average embedding similarity between any two words in these two texts.
  // IMPORTANT: words here must be words not lemmas!
  def avgSimilarity(text1: Iterable[String], text2: Iterable[String]): OldCompactWordEmbeddingMap.ValueType = {
    val sanitizedText1 = text1.map(EmbeddingUtils.sanitizeWord(_))
    val sanitizedText2 = text2.map(EmbeddingUtils.sanitizeWord(_))

    sanitizedAvgSimilarity(sanitizedText1, sanitizedText2)
  }

  // Find the average embedding similarity between any two words in these two texts.
  protected def sanitizedAvgSimilarity(text1: Iterable[String], text2: Iterable[String]): OldCompactWordEmbeddingMap.ValueType = {
    var avg = 0.asInstanceOf[OldCompactWordEmbeddingMap.ValueType] // optimization
    var count = 0 // optimization

    for (word1 <- text1) {
      val row1 = map.get(word1)

      if (row1.isDefined) {
        for (word2 <- text2) {
          val row2 = map.get(word2)

          if (row2.isDefined) {
            avg += dotProduct(row1.get, row2.get)
            count += 1
          }
        }
      }
    }
    if (count != 0) avg / count
    else 0
  }

  /** The dimension (width) of an embedding vector */
  override def dim: Int = columns

  /** Retrieves the embedding for this word, if it exists in the map */
  override def get(word: String): Option[IndexedSeq[Float]] = ???

  /** Retrieves the embedding for this word; if it doesn't exist in the map uses the unknown
    * embedding instead.  That embedding is defined in the vector file.  If it is needed,
    * but doesn't exist, a RuntimeException is thrown.
    */

  val fakeResult = new Array[Float](columns)
  override def getOrElseUnknown(word: String): IndexedSeq[Float] = {
    oldGet(word)
    fakeResult
  }
}

object OldCompactWordEmbeddingMap {
  protected type MutableMapType = MutableHashMap[String, Int]
  protected type ImmutableMapType = HashMap[String, Int]

  protected type ImplementationMapType = MutableMapType // optimization

  // These were meant to allow easy switching between implementations.
  type MapType = MutableMap[String, Int]
  type ValueType = Float
  type ArrayType = Array[ValueType]

  protected type BuildType = (MapType, ArrayType)
  protected type StoreType = (String, ArrayType)

  protected val logger: Logger = LoggerFactory.getLogger(classOf[OldCompactWordEmbeddingMap])

  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): OldCompactWordEmbeddingMap = {
    logger.trace("Started to load embedding matrix from file " + filename + "...")
    val buildType =
      if (cached) loadBin(filename)
      else loadTxt(filename, resource)
    logger.trace("Completed embedding matrix loading.")
    new OldCompactWordEmbeddingMap(buildType)
  }

  protected def loadTxt(filename: String, resource: Boolean): BuildType = {
    (
        if (resource) Sourcer.sourceFromResource(filename, StandardCharsets.ISO_8859_1.toString)
        else Sourcer.sourceFromFilename(filename, StandardCharsets.ISO_8859_1.toString)
        ).autoClose { source =>
      val lines = source.getLines()

      buildMatrix(lines)
    }
  }

  protected def loadBin(filename: String): BuildType = {
    // This is the original code
    //    val (text, array) = updatedLoad[StoreType](filename, this)
    //    val words = text.split('\n')
    //    val map: MapType = words.zipWithIndex.toMap.asInstanceOf[MapType]
    //    (map, array)

    // This is "unrolled" for performance purposes.
    new ClassLoaderObjectInputStream(this.getClass.getClassLoader, new BufferedInputStream(new FileInputStream(filename))).autoClose { objectInputStream =>
      val map: MapType = new MutableMapType()

      {
        // This block is so that text can be abandoned at the end of the block, before the array is read.
        val text = objectInputStream.readObject().asInstanceOf[String]
        val stringBuilder = new StringBuilder

        for (i <- 0 until text.length) {
          val c = text(i)

          if (c == '\n') {
            map += ((stringBuilder.result(), map.size))
            stringBuilder.clear()
          }
          else
            stringBuilder.append(c)
        }
        map += ((stringBuilder.result(), map.size))
      }

      val array = objectInputStream.readObject().asInstanceOf[ArrayType]
      (map, array)
    }
  }

  protected def buildMatrix(lines: Iterator[String]): BuildType = {

    def norm(array: ArrayType, rowIndex: Int, columns: Int): Unit = {
      val offset = rowIndex * columns
      var len = 0.asInstanceOf[OldCompactWordEmbeddingMap.ValueType] // optimization
      var i = 0 // optimization

      while (i < columns) {
        len += array(offset + i) * array(offset + i)
        i += 1
      }
      len = math.sqrt(len).asInstanceOf[ValueType]

      if (len != 0) {
        i = 0
        while (i < columns) {
          array(offset + i) /= len
          i += 1
        }
      }
    }

    val linesZipWithIndex = lines.zipWithIndex
    val (wordCount, columns) =
      if (linesZipWithIndex.hasNext) {
        val bits = linesZipWithIndex.next()._1.split(' ')

        assert(bits.length == 2, "The first line must specify wordCount and dimension.")
        (bits(0).toInt, bits(1).toInt)
      }
      else (0, 0)
    var map = new ImplementationMapType()
    val array = new ArrayType(wordCount * columns)

    for ((line, lineIndex) <- linesZipWithIndex) {
      val bits = line.split(' ')
      assert(bits.length == columns + 1, s"${bits.length} != ${columns + 1} found on line ${lineIndex + 1}")
      val word = bits(0)
      val row =
        if (map.contains(word)) {
          logger.info(s"'$word' is duplicated in the vector file.")
          // Use space because we will not be looking for words like that.
          // The array will not be filled in for this map.size value.
          map += (" " + map.size -> map.size)
          map(word)
        }
        else map.size
      assert(row < wordCount)
      map += (word -> row)

      val offset = row * columns
      var i = 0 // optimization

      while (i < columns) {
        array(offset + i) = bits(i + 1).toDouble.asInstanceOf[ValueType]
        i += 1
      }
      norm(array, row, columns)
    }
    assert(map.size == wordCount, s"The file should have had ${map.size} words.")
    (map, array)
  }
}
