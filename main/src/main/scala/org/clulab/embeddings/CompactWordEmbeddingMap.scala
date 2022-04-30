package org.clulab.embeddings

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output

import java.io._
import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Logging
import org.clulab.utils.Sourcer
import org.clulab.utils.Timers

import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer // IntelliJ doesn't complain about this.
import scala.collection.mutable.{ArrayBuilder => MutableArrayBuilder}
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
  val columns: Int = buildType.columns
  val rows: Int = map.size // which is not necessarily the same as array.length / columns
  val unkEmbeddingOpt: Option[IndexedSeq[Float]] = buildType.unknownArray.map(_.view)

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

  def knownKeys: Iterable[String] = map.keys // debug use only

  /** Returns all keys presented in the map, excluding the key for the unknown token */
  override def keys: Set[String] = map.keys.toSet // + CompactWordEmbeddingMap.UNK

  override def unknownEmbedding: IndexedSeq[Float] = unkEmbeddingOpt.get

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

  protected def mkTextFromMap(): String =
    // Sort the map entries (word -> row) by row and then keep just the word.
    // This should put "", the unknown word, first.
    map.toArray.sortBy(_._2).map(_._1).mkString("\n")

  def save(filename: String): Unit = {
    new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(filename))).autoClose { objectOutputStream =>
      objectOutputStream.writeObject(mkTextFromMap())
      objectOutputStream.writeObject(array)
      objectOutputStream.writeObject(buildType.unknownArray.orNull)
      objectOutputStream.writeObject(columns)
    }
  }

  def saveKryo(filename: String): Unit = {
    val kryo = CompactWordEmbeddingMap.newKryo()

    new Output(new BufferedOutputStream(new FileOutputStream(filename))).autoClose { output =>
      kryo.writeObject(output, mkTextFromMap())
      kryo.writeObject(output, array)
      kryo.writeObject(output, buildType.unknownArray.orNull)
      kryo.writeObject(output, columns)
    }
  }
}

object CompactWordEmbeddingMap extends Logging {
  // Choose one of these.  If loadBin() is called, this setting is used to route the request to
  // a particular binary format.
  val binIsSer    = false
  val binIsKryo   = !binIsSer

  protected type ImplMapType = MutableHashMap[String, Int]

  case class BuildType(map: ImplMapType, array: Array[Float], unknownArray: Option[Array[Float]], columns: Int)

  val UNK = "" // token for unknowns

  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): CompactWordEmbeddingMap = {
    logger.trace("Started to load embedding matrix from file " + filename + "...")
    val buildType =
        // Cached versions are expected to be binary.
        if (cached) loadBin(filename)
        else loadTxt(filename, resource)
    val result = new CompactWordEmbeddingMap(buildType)
    logger.trace("Completed embedding matrix loading.")
    result
  }

  def apply(inputStream: InputStream, binary: Boolean): CompactWordEmbeddingMap = {
    val buildType =
        if (binary) loadBin(inputStream)
        else loadTxt(inputStream)

    new CompactWordEmbeddingMap(buildType)
  }

  def loadTxt(filename: String, resource: Boolean): BuildType = {
    loadTxt(
        if (resource) Sourcer.sourceFromResource(filename, StandardCharsets.ISO_8859_1.toString)
        else Sourcer.sourceFromFilename(filename, StandardCharsets.ISO_8859_1.toString)
    )
  }

  def loadTxt(inputStream: InputStream): BuildType =
      loadTxt(Source.fromInputStream(inputStream, StandardCharsets.ISO_8859_1.toString))

  def loadTxt(source: Source): BuildType = {
    source.autoClose { source =>
      val lines = source.getLines()

      buildMatrix(lines)
    }
  }

  def loadBin(filename: String): BuildType = {
    if (binIsSer)
      loadSer(filename)
    else if (binIsKryo)
      loadKryo(filename)
    else
      throw new RuntimeException("Unknown binary serialization format")
  }

  def loadBin(inputStream: InputStream): BuildType = {
    if (binIsSer)
      loadSer(inputStream)
    else if (binIsKryo)
      loadKryo(inputStream)
    else
      throw new RuntimeException("Unknown binary serialization format")
  }

  protected def mkMapFromText(text: String): ImplMapType = {
    val map: ImplMapType = new ImplMapType()
    map ++= new SplitterIter(text)
  }

  def loadSer(filename: String): BuildType = loadSer(new FileInputStream(filename))

  def loadSer(inputStream: InputStream): BuildType = {
    new ClassLoaderObjectInputStream(this.getClass.getClassLoader, new BufferedInputStream(inputStream)).autoClose { objectInputStream =>
      val map = mkMapFromText(objectInputStream.readObject().asInstanceOf[String])
      val array = objectInputStream.readObject().asInstanceOf[Array[Float]]
      val unknownArrayOpt = Option(objectInputStream.readObject().asInstanceOf[Array[Float]])
      val columns = objectInputStream.readObject().asInstanceOf[Int]

      BuildType(map, array, unknownArrayOpt, columns)
    }
  }

  protected def newKryo(): Kryo = {
    val kryo = new Kryo()
    kryo.register(classOf[Array[Float]])

    kryo
  }

  def loadKryo(filename: String): BuildType = loadKryo(new FileInputStream(filename))

  def loadKryo(inputStream: InputStream): BuildType = {
    val kryo = newKryo()

    new Input(new BufferedInputStream(inputStream)).autoClose { input =>
      val map = mkMapFromText(kryo.readObject(input, classOf[String]))
      val array = kryo.readObject(input, classOf[Array[Float]])
      val unknownArrayOpt = Option(kryo.readObjectOrNull(input, classOf[Array[Float]]))
      val columns = kryo.readObject(input, classOf[Int])

      BuildType(map, array, unknownArrayOpt, columns)
    }
  }

  // This saves about a second over plain text.split('\n').
  class SplitterIter(text: String) extends Iterator[(String, Int)] {
    protected var index: Int = 0
    protected var count: Int = 0

    override def hasNext: Boolean = index < text.length

    override def next(): (String, Int) = {
      val nextSeparator = text.indexOf('\n', index)
      val until = if (nextSeparator >= 0) nextSeparator else text.length
      val word = text.slice(index, until)
      val result = word -> count

      index = until + 1 // skip the LF (or trailing \0)
      count += 1
      result
    }
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

  abstract class Appender(val columns: Int) {
    def append(value: Float): Unit
    def normed(): Array[Float]

    def norm(array: Array[Float]): Array[Float] = {
      val length = array.length
      var index = 0 // optimization

      while (index < length) {
        // Lengths of vectors are generally around 5.  They are _not_ normalized.
        WordEmbeddingMap.norm(array.view(index, index + columns))
        index += columns
      }
      array
    }
  }

  object Appender {

    def apply(wordCountOpt: Option[Int], columns: Int): Appender = {
      wordCountOpt.map { wordCount =>
       new SizedAppender(wordCount, columns)
      }.getOrElse {
        new ArrayBuilderAppender(columns)   //  48,308 ms
        // new ArrayBufferAppender(columns) // 286,159 ms
      }
    }
  }

  class SizedAppender protected (array: Array[Float], columns: Int) extends Appender(columns) {

    def this(rows: Integer, columns: Integer) = this(new Array[Float](rows * columns), columns)

    protected var index = 0

    def append(value: Float): Unit = {
      array(index) = value
      index += 1
    }

    def normed(): Array[Float] = norm(array)
  }

  abstract class UnsizedAppender(columns: Int) extends Appender(columns)

  class ArrayBufferAppender protected (arrayBuffer: ArrayBuffer[Float], columns: Int) extends UnsizedAppender(columns) {

    def this(columns: Int) = this(new ArrayBuffer[Float](100000 * columns), columns)

    def append(value: Float): Unit = arrayBuffer += value

    def normed(): Array[Float] = norm(arrayBuffer.toArray)
  }

  class ArrayBuilderAppender protected (arrayBuilder: MutableArrayBuilder[Float], columns: Int) extends UnsizedAppender(columns) {

    def this(columns: Int) = this(new MutableArrayBuilder.ofFloat, columns)

    def append(value: Float): Unit = arrayBuilder += value

    def normed(): Array[Float] = norm(arrayBuilder.result())
  }

  protected def buildMatrix(lines: Iterator[String]): BuildType = {
    val linesAndIndices = lines.zipWithIndex.buffered
    val map = new ImplMapType()
    val (wordCountOpt, columns) = getWordCountOptAndColumns(linesAndIndices)
    val knownAppender = Appender(wordCountOpt, columns)
    var unknownAppenderOpt: Option[Appender] = None
    var total = 0

    linesAndIndices.foreach { case (line, index) =>
      total += 1
      val bits = line.split(' ')
      require(bits.length == columns + 1, { s"${bits.length} != ${columns + 1} found on line ${index + 1}" })
      val word = bits(0)
      if (map.contains(word))
        logger.warn(s"The word '$word' is duplicated in the vector file on line ${index + 1} and this later instance is skipped.")
      else if (word == UNK && unknownAppenderOpt.isDefined)
        logger.warn(s"The unknown vector is duplicated in the vector file on line ${index + 1} and this later instance is skipped.")
      else {
        val appender =
          if (word != UNK) {
            // If there is an unknown vector, then the array will not be completely filled.
            // That's also the case if there are duplicate words.  Use the tree size for the real count.
            map += (word -> map.size)
            knownAppender
          }
          else {
            val unknownAppender = Appender(Some(1), columns)
            unknownAppenderOpt = Some(unknownAppender)
            unknownAppender
          }
        var i = 0 // optimization
        while (i < columns) {
          appender.append(bits(i + 1).toFloat)
          i += 1
        }
      }
    }
    logger.info(s"Completed matrix loading. Kept ${map.size} words from $total lines of words.")
    if (unknownAppenderOpt.isDefined)
      logger.info(s"An unknown vector is defined for the matrix.")
    if (wordCountOpt.isDefined)
      require(wordCountOpt.get == total, s"The matrix file should have had ${wordCountOpt.get} lines of words.")
    BuildType(map, knownAppender.normed(), unknownAppenderOpt.map(_.normed()), columns)
  }
}

object CompactWordEmbeddingMapApp extends App {
  val filename = "../glove.840B.300d.10f.txt"
  val buildType = Timers.getOrNew("load text").time {
    CompactWordEmbeddingMap.loadTxt(filename: String, resource = false)
  }
  val compactWordEmbeddingMap = new CompactWordEmbeddingMap(buildType)

  Timers.getOrNew("save ser").time {
    compactWordEmbeddingMap.save("../glove.840B.300d.10f.ser")
  }
  Timers.getOrNew("save kryo").time {
    compactWordEmbeddingMap.saveKryo("../glove.840B.300d.10f.kryo")
  }

  Timers.getOrNew("load ser").time {
    CompactWordEmbeddingMap.loadSer("../glove.840B.300d.10f.ser")
  }
  Timers.getOrNew("load kryo").time {
    CompactWordEmbeddingMap.loadKryo("../glove.840B.300d.10f.kryo")
  }

  Timers.summarize()
}
