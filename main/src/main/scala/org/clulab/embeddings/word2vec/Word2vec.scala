package org.clulab.embeddings.word2vec

import java.io._
import java.nio.{ ByteBuffer, ByteOrder }

import org.slf4j.LoggerFactory
import org.clulab.utils.MathUtils
import org.apache.commons.io.{ IOUtils, FileUtils }

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Implements similarity metrics using the word2vec matrix
 * IMPORTANT: In our implementation, words are lower cased but NOT lemmatized or stemmed (see sanitizeWord)
 * User: mihais, dfried, gus
 * Date: 11/25/13
 * Last Modified: Fix compiler issue: import scala.io.Source.
 */

// matrixConstructor is lazy, meant to save memory space if we're caching features
class Word2Vec(matrixConstructor: => Map[String, Array[Double]]) {

  lazy val dimensions = matrix.values.head.length

  /** alternate constructor to allow loading from a file, possibly with a set of words to constrain the vocab */
  def this(mf: String, wordsToUse: Option[Set[String]] = None) = {
    this(Word2Vec.loadMatrix(mf, wordsToUse)._1)
  }

  /** alternate constructor to allow loading from a source, possibly with a set of words to constrain the vocab */
  def this(src: Source, wordsToUse: Option[Set[String]]) = {
    this(Word2Vec.loadMatrixFromSource(src, wordsToUse)._1)
  }

  /** alternate constructor to allow loading from a stream, possibly with a set of words to constrain the vocab */
  def this(is: InputStream, wordsToUse: Option[Set[String]]) = {
    this(Word2Vec.loadMatrixFromStream(is, wordsToUse)._1)
  }

  // laziness here causes problems with InputStream-based alternate constructor
  val matrix : Map[String, Array[Double]] = matrixConstructor

  def saveMatrix(mf: String) {
    val pw = new PrintWriter(mf)
    pw.println(s"${matrix.size}, $dimensions")
    for ((word, vec) <- matrix) {
      val strRep = vec.map(_.formatted("%.6f")).mkString(" ")
      pw.println(s"$word $strRep")
    }
    pw.close()
  }

  /**
   * Computes the similarity between two given words
   * IMPORTANT: words here must already be normalized using Word2vec.sanitizeWord()!
   * @param w1 The first word
   * @param w2 The second word
   * @return The cosine similarity of the two corresponding vectors
   */
  def similarity(w1:String, w2:String):Double = {
    val v1o = matrix.get(w1)
    if(v1o.isEmpty) return -1
    val v2o = matrix.get(w2)
    if(v2o.isEmpty) return -1
    Word2Vec.dotProduct(v1o.get, v2o.get)
  }

  /** Adds the content of src to dest, in place */
  private def add(dest:Array[Double], src:Array[Double]) {
    var i = 0
    while(i < dimensions) {
      dest(i) += src(i)
      i += 1
    }
  }

  /** filterPredicate: if passed, only returns words that match the predicate */
  def mostSimilarWords(v: Array[Double], howMany:Int, filterPredicate: Option[String => Boolean]):List[(String,  Double)] = {
    val words = filterPredicate match {
      case None => matrix.keys
      case Some(p) => matrix.keys.filter(p)
    }
    MathUtils.nBest[String](word => Word2Vec.dotProduct(v, matrix(word)))(words, howMany)
  }

  /**
   * Finds the words most similar to this set of inputs
   * IMPORTANT: words here must already be normalized using Word2vec.sanitizeWord()!
   */
  def mostSimilarWords(words:Set[String], howMany:Int):List[(String, Double)] = {
    val v = new Array[Double](dimensions)
    var found = false
    for(w1 <- words) {
      val w = Word2Vec.sanitizeWord(w1)         // sanitize words
      val vo = matrix.get(w)
      if(vo.isDefined) {
        found = true
        add(v, vo.get)
      }
    }
    if(! found) return List()
    Word2Vec.norm(v)
    mostSimilarWords(v, howMany, None)
  }

  def mostSimilarWords(word: String, howMany: Int, filterPredicate: Option[String => Boolean] = None): List[(String,
    Double)] = matrix.get(word) match {
    case Some(v) => mostSimilarWords(v, howMany, filterPredicate)
    case None => List()
  }

  def makeCompositeVector(t:Iterable[String]):Array[Double] = {
    val vTotal = new Array[Double](dimensions)
    for(s <- t) {
      val v = matrix.get(s)
      if(v.isDefined) add(vTotal, v.get)
    }
    Word2Vec.norm(vTotal)
    vTotal
  }

  /**
    * Fetches the embeddings vector for a given word (not lemma)
    * @param word The word
    * @return the array of embeddings weights
    */
  def getWordVector(word:String):Option[Array[Double]] = {
    val sw = Word2Vec.sanitizeWord(word)
    matrix.get(sw)
  }

  /**
   * Computes the cosine similarity between two texts, according to the word2vec matrix
   * IMPORTANT: t1, t2 must be arrays of words, not lemmas!
   */
  def textSimilarity(t1:Iterable[String], t2:Iterable[String]):Double = {
    val st1 = new ArrayBuffer[String]()
    t1.foreach(st1 += Word2Vec.sanitizeWord(_))
    val st2 = new ArrayBuffer[String]()
    t2.foreach(st2 += Word2Vec.sanitizeWord(_))
    sanitizedTextSimilarity(st1, st2)
  }

  /**
   * Computes the cosine similarity between two texts, according to the word2vec matrix
   * IMPORTANT: words here must already be normalized using Word2vec.sanitizeWord()!
   */
  def sanitizedTextSimilarity(t1:Iterable[String], t2:Iterable[String]):Double = {
    val v1 = makeCompositeVector(t1)
    val v2 = makeCompositeVector(t2)
    Word2Vec.dotProduct(v1, v2)
  }

  /**
   * Similar to textSimilarity, but using the multiplicative heuristic of Levy and Goldberg (2014)
   * IMPORTANT: t1, t2 must be arrays of words, not lemmas!
   */
  def multiplicativeTextSimilarity(t1:Iterable[String], t2:Iterable[String]):Double = {
    val st1 = new ArrayBuffer[String]()
    t1.foreach(st1 += Word2Vec.sanitizeWord(_))
    val st2 = new ArrayBuffer[String]()
    t2.foreach(st2 += Word2Vec.sanitizeWord(_))
    multiplicativeSanitizedTextSimilarity(st1, st2)
  }

  /**
   * Similar to sanitizedTextSimilarity, but but using the multiplicative heuristic of Levy and Goldberg (2014)
   * IMPORTANT: words here must already be normalized using Word2vec.sanitizeWord()!
   * @return Similarity value
   */
  def multiplicativeSanitizedTextSimilarity(t1:Iterable[String], t2:Iterable[String]):Double = {
    var sim = 1.0
    for(w1 <- t1) {
      for(w2 <- t2) {
        // no need to add the log sim if identical (log(1) == 0)
        if(w1 != w2) {
          val v1 = matrix.get(w1)
          val v2 = matrix.get(w2)
          if(v1.isDefined && v2.isDefined) {
            // *multiply* rather than add similarities!
            sim *= Word2Vec.dotProduct(v1.get, v2.get)
          }
        }
      }
    }
    sim
  }

  def logMultiplicativeTextSimilarity(t1: Iterable[String],
                                      t2: Iterable[String],
                                      method: Symbol = 'linear,
                                      normalize: Boolean = false): Double = {
    val st1 = t1.map(Word2Vec.sanitizeWord(_))
    val st2 = t2.map(Word2Vec.sanitizeWord(_))
    logMultiplicativeSanitizedTextSimilarity(st1, st2, method, normalize)
  }

  def logMultiplicativeSanitizedTextSimilarity(t1:Iterable[String],
                                               t2:Iterable[String],
                                               method: Symbol = 'linear,
                                               normalize: Boolean = false):Double = {
    val t1Vecs = t1.flatMap(matrix.get) // this will drop any words that don't have vectors
    val t2Vecs = t2.flatMap(matrix.get)
    val sims = for {
      v1 <- t1Vecs
      v2 <- t2Vecs
      cosSim = Word2Vec.dotProduct(v1, v2)
      toYield = method match {
        case 'linear => math.log(cosSim + 1)
        case 'linear_scaled => math.log((cosSim + 1) / 2)
        case 'angular => math.log(1 - (math.acos(math.min(1, math.max(-1, cosSim))) / math.Pi))
        case _ => throw new Exception(s"invalid method $method")
      }
    } yield toYield
    val sum = sims.sum
    if (normalize && t2Vecs.nonEmpty)
      sum / t2Vecs.size
    else
      sum
  }

  /**
   * Finds the maximum word2vec similarity between any two words in these two texts
   * IMPORTANT: IMPORTANT: t1, t2 must be arrays of words, not lemmas!
   */
  def maxSimilarity(t1:Iterable[String], t2:Iterable[String]):Double = {
    val st1 = new ArrayBuffer[String]()
    t1.foreach(st1 += Word2Vec.sanitizeWord(_))
    val st2 = new ArrayBuffer[String]()
    t2.foreach(st2 += Word2Vec.sanitizeWord(_))
    sanitizedMaxSimilarity(st1, st2)
  }

  def minSimilarity(t1: Iterable[String], t2: Iterable[String]): Double = {
    val st1 = t1.map(Word2Vec.sanitizeWord(_))
    val st2 = t2.map(Word2Vec.sanitizeWord(_))
    sanitizedMinSimilarity(st1, st2)
  }

  /**
   * Finds the maximum word2vec similarity between any two words in these two texts
   * IMPORTANT: words here must already be normalized using Word2vec.sanitizeWord()!
   */
  def sanitizedMaxSimilarity(t1:Iterable[String], t2:Iterable[String]):Double = {
    var max = Double.MinValue
    for(s1 <- t1) {
      val v1 = matrix.get(s1)
      if(v1.isDefined) {
        for(s2 <- t2) {
          val v2 = matrix.get(s2)
          if(v2.isDefined) {
            val s = Word2Vec.dotProduct(v1.get, v2.get)
            if(s > max) max = s
          }
        }
      }
    }
    max
  }

  /**
   * Finds the minimum word2vec similarity between any two words in these two texts
   * IMPORTANT: words here must already be normalized using Word2vec.sanitizeWord()!
   */
  def sanitizedMinSimilarity(t1:Iterable[String], t2:Iterable[String]):Double = {
    var min = Double.MaxValue
    for(s1 <- t1) {
      val v1 = matrix.get(s1)
      if(v1.isDefined) {
        for(s2 <- t2) {
          val v2 = matrix.get(s2)
          if(v2.isDefined) {
            val s = Word2Vec.dotProduct(v1.get, v2.get)
            if(s < min) min = s
          }
        }
      }
    }
    min
  }

  /**
   * Finds the average word2vec similarity between any two words in these two texts
   * IMPORTANT: words here must be words not lemmas!
   */
  def avgSimilarity(t1:Iterable[String], t2:Iterable[String]):Double = {
    val st1 = new ArrayBuffer[String]()
    t1.foreach(st1 += Word2Vec.sanitizeWord(_))
    val st2 = new ArrayBuffer[String]()
    t2.foreach(st2 += Word2Vec.sanitizeWord(_))
    val (score, pairs) = sanitizedAvgSimilarity(st1, st2)

    score
  }

  def avgSimilarityReturnTop(t1:Iterable[String], t2:Iterable[String]):(Double, Array[(Double, String, String)]) = {
    val st1 = new ArrayBuffer[String]()
    t1.foreach(st1 += Word2Vec.sanitizeWord(_))
    val st2 = new ArrayBuffer[String]()
    t2.foreach(st2 += Word2Vec.sanitizeWord(_))
    val (score, pairs) = sanitizedAvgSimilarity(st1, st2)

    val sorted = pairs.sortBy(- _._1).toArray
    //if (sorted.size > 10) return (score, sorted.slice(0, 10))     // Commented out -- return all pairs for UASupport structure (it can filter them if it wants)
    (score, sorted)
  }

  /**
   * Finds the average word2vec similarity between any two words in these two texts
   * IMPORTANT: words here must already be normalized using Word2vec.sanitizeWord()!
   * Changelog: (Peter/June 4/2014) Now returns words list of pairwise scores, for optional answer justification.
   */
  def sanitizedAvgSimilarity(t1:Iterable[String], t2:Iterable[String]):(Double, ArrayBuffer[(Double, String, String)]) = {
    // Top words
    val pairs = new ArrayBuffer[(Double, String, String)]

    var avg = 0.0
    var count = 0
    for(s1 <- t1) {
      val v1 = matrix.get(s1)
      if(v1.isDefined) {
        for(s2 <- t2) {
          val v2 = matrix.get(s2)
          if(v2.isDefined) {
            val s = Word2Vec.dotProduct(v1.get, v2.get)
            avg += s
            count += 1

            // Top Words
            pairs.append ( (s, s1, s2) )
          }
        }
      }
    }
    if(count != 0) (avg / count, pairs)
    else (0, pairs)
  }

  /**
   * for a sequence of (word, weight) pairs, interpolate the vectors corresponding to the words by their respective
   * weights, and normalize the resulting vector
   */
  def interpolate(wordsAndWeights: Iterable[(String, Double)]) = {
    // create a vector to store the weighted sum
    val v = new Array[Double](dimensions)
    for ((word, p) <- wordsAndWeights) {
      // get this word's vector, scaled by the weight
      val scaled = for {
        x <- matrix(word)
      } yield x * p
      // add it in place to the sum vector
      add(v, scaled)
    }
    Word2Vec.norm(v)
    v
  }
}

object Word2Vec {
  val logger = LoggerFactory.getLogger(classOf[Word2Vec])

  /**
   * Normalizes words for word2vec
   * @param uw A word (NOT lemma)
   * @return The normalized form of the word
   */
  def sanitizeWord(uw:String, keepNumbers:Boolean = true):String = {
    val w = uw.toLowerCase()

    // skip parens from corenlp
    if(w == "-lrb-" || w == "-rrb-" || w == "-lsb-" || w == "-rsb-") {
      return ""
    }

    // skip URLS
    if(w.startsWith("http") || w.contains(".com") || w.contains(".org")) //added .com and .org to cover more urls (becky)
      return ""

    // normalize numbers to a unique token
    if(isNumber(w)) {
      if(keepNumbers) return "xnumx"
      else return ""
    }

    // remove all non-letters; convert letters to lowercase
    val os = new collection.mutable.StringBuilder()
    var i = 0
    while(i < w.length) {
      val c = w.charAt(i)
      // added underscore since it is our delimiter for dependency stuff...
      if(Character.isLetter(c) || c == '_') os += c
      i += 1
    }
    os.toString()
  }

  def isNumber(w:String):Boolean = {
    var i = 0
    var foundDigit = false
    while(i < w.length) {
      val c = w.charAt(i)
      if(! Character.isDigit(c) &&
        c != '-' && c != '+' &&
        c != ',' && c != '.' &&
        c != '/' && c != '\\')
        return false
      if(Character.isDigit(c))
        foundDigit = true
      i += 1
    }
    foundDigit
  }

  /** Normalizes this vector to length 1, in place */
  def norm(weights:Array[Double]) {
    var i = 0
    var len = 0.0
    while (i < weights.length) {
      len += weights(i) * weights(i)
      i += 1
    }
    len = math.sqrt(len)
    i = 0
    if (len != 0) {
      while (i < weights.length) {
        weights(i) /= len
        i += 1
      }
    }
  }

  def dotProduct(v1:Array[Double], v2:Array[Double]):Double = {
    assert(v1.length == v2.length) //should we always assume that v2 is longer? perhaps set shorter to length of longer...
    var sum = 0.0
    var i = 0
    while(i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }

  private def loadMatrix(mf: String, wordsToUse: Option[Set[String]]):(Map[String, Array[Double]], Int) = {
    logger.debug("Started to load word2vec matrix from file " + mf + "...")
    val src: Source = Source.fromFile(mf, "iso-8859-1")
    val lines: Iterator[String] = src.getLines()
    val matrix = buildMatrix(lines, wordsToUse)
    src.close()
    logger.debug("Completed matrix loading.")
    matrix
  }

  private def loadMatrixFromStream(is: InputStream, wordsToUse: Option[Set[String]]):(Map[String, Array[Double]], Int) = {
    logger.debug("Started to load word2vec matrix from stream ...")
    val src: Source = Source.fromInputStream(is, "iso-8859-1")
    val lines: Iterator[String] = src.getLines
    val matrix = buildMatrix(lines, wordsToUse)
    src.close()
    logger.debug("Completed matrix loading.")
    matrix
  }
  private def loadMatrixFromSource(src: Source, wordsToUse: Option[Set[String]]):(Map[String, Array[Double]], Int) = {
    logger.debug("Started to load word2vec matrix from source ...")
    val lines: Iterator[String] = src.getLines()
    val matrix = buildMatrix(lines, wordsToUse)
    logger.debug("Completed matrix loading.")
    matrix
  }

  private def buildMatrix(lines: Iterator[String], wordsToUse: Option[Set[String]]): (Map[String, Array[Double]], Int) = {
    val m = new collection.mutable.HashMap[String, Array[Double]]()
    var first = true
    var dims = 0

    for((line, index) <- lines.zipWithIndex) {
      val bits = line.split("\\s+")
      if(first) {
        dims = bits(1).toInt
        first = false
      } else {
        if (bits.length != dims + 1) {
          println(s"${bits.length} != ${dims + 1} found on line ${index + 1}")
        }
        assert(bits.length == dims + 1)
        val w = bits(0)
        if (wordsToUse.isEmpty || wordsToUse.get.contains(w)) {
          val weights = new Array[Double](dims)
          var i = 0
          while(i < dims) {
            weights(i) = bits(i + 1).toDouble
            i += 1
          }
          norm(weights)
          m.put(w, weights)
        }
      }
    }
    logger.debug("Completed matrix loading.")
    (m.toMap, dims)
  }

  def fromBinary(filename: String): Word2Vec = fromBinary(new File(filename))

  def fromBinary(file: File): Word2Vec = {
    new Word2Vec(readBinaryMatrix(FileUtils.readFileToByteArray(file)))
  }

  def fromBinary(inputStream: InputStream): Word2Vec = {
    new Word2Vec(readBinaryMatrix(IOUtils.toByteArray(inputStream)))
  }

  def fromBinary(bytes: Array[Byte]): Word2Vec = {
    new Word2Vec(readBinaryMatrix(bytes))
  }

  // reads non-space chars
  private def readNonSpace(bb: ByteBuffer): String = {
    val buffer = new ArrayBuffer[Byte]
    var byte = bb.get()
    while (byte != ' '.toByte && byte != '\n'.toByte) {
      buffer += byte
      byte = bb.get()
    }
    new String(buffer.toArray)
  }

  private def readBinaryMatrix(bytes: Array[Byte]): Map[String, Array[Double]] = {
    val m = new collection.mutable.HashMap[String, Array[Double]]
    val bb = ByteBuffer.wrap(bytes)
    bb.order(ByteOrder.nativeOrder())
    // read number of words
    val words = readNonSpace(bb).toLong
    // read number of dimensions
    val size = readNonSpace(bb).toLong
    // consume spaces
    var byte = bb.get()
    while (byte == ' '.toByte || byte == '\n'.toByte) {
      byte = bb.get()
    }
    // rewind one byte
    bb.position(bb.position() - 1)
    // start reading words
    var w = 0L
    while (w < words) {
      w += 1
      // read word
      val word = readNonSpace(bb)
      // populate embedding
      val embedding = new Array[Double](size.toInt)
      var s = 0
      while (s < size) {
        embedding(s) = bb.getFloat()
        s += 1
      }
      // normalize
      norm(embedding)
      // add word to map
      m.put(word, embedding)
      // skip spaces if needed
      if (bb.hasRemaining) {
        // consume spaces
        byte = bb.get()
        while (byte == ' '.toByte || byte == '\n'.toByte) {
          byte = bb.get()
        }
        // rewind 1 byte
        bb.position(bb.position() - 1)
      }
    }
    m.toMap
  }

  def main(args:Array[String]) {
    val w2v = new Word2Vec(args(0), None)

    println("Words most similar to \"house\":")
    for(t <- w2v.mostSimilarWords(Set("house"), 40)) {
      println(t._1 + " " + t._2)
    }

    val t1 = List("a", "delicious", "apple")
    val t2 = List("the", "tasty", "pear")
    val t3 = List("computer", "oxygen")
    println("Text similarity: " + w2v.sanitizedTextSimilarity(t1, t2))
    println("Text similarity: " + w2v.sanitizedTextSimilarity(t1, t3))
    println("Max similarity: " + w2v.sanitizedMaxSimilarity(t1, t2))
    println("Avg similarity: " + w2v.sanitizedAvgSimilarity(t1, t2))
  }
}
