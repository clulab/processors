package org.clulab.dynet

import java.io._
import edu.cmu.dynet.Expression.{concatenate, input, logSumExp, lookup, pick, pickNegLogSoftmax, sum}
import edu.cmu.dynet._
import edu.cmu.dynet.ComputationGraph
import org.clulab.embeddings.SanitizedWordEmbeddingMap
import org.clulab.fatdynet.utils.BaseTextLoader
import org.clulab.fatdynet.utils.Initializer
import org.clulab.struct.{Counter, MutableNumber}
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
 * Utility methods used by DyNet applications
 */
object Utils {
  private val logger: Logger = LoggerFactory.getLogger(classOf[Utils])
  var concatenateCount = 0

  val UNK_WORD = "<UNK>"
  val EOS_WORD = "<EOS>"

  val UNK_EMBEDDING = 0

  val START_TAG = "<START>"
  val STOP_TAG = "<STOP>"

  val RANDOM_SEED = 2522620396L // used for both DyNet, and the JVM seed for shuffling data
  val WEIGHT_DECAY = 1e-5f

  val LOG_MIN_VALUE: Float = -10000

  val DEFAULT_DROPOUT_PROBABILITY = 0f // no dropout by  default

  private var IS_DYNET_INITIALIZED = false

  def initializeDyNet(autoBatch: Boolean = false, mem: String = ""): Unit = {
    // At the very least, IS_DYNET_INITIALIZED must be protected.
    Utils.synchronized {
      if (!IS_DYNET_INITIALIZED) {
        logger.debug("Initializing DyNet...")

        val params = new mutable.HashMap[String, Any]()
        params += "random-seed" -> RANDOM_SEED
        params += "weight-decay" -> WEIGHT_DECAY
        if (autoBatch) {
          params += "autobatch" -> 1
          params += "dynet-mem" -> mem
        }

        Initializer.initialize(params.toMap)
        logger.debug("DyNet initialization complete.")
        IS_DYNET_INITIALIZED = true
      }
      else
        logger.debug("DyNet re-initialization skipped.")
    }
  }

  def fromIndexToString(s2i: Map[String, Int]): Array[String] = {
    var max = Int.MinValue
    for (v <- s2i.values) {
      if (v > max) {
        max = v
      }
    }
    assert(max > 0)
    val i2s = new Array[String](max + 1)
    for (k <- s2i.keySet) {
      i2s(s2i(k)) = k
    }
    i2s
  }

  def fromIndexToChar(s2i: Map[Char, Int]): Array[Char] = {
    var max = Int.MinValue
    for (v <- s2i.values) {
      if (v > max) {
        max = v
      }
    }
    assert(max > 0)
    val i2s = new Array[Char](max + 1)
    for (k <- s2i.keySet) {
      i2s(s2i(k)) = k
    }
    i2s
  }

  /**
   * Initializes the transition matrix for a tagset of size size
   * T[i, j] stores a transition *to* i *from* j
   */
  def mkTransitionMatrix(parameters: ParameterCollection, t2i: Map[String, Int]): LookupParameter = {
    val size = t2i.size
    val rows = parameters.addLookupParameters(size, Dim(size))
    rows
  }

  def viterbi( emissionScores: Array[Array[Float]],
               transitionMatrix: Array[Array[Float]],
               tagCount: Int,
               startTagIdx: Int,
               stopTagIdx: Int): Array[Int] = {

    // initial scores in log space
    val initScores = new Array[Float](tagCount)
    for (i <- initScores.indices) initScores(i) = LOG_MIN_VALUE
    initScores(startTagIdx) = 0

    // the best overall scores at time step -1 (start)
    var forwardVar = initScores

    // backpointers for the entire lattice
    val backPointers = new ArrayBuffer[Array[Int]]()

    // iterate over all the words in this sentence
    for (t <- emissionScores.indices) {
      // scores for *all* tags for time step t
      val scoresAtT = new Array[Float](emissionScores(t).length)

      // backpointers for this time step
      val backPointersAtT = new Array[Int](emissionScores(t).length)

      // iterate over all possible tags for this time step
      for (nextTag <- emissionScores(t).indices) {

        // compute the score of transitioning into this tag from *any* previous tag
        val transitionIntoNextTag = ArrayMath.sum(forwardVar, transitionMatrix(nextTag))

        //printTagScores(s"\tforwardVar + transitionMatrix:", transitionIntoNextTag)

        // this previous tag has the best transition score into nextTag
        val bestPrevTag = ArrayMath.argmax(transitionIntoNextTag)
        // keep track of the best backpointer for nextTag
        backPointersAtT(nextTag) = bestPrevTag

        // this is the best *transition* score into nextTag
        scoresAtT(nextTag) = transitionIntoNextTag(bestPrevTag)
      }

      // these are the best overall scores at time step t = transition + emission + previous
      // note that the emission scores are the same for a given nextTag, so it's Ok to do this outside of the above loop
      forwardVar = ArrayMath.sum(scoresAtT, emissionScores(t))

      // keep track of the backpointers at this time step
      backPointers += backPointersAtT
    }

    assert(emissionScores.length == backPointers.length)

    // transition into the stop tag
    forwardVar = ArrayMath.sum(forwardVar, transitionMatrix(stopTagIdx))
    var bestLastTag = ArrayMath.argmax(forwardVar)
    val pathScore = forwardVar(bestLastTag)

    // best path in the lattice, in reverse order
    val bestPathReversed = new ListBuffer[Int]
    bestPathReversed += bestLastTag
    for (backPointersAtT <- backPointers.reverse) {
      bestLastTag = backPointersAtT(bestLastTag)
      bestPathReversed += bestLastTag
    }
    assert(bestPathReversed.last == startTagIdx)
    val bestPath = bestPathReversed.slice(0, bestPathReversed.size - 1).reverse.toArray

    bestPath
  }

  /**
   * Implements the forward algorithm to compute the partition score for this lattice
   * This code inspired by this PyTorch implementation: https://pytorch.org/tutorials/beginner/nlp/advanced_tutorial.html
   */
  def mkPartitionScore(emissionScoresForSeq: ExpressionVector, // Dim: sentenceSize x tagCount
                       transitionMatrix: ExpressionVector,
                       startTag: Int, stopTag: Int): Expression = { // Dim: tagCount x tagCount
    val tagCount = transitionMatrix.size

    // sum of scores of reaching each tag at this time step
    var forward = new ExpressionVector()
    for (t <- 0 until tagCount) {
      //
      // cost (in log space) of starting at a given tag
      // the only possible starting tag is START; all others are disabled
      //
      val alphaAtT0: Float = if (t == startTag) 0 else LOG_MIN_VALUE
      forward.add(input(alphaAtT0))
    }

    for (t <- emissionScoresForSeq.indices) {
      val alphasAtT = new ExpressionVector()
      val emitScores = emissionScoresForSeq(t)

      for (nextTag <- 0 until tagCount) {
        val alphasForTag = new ExpressionVector()
        val emitScore = pick(emitScores, nextTag) // scalar: emision score for nextTag

        for (srcTag <- 0 until tagCount) {
          val transScore = pick2D(transitionMatrix, nextTag, srcTag) // scalar: transition score to nextTag from srcTag
          val alphaToTagFromSrc =
            forward(srcTag) +
              transScore +
              emitScore

          alphasForTag.add(alphaToTagFromSrc)
        }

        alphasAtT.add(logSumExp(alphasForTag))
      }

      forward = alphasAtT
    }

    val terminalVars = new ExpressionVector()
    for (t <- 0 until tagCount) {
      terminalVars.add(forward(t) + pick2D(transitionMatrix, stopTag, t))
    }

    val total = logSumExp(terminalVars)
    total
  }

  def toIds[T](tags: IndexedSeq[T], t2i: Map[T, Int]): IndexedSeq[Int] = {
    val ids = new ArrayBuffer[Int]()
    for (tag <- tags) {
      ids += t2i(tag)
    }
    ids
  }

  def printCoNLLOutput(pw: PrintWriter,
                       words: IndexedSeq[String],
                       golds: IndexedSeq[String],
                       preds: IndexedSeq[String]): Unit = {

    assert(words.length == golds.length)
    assert(words.length == preds.length)

    for (i <- words.indices) {
      pw.println(words(i) + " " + golds(i) + " " + preds(i))
    }
    pw.println()
  }

  /** Runs a greedy algorithm to generate the sequence of tag ids, ignoring transition scores */
  def greedyPredict(lattice: Array[Array[Float]]): Array[Int] = {
    val tagIds = new ArrayBuffer[Int]()
    for (probs <- lattice) {
      var max = Float.MinValue
      var tid = -1
      for (i <- probs.indices) {
        if (probs(i) > max) {
          max = probs(i)
          tid = i
        }
      }
      assert(tid > -1)
      tagIds += tid
    }
    tagIds.toArray
  }

  def srlPredict(lattice: Array[Array[Float]], predPosition: Int, oId: Int): Array[Int] = {
    val tagIds = new Array[Int](lattice.length)
    for (i <- tagIds.indices) {
      tagIds(i) = oId
    }

    val tags = new ArrayBuffer[(Int, Int, Float)]()
    for (i <- lattice.indices) {
      for (j <- lattice(i).indices) {
        tags += Tuple3(i, j, lattice(i)(j))
      }
    }
    val sortedTags = tags.sortBy(0f - _._3)
    val usedPositions = new mutable.HashSet[Int]()
    usedPositions += predPosition // do not allow self loops
    tagIds(predPosition) = oId
    val usedArgs = new mutable.HashSet[Int]()
    for (t3 <- sortedTags) {
      val position = t3._1
      val tagId = t3._2
      if (!usedPositions.contains(position) && (tagId == oId || !usedArgs.contains(tagId))) {
        tagIds(position) = tagId
        usedPositions += position
        if (tagId != oId) usedArgs += tagId
      }
    }

    /*
    val windowSize = 30
    for(i <- math.max(0, predPosition - windowSize) until math.min(tagIds.length, predPosition + windowSize)) {
      val probs = lattice(i)
      var max = Float.MinValue
      var tid = -1
      for(j <- probs.indices) {
        if(probs(j) > max) {
          max = probs(j)
          tid = j
        }
      }
      assert(tid > -1)
      tagIds(i) = tid
    }
    */

    tagIds
  }

  def printTagScores(header: String, scores: Array[Float], i2t: Array[String]): Unit = {
    print(header)
    for (j <- scores.indices) {
      val tag = i2t(j)
      print(s" {$tag, ${scores(j)}}")
    }
    println()
  }

  /** Picks the scalar element from an expression that is a matrix */
  def pick2D(matrix: ExpressionVector, row: Int, column: Int): Expression = {
    pick(matrix(row), column)
  }

  /** Computes the score of the given sequence of tags (tagSeq) */
  def sentenceScore(emissionScoresForSeq: ExpressionVector, // Dim: sentenceSize x tagCount
                    transitionMatrix: ExpressionVector, // Dim: tagCount x tagCount
                    tagCount: Int,
                    tagSeq: IndexedSeq[Int],
                    startTag: Int,
                    stopTag: Int): Expression = {
    // start with the transition score to first tag from START
    var score = pick2D(transitionMatrix, tagSeq.head, startTag)

    for (i <- tagSeq.indices) {
      if (i > 0) {
        // transition score from the previous tag
        score = score + pick2D(transitionMatrix, tagSeq(i), tagSeq(i - 1))
      }

      // emission score for the current tag
      score = score + pick(emissionScoresForSeq(i), tagSeq(i))
    }

    // conclude with the transition score to STOP from last tag
    score = score + pick2D(transitionMatrix, stopTag, tagSeq.last)

    score
  }

  def concatenateStates(l1: Iterable[Expression], l2: Iterable[Expression]): Iterable[Expression] = {
    l1.zip(l2).map { case (left, right) => concatenate(left, right) }
  }

  def mkCharacterEmbedding(word: String,
      c2i: Map[Char, Int],
      charLookupParameters: LookupParameter,
      charFwRnnBuilder: RnnBuilder,
      charBwRnnBuilder: RnnBuilder): Expression = {

    // Do not use an ExpressionVector, but instead just take the last element.
    def safelyTransduceLast1(charEmbeddings: Seq[Expression], rnnBuilder: RnnBuilder): Expression = {
      val outsOpt = transduceLastOpt(charEmbeddings, rnnBuilder)
      val nonEmptyOuts = outsOpt.getOrElse {
        // Some embeddings may be empty in some weird Unicode encodings.
        logger.warn(s"A strange character was encountered in word '$word'.")
        val safeCharEmbeddings = Array(lookup(charLookupParameters, UNK_EMBEDDING))
        // This one shouldn't be empty, or could it be?
        transduceLastOpt(safeCharEmbeddings, rnnBuilder).get
      }

      nonEmptyOuts
    }

    // Transduce the entire sequence, resulting in an ExpressionVector, then take the last element.
    def safelyTransduceLast2(charEmbeddings: Seq[Expression], rnnBuilder: RnnBuilder): Expression = {
      val outs = transduce(charEmbeddings, rnnBuilder)
      val nonEmptyOuts = if (outs.length != 0) outs else {
        // Some embeddings may be empty in some weird Unicode encodings.
        logger.warn(s"A strange character was encountered in word '$word'.")
        val safeCharEmbeddings = Array(lookup(charLookupParameters, UNK_EMBEDDING))
        // This one shouldn't be empty, or could it be?
        transduce(safeCharEmbeddings, rnnBuilder)
      }

      nonEmptyOuts(nonEmptyOuts.length - 1)
    }

    val charEmbeddings = word.map { c: Char =>
      lookup(charLookupParameters, c2i.getOrElse(c, UNK_EMBEDDING))
    }
    val fwOutsLast = safelyTransduceLast1(charEmbeddings, charFwRnnBuilder)
    val bwOutsLast = safelyTransduceLast1(charEmbeddings.reverse, charBwRnnBuilder)
  // val fwOutsLast = safelyTransduceLast2(charEmbeddings, charFwRnnBuilder)
  // val bwOutsLast = safelyTransduceLast2(charEmbeddings.reverse, charBwRnnBuilder)
    val result = concatenate(fwOutsLast, bwOutsLast)

    result
  }

  def transduceLastOpt(embeddings: Iterable[Expression], builder: RnnBuilder): Option[Expression] = {

    if (embeddings.isEmpty)
      None
    else {
      builder.newGraph()
      builder.startNewSequence()
      embeddings.dropRight(1).foreach { embedding => builder.addInput(embedding) }
      Some(builder.addInput(embeddings.last))
    }
  }

  def transduce(embeddings: Iterable[Expression], builder: RnnBuilder): ExpressionVector = {

    // Build up an ExpressionVector one Expression at a time.
    def transduceEV(embeddings: Iterable[Expression], builder: RnnBuilder): ExpressionVector = {
      builder.newGraph()
      builder.startNewSequence()
      val ev = new ExpressionVector()
      for (e <- embeddings) {
        ev.add(builder.addInput(e))
      }
      ev
    }

    // Iterate through the expressions and construct the ExpressionVector at the end.
    def transduceItr(embeddings: Iterable[Expression], builder: RnnBuilder): ExpressionVector = {
      builder.newGraph()
      builder.startNewSequence()
      val expressions = embeddings.map(builder.addInput).toSeq

      expressions
    }

    // transduceEV(embeddings, builder)
    transduceItr(embeddings, builder)
  }

  /** Greedy loss function, ignoring transition scores */
  def sentenceLossGreedy(emissionScoresForSeq: ExpressionVector, // Dim: sentenceSize x tagCount
                         golds: IndexedSeq[Int]): Expression = { // Dim: sentenceSize

    val goldLosses = new ExpressionVector()
    assert(emissionScoresForSeq.length == golds.length)

    for (i <- emissionScoresForSeq.indices) {
      // gold tag for word at position i
      val goldTid = golds(i)
      // emissionScoresForSeq(i) = all tag emission scores for the word at position i
      goldLosses.add(pickNegLogSoftmax(emissionScoresForSeq(i), goldTid))
    }

    sum(goldLosses)
  }

  /**
   * Objective function that maximizes the CRF probability of the gold sequence of tags for a given sentence
   *
   * @param emissionScoresForSeq emission scores for the whole sequence, and all tags
   * @param transitionMatrix     transition matrix between all tags
   * @param golds                gold sequence of tags
   * @return the negative prob of the gold sequence (in log space)
   */
  def sentenceLossCrf(emissionScoresForSeq: ExpressionVector, // Dim: sentenceSize x tagCount
                      transitionMatrix: ExpressionVector, // Dim: tagCount x tagCount
                      golds: IndexedSeq[Int],
                      t2i: Map[String, Int]): Expression = { // Dim: sentenceSize
    val startTag = t2i(START_TAG)
    val stopTag = t2i(STOP_TAG)

    val scoreOfGoldSeq =
      sentenceScore(emissionScoresForSeq, transitionMatrix, t2i.size, golds, startTag, stopTag)

    val partitionScore =
      mkPartitionScore(emissionScoresForSeq, transitionMatrix, startTag, stopTag)

    partitionScore - scoreOfGoldSeq
  }

  def emissionScoresToArrays(expressions: Iterable[Expression]): Array[Array[Float]] = {
    val lattice = new ArrayBuffer[Array[Float]]()
    for (expression <- expressions) {
      val probs = expression.value().toVector().toArray
      lattice += probs
    }
    lattice.toArray
  }

  def emissionScoresToArraysAllTasks(expressions: Array[ExpressionVector]): Array[Array[Array[Float]]] = {
    val latticesAllTasks = new Array[Array[Array[Float]]](expressions.length)

    for (tid <- latticesAllTasks.indices) {
      val lattice = new ArrayBuffer[Array[Float]]()
      for (expression <- expressions(tid)) {
        val probs = expression.value().toVector().toArray
        lattice += probs
      }
      latticesAllTasks(tid) = lattice.toArray
    }

    latticesAllTasks
  }

  def transitionMatrixToArrays(trans: LookupParameter, size: Int): Array[Array[Float]] = {
    val transitionMatrix = new ArrayBuffer[Array[Float]]()
    for (i <- 0 until size) {
      transitionMatrix += lookup(trans, i).value().toVector().toArray
    }
    transitionMatrix.toArray
  }

  // This <% seems to be called an "implicit conversion declaration".
  // def save[T <% Ordered[T]](printWriter: PrintWriter, values: Map[T, Int], comment: String): Unit = {
  // [warn] ... view bounds are deprecated; use an implicit parameter instead.
  // [warn]   example: instead of `def f[A <% Int](a: A)` use `def f[A](a: A)(implicit ev: A => Int)`
  // [warn]   def save[T <% Ordered[T]](printWriter: PrintWriter, values: Map[T, Int], comment: String): Unit = {
  def save[T](printWriter: PrintWriter, values: Map[T, Int], comment: String)(implicit ev: T => Ordered[T]): Unit = {
    printWriter.println("# " + comment)
    // Sort these so that the same file always results, even it this is slow.
    values.toSeq.sorted.foreach { case (key, value) =>
      printWriter.println(s"$key\t$value")
    }
    printWriter.println() // Separator
  }

  def save[T](printWriter: PrintWriter, values: Counter[T], comment: String)(implicit ev: T => Ordered[T]): Unit = {
    printWriter.println("# " + comment)
    // Sort these so that the same file always results, even it this is slow.
    val keys = values.keySet.toList.sorted
    for (key <- keys) {
      val value = values.getCount(key)
      printWriter.println(s"$key\t$value")
    }
    printWriter.println() // Separator
  }

  def saveCharMap(printWriter: PrintWriter, values: Map[Char, Int], comment: String): Unit = {
    printWriter.println("# " + comment)
    // Sort these so that the same file always results, even it this is slow.
    values.toSeq.sorted.foreach { case (key, value) =>
      printWriter.println(s"${key.toInt}\t$value") // save characters as int, to store the unprintable ones correctly
    }
    printWriter.println() // Separator
  }

  def save[T](printWriter: PrintWriter, values: Array[T], comment: String): Unit = {
    printWriter.println("# " + comment)
    values.foreach(printWriter.println)
    printWriter.println() // Separator
  }

  def save(printWriter: PrintWriter, value: Long, comment: String): Unit = {
    printWriter.println("# " + comment)
    printWriter.println(value)
    printWriter.println() // Separator
  }

  def save(printWriter: PrintWriter, value: Float, comment: String): Unit = {
    printWriter.println("# " + comment)
    printWriter.println(value)
    printWriter.println() // Separator
  }

  def save(printWriter: PrintWriter, value: String, comment: String): Unit = {
    printWriter.println("# " + comment)
    printWriter.println(value)
    printWriter.println() // Separator
  }

  abstract class ByLineBuilder[IntermediateValueType, FinalValueType, DefaultValueType] {

    protected def setDefaultValue(intermediateValue: IntermediateValueType,
                                  defaultValue: DefaultValueType): Unit

    protected def getComment(line: String): String = {
      assert(line.startsWith("#"))
      line.substring(1).trim
    }

    protected def addLines(intermediateValue: IntermediateValueType,
                           lines: BufferedIterator[String],
                           fieldName: Option[String],
                           defaultValue: Option[DefaultValueType]): Unit = {

      //
      // sanity check: verify if we are reading the expected field, by checking the string in the comment
      // if we are not seeing the expected field, but we have a default value for this field, use that
      //   this is necessary to make new code backwards compatible with older models that may not have that field
      // if we are not seeing the expected field, and no default value provided, bail
      //
      if(fieldName.nonEmpty) {
        val head = if(lines.hasNext) Some(lines.head) else None

        // if the field name doesn't match, set it to the default value
        if (defaultValue.nonEmpty) {
          if(head.isEmpty) {
            logger.warn(s"Did not see the expected field [${fieldName.get}] in the model; instead I am seeing an empty line.")
            logger.warn(s"Attempting to recover by using default value of [${defaultValue.get}].")
            setDefaultValue(intermediateValue, defaultValue.get)
            return
          } else if (! head.get.startsWith("#") || getComment(head.get) != fieldName.get) {
            logger.warn(s"Did not see the expected field [${fieldName.get}] in the model; instead I am seeing this line: [${head.get}].")
            logger.warn(s"Attempting to recover by using default value of [${defaultValue.get}].")
            setDefaultValue(intermediateValue, defaultValue.get)
            return
          }
        } else {
          if(head.isEmpty) {
            throw new RuntimeException(s"ERROR: expecting field name ${fieldName.get}; instead I am seeing an empty line!")
          } else if (! head.get.startsWith("#") || getComment(head.get) != fieldName.get) {
            throw new RuntimeException(s"ERROR: expecting field name ${fieldName.get}; instead I am seeing this line: [${head.get}]!")
          }
        }
      }

      // skip exactly 1 comment line (optional)
      if (lines.head.nonEmpty && lines.head.startsWith("#")) {
        lines.next()
      }

      def nextLine(): Boolean = {
        val line = lines.next()
        //println(s"LINE: [$line]")

        if (line.nonEmpty) {
          addLine(intermediateValue, line)
          true // Continue on non-blank lines.
        }
        else {
          false // Stop at first blank line.
        }
      }

      while (nextLine()) {}
    }

    def addLine(intermediateValue: IntermediateValueType, line: String): Unit

    protected def build(lines: BufferedIterator[String],
                        fieldName: Option[String],
                        defaultValue: Option[DefaultValueType]): FinalValueType

    def build(lines: BufferedIterator[String]): FinalValueType = {
      build(lines, None, None)
    }

    def build(lines: BufferedIterator[String],
              fieldName: String): FinalValueType = {
      build(lines, Some(fieldName), None)
    }

    def build(lines: BufferedIterator[String],
              fieldName: String,
              defaultValue: DefaultValueType): FinalValueType = {
      build(lines, Some(fieldName), Some(defaultValue))
    }
  }

  // This is a little fancy because it works with both String and Char keys.
  class ByLineMapBuilder[KeyType](val converter: String => KeyType)
    extends ByLineBuilder[mutable.Map[KeyType, Int], Map[KeyType, Int], IndexedSeq[(KeyType, Int)]] {

    def addLine(mutableMap: mutable.Map[KeyType, Int], line: String): Unit = {
      //println(s"READING LINE [$line]")
      val Array(key, value) = line.split('\t')

      mutableMap += ((converter(key), value.toInt))
    }

    override protected def build(lines: BufferedIterator[String],
                                 fieldName: Option[String],
                                 defaultValue: Option[IndexedSeq[(KeyType, Int)]]): Map[KeyType, Int] = {
      val mutableMap: mutable.Map[KeyType, Int] = new mutable.HashMap

      addLines(mutableMap, lines, fieldName, defaultValue)

      mutableMap.toMap
    }

    override protected def setDefaultValue(mutableMap: mutable.Map[KeyType, Int],
                                           defaultValue: IndexedSeq[(KeyType, Int)]): Unit = {
      for(kv <- defaultValue) {
        mutableMap += kv
      }
    }
  }

  class ByLineCounterBuilder[KeyType](val converter: String => KeyType)
    extends ByLineBuilder[Counter[KeyType], Counter[KeyType], IndexedSeq[(KeyType, Double)]] {

    def addLine(counter: Counter[KeyType], line: String): Unit = {
      val Array(key, value) = line.split('\t')

      counter.setCount(converter(key), value.toDouble)
    }

    override protected def build(lines: BufferedIterator[String],
                                 fieldName: Option[String],
                                 defaultValue: Option[IndexedSeq[(KeyType, Double)]]): Counter[KeyType] = {
      val counter: Counter[KeyType] = new Counter[KeyType]()

      addLines(counter, lines, fieldName, defaultValue)
      counter
    }

    override protected def setDefaultValue(counter: Counter[KeyType],
                                           defaultValue: IndexedSeq[(KeyType, Double)]): Unit = {
      for(kv <- defaultValue) {
        counter.setCount(kv._1, kv._2)
      }
    }
  }

  protected def stringToString(string: String): String = string

  protected def stringToChar(string: String): Char = string.charAt(0)

  protected def stringToCharInt(string: String): Char = string.toInt.toChar

  class ByLineStringMapBuilder extends ByLineMapBuilder(stringToString)

  class ByLineStringCounterBuilder extends ByLineCounterBuilder(stringToString)

  class ByLineCharMapBuilder extends ByLineMapBuilder(stringToChar)

  class ByLineCharIntMapBuilder extends ByLineMapBuilder(stringToCharInt)

  class ByLineArrayBuilder extends ByLineBuilder[ArrayBuffer[String], Array[String], IndexedSeq[String]] {

    def addLine(arrayBuffer: ArrayBuffer[String], line: String): Unit = {
      arrayBuffer += line
    }

    override protected def build(lines: BufferedIterator[String],
                                 fieldName: Option[String],
                                 defaultValue: Option[IndexedSeq[String]]): Array[String] = {
      val arrayBuffer: ArrayBuffer[String] = ArrayBuffer.empty

      addLines(arrayBuffer, lines, fieldName, defaultValue)
      arrayBuffer.toArray
    }

    override protected def setDefaultValue(intermediateValue: ArrayBuffer[String],
                                           defaultValue: IndexedSeq[String]): Unit = {
      for(v <- defaultValue) {
        intermediateValue += v
      }
    }
  }

  class ByLineIntBuilder extends ByLineBuilder[MutableNumber[Option[Int]], Int, Int] {

    def addLine(mutableNumberOpt: MutableNumber[Option[Int]], line: String): Unit = {
      mutableNumberOpt.value = Some(line.toInt)
    }

    override protected def build(lines: BufferedIterator[String],
                                 fieldName: Option[String],
                                 defaultValue: Option[Int]): Int = {
      val mutableNumberOpt: MutableNumber[Option[Int]] = new MutableNumber(None)

      addLines(mutableNumberOpt, lines, fieldName, defaultValue)
      mutableNumberOpt.value.get
    }

    override protected def setDefaultValue(intermediateValue: MutableNumber[Option[Int]],
                                           defaultValue: Int): Unit = {
      intermediateValue.value = Some(defaultValue)
    }
  }

  class ByLineFloatBuilder extends ByLineBuilder[MutableNumber[Option[Float]], Float, Float] {

    def addLine(mutableNumberOpt: MutableNumber[Option[Float]], line: String): Unit = {
      mutableNumberOpt.value = Some(line.toFloat)
    }

    override protected def build(lines: BufferedIterator[String],
                                 fieldName: Option[String],
                                 defaultValue: Option[Float]): Float = {
      val mutableNumberOpt: MutableNumber[Option[Float]] = new MutableNumber(None)

      addLines(mutableNumberOpt, lines, fieldName, defaultValue)
      mutableNumberOpt.value.get
    }

    override protected def setDefaultValue(intermediateValue: MutableNumber[Option[Float]],
                                           defaultValue: Float): Unit = {
      intermediateValue.value = Some(defaultValue)
    }
  }

  class ByLineStringBuilder extends ByLineBuilder[mutable.HashSet[String], String, String] {

    def addLine(stringContainer: mutable.HashSet[String], line: String): Unit = {
      set(stringContainer, line)
    }

    override protected def build(lines: BufferedIterator[String],
                                 fieldName: Option[String],
                                 defaultValue: Option[String]): String = {
      val stringContainer = new mutable.HashSet[String]()
      addLines(stringContainer, lines, fieldName, defaultValue)
      stringContainer.head
    }

    override protected def setDefaultValue(intermediateValue: mutable.HashSet[String],
                                           defaultValue: String): Unit = {
      set(intermediateValue, defaultValue)
    }

    private def set(container:mutable.HashSet[String], value: String): Unit = {
      container.clear()
      container += value
    }
  }

  def newPrintWriter(filename: String): PrintWriter = {
    new PrintWriter(
      new OutputStreamWriter(
        new BufferedOutputStream(
          new FileOutputStream(filename)
        ),
        "UTF-8"
      )
    )
  }

  def newSource(filename: String): Source = {
    val f = new File(filename)
    if (f.exists()) {
      // this file exists on disk
      Source.fromFile(filename, "UTF-8")
    } else {
      // the file does not exist on disk. let's hope it's in the classpath
      // this should work for both scala 2.11 and 2.12
      Source.fromInputStream(getClass.getResourceAsStream("/" + filename))
      // this only works for scala 2.12, so we can't cross compile with 2.11
      // Source.fromResource(filename)
    }
  }

  def readString2Ids(s2iFilename: String): Map[String, Int] = {
    val s2i = Serializer.using(Utils.newSource(s2iFilename)) { source =>
      val byLineStringMapBuilder = new Utils.ByLineStringMapBuilder()
      val lines = source.getLines().buffered
      val s2i = byLineStringMapBuilder.build(lines)
      s2i
    }
    s2i
  }

  def loadParameters(dynetFilename: String, modelParameters: ParameterCollection, key: String = "/all"): Unit = {
    val textLoader = BaseTextLoader.newTextLoader(dynetFilename)
    val textModelLoader = textLoader.newTextModelLoader()

    textModelLoader.populateModel(modelParameters, key)
  }

  def mkDynetFilename(baseFilename: String): String = baseFilename + ".rnn"

  def mkX2iFilename(baseFilename: String): String = baseFilename + ".x2i"

  def mkWordVocab(w2v: SanitizedWordEmbeddingMap): Map[String, Int] = {
    val commonWords = new ListBuffer[String]
    commonWords += Utils.UNK_WORD // the word at position 0 is reserved for unknown words
    for (w <- w2v.matrix.keySet.toList.sorted) {
      commonWords += w
    }
    val w2i = commonWords.zipWithIndex.toMap
    w2i
  }

  def initializeEmbeddings(w2v: SanitizedWordEmbeddingMap, w2i: Map[String, Int], lookupParameters: LookupParameter): Unit = {
    logger.debug("Initializing DyNet embedding parameters...")
    for (word <- w2v.matrix.keySet) {
      lookupParameters.initialize(w2i(word), new FloatVector(ArrayMath.toFloatArray(w2v.matrix(word))))
    }
    logger.debug(s"Completed initializing embedding parameters for a vocabulary of size ${w2v.matrix.size}.")
  }

  def setRnnDropout(rnnBuilder: RnnBuilder, dropoutProb: Float, doDropout: Boolean): Unit = {
    if(doDropout && dropoutProb > 0) {
      rnnBuilder.setDropout(dropoutProb)
    } else {
      rnnBuilder.disableDropout()
    }
  }

  def expressionDropout(expression: Expression, dropoutProb: Float, doDropout: Boolean): Expression = {
    if(doDropout && dropoutProb > 0) {
      Expression.dropout(expression, dropoutProb)
    } else {
      expression
    }
  }
}

class Utils
