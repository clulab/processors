package org.clulab.sequences

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStreamWriter, PrintWriter}

import edu.cmu.dynet.Expression.{concatenate, input, logSumExp, lookup, pick, pickNegLogSoftmax, sum}
import edu.cmu.dynet.{Dim, Expression, ExpressionVector, FloatVector, Initialize, LookupParameter, ParameterCollection, RnnBuilder, Trainer}
import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.fatdynet.utils.BaseTextLoader
import org.clulab.fatdynet.utils.CloseableModelLoader
import org.clulab.fatdynet.utils.CloseableZipModelLoader
import org.clulab.processors.clu.tokenizer.EnglishLemmatizer
import org.clulab.utils.Closer.AutoCloser
import org.clulab.struct.MutableNumber
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
 * Utility methods used by LstmCrf and LstmCrfMtl
 */
object LstmUtils {
  private val logger:Logger = LoggerFactory.getLogger(classOf[LstmUtils])

  val UNK_WORD = "<UNK>"
  val EOS_WORD = "<EOS>"

  val START_TAG = "<START>"
  val STOP_TAG = "<STOP>"

  val RANDOM_SEED = 2522620396L // used for both DyNet, and the JVM seed for shuffling data
  val WEIGHT_DECAY:Float = 1e-5.toFloat

  val LOG_MIN_VALUE:Float = -10000

  private var IS_DYNET_INITIALIZED = false

  def initializeDyNet(autoBatch:Boolean = false, mem:String = ""): Unit = {
    this.synchronized {
      if(! IS_DYNET_INITIALIZED) {
        logger.debug("Initializing DyNet...")

        val params = new mutable.HashMap[String, Any]()
        params += "random-seed" -> RANDOM_SEED
        params += "weight-decay" -> WEIGHT_DECAY
        if(autoBatch) {
          params += "autobatch" -> 1
          params += "dynet-mem" -> mem
        }

        Initialize.initialize(params.toMap)
        logger.debug("DyNet initialization complete.")
        IS_DYNET_INITIALIZED = true
      }
    }
  }

  def loadEmbeddings(docFreqFileName:Option[String],
                     minDocFreq:Int,
                     embeddingsFile:String,
                     mandatoryWords:Option[String] = None): Word2Vec = {
    val wordsToUse = loadWordsToUse(docFreqFileName, minDocFreq)
    logger.debug(s"Loading embeddings from file $embeddingsFile...")

    if(mandatoryWords.isDefined && wordsToUse.isDefined) {
      val source = Source.fromFile(mandatoryWords.get)
      for(line <- source.getLines()) {
        wordsToUse.get += line.trim.toLowerCase()
      }
      source.close()
    }
    logger.debug(s"Word count after adding mandatory words is ${wordsToUse.get.size}.")

    val w2v = new Word2Vec(embeddingsFile, Some(wordsToUse.get.toSet), caseInsensitiveWordsToUse = true) // TODO: our DF scores are case insensitive
    logger.debug(s"Completed loading embeddings for a vocabulary of size ${w2v.matrix.size}.")

    w2v
  }

  def loadWordsToUse(docFreqFileName: Option[String], minDocFreq: Int):Option[mutable.HashSet[String]] = {
    if(docFreqFileName.isDefined) {
      logger.debug(s"Loading words to use from file ${docFreqFileName.get} using min frequency of $minDocFreq.")
      val wordsToUse = new mutable.HashSet[String]()
      val source = Source.fromFile(docFreqFileName.get)
      var total = 0
      var kept = 0
      for(line <- source.getLines()) {
        total += 1
        val tokens = line.split("\\s+")
        // println(s"Reading line: ${tokens.mkString(", ")}")
        assert(tokens.length == 2)
        if(tokens(1).toInt > minDocFreq) {
          kept += 1
          wordsToUse += tokens(0)
        }
      }
      source.close()
      logger.debug(s"Loaded $kept words to use, from a total of $total words.")
      Some(wordsToUse)
    } else {
      None
    }
  }

  def fromIndexToString(s2i: Map[String, Int]):Array[String] = {
    var max = Int.MinValue
    for(v <- s2i.values) {
      if(v > max) {
        max = v
      }
    }
    assert(max > 0)
    val i2s = new Array[String](max + 1)
    for(k <- s2i.keySet) {
      i2s(s2i(k)) = k
    }
    i2s
  }

  def fromIndexToChar(s2i: Map[Char, Int]):Array[Char] = {
    var max = Int.MinValue
    for(v <- s2i.values) {
      if(v > max) {
        max = v
      }
    }
    assert(max > 0)
    val i2s = new Array[Char](max + 1)
    for(k <- s2i.keySet) {
      i2s(s2i(k)) = k
    }
    i2s
  }

  /**
   * Initializes the transition matrix for a tagset of size size
   * T[i, j] stores a transition *to* i *from* j
   */
  def mkTransitionMatrix(parameters:ParameterCollection, t2i:Map[String, Int], i2t:Array[String]): LookupParameter = {
    val size = t2i.size
    val rows = parameters.addLookupParameters(size, Dim(size))
    rows
  }

  def viterbi(
    emissionScores: Array[Array[Float]],
    transitionMatrix: Array[Array[Float]],
    tagCount:Int,
    startTagIdx:Int,
    stopTagIdx:Int): Array[Int] = {

    // initial scores in log space
    val initScores = new Array[Float](tagCount)
    for(i <- initScores.indices) initScores(i) = LOG_MIN_VALUE
    initScores(startTagIdx) = 0

    // the best overall scores at time step -1 (start)
    var forwardVar = initScores

    // backpointers for the entire lattice
    val backPointers = new ArrayBuffer[Array[Int]]()

    // iterate over all the words in this sentence
    for(t <- emissionScores.indices) {
      // scores for *all* tags for time step t
      val scoresAtT = new Array[Float](emissionScores(t).length)

      // backpointers for this time step
      val backPointersAtT = new Array[Int](emissionScores(t).length)

      // iterate over all possible tags for this time step
      for(nextTag <- emissionScores(t).indices) {

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
    for(backPointersAtT <- backPointers.reverse) {
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
  def mkPartitionScore(emissionScoresForSeq:ExpressionVector, // Dim: sentenceSize x tagCount
                       transitionMatrix:ExpressionVector,
                       startTag:Int, stopTag:Int): Expression = { // Dim: tagCount x tagCount
    val tagCount = transitionMatrix.size

    // sum of scores of reaching each tag at this time step
    var forward = new ExpressionVector()
    for(t <- 0 until tagCount) {
      //
      // cost (in log space) of starting at a given tag
      // the only possible starting tag is START; all others are disabled
      //
      val alphaAtT0:Float = if(t == startTag) 0 else LOG_MIN_VALUE
      forward.add(input(alphaAtT0))
    }

    for(t <- emissionScoresForSeq.indices) {
      val alphasAtT = new ExpressionVector()
      val emitScores = emissionScoresForSeq(t)

      for(nextTag <- 0 until tagCount) {
        val alphasForTag = new ExpressionVector()
        val emitScore = pick(emitScores, nextTag) // scalar: emision score for nextTag

        for(srcTag <- 0 until tagCount) {
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
    for(t <- 0 until tagCount) {
      terminalVars.add(forward(t) + pick2D(transitionMatrix, stopTag, t))
    }

    val total = logSumExp(terminalVars)
    total
  }

  def toIds[T](tags: Array[T], t2i:Map[T, Int]):Array[Int] = {
    val ids = new ArrayBuffer[Int]()
    for(tag <- tags) {
      ids += t2i(tag)
    }
    ids.toArray
  }

  def printCoNLLOutput(pw:PrintWriter, words:Array[String], golds:Array[String], preds:Array[String]): Unit = {
    for(i <- words.indices) {
      pw.println(words(i) + " " + golds(i) + " " + preds(i))
    }
    pw.println()
  }

  def accuracy(golds:Array[String], preds:Array[String]): (Int, Int) = {
    assert(golds.length == preds.length)
    var correct = 0
    for(e <- preds.zip(golds)) {
      if(e._1 == e._2) {
        correct += 1
      }
    }
    (golds.length, correct)
  }

  /** Runs a greedy algorithm to generate the sequence of tag ids, ignoring transition scores (not used) */
  def greedyPredict(lattice:Array[Array[Float]]):Array[Int] = {
    val tagIds = new ArrayBuffer[Int]()
    for(probs <- lattice) {
      var max = Float.MinValue
      var tid = -1
      for(i <- probs.indices) {
        if(probs(i) > max) {
          max = probs(i)
          tid = i
        }
      }
      assert(tid > -1)
      tagIds += tid
    }
    tagIds.toArray
  }

  def printTagScores(header:String, scores:Array[Float], i2t:Array[String]): Unit = {
    print(header)
    for(j <- scores.indices) {
      val tag = i2t(j)
      print(s" {$tag, ${scores(j)}}")
    }
    println()
  }

  /** Picks the scalar element from an expression that is a matrix */
  def pick2D(matrix:ExpressionVector, row:Int, column:Int): Expression = {
    pick(matrix(row), column)
  }

  /** Computes the score of the given sequence of tags (tagSeq) */
  def sentenceScore(emissionScoresForSeq:ExpressionVector, // Dim: sentenceSize x tagCount
                    transitionMatrix:ExpressionVector, // Dim: tagCount x tagCount
                    tagCount:Int,
                    tagSeq:Array[Int],
                    startTag:Int,
                    stopTag:Int): Expression = {
    // start with the transition score to first tag from START
    var score = pick2D(transitionMatrix, tagSeq.head, startTag)

    for(i <- tagSeq.indices) {
      if(i > 0) {
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
    val c = new ArrayBuffer[Expression]()
    for(e <- l1.zip(l2)) {
      c += concatenate(e._1, e._2)
    }
    c
  }

  val lemmatizer = new EnglishLemmatizer

  def mkWordEmbedding(word: String,
                      w2i:Map[String, Int],
                      wordLookupParameters:LookupParameter,
                      c2i:Map[Char, Int],
                      charLookupParameters:LookupParameter,
                      charFwRnnBuilder:RnnBuilder,
                      charBwRnnBuilder:RnnBuilder):Expression = {
    //
    // make sure you preprocess the word similarly to the embedding library used!
    //   GloVe large does not do any preprocessing
    //   GloVe small lowers the case
    //   Our Word2Vec uses Word2Vec.sanitizeWord
    //
    val sanitized = word // lemmatizer.lemmatizeWord(word) // word // word.toLowerCase() // Word2Vec.sanitizeWord(word)

    val wordEmbedding =
      if(w2i.contains(sanitized))
        // found the word in the known vocabulary
        lookup(wordLookupParameters, w2i(sanitized))
      else {
        // not found; return the embedding at position 0, which is reserved for unknown words
        lookup(wordLookupParameters, 0) // w2i(LstmUtils.UNK_WORD)) // 0)
      }

    // biLSTM over character embeddings
    val charEmbedding =
      mkCharacterEmbedding(word, c2i, charLookupParameters, charFwRnnBuilder, charBwRnnBuilder)

    concatenate(wordEmbedding, charEmbedding)
  }

  private def mkCharacterEmbedding(word: String,
                                   c2i:Map[Char, Int],
                                   charLookupParameters:LookupParameter,
                                   charFwRnnBuilder:RnnBuilder,
                                   charBwRnnBuilder:RnnBuilder): Expression = {
    //println(s"make embedding for word [$word]")
    val charEmbeddings = new ArrayBuffer[Expression]()
    for(i <- word.indices) {
      if(c2i.contains(word.charAt(i))) {
        charEmbeddings += lookup(charLookupParameters, c2i(word.charAt(i)))
      } else {
        charEmbeddings += lookup(charLookupParameters, 0) // 0 reserved for unknown chars
      }
    }
    val fwOut = transduce(charEmbeddings, charFwRnnBuilder).last
    val bwOut = transduce(charEmbeddings.reverse, charBwRnnBuilder).last
    concatenate(fwOut, bwOut)
  }

  def transduce(embeddings:Iterable[Expression], builder:RnnBuilder): Iterable[Expression] = {
    builder.newGraph()
    builder.startNewSequence()
    val states = embeddings.map(builder.addInput)
    states
  }

  /** Greedy loss function, ignoring transition scores */
  def sentenceLossGreedy(emissionScoresForSeq:ExpressionVector, // Dim: sentenceSize x tagCount
                         golds:Array[Int]): Expression = { // Dim: sentenceSize

    val goldLosses = new ExpressionVector()
    assert(emissionScoresForSeq.length == golds.length)

    for(i <- emissionScoresForSeq.indices) {
      // gold tag for word at position i
      val goldTid = golds(i)
      // emissionScoresForSeq(i) = all tag emission scores for the word at position i
      goldLosses.add(pickNegLogSoftmax(emissionScoresForSeq(i), goldTid))
    }

    sum(goldLosses)
  }

  /**
   * Objective function that maximizes the CRF probability of the gold sequence of tags for a given sentence
   * @param emissionScoresForSeq emission scores for the whole sequence, and all tags
   * @param transitionMatrix transition matrix between all tags
   * @param golds gold sequence of tags
   * @return the negative prob of the gold sequence (in log space)
   */
  def sentenceLossCrf(emissionScoresForSeq:ExpressionVector, // Dim: sentenceSize x tagCount
                      transitionMatrix:ExpressionVector, // Dim: tagCount x tagCount
                      golds:Array[Int],
                      t2i:Map[String, Int]): Expression = { // Dim: sentenceSize
    val startTag = t2i(START_TAG)
    val stopTag = t2i(STOP_TAG)

    val scoreOfGoldSeq =
      sentenceScore(emissionScoresForSeq, transitionMatrix, t2i.size, golds, startTag, stopTag)

    val partitionScore =
      mkPartitionScore(emissionScoresForSeq, transitionMatrix, startTag, stopTag)

    partitionScore - scoreOfGoldSeq
  }

  def emissionScoresToArrays(expressions:Iterable[Expression]): Array[Array[Float]] = {
    val lattice = new ArrayBuffer[Array[Float]]()
    for(expression <- expressions) {
      val probs = expression.value().toVector().toArray
      lattice += probs
    }
    lattice.toArray
  }

  def emissionScoresToArraysAllTasks(expressions:Array[ExpressionVector]): Array[Array[Array[Float]]] = {
    val latticesAllTasks = new Array[Array[Array[Float]]](expressions.length)

    for(tid <- latticesAllTasks.indices) {
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
    for(i <- 0 until size) {
      transitionMatrix += lookup(trans, i).value().toVector().toArray
    }
    transitionMatrix.toArray
  }

  // This <% seems to be called an "implicit conversion declaration".
  def save[T <% Ordered[T]](printWriter: PrintWriter, values: Map[T, Int], comment: String): Unit = {
    printWriter.println("# " + comment)
    // Sort these so that the same file always results, even it this is slow.
    values.toSeq.sorted.foreach { case (key, value) =>
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

  def save[T](printWriter: PrintWriter, value: Long, comment: String): Unit = {
    printWriter.println("# " + comment)
    printWriter.println(value)
    printWriter.println() // Separator
  }

  abstract class ByLineBuilder[IntermediateValueType] {

    protected def addLines(intermediateValue: IntermediateValueType, lines: Iterator[String]): Unit = {
      // we need to look ahead to skip the comments, hence the buffered iterator
      val bufferedIterator = lines.buffered

      // skip all comment lines
      while(bufferedIterator.head.nonEmpty && bufferedIterator.head.startsWith("#")) {
        bufferedIterator.next()
      }

      def nextLine(): Boolean = {
        val line = bufferedIterator.next()
        //println(s"LINE: [$line]")

        if (line.nonEmpty) {
          addLine(intermediateValue, line)
          true // Continue on non-blank lines.
        }
        else
          false // Stop at first blank line.
      }

      while (nextLine()) { }
    }

    def addLine(intermediateValue: IntermediateValueType, line: String): Unit
  }

  // This is a little fancy because it works with both String and Char keys.
  class ByLineMapBuilder[KeyType](val converter: String => KeyType) extends ByLineBuilder[mutable.Map[KeyType, Int]] {

    def addLine(mutableMap: mutable.Map[KeyType, Int], line: String): Unit = {
      val Array(key, value) = line.split('\t')

      mutableMap += ((converter(key), value.toInt))
    }

    def build(lines: Iterator[String]): Map[KeyType, Int] = {
      val mutableMap: mutable.Map[KeyType, Int] = new mutable.HashMap

      addLines(mutableMap, lines)
      mutableMap.toMap
    }
  }

  protected def stringToString(string: String): String = string

  protected def stringToChar(string: String): Char = string.charAt(0)

  protected def stringToCharInt(string: String): Char = string.toInt.toChar

  class ByLineStringMapBuilder extends ByLineMapBuilder(stringToString)

  class ByLineCharMapBuilder extends ByLineMapBuilder(stringToChar)

  class ByLineCharIntMapBuilder extends ByLineMapBuilder(stringToCharInt)

  class ByLineArrayBuilder extends ByLineBuilder[ArrayBuffer[String]] {

    def addLine(arrayBuffer: ArrayBuffer[String], line: String): Unit = {
      arrayBuffer += line
    }

    def build(lines: Iterator[String]): Array[String] = {
      val arrayBuffer: ArrayBuffer[String] = ArrayBuffer.empty

      addLines(arrayBuffer, lines)
      arrayBuffer.toArray
    }
  }

  class ByLineIntBuilder extends ByLineBuilder[MutableNumber[Option[Int]]] {

    def addLine(mutableNumberOpt: MutableNumber[Option[Int]], line: String): Unit = {
      mutableNumberOpt.value = Some(line.toInt)
    }

    def build(lines: Iterator[String]): Int = {
      val mutableNumberOpt: MutableNumber[Option[Int]] = new MutableNumber(None)

      addLines(mutableNumberOpt, lines)
      mutableNumberOpt.value.get
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
    if(f.exists()) {
      // this file exists on disk
      Source.fromFile(filename, "UTF-8")
    } else {
      // the file does not exist on disk. let's hope it's in the classpath
      Source.fromResource(filename)
    }
  }

  def loadParameters(dynetFilename: String, modelParameters: ParameterCollection, key:String = "/all"): Unit = {
    val textLoader = BaseTextLoader.newTextLoader(dynetFilename)
    val textModelLoader = textLoader.newTextModelLoader()

    textModelLoader.populateModel(modelParameters, key)
  }

  def mkDynetFilename(baseFilename: String): String = baseFilename + ".rnn"

  def mkX2iFilename(baseFilename: String): String = baseFilename + ".x2i"

  def mkWordVocab(w2v:Word2Vec): Map[String, Int] = {
    val commonWords = new ListBuffer[String]
    commonWords += LstmUtils.UNK_WORD // the word at position 0 is reserved for unknown words
    for(w <- w2v.matrix.keySet.toList.sorted) {
      commonWords += w
    }
    val w2i = commonWords.zipWithIndex.toMap
    w2i
  }

  def initializeEmbeddings(w2v:Word2Vec, w2i:Map[String, Int], lookupParameters: LookupParameter): Unit = {
    logger.debug("Initializing DyNet embedding parameters...")
    for(word <- w2v.matrix.keySet){
      lookupParameters.initialize(w2i(word), new FloatVector(ArrayMath.toFloatArray(w2v.matrix(word))))
    }
    logger.debug(s"Completed initializing embedding parameters for a vocabulary of size ${w2v.matrix.size}.")
  }
}

class SafeTrainer(val trainer: Trainer, val clipThreshold:Float) {
  trainer.clippingEnabled_=(true)
  trainer.clipThreshold_=(clipThreshold)

  /**
   * Updates the model, catching vanishing/exploding gradients and trying to recover
   * @param parameters Model
   */
  def update(parameters: ParameterCollection): Unit = {
    try {
      trainer.update()
    } catch {
      case exception: RuntimeException if exception.getMessage.startsWith("Magnitude of gradient is bad") =>
        // aim to reset the gradient and continue training
        parameters.resetGradient()
        SafeTrainer.logger.info(s"Caught an invalid gradient exception: ${exception.getMessage}. Reset gradient L2 norm to: ${parameters.gradientL2Norm()}")
    }
  }
}

object SafeTrainer {
  val logger = LoggerFactory.getLogger(classOf[SafeTrainer])

  val CLIP_THRESHOLD = 5.0f

  def apply(trainer: Trainer, clipThreshold: Float = CLIP_THRESHOLD): SafeTrainer = {
    new SafeTrainer(trainer, clipThreshold)
  }
}

class LstmUtils
