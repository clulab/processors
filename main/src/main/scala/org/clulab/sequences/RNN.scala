package org.clulab.sequences

import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.{FileWriter, PrintWriter}

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import edu.cmu.dynet._
import edu.cmu.dynet.Expression._
import RNN._
import org.clulab.utils.{MathUtils, Serializer}

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

class RNN {
  var model:RNNParameters = _

  def train(trainSentences:Array[Array[Row]], devSentences:Array[Array[Row]], embeddingsFile:String): Unit = {
    val (w2i, t2i, c2i) = mkVocabs(trainSentences)
    logger.debug(s"Tag vocabulary has ${t2i.size} entries.")
    logger.debug(s"Word vocabulary has ${w2i.size} entries (including 1 for unknown).")
    logger.debug(s"Character vocabulary has ${c2i.size} entries.")

    initialize(w2i, t2i, c2i, embeddingsFile)
    update(trainSentences:Array[Array[Row]], devSentences:Array[Array[Row]])
  }

  def update(trainSentences: Array[Array[Row]], devSentences:Array[Array[Row]]): Unit = {
    //val trainer = new SimpleSGDTrainer(model.parameters, learningRate = 0.01f)
    val trainer = new RMSPropTrainer(model.parameters)
    var cummulativeLoss = 0.0
    var numTagged = 0
    var sentCount = 0
    var sentences = trainSentences
    val rand = new Random(RANDOM_SEED)

    // model.printTransitionMatrix()

    for(epoch <- 0 until EPOCHS) {
      sentences = MathUtils.randomize(sentences, rand)

      logger.info(s"Started epoch $epoch.")
      for(sentence <- sentences) {
        sentCount += 1
        ComputationGraph.renew()
        //print("Current sentence:")
        //for(w <- sentence) print(" " + w.get(0) + "/" + w.get(1))
        //println()

        // predict tag emission scores for one sentence, from the biLSTM hidden states
        val words = sentence.map(_.get(0))
        val emissionScores = emissionScoresAsExpressions(words,  doDropout = DO_DROPOUT)
        val transitionMatrix = new ExpressionVector
        for(i <- 0 until model.t2i.size) {
          transitionMatrix.add(lookup(model.T, i))
        }

        // get the gold tags for this sentence
        val goldTagIds = toTagIds(sentence.map(_.get(1)))

        // compute loss for this sentence
        val loss = sentenceLoss(emissionScores, transitionMatrix, goldTagIds)

        cummulativeLoss += loss.value().toFloat
        numTagged += sentence.length

        if(sentCount % 1000 == 0) {
          logger.info("Cummulative loss: " + cummulativeLoss / numTagged)
          cummulativeLoss = 0.0
          numTagged = 0

          model.printTransitionMatrix()
        }

        // backprop
        ComputationGraph.backward(loss)
        trainer.update()
      }

      evaluate(devSentences, epoch)
    }
  }

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

  /** Picks the scalar element from an expression that is a matrix */
  def pick2D(matrix:ExpressionVector, row:Int, column:Int): Expression = {
    pick(matrix(row), column)
  }

  /** Implements the forward algorithm to compute the partition score for this lattice */
  def mkPartitionScore(emissionScoresForSeq:ExpressionVector, // Dim: sentenceSize x tagCount
                       transitionMatrix:ExpressionVector,
                       startTag:Int, stopTag:Int): Expression = { // Dim: tagCount x tagCount
    val tagCount = transitionMatrix.size

    //println("emissionScoresForSeq dim: " + emissionScoresForSeq.size + " x " + emissionScoresForSeq.head.value().toVector().size)

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
    //println("forward dim: " + forward.size + " x 1")
    //println("\tValue for start: " + forward(startTag).value().toFloat())

    for(t <- emissionScoresForSeq.indices) {
      val alphasAtT = new ExpressionVector()
      val emitScores = emissionScoresForSeq(t)
      //println("In sentence at " + t)

      for(nextTag <- 0 until tagCount) {
        //println("Looking at tag: " + nextTag)
        val alphasForTag = new ExpressionVector()
        val emitScore = pick(emitScores, nextTag) // scalar: emision score for nextTag
        //println("\temitScore: " + emitScore.value().toFloat())

        for(srcTag <- 0 until tagCount) {
          val transScore = pick2D(transitionMatrix, nextTag, srcTag) // scalar: transition score to nextTag from srcTag
          //println("\ttransScore: " + transScore.value().toFloat())
          val alphaToTagFromSrc =
            forward(srcTag) +
            transScore +
            emitScore
          //println("alphaToTagFromSrc: " + alphaToTagFromSrc.value().toFloat())

          alphasForTag.add(alphaToTagFromSrc)
        }
        //println("alphasForTag dim: " + alphasForTag.size + " x " + alphasForTag.head.value().toVector().size)

        alphasAtT.add(logSumExp(alphasForTag))
      }

      forward = alphasAtT
    }

    val terminalVars = new ExpressionVector()
    for(t <- 0 until tagCount) {
      terminalVars.add(forward(t) + pick2D(transitionMatrix, stopTag, t))
    }

    val total = logSumExp(terminalVars)
    //println("partition score = " + total.value().toFloat())
    total
  }

  def sentenceLoss(emissionScoresForSeq:ExpressionVector, // Dim: sentenceSize x tagCount
                   transitionMatrix:ExpressionVector, // Dim: tagCount x tagCount
                   golds:Array[Int]): Expression = { // Dim: sentenceSize
    val startTag = model.t2i(START_TAG)
    val stopTag = model.t2i(STOP_TAG)

    val scoreOfGoldSeq =
      sentenceScore(emissionScoresForSeq, transitionMatrix, model.t2i.size, golds, startTag, stopTag)
    //println("Gold score: " + scoreOfGoldSeq.value().toFloat())

    val partitionScore =
      mkPartitionScore(emissionScoresForSeq, transitionMatrix, startTag, stopTag)
    //println("Partition score: " + partitionScore.value().toFloat())

    partitionScore - scoreOfGoldSeq
  }

  def sentenceLossGreedy(emissionScoresForSeq:Array[Expression], // Dim: sentenceSize x tagCount
                         transitionMatrix:Expression, // Dim: tagCount x tagCount
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

  def toTagIds(tags: Array[String]):Array[Int] = {
    val ids = new ArrayBuffer[Int]()
    for(tag <- tags) {
      ids += model.t2i(tag)
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

  def evaluate(devSentences:Array[Array[Row]], epoch:Int): Unit = {
    var total = 0
    var correct = 0
    var correctMax = 0

    var totalNonO = 0
    var invalidNonO = 0

    val pw = new PrintWriter(new FileWriter("dev.output." + epoch))
    val pwm = new PrintWriter(new FileWriter("devmax.output." + epoch))
    logger.debug("Started evaluation on dev...")
    for(sent <- devSentences) {
      val words = sent.map(_.get(0))
      val golds = sent.map(_.get(1))

      val preds = predict(words)
      val (t, c) = accuracy(golds, preds)
      total += t
      correct += c

      printCoNLLOutput(pw, words, golds, preds)

      // analytics:
      //   how many labels break the BIO format?
      //   what is the max accuracy, if we correctly fixed all of them?
      val maxPreds = new Array[String](preds.length)
      for(i <- preds.indices) maxPreds(i) = preds(i)
      for(i <- preds.indices) {
        val crt = preds(i)
        val prev = if(i > 0) preds(i - 1) else "O"

        if(crt != "O") totalNonO += 1
        if(crt.startsWith("I-")) {
          if(! prev.startsWith("O") && crt.substring(2) == prev.substring(2)) {
            // valid output
          } else {
            invalidNonO += 1

            var j = i
            while(j >= 0 && maxPreds(j) != "O") {
              maxPreds(j) = golds(j)
              j -= 1
            }
          }
        }
      }

      val (_, cm) = accuracy(golds, maxPreds)
      correctMax += cm

      printCoNLLOutput(pwm, words, golds, maxPreds)
    }

    pw.close()
    pwm.close()
    logger.info(s"Accuracy on ${devSentences.length} dev sentences: " + correct.toDouble / total)
    logger.info("Percentage of invalid non-O labels: " + invalidNonO.toDouble / totalNonO + " (" + invalidNonO + "/" + totalNonO + ")")
    logger.info(s"Max accuracy on ${devSentences.length} dev sentences: " + correctMax.toDouble / total)
  }

  /**
    * Generates tag emission scores for the words in this sequence, stored as Expressions
    * @param words One training or testing sentence
    */
  def emissionScoresAsExpressions(words: Array[String], doDropout:Boolean): ExpressionVector = {
    val embeddings = words.map(mkEmbedding)

    val fwStates = transduce(embeddings, model.fwRnnBuilder)
    val bwStates = transduce(embeddings.reverse, model.bwRnnBuilder).toArray.reverse
    assert(fwStates.size == bwStates.length)
    val states = concatenateStates(fwStates, bwStates).toArray
    assert(states.length == words.length)

    val H = parameter(model.H)
    val O = parameter(model.O)

    val emissionScores = new ExpressionVector()
    for(s <- states) {
      var l1 = Expression.tanh(H * s)
      if(doDropout) {
        l1 = Expression.dropout(l1, DROPOUT_PROB)
      }
      emissionScores.add(O * l1)
    }

    emissionScores
  }

  /** Creates the lattice of probs for a given sequence */
  def emissionScoresToLattice(expressions:Iterable[Expression]): Array[Array[Float]] = {
    val lattice = new ArrayBuffer[Array[Float]]()
    for(expression <- expressions) {
      val probs = expression.value().toVector().toArray
      lattice += probs
    }
    lattice.toArray
  }

  /**
    * Runs the Viterbi algorithm to generate the best sequence of tag ids
    */
  def viterbi(lattice:Array[Array[Float]]):Array[Int] = {
    // TODO: this is currently greedy, not Viterbi. Fix me.

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

  def predict(words:Array[String]):Array[String] = synchronized {
    val probsAsExpressions = emissionScoresAsExpressions(words, doDropout = false)
    val lattice = emissionScoresToLattice(probsAsExpressions)
    val tagIds = viterbi(lattice) // TODO: add transition scores here
    val tags = new ArrayBuffer[String]()
    for(tid <- tagIds) tags += model.i2t(tid)
    tags.toArray
  }

  def concatenateStates(l1: Iterable[Expression], l2: Iterable[Expression]): Iterable[Expression] = {
    val c = new ArrayBuffer[Expression]()
    for(e <- l1.zip(l2)) {
      c += concatenate(e._1, e._2)
    }
    c
  }

  def mkEmbedding(word: String):Expression = {
    val sanitized = word // word.toLowerCase() // Word2Vec.sanitizeWord(word)

    val wordEmbedding =
      if(model.w2i.contains(sanitized))
      // found the word in the known vocabulary
        lookup(model.lookupParameters, model.w2i(sanitized))
      else {
        // not found; return the embedding at position 0, which is reserved for unknown words
        lookup(model.lookupParameters, 0)
        //val caseIdx = casing(word)
        //lookup(model.lookupParameters, caseIdx)
      }

    // biLSTM over character embeddings
    val charEmbedding =
      mkCharacterEmbedding(word)

    // explicit features that capture the shape of the word
    /*
    val c = casing(word)
    val cases = new Array[Float](CASE_o + 1)
    for(i <- cases.indices) cases(i) = 0
    cases(c) = 1

    concatenate(wordEmbedding, charEmbedding, input(Dim(CASE_o + 1), new FloatVector(cases)))
    */
    concatenate(wordEmbedding, charEmbedding)
  }

  def mkCharacterEmbedding(word: String): Expression = {
    val charEmbeddings = new ArrayBuffer[Expression]()
    for(i <- word.indices) {
      if(model.c2i.contains(word.charAt(i)))
        charEmbeddings += lookup(model.charLookupParameters, model.c2i(word.charAt(i)))
    }
    val fwOut = transduce(charEmbeddings, model.charFwRnnBuilder).last
    val bwOut = transduce(charEmbeddings.reverse, model.charBwRnnBuilder).last
    concatenate(fwOut, bwOut)
  }

  def transduce(embeddings:Iterable[Expression], builder:RnnBuilder): Iterable[Expression] = {
    builder.newGraph()
    builder.startNewSequence()
    val states = embeddings.map(builder.addInput)
    states
  }

  def initialize(w2i:Map[String, Int], t2i:Map[String, Int], c2i:Map[Char, Int], embeddingsFile:String): Unit = {
    logger.debug("Initializing DyNet...")
    Initialize.initialize(Map("random-seed" -> RANDOM_SEED))
    model = mkParams(w2i, t2i, c2i)
    model.initialize(embeddingsFile) // Only necessary if constructing from scratch by training
    logger.debug("Completed initialization.")
  }
}

class RNNParameters(
  val w2i:Map[String, Int],
  val t2i:Map[String, Int],
  val i2t:Array[String],
  val c2i:Map[Char, Int],
  val parameters:ParameterCollection,
  val lookupParameters:LookupParameter,
  val fwRnnBuilder:RnnBuilder,
  val bwRnnBuilder:RnnBuilder,
  val H:Parameter,
  val O:Parameter,
  val T:LookupParameter, // transition matrix for Viterbi; T[i][j] = transition *to* i *from* j
  val charLookupParameters:LookupParameter,
  val charFwRnnBuilder:RnnBuilder,
  val charBwRnnBuilder:RnnBuilder) {

  protected def toFloatArray(doubles: Array[Double]): Array[Float] = {
    val floats = new Array[Float](doubles.length)
    for (i <- doubles.indices) {
      floats(i) = doubles(i).toFloat
    }
    floats
  }


  protected def add(dst:Array[Double], src:Array[Double]): Unit = {
    assert(dst.length == src.length)
    for(i <- dst.indices) {
      dst(i) += src(i)
    }
  }

  def initialize(embeddingsFile: String): Unit = {
    initializeEmbeddings(embeddingsFile)
    initializeTransitions()
  }

  def initializeTransitions(): Unit = {
    val startTag = t2i(START_TAG)
    val stopTag = t2i(STOP_TAG)

    for (i <- 0 until t2i.size) {
      T.initialize(i, initTransitionsTo(i, t2i.size, startTag, stopTag))
    }
  }

  def initTransitionsTo(dst: Int, size:Int, startTag: Int, stopTag: Int): FloatVector = {
    val transScores = new Array[Float](size)

    for(i <- 0 until size) {
      transScores(i) = randomNormal(Dim(1)).value().toFloat() / size // pseudo Glorot
    }

    if(RNN.USE_DOMAIN_CONSTRAINTS) {
      // discourage transitions to START from anything
      if (dst == startTag) {
        for (i <- 0 until size)
          transScores(i) = LOG_MIN_VALUE
      } else {
        // discourage transitions to anything from STOP
        transScores(stopTag) = LOG_MIN_VALUE

        // discourage transitions to I-X from B-Y or I-Y
        val dstTag = i2t(dst)
        if (dstTag.startsWith("I-")) {
          for (i <- 0 until size) {
            val srcTag = i2t(i)
            if ((srcTag.startsWith("B-") || srcTag.startsWith("I-")) &&
              srcTag.substring(2) != dstTag.substring(2)) {
              transScores(i) = LOG_MIN_VALUE
            }
          }
        }
      }
    }

    new FloatVector(transScores)
  }

  def initializeEmbeddings(embeddingsFile: String): Unit = {
    logger.debug(s"Loading embeddings from file $embeddingsFile...")
    val w2v = new Word2Vec(embeddingsFile) // Some(w2i.keySet))
    val unknownEmbed = new Array[Double](EMBEDDING_SIZE)
    for(i <- unknownEmbed.indices) unknownEmbed(i) = 0.0

    var unknownCount = 0
    for(word <- w2v.matrix.keySet){// w2i.keySet) {
      if(w2i.contains(word)) {
        lookupParameters.initialize(w2i(word), new FloatVector(toFloatArray(w2v.matrix(word))))
      } else {
        add(unknownEmbed, w2v.matrix(word))
        unknownCount += 1
      }
    }
    for(i <- unknownEmbed.indices) {
      unknownEmbed(i) /= unknownCount
    }
    lookupParameters.initialize(0, new FloatVector(toFloatArray(unknownEmbed)))
    logger.debug(s"Loaded ${w2v.matrix.size} embeddings.")

  }

  def printTransitionMatrix(): Unit = {
    val tagCount = t2i.size
    for(dstTag <- 0 until tagCount) {
      println("Transitions TO tag " + i2t(dstTag) + ":")
      val transScores = lookup(T, dstTag).value().toVector()
      for(srcTag <- 0 until tagCount) {
        println("\tFROM " + i2t(srcTag) + ": " + transScores(srcTag))
      }
    }
  }
}

object RNN {
  val logger:Logger = LoggerFactory.getLogger(classOf[RNN])

  val EPOCHS = 2
  val RANDOM_SEED = 2522620396l // used for both DyNet, and the JVM seed for shuffling data
  val DROPOUT_PROB = 0.1f
  val DO_DROPOUT = false
  val EMBEDDING_SIZE = 300
  val RNN_STATE_SIZE = 100
  val NONLINEAR_SIZE = 32
  val RNN_LAYERS = 1
  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16

  val UNK_WORD = "<UNK>"
  val START_TAG = "<START>"
  val STOP_TAG = "<STOP>"

  val LOG_MIN_VALUE:Float = -10000

  // case features
  val CASE_x = 0
  val CASE_X = 1
  val CASE_Xx = 2
  val CASE_xX = 3
  val CASE_n = 4
  val CASE_o = 5

  val USE_DOMAIN_CONSTRAINTS = true

  def casing(w:String): Int = {
    if(w.charAt(0).isLetter) { // probably an actual word
      // count upper and lower-case chars
      var uppers = 0
      for(j <- 0 until w.length) {
        if(w.charAt(j).isUpper) {
          uppers += 1
        }
      }

      var v = CASE_x
      if (uppers == w.length) v = CASE_X
      else if (uppers == 1 && w.charAt(0).isUpper) v = CASE_Xx
      else if (uppers >= 1 && !w.charAt(0).isUpper) v = CASE_xX
      v
    } else if(isNumber(w))
      CASE_n
    else
      CASE_o
  }

  def isNumber(w:String): Boolean = {
    for(i <- 0 until w.length) {
      val c = w.charAt(i)
      if(! c.isDigit && c != '-' && c != '.' && c != ',')
        return false
    }
    true
  }

  protected def save[T](printWriter: PrintWriter, map: Map[T, Int]): Unit = {
    map.foreach { case (key, value) =>
      printWriter.println(s"$key\t$value")
    }
    printWriter.println() // Separator
  }

  def save(dynetFilename:String, x2iFilename: String, rnnParameters: RNNParameters):Unit = {
    val modelSaver = new ModelSaver(dynetFilename)
    modelSaver.addModel(rnnParameters.parameters, "/all")
    modelSaver.done()

    Serializer.using(new PrintWriter(new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(x2iFilename)), "UTF-8"))) { printWriter =>
      save(printWriter, rnnParameters.w2i)
      save(printWriter, rnnParameters.t2i)
      save(printWriter, rnnParameters.c2i)
    }
  }

  def load(filename: String, keyValueBuilders: Array[KeyValueBuilder]): Unit = {
    var index = 0

    Serializer.using(Source.fromFile(filename, "UTF-8")) { source =>
      source.getLines.foreach { line =>
        if (line.nonEmpty) {
          val Array(key, value) = line.split('\t')
          keyValueBuilders(index).add(key, value)
        }
        else
          index += 1
      }
    }
  }

  trait KeyValueBuilder {
    def add(key: String, value: String): Unit
  }

  class MapBuilder[KeyType](val converter: String => KeyType) extends KeyValueBuilder {
    val mutableMap: mutable.Map[KeyType, Int] = new mutable.HashMap

    def add(key: String, value: String): Unit = mutableMap += ((converter(key), value.toInt))

    def toMap: Map[KeyType, Int] = mutableMap.toMap
  }

  def load(dynetFilename:String, x2iFilename: String):RNNParameters = {
    def stringToString(string: String): String = string
    def stringToChar(string: String): Char = string.charAt(0)

    val w2iBuilder = new MapBuilder(stringToString)
    val t2iBuilder = new MapBuilder(stringToString)
    val c2iBuilder = new MapBuilder(stringToChar)
    val builders: Array[KeyValueBuilder] = Array(w2iBuilder, t2iBuilder, c2iBuilder)

    load(x2iFilename, builders)

    val model = mkParams(w2iBuilder.toMap, t2iBuilder.toMap, c2iBuilder.toMap)

    new ModelLoader(dynetFilename).populateModel(model.parameters, "/all")
    model
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

  def mkVocabs(trainSentences:Array[Array[Row]]): (Map[String, Int], Map[String, Int], Map[Char, Int]) = {
    val words = new Counter[String]()
    val tags = new Counter[String]()
    val chars = new mutable.HashSet[Char]()
    for(sentence <- trainSentences) {
      for(token <- sentence) {
        val word = token.get(0)
        words += word // Word2Vec.sanitizeWord(word)
        for(i <- word.indices) {
          chars += word.charAt(i)
        }
        tags += token.get(1)
      }
    }

    val commonWords = new ListBuffer[String]
    commonWords += UNK_WORD // the word at position 0 is reserved for unknown words
    for(w <- words.keySet) {
      if(words.getCount(w) > 1) {
        commonWords += w
      }
    }

    tags += START_TAG
    tags += STOP_TAG

    val w2i = commonWords.sorted.zipWithIndex.toMap // These must be sorted for consistency across runs
    val t2i = tags.keySet.toList.sorted.zipWithIndex.toMap
    val c2i = chars.toList.sorted.zipWithIndex.toMap

    (w2i, t2i, c2i)
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

  def mkParams(w2i:Map[String, Int], t2i:Map[String, Int], c2i:Map[Char, Int]): RNNParameters = {
    val parameters = new ParameterCollection()
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(EMBEDDING_SIZE))
    val embeddingSize = EMBEDDING_SIZE + 2 * CHAR_RNN_STATE_SIZE // + CASE_o + 1
    val fwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)
    val bwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)
    val H = parameters.addParameters(Dim(NONLINEAR_SIZE, 2 * RNN_STATE_SIZE))
    val O = parameters.addParameters(Dim(t2i.size, NONLINEAR_SIZE)) // + CASE_o + 1))
    val i2t = fromIndexToString(t2i)
    val T = mkTransitionMatrix(parameters, t2i, i2t)
    logger.debug("Created parameters.")

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)

    new RNNParameters(w2i, t2i, i2t, c2i, parameters, lookupParameters, fwBuilder, bwBuilder, H, O, T,
      charLookupParameters, charFwBuilder, charBwBuilder)
  }

  def main(args: Array[String]): Unit = {
    val trainFile = args(0)
    val devFile = args(1)
    val trainSentences = ColumnReader.readColumns(trainFile)
    val devSentences = ColumnReader.readColumns(devFile)
    val embeddingsFile = args(2)

    val rnn = new RNN()
    rnn.train(trainSentences, devSentences, embeddingsFile)

    val dynetFilename = "rnn.dat"
    val x2iFilename = "x2i.dat"

    save(dynetFilename, x2iFilename, rnn.model)

    val pretrainedRnn = new RNN()
    val rnnParameters = load(dynetFilename, x2iFilename)
    pretrainedRnn.model = rnnParameters

    rnn.evaluate(devSentences, -1)
    pretrainedRnn.evaluate(devSentences, -1)
  }
}