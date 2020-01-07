package org.clulab.sequences

import java.io.{FileWriter, PrintWriter}

import edu.cmu.dynet._
import edu.cmu.dynet.Expression._
import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser
import org.clulab.sequences.LstmCrf._
import org.clulab.sequences.LstmUtils._
import org.clulab.struct.Counter
import org.clulab.utils.{MathUtils, Serializer, StringUtils}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

/**
  * Implements the biLSTM-CRF of Lample et al. (2016), using the GloVe embeddings and learned character embeddings
  * @param greedyInference If true use a CRF layer; otherwise perform greedy inference
  * @author Mihai
  */

class LstmCrf protected(val greedyInference: Boolean, initializationParamsOpt: Option[LstmCrf.InitializationParams], lstmCrfParametersOpt: Option[LstmCrfParameters]) {
  // Constructor for case of uninitialized model
  def this(greedyInference: Boolean, initializationParams: LstmCrf.InitializationParams) = this(greedyInference, Some(initializationParams), None)
  // Constructor for case of model previously initialized because, e.g., it was read from file
  def this(greedyInference: Boolean, lstmCrfParameters: LstmCrfParameters) = this(greedyInference, None, Some(lstmCrfParameters))

  var model: LstmCrfParameters = lstmCrfParametersOpt.getOrElse(initialize(initializationParamsOpt.get))

  protected def initialize(initializationParams: LstmCrf.InitializationParams): LstmCrfParameters = {
    val w2v = loadEmbeddings(initializationParams.docFreqFileName, initializationParams.minDocFreq, initializationParams.embeddingsFile)
    val (w2i, t2i, c2i) = mkVocabs(initializationParams.trainSentences, w2v)

    logger.debug(s"Tag vocabulary has ${t2i.size} entries.")
    logger.debug(s"Word vocabulary has ${w2i.size} entries (including 1 for unknown).")
    logger.debug(s"Character vocabulary has ${c2i.size} entries.")

    // TODO Perhaps put this elsewhere, like in main.
    logger.debug("Initializing DyNet...")
    Initialize.initialize(Map("random-seed" -> RANDOM_SEED))
    val model = LstmCrfParameters.create(w2i, t2i, c2i, w2v)
    logger.debug("Completed initialization.")
    model
  }

  def mkVocabs(trainSentences:Array[Array[Row]], w2v:Word2Vec): (Map[String, Int], Map[String, Int], Map[Char, Int]) = {
    val tags = new Counter[String]()
    val chars = new mutable.HashSet[Char]()
    for(sentence <- trainSentences) {
      for(token <- sentence) {
        val word = token.getWord
        for(i <- word.indices) {
          chars += word.charAt(i)
        }
        tags += token.getTag
      }
    }

    val commonWords = new ListBuffer[String]
    commonWords += UNK_WORD // the word at position 0 is reserved for unknown words
    for(w <- w2v.matrix.keySet.toList.sorted) {
      commonWords += w
    }

    tags += START_TAG
    tags += STOP_TAG

    val w2i = commonWords.zipWithIndex.toMap
    val t2i = tags.keySet.toList.sorted.zipWithIndex.toMap
    val c2i = chars.toList.sorted.zipWithIndex.toMap

    (w2i, t2i, c2i)
  }

  /**
    * Trains on the given training sentences, and report accuracy after each epoch on development sentences
    * @param trainSentences Training sentences
    * @param devSentences Development/validation sentences, used for logging purposes only
    */
  def train(trainSentences: Array[Array[Row]], devSentences:Option[Array[Array[Row]]]): Unit = {
    //val trainer = new SimpleSGDTrainer(model.parameters, learningRate = 0.01f)
    val trainer = new RMSPropTrainer(model.parameters)
    var cummulativeLoss = 0.0
    var numTagged = 0
    var sentCount = 0
    var sentences = trainSentences
    val rand = new Random(RANDOM_SEED)

    for(epoch <- 0 until EPOCHS) {
      sentences = MathUtils.randomize(sentences, rand)

      logger.info(s"Started epoch $epoch.")
      for(sentence <- sentences) {
        sentCount += 1
        ComputationGraph.renew()

        // predict tag emission scores for one sentence, from the biLSTM hidden states
        val words = sentence.map(_.getWord)
        val emissionScores = emissionScoresAsExpressions(words,  doDropout = DO_DROPOUT)

        // get the gold tags for this sentence
        val goldTagIds = toIds(sentence.map(_.getTag), model.t2i)

        val loss =
          if(greedyInference) {
            // greedy loss
            sentenceLossGreedy(emissionScores, goldTagIds)
          }
          else {
            // fetch the transition probabilities from the lookup storage
            val transitionMatrix = new ExpressionVector
            for(i <- 0 until model.t2i.size) {
              transitionMatrix.add(lookup(model.T, i))
            }

            // CRF loss
            sentenceLossCrf(emissionScores, transitionMatrix, goldTagIds, model.t2i)
          }

        cummulativeLoss += loss.value().toFloat
        numTagged += sentence.length

        if(sentCount % 1000 == 0) {
          logger.info("Cummulative loss: " + cummulativeLoss / numTagged)
          cummulativeLoss = 0.0
          numTagged = 0
        }

        // backprop
        ComputationGraph.backward(loss)
        trainer.update()
      }

      // check dev performance in this epoch
      if(devSentences.nonEmpty)
        evaluate(devSentences.get, epoch)
    }
  }

  def evaluate(sentences:Array[Array[Row]], epoch:Int): Unit = {
    evaluate(sentences, "development", epoch)
  }

  def evaluate(sentences:Array[Array[Row]]): Unit = {
    evaluate(sentences, "testing", -1)
  }

  /** Logs accuracy score on devSentences; also saves the output in the file dev.output.<EPOCH> */
  def evaluate(sentences:Array[Array[Row]], name:String, epoch:Int): Unit = {
    var total = 0
    var correct = 0

    val pw = new PrintWriter(new FileWriter("dev.output." + epoch))
    logger.debug(s"Started evaluation on the $name dataset...")
    for(sent <- sentences) {
      val words = sent.map(_.getWord)
      val golds = sent.map(_.getTag)

      // println("PREDICT ON SENT: " + words.mkString(", "))
      val preds = predict(words)
      val (t, c) = accuracy(golds, preds)
      total += t
      correct += c

      printCoNLLOutput(pw, words, golds, preds)
    }

    pw.close()
    logger.info(s"Accuracy on ${sentences.length} $name sentences: " + correct.toDouble / total)
  }

  /**
    * Predict the sequence tags that applies to the given sequence of words
    * @param words The input words
    * @return The predicted sequence of tags
    */
  def predict(words:Array[String]):Array[String] = synchronized {
    // Note: this block MUST be synchronized. Currently the computational graph in DyNet is a static variable.
    val emissionScores:Array[Array[Float]] = synchronized {
      ComputationGraph.renew()
      emissionScoresToArrays(emissionScoresAsExpressions(words, doDropout = false)) // these scores do not have softmax
    }

    val tags = new ArrayBuffer[String]()
    if(greedyInference) {
      val tagIds = greedyPredict(emissionScores)
      for (tid <- tagIds) tags += model.i2t(tid)
    } else {
      val transitionMatrix: Array[Array[Float]] =
        transitionMatrixToArrays(model.T, model.t2i.size)

      val tagIds = viterbi(emissionScores, transitionMatrix, model.t2i.size, model.t2i(START_TAG), model.t2i(STOP_TAG))
      for (tid <- tagIds) tags += model.i2t(tid)
    }
    tags.toArray
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

  def mkEmbedding(word: String):Expression =
    LstmUtils.mkWordEmbedding(word,
      model.w2i, model.lookupParameters,
      model.c2i, model.charLookupParameters,
      model.charFwRnnBuilder, model.charBwRnnBuilder)

  def save(baseFilename: String): Unit = model.save(baseFilename)
}

class LstmCrfParameters(
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

    if(LstmCrf.USE_DOMAIN_CONSTRAINTS) {
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

  def initializeEmbeddings(w2v:Word2Vec): Unit = {
    logger.debug("Initializing DyNet embedding parameters...")
    for(word <- w2v.matrix.keySet){
      lookupParameters.initialize(w2i(word), new FloatVector(ArrayMath.toFloatArray(w2v.matrix(word))))
    }
    logger.debug(s"Completed initializing embedding parameters for a vocabulary of size ${w2v.matrix.size}.")
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

  def save(modelFilename: String): Unit = {
    val dynetFilename = mkDynetFilename(modelFilename)
    val x2iFilename = mkX2iFilename(modelFilename)

    new CloseableModelSaver(dynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(parameters, "/all")
    }

    Serializer.using(LstmUtils.newPrintWriter(x2iFilename)) { printWriter =>
      val dim = lookupParameters.dim().get(0)

      LstmUtils.save(printWriter, w2i, "w2i")
      LstmUtils.save(printWriter, t2i, "t2i")
      LstmUtils.save(printWriter, c2i, "c2i")
      LstmUtils.save(printWriter, dim, "dim")
    }
  }
}

object LstmCrfParameters {
  val RNN_STATE_SIZE = 50
  val NONLINEAR_SIZE = 32
  val RNN_LAYERS = 1
  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16

  def load(baseFilename: String): LstmCrfParameters = {
    val dynetFilename = mkDynetFilename(baseFilename)
    val x2iFilename = mkX2iFilename(baseFilename)
    val (w2i, t2i, c2i, dim) = Serializer.using(LstmUtils.newSource(x2iFilename)) { source =>
      val byLineStringMapBuilder = new LstmUtils.ByLineStringMapBuilder()
      val byLineCharMapBuilder = new LstmUtils.ByLineCharMapBuilder()
      val lines = source.getLines()
      val w2i = byLineStringMapBuilder.build(lines)
      val t2i = byLineStringMapBuilder.build(lines)
      val c2i = byLineCharMapBuilder.build(lines)
      val dim = new LstmUtils.ByLineIntBuilder().build(lines)

      (w2i, t2i, c2i, dim)
    }
    val model = {
      val model = mkParams(w2i, t2i, c2i, dim)
      new ModelLoader(dynetFilename).populateModel(model.parameters, "/all")
      model
    }

    model
  }

  protected def mkParams(w2i:Map[String, Int], t2i:Map[String, Int], c2i:Map[Char, Int], embeddingDim:Int): LstmCrfParameters = {
    val parameters = new ParameterCollection()
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(embeddingDim))
    val embeddingSize = embeddingDim + 2 * CHAR_RNN_STATE_SIZE
    val fwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)
    val bwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)
    val H = parameters.addParameters(Dim(NONLINEAR_SIZE, 2 * RNN_STATE_SIZE))
    val O = parameters.addParameters(Dim(t2i.size, NONLINEAR_SIZE))
    val i2t = fromIndexToString(t2i)
    val T = mkTransitionMatrix(parameters, t2i, i2t)
    logger.debug("Created parameters.")

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)

    new LstmCrfParameters(w2i, t2i, i2t, c2i,
      parameters, lookupParameters, fwBuilder, bwBuilder, H, O, T,
      charLookupParameters, charFwBuilder, charBwBuilder)
  }

  def create(w2i: Map[String, Int], t2i: Map[String, Int], c2i: Map[Char, Int], w2v: Word2Vec): LstmCrfParameters = {
    val model = mkParams(w2i, t2i, c2i, w2v.dimensions)

    model.initializeEmbeddings(w2v)
    model.initializeTransitions()
    model
  }
}

object LstmCrf {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmCrf])

  val EPOCHS = 2
  val DROPOUT_PROB = 0.1f
  val DO_DROPOUT = true
  val USE_DOMAIN_CONSTRAINTS = true

  case class InitializationParams(
    trainSentences: Array[Array[Row]],
    embeddingsFile: String,
    docFreqFileName: Option[String],
    minDocFreq: Int
  )

  def apply(baseFilename: String, greedyInference: Boolean = false): LstmCrf = {
    // make sure DyNet is initialized!
    Initialize.initialize(Map("random-seed" -> RANDOM_SEED))

    val model = LstmCrfParameters.load(baseFilename)
    val rnn = new LstmCrf(greedyInference, model)

    rnn
  }

  def main(args: Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)
    if(props.size() < 2) {
      usage()
      System.exit(1)
    }

    val greedy = StringUtils.getBool(props, "greedy", default = false)
    if(greedy) logger.debug("Using greedy inference!")

    if(props.containsKey("train") && props.containsKey("embed")) {
      logger.debug("Starting training procedure...")

      val trainSentences = ColumnReader.readColumns(props.getProperty("train"))
      val initializationParams = {
        val docFreqFileName: Option[String] =
          if (props.containsKey("docfreq")) Some(props.getProperty("docfreq"))
          else None
        val minDocFreq: Int = StringUtils.getInt(props, "minfreq", 100)
        val embeddingsFile = props.getProperty("embed")

        LstmCrf.InitializationParams(
          trainSentences, embeddingsFile, docFreqFileName, minDocFreq
        )
      }
      val rnn = new LstmCrf(greedy, initializationParams)

      val devSentences =
        if(props.containsKey("dev"))
          Some(ColumnReader.readColumns(props.getProperty("dev")))
        else
          None

      rnn.train(trainSentences, devSentences)

      if(props.containsKey("model")) {
        val modelFilePrefix = props.getProperty("model")
        rnn.save(modelFilePrefix)
      }
    }

    if(props.containsKey("test") && props.containsKey("model")) {
      logger.debug("Starting evaluation procedure...")
      val testSentences = ColumnReader.readColumns(props.getProperty("test"))
      val rnn = LstmCrf(props.getProperty("model"), greedy)
      rnn.evaluate(testSentences)
    }
  }

  def usage(): Unit = {
    println("Usage: " + classOf[LstmCrf].getName + " <ARGUMENTS>")
    println("Accepted arguments:")
    println("\t-train <two-column training corpus in the CoNLL BIO or IO format>")
    println("\t-embed <embeddings file in the word2vec format")
    println("\t-model <prefix of the model file name>")
    println("\t-dev <two-column development corpus in the CoNLL BIO or IO format>")
    println("\t-test <two-column test corpus in the CoNLL BIO or IO format>")
    println("\t-docfreq <file containing document frequency counts of vocabulary terms> (OPTIONAL)")
    println("\t-minfreq <minimum frequency threshold for a word to be included in the vocabulary; default = 100> (OPTIONAL)")
  }
}
