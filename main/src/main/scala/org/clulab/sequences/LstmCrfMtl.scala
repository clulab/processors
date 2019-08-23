package org.clulab.sequences

import java.io.{FileWriter, PrintWriter}

import com.typesafe.config.ConfigFactory
import org.slf4j.{Logger, LoggerFactory}
import edu.cmu.dynet.{ComputationGraph, Dim, Expression, ExpressionVector, FloatVector, Initialize, LookupParameter, LstmBuilder, Parameter, ParameterCollection, RMSPropTrainer, RnnBuilder}
import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.struct.Counter

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import edu.cmu.dynet.Expression.{lookup, parameter, randomNormal}
import org.clulab.sequences.ArrayMath.toFloatArray
import LstmUtils._
import LstmCrfMtl._
import edu.cmu.dynet.ModelLoader
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser
import org.clulab.sequences.LstmCrfMtl.logger
import org.clulab.utils.Serializer

import scala.util.Random

/**
  * Implements a multi-task learning (MTL) framework around the biLSTM-CRF of Lample et al. (2016)
  * We use GloVe embeddings and learned character embeddings
  * @author Mihai
  */
class LstmCrfMtl(val taskManager: TaskManager, lstmCrfMtlParametersOpt: Option[LstmCrfMtlParameters] = None) {
  /** Stores the DyNet parameters required by all tasks */
  val model: LstmCrfMtlParameters = lstmCrfMtlParametersOpt.getOrElse(initialize())

  // This taskManager might not be suitable for the model.
  require(taskManager.taskCount == model.taskCount)

  protected def initialize(): LstmCrfMtlParameters = {
    val w2v = loadEmbeddings(Some(taskManager.docFreqFileName), taskManager.minWordFreq, taskManager.embeddingsFileName)
    val (w2i, c2i, t2is) = mkVocabs(w2v)

    logger.debug(s"Word vocabulary has ${w2i.size} entries (including 1 for unknown).")
    logger.debug(s"Character vocabulary has ${c2i.size} entries.")
    logger.debug(s"Tag vocabulary has:")
    for(i <- t2is.indices) {
      logger.debug(s"  ${t2is(i).size} entries for task ${taskManager.tasks(i).taskNumber}")
    }

    // TODO Perhaps put this elsewhere, like in main.
    logger.debug("Initializing DyNet...")
    Initialize.initialize(Map("random-seed" -> RANDOM_SEED))
    val model = LstmCrfMtlParameters.create(taskManager.taskCount, w2i, c2i, t2is, w2v)
    logger.debug("Completed initialization.")
    model
  }

  private def mkVocabs(w2v:Word2Vec): (Map[String, Int], Map[Char, Int], Array[Map[String, Int]]) = {
    val tags = new Array[Counter[String]](taskManager.taskCount)
    for(i <- tags.indices) tags(i) = new Counter[String]()
    val chars = new mutable.HashSet[Char]()

    for(tid <- taskManager.indices) {
      for (sentence <- taskManager.tasks(tid).trainSentences) {
        for (token <- sentence) {
          val word = token.getWord
          for (i <- word.indices) {
            chars += word.charAt(i)
          }
          tags(tid) += token.getTag
        }
      }
    }

    val commonWords = new ListBuffer[String]
    commonWords += UNK_WORD // the word at position 0 is reserved for unknown words
    for(w <- w2v.matrix.keySet.toList.sorted) {
      commonWords += w
    }

    for(tid <- tags.indices) {
      tags(tid) += START_TAG
      tags(tid) += STOP_TAG
    }

    val w2i = commonWords.zipWithIndex.toMap
    val c2i = chars.toList.sorted.zipWithIndex.toMap
    val t2is = new Array[Map[String, Int]](taskManager.taskCount)
    for(tid <- taskManager.indices) {
      t2is(tid) = tags(tid).keySet.toList.sorted.zipWithIndex.toMap
    }

    (w2i, c2i, t2is)
  }

  def train():Unit = {
    val trainer = new RMSPropTrainer(model.parameters)
    var cummulativeLoss = 0.0
    var numTagged = 0
    var sentCount = 0
    val rand = new Random(RANDOM_SEED)

    for (epoch <- 0 until taskManager.totalEpochs) {
      logger.info(s"Started epoch $epoch.")
      // this fetches randomized training sentences from all tasks
      val sentenceIterator = taskManager.getSentences(rand)

      for(metaSentence <- sentenceIterator) {
        val taskId = metaSentence._1
        val sentence = metaSentence._2
        sentCount += 1
        ComputationGraph.renew()

        // predict tag emission scores for one sentence and the current task, from the biLSTM hidden states
        val words = sentence.map(_.getWord)
        val emissionScores = emissionScoresAsExpressions(words, taskId, doDropout = DO_DROPOUT)

        // get the gold tags for this sentence
        val goldTagIds = toTagIds(sentence.map(_.getTag), model.t2is(taskId))

        val loss =
          if(taskManager.tasks(taskId).greedyInference) {
            // greedy loss
            sentenceLossGreedy(emissionScores, goldTagIds)
          }
          else {
            // fetch the transition probabilities from the lookup storage
            val transitionMatrix = new ExpressionVector
            for(i <- 0 until model.t2is(taskId).size) {
              transitionMatrix.add(lookup(model.Ts(taskId), i))
            }

            // CRF loss
            sentenceLossCrf(emissionScores, transitionMatrix, goldTagIds, model.t2is(taskId))
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

      // check dev performance in this epoch, for all tasks
      var totalAcc = 0.0
      for(taskId <- 0 until taskManager.taskCount) {
        val taskName = taskManager.tasks(taskId).taskName
        val devSentences = taskManager.tasks(taskId).devSentences
        if(devSentences.nonEmpty) {
          totalAcc += evaluate(taskId, taskName, devSentences.get, epoch)
        }
      }
      val avgAcc = totalAcc / taskManager.taskCount
      logger.info(s"Average accuracy across ${taskManager.taskCount} tasks: $avgAcc")
    }
  }

  def test(): Unit = {
    // check final performance on the test dataset
    for(taskId <- 0 until taskManager.taskCount) {
      val taskName = taskManager.tasks(taskId).taskName
      val testSentences = taskManager.tasks(taskId).testSentences
      if(testSentences.nonEmpty) {
        evaluate(taskId, taskName, testSentences.get)
      }
    }
  }

  def evaluate(taskId:Int, taskName:String, sentences:Array[Array[Row]], epoch:Int): Double = {
    evaluate(taskId, taskName, sentences, "development", epoch)
  }

  def evaluate(taskId:Int, taskName:String, sentences:Array[Array[Row]]): Double = {
    evaluate(taskId, taskName, sentences, "testing", -1)
  }

  /** Logs accuracy score on devSentences; also saves the output in the file dev.output.<EPOCH> */
  def evaluate(taskId:Int, taskName:String, sentences:Array[Array[Row]], name:String, epoch:Int): Double = {
    var total = 0
    var correct = 0
    val taskNumber = taskId + 1

    val pw =
      if(epoch >= 0) new PrintWriter(new FileWriter(s"task$taskNumber.dev.output.$epoch"))
      else new PrintWriter(new FileWriter(s"task$taskNumber.test.output"))
    logger.debug(s"Started evaluation on the $name dataset for task $taskNumber ($taskName)...")
    for(sent <- sentences) {
      val words = sent.map(_.getWord)
      val golds = sent.map(_.getTag)

      // println("PREDICT ON SENT: " + words.mkString(", "))
      val preds = predict(taskId, words)
      val (t, c) = accuracy(golds, preds)
      total += t
      correct += c

      printCoNLLOutput(pw, words, golds, preds)
    }

    pw.close()
    val acc = correct.toDouble / total
    logger.info(s"Accuracy on ${sentences.length} $name sentences for task $taskNumber ($taskName): $acc")

    acc
  }

  /**
   * Predict the sequence tags that applies to the given sequence of words
   * @param words The input words
   * @return The predicted sequence of tags
   */
  def predict(taskId:Int, words:Array[String]):Array[String] = synchronized {
    // Note: this block MUST be synchronized. Currently the computational graph in DyNet is a static variable.
    val emissionScores:Array[Array[Float]] = synchronized {
      ComputationGraph.renew()
      emissionScoresToArrays(emissionScoresAsExpressions(words, taskId, doDropout = false)) // these scores do not have softmax
    }

    val tags = new ArrayBuffer[String]()
    if(taskManager.tasks(taskId).greedyInference) {
      val tagIds = greedyPredict(emissionScores)
      for (tid <- tagIds) tags += model.i2ts(taskId)(tid)
    } else {
      val transitionMatrix: Array[Array[Float]] =
        transitionMatrixToArrays(model.Ts(taskId), model.t2is(taskId).size)

      val tagIds = viterbi(emissionScores, transitionMatrix,
        model.t2is(taskId).size, model.t2is(taskId)(START_TAG), model.t2is(taskId)(STOP_TAG))
      for (tid <- tagIds) tags += model.i2ts(taskId)(tid)
    }
    tags.toArray
  }

  /**
   * Generates tag emission scores for the words in this sequence, stored as Expressions
   * @param words One training or testing sentence
   */
  def emissionScoresAsExpressions(words: Array[String], taskId:Int, doDropout:Boolean): ExpressionVector = {
    val embeddings = words.map(mkEmbedding)

    // this is the biLSTM over words that is shared across all tasks
    val fwStates = transduce(embeddings, model.fwRnnBuilder)
    val bwStates = transduce(embeddings.reverse, model.bwRnnBuilder).toArray.reverse
    assert(fwStates.size == bwStates.length)
    val states = concatenateStates(fwStates, bwStates).toArray
    assert(states.length == words.length)

    // this is the feed forward network that is specific to each task
    val H = parameter(model.Hs(taskId))
    val O = parameter(model.Os(taskId))

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

  /** Creates an overall word embedding by concatenating word and character embeddings */
  def mkEmbedding(word: String):Expression =
    LstmUtils.mkWordEmbedding(word,
      model.w2i, model.lookupParameters,
      model.c2i, model.charLookupParameters,
      model.charFwRnnBuilder, model.charBwRnnBuilder)

  def save(baseFilename: String): Unit = model.save(baseFilename)
}

class LstmCrfMtlParameters(
  val w2i:Map[String, Int],
  val c2i:Map[Char, Int],
  val t2is:Array[Map[String, Int]], // one per task
  val i2ts:Array[Array[String]], // one per task
  val parameters:ParameterCollection,
  val lookupParameters:LookupParameter,
  val fwRnnBuilder:RnnBuilder,
  val bwRnnBuilder:RnnBuilder,
  val charLookupParameters:LookupParameter,
  val charFwRnnBuilder:RnnBuilder,
  val charBwRnnBuilder:RnnBuilder,
  val Hs:Array[Parameter], // one per task
  val Os:Array[Parameter], // one per task
  val Ts:Array[LookupParameter]) { // transition matrix for Viterbi; T[i][j] = transition *to* i *from* j, one per task

  def taskCount:Int = t2is.length
  def indices:Range = t2is.indices

  def initializeEmbeddings(w2v:Word2Vec): Unit = {
    logger.debug("Initializing DyNet embedding parameters...")
    for(word <- w2v.matrix.keySet){
      lookupParameters.initialize(w2i(word), new FloatVector(toFloatArray(w2v.matrix(word))))
    }
    logger.debug(s"Completed initializing embedding parameters for a vocabulary of size ${w2v.matrix.size}.")
  }

  def initializeTransitions(): Unit = {
    // needs to be done separately for each task
    for(tid <- indices) {
      val startTag = t2is(tid)(START_TAG)
      val stopTag = t2is(tid)(STOP_TAG)

      for (i <- 0 until t2is(tid).size) {
        Ts(tid).initialize(i, initTransitionsTo(tid, i, t2is(tid).size, startTag, stopTag))
      }
    }
  }

  def initTransitionsTo(tid:Int, dst: Int, size:Int, startTag: Int, stopTag: Int): FloatVector = {
    val transScores = new Array[Float](size)

    for(i <- 0 until size) {
      transScores(i) = randomNormal(Dim(1)).value().toFloat() / size // pseudo Glorot
    }

    if(LstmCrfMtl.USE_DOMAIN_CONSTRAINTS) {
      // discourage transitions to START from anything
      if (dst == startTag) {
        for (i <- 0 until size)
          transScores(i) = LOG_MIN_VALUE
      } else {
        // discourage transitions to anything from STOP
        transScores(stopTag) = LOG_MIN_VALUE

        // discourage transitions to I-X from B-Y or I-Y
        val dstTag = i2ts(tid)(dst)
        if (dstTag.startsWith("I-")) {
          for (i <- 0 until size) {
            val srcTag = i2ts(tid)(i)
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

  def save(baseFilename: String): Unit = {
    val dynetFilename = LstmCrfMtlParameters.mkDynetFilename(baseFilename)
    val x2iFilename = LstmCrfMtlParameters.mkX2iFilename(baseFilename)

    new CloseableModelSaver(dynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(parameters, "/all")
    }

    Serializer.using(LstmUtils.newPrintWriter(x2iFilename)) { printWriter =>
      val dim = lookupParameters.dim().get(0)

      LstmUtils.save(printWriter, w2i, "w2i")
      LstmUtils.save(printWriter, c2i, "c2i")
      LstmUtils.save(printWriter, taskCount, "taskCount")
      t2is.zipWithIndex.foreach { case (t2i, index) =>
        LstmUtils.save(printWriter, t2i, s"t2i($index)")
      }
      LstmUtils.save(printWriter, dim, "dim")
    }
  }
}

object LstmCrfMtlParameters {
  val RNN_STATE_SIZE = 50
  val NONLINEAR_SIZE = 32
  val RNN_LAYERS = 1
  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 32
  val CHAR_RNN_STATE_SIZE = 16

  def mkDynetFilename(baseFilename: String): String = baseFilename + ".rnn"

  def mkX2iFilename(baseFilename: String): String = baseFilename + ".x2i"

  def load(baseFilename: String): LstmCrfMtlParameters = {
    val dynetFilename = mkDynetFilename(baseFilename)
    val x2iFilename = mkX2iFilename(baseFilename)
    val (taskCount, w2i, c2i, t2is, dim) = Serializer.using(LstmUtils.newSource(x2iFilename)) { source =>
      val byLineStringMapBuilder = new LstmUtils.ByLineStringMapBuilder()
      val byLineCharMapBuilder = new LstmUtils.ByLineCharMapBuilder()
      val lines = source.getLines()
      val w2i = byLineStringMapBuilder.build(lines)
      val c2i = byLineCharMapBuilder.build(lines)
      val taskCount = new LstmUtils.ByLineIntBuilder().build(lines)
      val t2is = 0.until(taskCount).map { _ =>
        byLineStringMapBuilder.build(lines)
      }.toArray
      val dim = new LstmUtils.ByLineIntBuilder().build(lines)

      (taskCount, w2i, c2i, t2is, dim)
    }
    val model = {
      val model = mkParams(taskCount, w2i, c2i, t2is, dim)
      new ModelLoader(dynetFilename).populateModel(model.parameters, "/all")
      model
    }

    model
  }

  protected def mkParams(taskCount: Int,
      w2i: Map[String, Int],
      c2i: Map[Char, Int],
      t2is: Array[Map[String, Int]],
      embeddingDim: Int): LstmCrfMtlParameters = {
    val parameters = new ParameterCollection()

    // These parameters correspond to the LSTM(s) shared by all tasks
    val lookupParameters = parameters.addLookupParameters(w2i.size, Dim(embeddingDim))
    val embeddingSize = embeddingDim + 2 * CHAR_RNN_STATE_SIZE
    val fwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)
    val bwBuilder = new LstmBuilder(RNN_LAYERS, embeddingSize, RNN_STATE_SIZE, parameters)

    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)

    // These parameters are unique for each task
    val Hs = new Array[Parameter](taskCount)
    val Os = new Array[Parameter](taskCount)
    val i2ts = new Array[Array[String]](taskCount)
    val Ts = new Array[LookupParameter](taskCount)
    for(tid <- 0.until(taskCount)) {
      Hs(tid) = parameters.addParameters(Dim(NONLINEAR_SIZE, 2 * RNN_STATE_SIZE))
      Os(tid) = parameters.addParameters(Dim(t2is(tid).size, NONLINEAR_SIZE))
      i2ts(tid) = fromIndexToString(t2is(tid))
      Ts(tid) = mkTransitionMatrix(parameters, t2is(tid), i2ts(tid))
    }
    logger.debug("Created parameters.")

    new LstmCrfMtlParameters(w2i, c2i, t2is, i2ts,
      parameters, lookupParameters,
      fwBuilder, bwBuilder,
      charLookupParameters, charFwBuilder, charBwBuilder,
      Hs, Os, Ts)
  }

  def create(taskCount: Int,
      w2i: Map[String, Int],
      c2i: Map[Char, Int],
      t2is: Array[Map[String, Int]],
      w2v: Word2Vec): LstmCrfMtlParameters = {
    val model = mkParams(taskCount, w2i, c2i, t2is, w2v.dimensions)

    model.initializeEmbeddings(w2v)
    model.initializeTransitions()
    model
  }
}

object LstmCrfMtl {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmCrfMtl])

  val DROPOUT_PROB = 0.1f
  val DO_DROPOUT = true
  /** Use domain constraints in the transition probabilities? */
  val USE_DOMAIN_CONSTRAINTS = true

  def apply(baseFilename: String, taskManager: TaskManager): LstmCrfMtl = {
    // make sure DyNet is initialized!
    Initialize.initialize(Map("random-seed" -> RANDOM_SEED))

    val model = LstmCrfMtlParameters.load(baseFilename)
    val mtl = new LstmCrfMtl(taskManager, Some(model))

    mtl
  }

  def main(args: Array[String]): Unit = {
    //
    // to set a custom config file add -Dconfig.file=/path/to/conf/file to the cmd line for sbt
    //
    val config = ConfigFactory.load()
    val taskManager = new TaskManager(config)
    val mtl = new LstmCrfMtl(taskManager)
    mtl.train()
    mtl.test()
    mtl.save("mtl")

    // load the model from disk and test again
//    val mtlFromDisk = LstmCrfMtl("mtl", taskManager)
//    mtlFromDisk.test() // These results match the original ones exactly
//    mtlFromDisk.save("mtl2") // These files match the original ones exactly
  }
}
