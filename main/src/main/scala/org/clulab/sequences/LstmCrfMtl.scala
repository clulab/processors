package org.clulab.sequences

import java.io.{FileWriter, PrintWriter}

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{ComputationGraph, Dim, Expression, ExpressionVector, FloatVector, LookupParameter, Parameter, ParameterCollection, RMSPropTrainer}
import edu.cmu.dynet.Expression.{lookup, parameter, randomNormal}
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser
import org.clulab.sequences.LstmCrfMtl._
import org.clulab.sequences.LstmUtils._
import org.clulab.lm.{FlairLM, LM, RnnLM}
import org.clulab.struct.Counter
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Implements a multi-task learning (MTL) framework around a RnnLM
  * @author Mihai
  */
class LstmCrfMtl(val taskManagerOpt: Option[TaskManager], lstmCrfMtlParametersOpt: Option[LstmCrfMtlParameters] = None) {
  /** Stores the DyNet parameters required by all tasks */
  val model: LstmCrfMtlParameters = lstmCrfMtlParametersOpt.getOrElse(initialize())

  if(taskManagerOpt.isDefined) {
    // This taskManager might not be suitable for the model.
    require(taskManagerOpt.get.taskCount == model.taskCount)
  }

  // Use this carefully. That is, only when taskManagerOpt.isDefined
  def taskManager: TaskManager = {
    assert(taskManagerOpt.isDefined)
    taskManagerOpt.get
  }

  protected def initialize(): LstmCrfMtlParameters = {
    // If we're here, we must have a task manager, because we will train and/or test all these tasks
    require(taskManagerOpt.isDefined)

    val parameters = new ParameterCollection()
    val lm = mkLM(taskManager.lmFileName, parameters)

    val t2is = mkVocabs()
    logger.debug(s"Tag vocabulary has:")
    for(i <- t2is.indices) {
      logger.debug(s"  ${t2is(i).size} entries for task ${taskManager.tasks(i).taskNumber}")
    }

    val model = LstmCrfMtlParameters.create(
      taskManager.taskCount, parameters, lm, t2is, taskManager.greedyInferences)
    logger.debug("Completed initialization.")
    model
  }

  private def mkVocabs(): Array[Map[String, Int]] = {
    val tags = new Array[Counter[String]](taskManager.taskCount)
    for(i <- tags.indices) tags(i) = new Counter[String]()

    for(tid <- taskManager.indices) {
      for (sentence <- taskManager.tasks(tid).trainSentences) {
        for (token <- sentence) {
          tags(tid) += token.getTag
        }
      }
    }

    for(tid <- tags.indices) {
      tags(tid) += START_TAG
      tags(tid) += STOP_TAG
    }

    val t2is = new Array[Map[String, Int]](taskManager.taskCount)
    for(tid <- taskManager.indices) {
      t2is(tid) = tags(tid).keySet.toList.sorted.zipWithIndex.toMap
    }

    t2is
  }

  def train(modelNamePrefix:String):Unit = {
    require(taskManagerOpt.isDefined)

    val trainer = SafeTrainer(new RMSPropTrainer(model.parameters))

    var cummulativeLoss = 0.0
    var numTagged = 0
    var sentCount = 0
    val rand = new Random(RANDOM_SEED)

    var maxAvgAcc = 0.0
    var bestEpoch = 0

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
        //println(s"TRAIN SENTENCE #$sentCount: ${words.mkString(" ")}")
        val emissionScores = emissionScoresAsExpressions(words, taskId, doDropout = DO_DROPOUT)

        // get the gold tags for this sentence
        val goldTagIds = toIds(sentence.map(_.getTag), model.t2is(taskId))

        var loss =
          if(model.greedyInference(taskId)) {
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

        if(taskManager.tasks(taskId).taskWeight != 1.0)
          loss = loss * Expression.input(taskManager.tasks(taskId).taskWeight)

        cummulativeLoss += loss.value().toFloat
        numTagged += sentence.length

        if(sentCount % 1000 == 0) {
          logger.info("Cummulative loss: " + cummulativeLoss / numTagged)
          cummulativeLoss = 0.0
          numTagged = 0
        }

        // backprop
        ComputationGraph.backward(loss)
        trainer.update(model.parameters)
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

      if(avgAcc > maxAvgAcc) {
        maxAvgAcc = avgAcc
        bestEpoch = epoch
      }

      // save model after each epoch
      save(s"$modelNamePrefix-epoch$epoch")
    }

    logger.info(s"The best epoch was epoch $bestEpoch with an average accuracy of $maxAvgAcc.")
  }

  def uniqueWords():Unit = {
    require(taskManagerOpt.isDefined)
    val rand = new Random(RANDOM_SEED)
    val sentenceIterator = taskManager.getSentences(rand)

    val uniqueWords = new Counter[String]()
    var sentCount = 0
    for(metaSentence <- sentenceIterator) {
      val sentence = metaSentence._2
      val words = sentence.map(_.getWord)
      for(word <- words) uniqueWords.incrementCount(word.toLowerCase())
      sentCount += 1
    }

    logger.info(s"Found ${uniqueWords.size} unique words in $sentCount training sentences.")
    val pw = new PrintWriter("unique_words.txt")
    for(word <- uniqueWords.sorted(descending = true)) {
      pw.println(word._1 + " " + word._2.toInt)
    }
    pw.close()
  }

  def test(): Unit = {
    require(taskManagerOpt.isDefined)

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
    var sentCount = 0

    val pw =
      if(epoch >= 0) new PrintWriter(new FileWriter(s"task$taskNumber.dev.output.$epoch"))
      else new PrintWriter(new FileWriter(s"task$taskNumber.test.output"))
    logger.debug(s"Started evaluation on the $name dataset for task $taskNumber ($taskName)...")
    for(sent <- sentences) {
      sentCount += 1
      val words = sent.map(_.getWord)
      val golds = sent.map(_.getTag)

      // println("PREDICT ON SENT: " + words.mkString(", "))
      val preds = predict(taskId, words)
      val (t, c) = accuracy(golds, preds)
      total += t
      correct += c

      /*
      if(sentCount % 10 == 0) {
        val crtAcc = correct.toDouble / total
        logger.debug(s"Processed $sentCount sentences. Current accuracy is $crtAcc.")
      }
      */

      printCoNLLOutput(pw, words, golds, preds)
    }

    pw.close()
    val acc = correct.toDouble / total
    logger.info(s"Accuracy on ${sentences.length} $name sentences for task $taskNumber ($taskName): $acc")

    acc
  }

  /**
   * Predict the sequence tags that applies to the given sequence of words for one given task
   * @param taskId: id of the current task to use
   * @param words The input words
   * @return The predicted sequence of tags
   */
  def predict(taskId:Int, words:Array[String]):Array[String] = {
    // Note: this block MUST be synchronized. Currently the computational graph in DyNet is a static variable.
    val emissionScores:Array[Array[Float]] = synchronized {
      ComputationGraph.renew()
      emissionScoresToArrays(emissionScoresAsExpressions(words, taskId, doDropout = false)) // these scores do not have softmax
    }

    val tags = new ArrayBuffer[String]()
    if(model.greedyInference(taskId)) {
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
   * Predicts the sequence of tags that applies to the given sequence of words for ALL tasks
   * @param words The input words
   * @return The predicted sequence of tags for all tasks
   */
  def predictJointly(words:Array[String]): Array[Array[String]] = {
    // Note: this block MUST be synchronized. Currently the computational graph in DyNet is a static variable.
    val emissionScoresAllTasks:Array[Array[Array[Float]]] = synchronized {
      ComputationGraph.renew()
      emissionScoresToArraysAllTasks(emissionScoresAsExpressionsAllTasks(words, doDropout = false)) // these scores do not have softmax
    }

    // stores the sequence of best tags for ALL tasks
    val tagsAllTasks = new Array[Array[String]](model.taskCount)

    // generate the sequence of argmax tags for each task, using its own inference procedure
    for(taskId <- 0 until model.taskCount) {
      val tags = new ArrayBuffer[String]()
      if(model.greedyInference(taskId)) {
        val tagIds = greedyPredict(emissionScoresAllTasks(taskId))
        for (tagId <- tagIds) tags += model.i2ts(taskId)(tagId)
      } else {
        val transitionMatrix: Array[Array[Float]] =
          transitionMatrixToArrays(model.Ts(taskId), model.t2is(taskId).size)

        val tagIds = viterbi(emissionScoresAllTasks(taskId), transitionMatrix,
          model.t2is(taskId).size, model.t2is(taskId)(START_TAG), model.t2is(taskId)(STOP_TAG))
        for (tagId <- tagIds) tags += model.i2ts(taskId)(tagId)
      }

      tagsAllTasks(taskId) = tags.toArray
    }

    tagsAllTasks
  }

  /**
   * Generates tag emission scores for the words in this sequence, stored as Expressions, for one given task
   * @param words One training or testing sentence
   */
  def emissionScoresAsExpressions(words: Array[String], taskId:Int, doDropout:Boolean): ExpressionVector = {
    val states = model.lm.mkEmbeddings(words, doDropout).toArray

    // this is the feed forward network that is specific to each task
    val H = parameter(model.Hs(taskId))

    val emissionScores = new ExpressionVector()
    for(s <- states) {
      var l1 = H * s
      if(doDropout) {
        l1 = Expression.dropout(l1, DROPOUT_PROB)
      }
      emissionScores.add(l1)
    }

    emissionScores
  }

  /**
   * Generates tag emission scores for the words in this sequence, stored as Expressions, for all tasks
   * @param words One training or testing sentence
   * @return The scores for all tasks
   */
  def emissionScoresAsExpressionsAllTasks(words: Array[String], doDropout:Boolean): Array[ExpressionVector] = {
    val states = model.lm.mkEmbeddings(words, doDropout).toArray

    val emissionScoresAllTasks = new Array[ExpressionVector](model.taskCount)

    // this is the feed forward network that is specific to each task
    for(taskId <- 0 until model.taskCount) {
      val H = parameter(model.Hs(taskId))

      val emissionScores = new ExpressionVector()
      for (s <- states) {
        var l1 = H * s
        if(doDropout) {
          l1 = Expression.dropout(l1, DROPOUT_PROB)
        }
        emissionScores.add(l1)
      }

      emissionScoresAllTasks(taskId) = emissionScores
    }

    emissionScoresAllTasks
  }

  def save(baseFilename: String): Unit = model.save(baseFilename)
}

class LstmCrfMtlParameters(
  val t2is:Array[Map[String, Int]], // one per task
  val i2ts:Array[Array[String]], // one per task
  val parameters:ParameterCollection,
  val lm:LM, // shared by all tasks
  val Hs:Array[Parameter], // one per task
  val Ts:Array[LookupParameter], // transition matrix for Viterbi; T[i][j] = transition *to* i *from* j, one per task
  val greedyInference:Array[Boolean]) {

  val taskCount: Int = t2is.length
  val indices: Range = t2is.indices

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
    val dynetFilename = mkDynetFilename(baseFilename)
    val x2iFilename = mkX2iFilename(baseFilename)

    new CloseableModelSaver(dynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(parameters, "/all")
    }

    Serializer.using(LstmUtils.newPrintWriter(x2iFilename)) { printWriter =>
      lm.saveX2i(printWriter)

      LstmUtils.save(printWriter, taskCount, "taskCount")
      LstmUtils.save(printWriter, greedyInference, "greedyInference")

      t2is.zipWithIndex.foreach { case (t2i, index) =>
        LstmUtils.save(printWriter, t2i, s"t2i($index)")
      }
    }
  }
}

object LstmCrfMtlParameters {

  def load(baseFilename: String): LstmCrfMtlParameters = {
    logger.debug(s"Loading MTL model from $baseFilename...")
    val dynetFilename = mkDynetFilename(baseFilename)
    val x2iFilename = mkX2iFilename(baseFilename)
    val parameters = new ParameterCollection()
    val (lm, taskCount, t2is, greedyInferences) = Serializer.using(LstmUtils.newSource(x2iFilename)) { source =>
      val lines = source.getLines()

      val lm = mkLM(lines, parameters)

      val byLineStringMapBuilder = new LstmUtils.ByLineStringMapBuilder()
      val byLineArrayBuilder = new LstmUtils.ByLineArrayBuilder()
      val taskCount = new LstmUtils.ByLineIntBuilder().build(lines)
      val greedyInferences:Array[Boolean] = byLineArrayBuilder.build(lines).map(_.toBoolean)
      val t2is = 0.until(taskCount).map { _ =>
        byLineStringMapBuilder.build(lines)
      }.toArray

      //println(s"${t2is(0).keySet.size} TAGS FOR TASK 0: " + t2is(0).keySet.toList.sorted.mkString(", "))
      //println(s"${t2is(1).keySet.size} TAGS FOR TASK 1: " + t2is(1).keySet.toList.sorted.mkString(", "))

      (lm, taskCount, t2is, greedyInferences)
    }

    val model = {
      val model = mkParams(taskCount, parameters, lm, t2is, greedyInferences)
      LstmUtils.loadParameters(dynetFilename, model.parameters)
      model
    }

    logger.debug("MTL loading complete.")
    model
  }

  protected def mkParams(taskCount: Int,
                         parameters: ParameterCollection,
                         lm: LM,
                         t2is: Array[Map[String, Int]],
                         greedyInferences: Array[Boolean]): LstmCrfMtlParameters = {

    // These parameters are unique for each task
    val Hs = new Array[Parameter](taskCount)
    val i2ts = new Array[Array[String]](taskCount)
    val Ts = new Array[LookupParameter](taskCount)
    for(tid <- 0.until(taskCount)) {
      //println(s"Creating parameters for task #$tid, using ${t2is(tid).size} labels, and ${lm.dimensions} LM dimensions.")
      Hs(tid) = parameters.addParameters(Dim(t2is(tid).size, lm.dimensions))
      i2ts(tid) = fromIndexToString(t2is(tid))
      Ts(tid) = mkTransitionMatrix(parameters, t2is(tid), i2ts(tid))
    }
    logger.debug("Created parameters.")

    new LstmCrfMtlParameters(t2is, i2ts,
      parameters, lm,
      Hs, Ts, greedyInferences)
  }

  def create(taskCount: Int,
      parameters: ParameterCollection,
      lm: LM,
      t2is: Array[Map[String, Int]],
      greedyInferences: Array[Boolean]): LstmCrfMtlParameters = {
    val model = mkParams(taskCount, parameters, lm, t2is, greedyInferences)
    model.initializeTransitions()
    model
  }
}

object LstmCrfMtl {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmCrfMtl])

  val DROPOUT_PROB = 0.2f
  val DO_DROPOUT = true

  /** Use domain constraints in the transition probabilities? */
  val USE_DOMAIN_CONSTRAINTS = true

  def mkLM(lmFileName:String, parameterCollection: ParameterCollection): LM = {
    if(LM_TYPE == "rnnlm") RnnLM.load(lmFileName, parameterCollection)
    else if(LM_TYPE == "flair") FlairLM.load(lmFileName, parameterCollection)
    else throw new RuntimeException(s"ERROR: unknown LM type for model file $lmFileName!")
  }

  def mkLM(linesIterator:Iterator[String], parameterCollection: ParameterCollection): LM = {
    if(LM_TYPE == "rnnlm") RnnLM.load(linesIterator, parameterCollection)
    else if(LM_TYPE == "flair") FlairLM.load(linesIterator, parameterCollection)
    else throw new RuntimeException(s"ERROR: unknown LM type!")
  }

  def apply(modelFilenamePrefix: String, taskManager: TaskManager): LstmCrfMtl = {
    initializeDyNet()
    val model = LstmCrfMtlParameters.load(modelFilenamePrefix)
    val mtl = new LstmCrfMtl(Some(taskManager), Some(model))
    mtl
  }

  def apply(modelFilenamePrefix: String): LstmCrfMtl = {
    initializeDyNet()
    val model = LstmCrfMtlParameters.load(modelFilenamePrefix)
    val mtl = new LstmCrfMtl(None, Some(model))
    mtl
  }

  val LM_TYPE = "rnnlm" // "flair"

  def main(args: Array[String]): Unit = {
    val runMode = "train" // "train", "test", or "shell"
    initializeDyNet()
    val modelName = "mtl-en-srl" // "mtl-en-ner" // "mtl-en-pos-chunk"
    val configName = "mtl-en-srl" // "mtl-en-ner" // "mtl-en-pos-chunk"

    if(runMode == "train") {
      val config = ConfigFactory.load(configName)
      val taskManager = new TaskManager(config)

      val mtl = new LstmCrfMtl(Some(taskManager))
      mtl.train(modelName)
      // mtl.test()
    } else if(runMode == "test") {
      val config = ConfigFactory.load(configName)
      val taskManager = new TaskManager(config)

      // load the model from disk and test again
      val mtlFromDisk = LstmCrfMtl(modelName, taskManager)
      mtlFromDisk.test() // These results match the original ones exactly

      // mtlFromDisk.save("mtl2") // These files match the original ones exactly
    } else if(runMode == "shell") {
      val mtlFromDisk = LstmCrfMtl(modelName)
      val sh = new MTLShell(mtlFromDisk)
      sh.shell()
    } else if(runMode == "wordstats") {
      val config = ConfigFactory.load(configName)
      val taskManager = new TaskManager(config)

      val mtl = new LstmCrfMtl(Some(taskManager))
      mtl.uniqueWords()
    } else {
      throw new RuntimeException(s"ERROR: unknown run mode $runMode!")
    }
  }
}
