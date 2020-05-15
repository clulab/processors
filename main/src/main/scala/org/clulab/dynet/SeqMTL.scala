package org.clulab.dynet

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{AdamTrainer, ComputationGraph, Expression, ParameterCollection}
import org.clulab.dynet.Utils._
import org.clulab.sequences.Row
import org.clulab.struct.Counter
import org.clulab.utils.StringUtils
import org.slf4j.{Logger, LoggerFactory}
import SeqMTL._

import scala.util.Random

/**
 * Implement multi-task learning (MTL) for sequence modeling
 *
 * @author Mihai
 */
class SeqMTL(val taskManagerOpt: Option[TaskManager],
             val parameters: ParameterCollection,
             modelOpt: Option[Array[Layers]]) {
  // One Layers object per task; model(0) contains the Layers shared between all tasks (if any)
  protected val model: Array[Layers] = modelOpt.getOrElse(initialize())

  // One combined Layers object per task, which merges the shared Layers with the task-specific Layers for each task
  protected val flows: Array[Layers] = mkFlows(model)

  // Use this carefully. That is, only when taskManagerOpt.isDefined
  def taskManager: TaskManager = {
    assert(taskManagerOpt.isDefined)
    taskManagerOpt.get
  }

  protected def initialize(): Array[Layers] = {
    // this should only be called during training, when the task manager should be defined
    require(taskManagerOpt.isDefined)

    // word and label vocabularies
    val (taskWords, taskLabels) = mkVocabularies()

    // 0 reserved for the shared Layers object
    val layersPerTask = new Array[Layers](taskManager.taskCount + 1)
    layersPerTask(0) =
      Layers(taskManager, "mtl.layers", parameters, taskWords(0), None)
    for (i <- taskManager.indices) {
      layersPerTask(i + 1) =
        Layers(taskManager, s"mtl.task${i + 1}.layers",
          parameters, taskWords(i + 1), Some(taskLabels(i + 1)))
    }
    for(i <- layersPerTask.indices) {
      logger.debug(s"Summary of layersPerTask($i):")
      logger.debug(layersPerTask(i).toString)
    }

    layersPerTask
  }

  protected def mkFlows(layers: Array[Layers]): Array[Layers] = {
    val flows = new Array[Layers](taskManager.taskCount)
    assert(flows.length + 1 == layers.length)

    for(i <- flows.indices) {
      flows(i) = Layers.merge(layers(0), layers(i + 1))
    }

    flows
  }

  protected def mkVocabularies(): (Array[Counter[String]], Array[Counter[String]]) = {
    // index 0 reserved for the shared Layers; tid + 1 corresponds to each task
    val labels = new Array[Counter[String]](taskManager.taskCount + 1)
    for (i <- 1 until labels.length) { // labels(0) not used, since only task-specific layers have a final layer
      labels(i) = new Counter[String]()
      labels(i) += START_TAG
      labels(i) += STOP_TAG
    }
    val words = new Array[Counter[String]](taskManager.taskCount + 1)
    for (i <- words.indices) words(i) = new Counter[String]()

    for (tid <- taskManager.indices) {
      for (sentence <- taskManager.tasks(tid).trainSentences) {
        for (token <- sentence) {
          words(tid + 1) += token.getWord
          words(0) += token.getWord
          if (taskManager.tasks(tid).taskType == TaskManager.TYPE_SRL) {
            for (j <- Row.ARG_START until token.tokens.length) {
              labels(tid + 1) += token.get(j)
            }
          } else { // basic tasks with the labels in the second column
            labels(tid + 1) += token.getLabel
          }
        }
      }
    }

    (words, labels)
  }

  def train(modelNamePrefix: String): Unit = {
    require(taskManagerOpt.isDefined)

    val trainer = SafeTrainer(new AdamTrainer(parameters)) // RMSPropTrainer(parameters))

    var cummulativeLoss = 0.0
    var numTagged = 0
    var sentCount = 0
    val rand = new Random(RANDOM_SEED)

    var maxAvgAcc = 0.0
    var maxAvgF1 = 0.0
    var bestEpoch = 0
    var epochPatience = taskManager.epochPatience

    for(epoch <- 0 until taskManager.maxEpochs if epochPatience > 0) {
      logger.info(s"Started epoch $epoch.")
      // this fetches randomized training sentences from all tasks
      val sentenceIterator = taskManager.getSentences(rand)

      for(metaSentence <- sentenceIterator) {
        val taskId = metaSentence._1
        val sentence = metaSentence._2
        sentCount += 1
        ComputationGraph.renew()

        val words = sentence.map(_.getWord)
        val posTags =
          if(flows(taskId).needsPosTags) Some(sentence.map(_.getPosTag).toIndexedSeq)
          else None

        val goldLabels = sentence.map(_.getLabel)

        val lossOpt =
          if(taskManager.tasks(taskId).isBasic) {
            Some(flows(taskId).loss(words, posTags, None, goldLabels))
          } else if(taskManager.tasks(taskId).isSrl) {
            // token positions for the predicates in this sentence
            val predicatePositions = sentence.map(_.getLabel).zipWithIndex.filter(_._1 == "B-P").map(_._2)
            // TODO
            None
          } else {
            throw new RuntimeException(s"ERROR: unknown task type ${taskManager.tasks(taskId).taskType}!")
          }

        if(lossOpt.isDefined) {
          var loss = lossOpt.get

          // task weighting
          if (taskManager.tasks(taskId).taskWeight != 1.0)
            loss = loss * Expression.input(taskManager.tasks(taskId).taskWeight)

          // for stats
          cummulativeLoss += loss.value().toFloat

          // backprop
          ComputationGraph.backward(loss)
          trainer.update(parameters)
        }

        numTagged += sentence.length

        if(sentCount % 1000 == 0) {
          logger.info("Cummulative loss: " + cummulativeLoss / numTagged)
          cummulativeLoss = 0.0
          numTagged = 0
        }
      }
    }
  }

  def predictJointly(words: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
    null // TODO
  }
}

object SeqMTL {
  val logger:Logger = LoggerFactory.getLogger(classOf[SeqMTL])

  def apply(modelFilenamePrefix: String): SeqMTL = {
    initializeDyNet()

    // TODO
    null
  }

  def main(args: Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)
    initializeDyNet() // autoBatch = true, mem = "1660,1664,2496,1400")

    if(props.containsKey("train")) {
      assert(props.containsKey("conf"))
      val configName = props.getProperty("conf")
      val config = ConfigFactory.load(configName)
      val parameters = new ParameterCollection()
      val taskManager = new TaskManager(config)
      val modelName = props.getProperty("train")

      val mtl = new SeqMTL(Some(taskManager), parameters, None)
      mtl.train(modelName)
    }

    else if(props.containsKey("test")) {

    }
  }
}
