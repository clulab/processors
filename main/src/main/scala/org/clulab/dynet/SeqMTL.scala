package org.clulab.dynet

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{AdamTrainer, ParameterCollection}
import org.clulab.dynet.Utils.initializeDyNet
import org.clulab.sequences.Row
import org.clulab.struct.Counter
import org.clulab.utils.StringUtils
import org.slf4j.{Logger, LoggerFactory}

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
      Layers(taskManager.getConf, "mtl.layers", taskWords(0), None)
    for (i <- taskManager.indices) {
      layersPerTask(i + 1) =
        Layers(taskManager.getConf, s"mtl.task${i + 1}.layers",
          taskWords(i + 1), Some(taskLabels(i + 1)))
    }

    layersPerTask
  }

  protected def mkVocabularies(): (Array[Counter[String]], Array[Counter[String]]) = {
    // index 0 reserved for the shared Layers; tid + 1 corresponds to each task
    val labels = new Array[Counter[String]](taskManager.taskCount + 1)
    for (i <- 1 until labels.length) labels(i) = new Counter[String]() // labels(0) not used
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
