package org.clulab.dynet

import java.io.{FileWriter, PrintWriter}

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{AdamTrainer, ComputationGraph, Expression, ExpressionVector, ParameterCollection}
import org.clulab.dynet.Utils._
import org.clulab.sequences.Row
import org.clulab.struct.Counter
import org.clulab.utils.{Serializer, StringUtils}
import org.slf4j.{Logger, LoggerFactory}
import Metal._
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Multi-task learning (MeTaL) for sequence modeling
 * Designed to model any sequence task (e.g., POS tagging, NER), and SRL
 * @author Mihai
 */
class Metal(val taskManagerOpt: Option[TaskManager],
            val parameters: ParameterCollection,
            modelOpt: Option[IndexedSeq[Layers]]) {
  // One Layers object per task; model(0) contains the Layers shared between all tasks (if any)
  protected lazy val model: IndexedSeq[Layers] = modelOpt.getOrElse(initialize())

  // One Layers object per task, which merges the shared Layers with the task-specific Layers for each task
  protected lazy val flows: Array[Layers] = mkFlows(model)

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

  protected def mkFlows(layers: IndexedSeq[Layers]): Array[Layers] = {
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

      //
      // traverse all training sentences
      //
      for(metaSentence <- sentenceIterator) {
        val taskId = metaSentence._1
        val sentence = metaSentence._2
        sentCount += 1
        ComputationGraph.renew()

        val words = sentence.map(_.getWord)
        val posTags =
          if(flows(taskId).needsPosTags) {
            assert(sentence(0).hasPosTag)
            Some(sentence.map(_.getPosTag).toIndexedSeq)
          }
          else None

        val lossOpt =
          // any CoNLL BIO task, e.g., NER, POS tagging, prediction of SRL predicates
          if(taskManager.tasks(taskId).isBasic) {
            val goldLabels = sentence.map(_.getLabel)
            Some(flows(taskId).loss(words, posTags, None, goldLabels))
          }

          // prediction of SRL arguments
          else if(taskManager.tasks(taskId).isSrl) {
            // token positions for the predicates in this sentence
            val predicatePositions = sentence.map(_.getLabel).zipWithIndex.filter(_._1 == "B-P").map(_._2)

            // traverse all SRL frames
            if(predicatePositions.nonEmpty) {
              val allPredLoss = new ExpressionVector()

              for(j <- Row.ARG_START until sentence(0).length) {
                // token position of the predicate for this frame
                val predPosition = predicatePositions(j - Row.ARG_START)

                // gold argument labels for this frame
                val goldLabels = sentence.map(_.tokens(j))

                val loss = flows(taskId).loss(words, posTags, Some(predPosition), goldLabels)
                allPredLoss.add(loss)
              }

              if(allPredLoss.length > 0) Some(Expression.sum(allPredLoss))
              else None
            } else {
              None
            }
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

      //
      // check dev performance in this epoch, for all tasks
      //
      var totalAcc = 0.0
      var totalPrec = 0.0
      var totalRec = 0.0
      var totalF1 = 0.0
      for(taskId <- 0 until taskManager.taskCount) {
        val taskName = taskManager.tasks(taskId).taskName
        val devSentences = taskManager.tasks(taskId).devSentences
        if(devSentences.nonEmpty) {
          val (acc, prec, rec, f1) = evaluate(taskId, taskName, devSentences.get, epoch)
          totalAcc += acc
          totalPrec += prec
          totalRec += rec
          totalF1 += f1
        }
      }
      val avgAcc = totalAcc / taskManager.taskCount
      val avgPrec = totalPrec / taskManager.taskCount
      val avgRec = totalRec / taskManager.taskCount
      val avgF1 = totalF1 / taskManager.taskCount
      logger.info(s"Average accuracy across ${taskManager.taskCount} tasks: $avgAcc")
      logger.info(s"Average P/R/F1 across ${taskManager.taskCount} tasks: $avgPrec / $avgRec / $avgF1")

      if(avgF1 > maxAvgF1) {
        maxAvgF1 = avgF1
        maxAvgAcc = avgAcc
        bestEpoch = epoch
        epochPatience = taskManager.epochPatience
      } else {
        epochPatience -= 1
      }
      logger.info(s"Best epoch so far is epoch $bestEpoch with an average F1 of $maxAvgF1, and average accuracy of $maxAvgAcc.")
      if(epochPatience < taskManager.epochPatience) {
        logger.info(s"Epoch patience is at $epochPatience.")
      }

      save(s"$modelNamePrefix-epoch$epoch")
    }
  }

  def evaluate(taskId:Int, taskName:String, sentences:Array[Array[Row]], epoch:Int): (Double, Double, Double, Double) = {
    evaluate(taskId, taskName, sentences, "development", epoch)
  }

  def evaluate(taskId:Int, taskName:String, sentences:Array[Array[Row]]): (Double, Double, Double, Double) = {
    evaluate(taskId, taskName, sentences, "testing", -1)
  }

  /**
   * Computes accuracy/P/R/F1 for the evaluation dataset of the given task
   * Where possible, it also saves CoNLL-2003 compatible files of the output
   */
  def evaluate(taskId:Int,
               taskName:String,
               sentences:Array[Array[Row]],
               name:String, epoch:Int): (Double, Double, Double, Double) = {

    val scoreCountsByLabel = new ScoreCountsByLabel
    val taskNumber = taskId + 1
    var sentCount = 0

    logger.debug(s"Started evaluation on the $name dataset for task $taskNumber ($taskName)...")

    val pw =
      if(epoch >= 0) new PrintWriter(new FileWriter(s"task$taskNumber.dev.output.$epoch"))
      else new PrintWriter(new FileWriter(s"task$taskNumber.test.output"))

    //
    // regular BIO evaluation, compatible with CoNLL-2003
    //
    if(taskManager.tasks(taskId).isBasic) {
      for (sent <- sentences) {
        sentCount += 1
        val words = sent.map(_.getWord)
        val golds = sent.map(_.getLabel)

        val posTags =
          if(flows(taskId).needsPosTags) {
            assert(sent(0).hasPosTag)
            Some(sent.map(_.getPosTag).toIndexedSeq)
          }
          else None

        val preds = flows(taskId).predict(words, posTags, None)

        val sc = SeqScorer.f1(golds, preds)
        scoreCountsByLabel.incAll(sc)

        printCoNLLOutput(pw, words, golds, preds)
      }
    }

    //
    // evaluation of SRL arguments
    //
    else if(taskManager.tasks(taskId).isSrl) {
      for (sent <- sentences) {
        sentCount += 1
        val words = sent.map(_.getWord)
        val posTags =
          if(flows(taskId).needsPosTags) {
            assert(sent(0).hasPosTag)
            Some(sent.map(_.getPosTag).toIndexedSeq)
          }
          else None

        // find the token positions of all predicates in this sentence
        val predPositions = sent.map(_.getLabel).zipWithIndex.filter(_._1 == "B-P").map(_._2)

        // traverse the argument columns corresponding to each predicate
        for(j <- Row.ARG_START until sent(0).length) {
          val golds = sent.map(_.tokens(j))
          val predPosition = predPositions(j - Row.ARG_START)

          val preds = flows(taskId).predict(words, posTags, Some(predPosition))

          val sc = SeqScorer.f1(golds, preds)
          scoreCountsByLabel.incAll(sc)

          pw.println(s"pred = ${words(predPositions(j - Row.ARG_START))} at position ${predPositions(j - Row.ARG_START)}")
          printCoNLLOutput(pw, words, golds, preds)
        }
      }
    }

    pw.close()

    logger.info(s"Accuracy on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.accuracy()}")
    logger.info(s"Precision on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.precision()}")
    logger.info(s"Recall on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.recall()}")
    logger.info(s"Micro F1 on ${sentences.length} $name sentences for task $taskNumber ($taskName): ${scoreCountsByLabel.f1()}")
    for(label <- scoreCountsByLabel.labels) {
      logger.info(s"\tP/R/F1 for label $label (${scoreCountsByLabel.map(label).gold}): ${scoreCountsByLabel.precision(label)} / ${scoreCountsByLabel.recall(label)} / ${scoreCountsByLabel.f1(label)}")
    }

    ( scoreCountsByLabel.accuracy(),
      scoreCountsByLabel.precision(),
      scoreCountsByLabel.recall(),
      scoreCountsByLabel.f1() )
  }

  def predictJointly(words: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
    null // TODO
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

  def save(baseFilename: String): Unit = {
    val dynetFilename = mkDynetFilename(baseFilename)
    val x2iFilename = mkX2iFilename(baseFilename)

    // save the DyNet parameters
    new CloseableModelSaver(dynetFilename).autoClose { modelSaver =>
      modelSaver.addModel(parameters, "/all")
    }

    // save all the other meta data
    Serializer.using(Utils.newPrintWriter(x2iFilename)) { printWriter =>
      Utils.save(printWriter, model.length, "layerCount")
      for(i <- model.indices) {
        model(i).saveX2i(printWriter)
      }
    }
  }
}

object Metal {
  val logger:Logger = LoggerFactory.getLogger(classOf[Metal])

  protected def load(parameters: ParameterCollection,
                     modelFilenamePrefix: String): IndexedSeq[Layers] = {

    logger.debug(s"Loading MTL model from $modelFilenamePrefix...")
    val dynetFilename = mkDynetFilename(modelFilenamePrefix)
    val x2iFilename = mkX2iFilename(modelFilenamePrefix)

    //
    // load the x2i meta data
    //
    val layersSeq = Serializer.using(Utils.newSource(x2iFilename)) { source =>
      val layersSeq = new ArrayBuffer[Layers]()
      val lines = source.getLines()

      val layersCount = new Utils.ByLineIntBuilder().build(lines)
      for(i <- 0 until layersCount) {
        val layers = Layers.loadX2i(parameters, lines)
        layersSeq += layers
      }

      layersSeq.toIndexedSeq
    }

    //
    // load the actual DyNet params
    //
    Utils.loadParameters(dynetFilename, parameters)

    logger.debug("MTL loading complete.")
    layersSeq
  }

  def apply(modelFilenamePrefix: String, taskManager: TaskManager): Metal = {
    val parameters = new ParameterCollection()
    val model = Metal.load(parameters, modelFilenamePrefix)
    val mtl = new Metal(Some(taskManager), parameters, Some(model))
    mtl
  }

  def apply(modelFilenamePrefix: String): Metal = {
    val parameters = new ParameterCollection()
    val model = Metal.load(parameters, modelFilenamePrefix)
    val mtl = new Metal(None, parameters, Some(model))
    mtl
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

      val mtl = new Metal(Some(taskManager), parameters, None)
      mtl.train(modelName)
    }

    else if(props.containsKey("test")) {
      assert(props.containsKey("conf"))
      val configName = props.getProperty("conf")
      val config = ConfigFactory.load(configName)
      val taskManager = new TaskManager(config)
      val modelName = props.getProperty("test")

      val mtl = Metal(modelName, taskManager)
      mtl.test()
    }
  }
}
