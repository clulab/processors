package org.clulab.dynet

import java.io.{FileWriter, PrintWriter}

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{AdamTrainer, ComputationGraph, Expression, ExpressionVector, ParameterCollection, RMSPropTrainer, SimpleSGDTrainer}
import org.clulab.dynet.Utils._
import org.clulab.sequences.Row
import org.clulab.struct.Counter
import org.clulab.utils.{Serializer, StringUtils}
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.fatdynet.utils.CloseableModelSaver
import org.clulab.fatdynet.utils.Closer.AutoCloser

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import Metal._

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
    val layersPerTask: Array[Layers] = new Array[Layers](taskManager.taskCount + 1)
    layersPerTask(0) =
      Layers(taskManager, "mtl.layers", parameters, taskWords(0),
        None, hasPredicate = false, providedInputSize = None)

    val inputSize = layersPerTask(0).outDim

    for (i <- taskManager.indices) {
      val hasPredicate = taskManager.tasks(i).isSrl
      layersPerTask(i + 1) =
        Layers(taskManager, s"mtl.task${i + 1}.layers",
          parameters, taskWords(i + 1), Some(taskLabels(i + 1)),
          hasPredicate, inputSize)
    }
    for(i <- layersPerTask.indices) {
      logger.debug(s"Summary of layersPerTask($i):")
      logger.debug(layersPerTask(i).toString)
    }

    layersPerTask
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

    val basicReader = new BasicRowReader
    val srlArgsReader = new SrlArgsRowReader

    for (tid <- taskManager.indices) {
      for (sentence <- taskManager.tasks(tid).trainSentences) {
        for (token <- sentence) {
          words(tid + 1) += basicReader.getWord(token)
          words(0) += basicReader.getWord(token)
          if (taskManager.tasks(tid).taskType == TaskManager.TYPE_SRL) {
            val ls = srlArgsReader.getLabels(token)
            for(l <- ls) labels(tid + 1) += l
          } else { // basic tasks with the labels in the second column
            labels(tid + 1) += basicReader.getLabel(token)
          }
        }
      }
    }

    (words, labels)
  }

  def train(modelNamePrefix: String): Unit = {
    require(taskManagerOpt.isDefined)

    val learningRate = taskManager.getArgFloat("mtl.learningRate", Some(0.001f))
    val trainerType = taskManager.getArgString("mtl.trainer", Some("adam"))
    val batchSize = taskManager.getArgInt("mtl.batchSize", Some(1))
    assert(batchSize > 0)

    val trainer = trainerType match {
      case "adam" => SafeTrainer(new AdamTrainer(parameters, learningRate))
      case "rmsprop" => SafeTrainer(new RMSPropTrainer(parameters, learningRate))
      case "sgd" => SafeTrainer(new SimpleSGDTrainer(parameters, learningRate))
      case _ => throw new RuntimeException(s"ERROR: unknown trainer $trainerType!")
    }

    val basicReader = new BasicRowReader
    val srlArgsRowReader = new SrlArgsRowReader
    val basicRowReaderWithPosTags = new BasicRowReaderWithPosTags

    var cummulativeLoss = 0.0
    var numTagged = 0
    val rand = new Random(RANDOM_SEED)

    var maxAvgAcc = 0.0
    var maxAvgF1 = 0.0
    var bestEpoch = 0
    var epochPatience = taskManager.epochPatience

    var skippedFrames = 0
    var totalFrames = 0

    // builds a histogram of num of preds/sentence
    /*
    val sentenceIterator = taskManager.getSentences(rand)
    val predCounts = new Counter[Int]
    var totalValue = 0
    var totalCount = 0
    for(metaSentence <- sentenceIterator) {
      val taskId = metaSentence._1
      val sentence = metaSentence._2
      var count = 0
      for(row <- sentence) {
        val l = srlArgsRowReader.getLabel(row)
        if(l == "B-P") count += 1
      }
      predCounts.incrementCount(count)
      totalValue += count
      totalCount += 1
    }
    println(predCounts.sorted.mkString(", "))
    println(s"AVG = ${totalValue.toDouble / totalCount}")
    System.exit(1)
    */


    for(epoch <- 0 until taskManager.maxEpochs if epochPatience > 0) {
      logger.info(s"Started epoch $epoch.")
      // this fetches randomized training sentences from all tasks
      val sentenceIterator = taskManager.getSentences(rand)
      var sentCount = 0

      ComputationGraph.renew()
      var batchLosses: ExpressionVector = new ExpressionVector()

      //
      // traverse all training sentences
      //
      for(metaSentence <- sentenceIterator) {
        val taskId = metaSentence._1
        val sentence = metaSentence._2
        val taskType = taskManager.tasks(taskId).taskType

        sentCount += 1

        val annotatedSentence = taskType match {
          case TaskManager.TYPE_BASIC => basicReader.toAnnotatedSentence(sentence)
          case TaskManager.TYPE_SRL => srlArgsRowReader.toAnnotatedSentence(sentence)
          case TaskManager.TYPE_DEPSH => basicRowReaderWithPosTags.toAnnotatedSentence(sentence)
          case _ => throw new RuntimeException(s"ERROR: unknown reader for task type $taskType!")
        }

        val lossOpt: Option[Expression] =
          // any CoNLL BIO task, e.g., NER, POS tagging, prediction of SRL predicates
          if(taskManager.tasks(taskId).isBasic) {
            Some(Layers.loss(model, taskId, annotatedSentence, basicReader.toLabels(sentence)))
          }

          // prediction of dependency head distances
          else if(taskManager.tasks(taskId).isDepsHead) {
            Some(Layers.loss(model, taskId, annotatedSentence, basicRowReaderWithPosTags.toLabels(sentence)))
          }

          // prediction of SRL arguments
          else if(taskManager.tasks(taskId).isSrl) {
            // token positions for the predicates in this sentence
            val predicatePositions = srlArgsRowReader.getPredicatePositions(sentence)

            if(predicatePositions.nonEmpty) {
              val frameLabels = new ArrayBuffer[IndexedSeq[String]]
              for (i <- predicatePositions.indices) {
                // gold arg labels for this frame
                val labels = srlArgsRowReader.toLabels(sentence, Some(i))
                frameLabels += labels
              }

              val loss = Layers.loss(model, taskId, annotatedSentence,
                frameLabels, predicatePositions)

              Some(loss)
            } else {
              None
            }

            /*
            // traverse all SRL frames
            if(predicatePositions.nonEmpty) {
              val allPredLoss = new ExpressionVector()

              for(predIdx <- predicatePositions.indices) {
                // token position of the predicate for this frame
                val predPosition = predicatePositions(predIdx)
                totalFrames += 1

                // gold arg labels for this frame
                val labels = srlArgsRowReader.toLabels(sentence, Some(predIdx))

                if(true) { // hasContent(labels)) { // does not seem to help
                  val loss = Layers.loss(model, taskId, annotatedSentence, labels, Some(predPosition))
                  allPredLoss.add(loss)
                } else {
                  skippedFrames += 1
                }
              }

              if(allPredLoss.length > 0) Some(Expression.sum(allPredLoss))
              else None
            } else {
              None
            }
            */
          } else {
            throw new RuntimeException(s"ERROR: unknown task type ${taskManager.tasks(taskId).taskType}!")
          }

        if(lossOpt.isDefined) {
          var loss = lossOpt.get

          // task weighting
          if (taskManager.tasks(taskId).taskWeight != 1.0)
            loss = loss * Expression.input(taskManager.tasks(taskId).taskWeight)

          batchLosses.add(loss)

          if(batchLosses.size >= batchSize) {
            // backprop
            cummulativeLoss += batchBackprop(batchLosses, trainer)

            // start a new batch
            ComputationGraph.renew()
            batchLosses = new ExpressionVector()
          }
        }

        numTagged += sentence.length

        if(sentCount % 1000 == 0) {
          logger.info("Cummulative loss: " + cummulativeLoss / numTagged + s" ($sentCount sentences)")
          cummulativeLoss = 0.0
          numTagged = 0

          if(skippedFrames > 0) {
            logger.debug(s"Skipped $skippedFrames out of $totalFrames total SRL frames.")
          }
        }
      }

      // we may have an incomplete batch here
      if(batchLosses.nonEmpty) {
        // backprop
        cummulativeLoss += batchBackprop(batchLosses, trainer)

        // start a new batch
        ComputationGraph.renew()
        batchLosses = new ExpressionVector()
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

  /** Returns true if this contains at least 1 non-O label */
  private def hasContent(labels: IndexedSeq[String]): Boolean = {
    labels.exists(_ != "O")
  }

  def batchBackprop(batchLosses: ExpressionVector, trainer: SafeTrainer): Float = {
    // this is were the auto batch magic happens
    val batchLoss = Expression.sum(batchLosses)

    // forward pass and stats
    val batchLossAsFloat = batchLoss.value().toFloat

    // backprop
    ComputationGraph.backward(batchLoss)
    trainer.update(parameters)

    batchLossAsFloat
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

    val basicReader = new BasicRowReader
    val srlArgsReader = new SrlArgsRowReader
    val basicRowReaderWithPosTags = new BasicRowReaderWithPosTags

    //
    // regular BIO evaluation, compatible with CoNLL-2003
    //
    if(taskManager.tasks(taskId).isBasic) {
      for (sent <- sentences) {
        sentCount += 1

        val sentence = basicReader.toAnnotatedSentence(sent)
        val golds = basicReader.toLabels(sent)

        val preds = Layers.predict(model, taskId, sentence)

        val sc = SeqScorer.f1(golds, preds(0))
        scoreCountsByLabel.incAll(sc)

        printCoNLLOutput(pw, sentence.words, golds, preds(0))
      }
    }

    //
    // evaluation of dependency head distance
    //
    else if(taskManager.tasks(taskId).isDepsHead) {
      for (sent <- sentences) {
        sentCount += 1

        val sentence = basicRowReaderWithPosTags.toAnnotatedSentence(sent)
        val golds = basicRowReaderWithPosTags.toLabels(sent)

        val preds = Layers.predict(model, taskId, sentence)

        val sc = SeqScorer.f1(golds, preds(0))
        scoreCountsByLabel.incAll(sc)

        printCoNLLOutput(pw, sentence.words, golds, preds(0))
      }
    }

    //
    // evaluation of SRL arguments
    //
    else if(taskManager.tasks(taskId).isSrl) {
      for (sent <- sentences) {
        sentCount += 1
        val annotatedSentence = srlArgsReader.toAnnotatedSentence(sent)

        // find the token positions of all predicates in this sentence
        val predPositions = srlArgsReader.getPredicatePositions(sent)

        if(predPositions.nonEmpty) {
          val goldLabels = new ArrayBuffer[IndexedSeq[String]]()
          // traverse the argument columns corresponding to each predicate
          for (j <- predPositions.indices) {
            val golds = srlArgsReader.toLabels(sent, Some(j))
            goldLabels += golds
          }

          val predLabels = Layers.predict(model, taskId, annotatedSentence, predPositions)
          assert(predLabels.length == goldLabels.length)
          assert(predLabels.length == predPositions.length)

          for (i <- predLabels.indices) {
            val sc = SeqScorer.f1(goldLabels(i), predLabels(i))
            scoreCountsByLabel.incAll(sc)

            pw.println(s"pred = ${annotatedSentence.words(predPositions(i))} at position ${predPositions(i)}")
            printCoNLLOutput(pw, annotatedSentence.words, goldLabels(i), predLabels(i))
          }
        }

        /*
        // traverse the argument columns corresponding to each predicate
        for(j <- predPositions.indices) {
          val golds = srlArgsReader.toLabels(sent, Some(j))
          val preds = Layers.predict(model, taskId, annotatedSentence, Some(predPositions(j)))

          val sc = SeqScorer.f1(golds, preds)
          scoreCountsByLabel.incAll(sc)

          pw.println(s"pred = ${annotatedSentence.words(predPositions(j))} at position ${predPositions(j)}")
          printCoNLLOutput(pw, annotatedSentence.words, golds, preds)
        }
        */
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

  // this only supports basic tasks for now
  def predictJointly(sentence: AnnotatedSentence): IndexedSeq[IndexedSeq[String]] = {
    Layers.predictJointly(model, sentence)
  }

  def predict(taskId: Int,
              sentence: AnnotatedSentence): IndexedSeq[String] = {
    predict(taskId, sentence, IndexedSeq())(0)
  }

  def predict(taskId: Int,
              sentence: AnnotatedSentence,
              predPositions: IndexedSeq[Int]): IndexedSeq[IndexedSeq[String]] = {
    Layers.predict(model, taskId, sentence, predPositions)
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
      val lines = source.getLines().buffered

      val layersCount = new Utils.ByLineIntBuilder().build(lines)
      for(i <- 0 until layersCount) {
        val layers = Layers.loadX2i(parameters, lines)
        //println("loadX2i done!")
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
    initializeDyNet() // autoBatch = true, mem = "2048,2048,2048,2048") // mem = "1660,1664,2496,1400")

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

    else if(props.containsKey("shell")) {
      val modelName = props.getProperty("shell")
      val mtl = Metal(modelName)
      val shell = new MetalShell(mtl)
      shell.shell()
    }
  }
}
