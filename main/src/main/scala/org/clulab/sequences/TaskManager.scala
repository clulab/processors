package org.clulab.sequences

import com.typesafe.config.Config
import org.clulab.utils.Configured
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer

import TaskManager._

/**
  * Manages the tasks in LstmCrfMtl
  */
class TaskManager(config:Config) extends Configured {
  override def getConf: Config = config

  /** Embeddings file */
  val embeddingsFileName:String = getArgString("mtl.embed", None)

  /** File with document frequency counts (needed for managing unknown words) */
  val docFreqFileName:String = getArgString("mtl.docFreq", None)

  /** How many shards to have per epoch */
  val shardsPerEpoch:Int = getArgInt("mtl.shardsPerEpoch", Some(10))

  /** Total number of epochs */
  val totalEpochs:Int = getArgInt("mtl.epochs", Some(1))

  /** Array of all tasks to be managed */
  val tasks:Array[Task] = readTasks()

  /** Training shards from all tasks */
  val shards:Array[Shard] = mkShards()

  /** Construct training shards by interleaving shards from all tasks */
  private def mkShards():Array[Shard] = {
    val shardsByTasks = new Array[Array[Shard]](tasks.length)

    // construct the shards for each task
    for(i <- tasks.indices) {
      shardsByTasks(i) = tasks(i).mkShards()
      assert(shardsByTasks(i).length == shardsPerEpoch)
    }

    // now interleave the tasks
    val interleavedShards = new ArrayBuffer[Shard]()
    for(i <- 0 until shardsPerEpoch) {
      for(j <- tasks.indices) {
        val crtShard = shardsByTasks(j)(i)
        interleavedShards += crtShard
      }
    }

    logger.debug("All shards:")
    for(i <- interleavedShards.indices) {
      logger.debug(s"${interleavedShards(i)}")
    }

    interleavedShards.toArray
  }

  /** Iterator over all sentences coming from all interleaved shards */
  def getSentences:Iterator[(Int, Array[Row])] = new SentenceIterator(tasks, shards)

  /** Reads all tasks from disk in memory */
  protected def readTasks(): Array[Task] = {
    val numberOfTasks = getArgInt("mtl.numberOfTasks", None)
    val tasks = new ArrayBuffer[Task]()
    for(i <- 0 until numberOfTasks) {
      tasks += readTask(i + 1)
    }
    logger.debug(s"Read $numberOfTasks tasks from config file.")
    tasks.toArray
  }

  protected def readTask(taskNumber: Int): Task = {
    val taskName = getArgString(s"mtl.task$taskNumber.name", None)
    val train = getArgString(s"mtl.task$taskNumber.train", None)

    val dev =
      if(contains(s"mtl.task$taskNumber.dev")) Some(getArgString(s"mtl.task$taskNumber.dev", None))
      else None

    val test =
      if(contains(s"mtl.task$taskNumber.test")) Some(getArgString(s"mtl.task$taskNumber.test", None))
      else None

    new Task(taskNumber - 1, taskName, train, dev, test, shardsPerEpoch)
  }

  def debugTraversal():Unit = {
    for(epoch <- 0 until totalEpochs) {
      logger.debug(s"Started epoch $epoch")
      var sentCount = 0
      var taskId = 0
      var totalSents = 0
      for(sentence <- getSentences) {
        totalSents += 1
        if(sentence._1 != taskId) {
          logger.debug(s"Read $sentCount sentences from task $taskId")
          taskId = sentence._1
          sentCount = 1
        } else {
          sentCount += 1
        }
      }
      logger.debug(s"Read $sentCount sentences from task $taskId")
      logger.debug(s"Read $totalSents sentences in epoch $epoch.")
    }
  }
}

class SentenceIterator(val tasks:Array[Task], val shards:Array[Shard]) extends Iterator[(Int, Array[Row])] {
  var shardPosition:Int = 0
  var sentencePosition:Int = 0

  override def hasNext: Boolean =
    (shardPosition < shards.length &&
      (sentencePosition < shards(shardPosition).endPosition ||
        shardPosition < shards.length - 1))

  override def next(): (Int, Array[Row]) = {
    if(sentencePosition >= shards(shardPosition).endPosition) {
      shardPosition += 1
      sentencePosition = shards(shardPosition).startPosition
    }

    val tid = shards(shardPosition).taskId
    val sentence = tasks(tid).trainSentences(sentencePosition)
    sentencePosition += 1

    //logger.debug(s"shardPosition = $shardPosition, sentencePosition = $sentencePosition")

    (tid, sentence)
  }
}

case class Shard(taskId:Int, startPosition:Int, endPosition:Int)

class Task(
  val taskId:Int, // this starts at 0 so we can use it as an index in the array of tasks
  val taskName:String,
  val trainFileName:String,
  val devFileName:Option[String],
  val testFileName:Option[String],
  shardsPerEpoch:Int) {
  logger.debug(s"Reading task $taskNumber ($taskName)...")
  val trainSentences:Array[Array[Row]] = ColumnReader.readColumns(trainFileName)
  val devSentences:Option[Array[Array[Row]]] =
    if(devFileName.isDefined) Some(ColumnReader.readColumns(devFileName.get))
    else None
  val testSentences:Option[Array[Array[Row]]] =
    if(testFileName.isDefined) Some(ColumnReader.readColumns(testFileName.get))
    else None

  // The size of the training shard for this task
  val shardSize:Int = math.ceil(trainSentences.length.toDouble / shardsPerEpoch).toInt

  // Current position in the training sentences when we iterate during training
  var currentTrainingSentencePosition:Int = 0

  logger.debug(s"Read ${trainSentences.length} training sentences for task $taskNumber, with shard size $shardSize.")
  if(devSentences.isDefined)
    logger.debug(s"Read ${devSentences.get.length} development sentences for task $taskNumber.")
  if(testSentences.isDefined)
    logger.debug(s"Read ${testSentences.get.length} testing sentences for task $taskNumber.")
  logger.debug(s"============ completed task $taskNumber ============")

  /** Construct the shards from all training sentences in this task */
  def mkShards():Array[Shard] = {
    val shards = new ArrayBuffer[Shard]()
    var crtPos = 0
    while(crtPos < trainSentences.length) {
      val endPos = math.min(crtPos + shardSize, trainSentences.length)
      shards += Shard(taskId, crtPos, endPos)
      crtPos = endPos
    }
    shards.toArray
  }

  def taskNumber:Int = taskId + 1
}

object TaskManager {
  val logger:Logger = LoggerFactory.getLogger(classOf[TaskManager])

}
