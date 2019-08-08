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

  val embeddingsFileName:String = getArgString("mtl.embed", None)

  val docFreqFileName:String = getArgString("mtl.docFreq", None)

  val minFreq:Int = getArgInt("mtl.minFreq", Some(100))

  val shardsPerEpoch:Int = getArgInt("mtl.shardsPerEpoch", Some(10))

  val tasks:Array[Task] = readTasks()

  /**
    * Retrieves the next training sentence to be processed from one of the tasks
    * @return (sentence, task id that own this sentence)
    */
  def getSentence: (Array[Row], Int) = {
    /*
    position:Array[Int] = zeros
    currentTask = 0

    while(position(ct) > 0 && (position(ct) % shardSize(ct) == 0 || position(ct) >= num-sentences in this task)
	      curentTask ++

    fetch sentence
    position(ct) ++
    sentence
     */
    null // TODO
  }

  /** Resets the traversal of the training sentences */
  def reset():Unit = {
    // TODO
  }

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
    logger.debug(s"Reading task $taskNumber ($taskName)...")
    val train = getArgString(s"mtl.task$taskNumber.train", None)

    val dev =
      if(contains(s"mtl.task$taskNumber.dev")) Some(getArgString(s"mtl.task$taskNumber.dev", None))
      else None

    val test =
      if(contains(s"mtl.task$taskNumber.test")) Some(getArgString(s"mtl.task$taskNumber.test", None))
      else None

    val epochs = getArgInt(s"mtl.task$taskNumber.epochs", None)

    new Task(taskNumber, taskName, train, dev, test, epochs)
  }
}

class Task(
  val taskNumber:Int,
  val taskName:String,
  val trainFileName:String,
  val devFileName:Option[String],
  val testFileName:Option[String],
  val epochs:Int) {
  val trainSentences:Array[Array[Row]] = ColumnReader.readColumns(trainFileName)
  val devSentences:Option[Array[Array[Row]]] =
    if(devFileName.isDefined) Some(ColumnReader.readColumns(devFileName.get))
    else None
  val testSentences:Option[Array[Array[Row]]] =
    if(testFileName.isDefined) Some(ColumnReader.readColumns(testFileName.get))
    else None

  logger.debug(s"Read ${trainSentences.length} training sentences for task $taskNumber.")
  if(devSentences.isDefined)
    logger.debug(s"Read ${devSentences.get.length} development sentences for task $taskNumber.")
  if(testSentences.isDefined)
    logger.debug(s"Read ${testSentences.get.length} testing sentences for task $taskNumber.")
}

object TaskManager {
  val logger:Logger = LoggerFactory.getLogger(classOf[TaskManager])
}
