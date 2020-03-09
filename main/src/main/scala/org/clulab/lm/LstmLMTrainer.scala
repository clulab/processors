package org.clulab.lm

import com.typesafe.config.ConfigFactory
import edu.cmu.dynet.{ComputationGraph, Dim, ExpressionVector, LookupParameter, LstmBuilder, Parameter, ParameterCollection, RMSPropTrainer, RnnBuilder, Trainer}
import org.clulab.sequences.LstmUtils
import org.clulab.sequences.LstmUtils.initializeDyNet
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.io.Source
import LstmLMTrainer._

class LstmLMTrainer (val w2i: Map[String, Int],
                     val t2i: Map[String, Int],
                     val parameters: ParameterCollection,
                     val wordLookupParameters: LookupParameter,
                     val wordFwRnnBuilder: RnnBuilder,
                     val wordBwRnnBuilder: RnnBuilder,
                     val fwO: Parameter,
                     val bwO: Parameter) {

  def mkTrainer(): Trainer = {
    val trainer = new RMSPropTrainer(parameters)
    trainer.clippingEnabled_=(true)
    trainer.clipThreshold_=(CLIP_THRESHOLD)
    trainer
  }

  /**
   * Trains the LM from the text in this file
   * The file must contain a sentence per line,
   *   with the white spaces between tokens normalized to a single space
   * @param trainFileName The name of the file with training sentences
   */
  def train( trainFileName:String,
             devFileName:Option[String],
             logCheckpoint:Int,
             saveCheckpoint:Int): Unit = {
    // initialize optimizer
    val trainer = mkTrainer()

    // train the fw and bw word LSTMs on all sentences in training
    val source = Source.fromFile(trainFileName)
    var sentCount = 0
    var cummulativeLoss = 0.0
    var numTagged = 0

    // start the first batch
    ComputationGraph.renew()
    var batchLosses = new ExpressionVector()

    for(sentence <- source.getLines()) {

    }
  }
}

object LstmLMTrainer {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmLMTrainer])

  val MIN_UNK_TAG_FREQ_RATIO  = 0.000001
  val MIN_UNK_WORD_FREQ_RATIO = 0.00000001

  val WORD_EMBEDDING_SIZE = 300
  val WORD_RNN_STATE_SIZE = 512

  val CLIP_THRESHOLD = 5.0f
  val DROPOUT_PROB:Float = 0.2f

  protected def generateKnownWords(trainFileName: String): (Set[String], Set[String], Int) = {
    logger.debug(s"Counting characters in file $trainFileName...")
    val counts = new Counter[String]()
    val source = Source.fromFile(trainFileName)
    var sentCount = 0
    for(line <- source.getLines()) {
      val tokens = line.split("\\s+")
      for(word <- tokens) {
        counts.incrementCount(word)
      }
      sentCount += 1
    }
    source.close()
    logger.debug("Counting completed.")
    logger.debug(s"Found ${counts.size} unique words.")
    var totalCounts = 0.0
    for(c <- counts.keySet) {
      totalCounts += counts.getCount(c)
    }
    val knownWords = new mutable.HashSet[String]()
    val tags = new mutable.HashSet[String]()
    for(c <- counts.keySet) {
      val count = counts.getCount(c)
      if(count > totalCounts * MIN_UNK_WORD_FREQ_RATIO) {
        knownWords += c
      }
      if(count > totalCounts * MIN_UNK_TAG_FREQ_RATIO) {
        tags += c
      }
    }
    logger.debug(s"Found ${knownWords.size} not unknown words.")

    // add the virtual characters we need
    knownWords += LstmUtils.UNK_WORD
    knownWords += LstmUtils.EOS_WORD

    (knownWords.toSet, tags.toSet, sentCount)
  }

  def mkParams(w2i: Map[String, Int], t2i: Map[String, Int]): LstmLMTrainer = {

    val parameters = new ParameterCollection()

    val wordLookupParameters = parameters.addLookupParameters(w2i.size, Dim(WORD_EMBEDDING_SIZE))

    val wordFwBuilder = new LstmBuilder(1, WORD_EMBEDDING_SIZE, WORD_RNN_STATE_SIZE, parameters)
    val wordBwBuilder = new LstmBuilder()(1, WORD_EMBEDDING_SIZE, WORD_RNN_STATE_SIZE, parameters)

    val fwO = parameters.addParameters(Dim(t2i.size, WORD_RNN_STATE_SIZE))
    val bwO = parameters.addParameters(Dim(t2i.size, WORD_RNN_STATE_SIZE))

    new LstmLMTrainer(w2i, t2i, parameters,
      wordLookupParameters, wordFwBuilder, wordBwBuilder,
      fwO, bwO)
  }

  def main(args: Array[String]): Unit = {
    initializeDyNet() // autoBatch = true, mem = "512")
    val configName = "lstm-lm"
    val config = new FlairConfig(ConfigFactory.load(configName))

    //
    // test mode
    //
    if(config.contains("lstm.test.model")) {
      logger.debug("Entering evaluation mode...")
    }

    //
    // train mode
    //
    else {
      logger.debug("Entering training mode...")

      // build the set of known characters
      val trainFileName = config.getArgString("lstm.train.train", None)
      val (knownWords, tags, _) = generateKnownWords(trainFileName)
      val w2i = knownWords.toArray.zipWithIndex.toMap
      val t2i = tags.toArray.zipWithIndex.toMap

      val lm = mkParams(w2i, t2i)
      lm.train(
        trainFileName,
        Some(config.getArgString("flair.train.dev", None)),
        config.getArgInt("flair.train.logCheckpoint", Some(1000)),
        config.getArgInt("flair.train.saveCheckpoint", Some(50000))
      )
    }
  }
}
