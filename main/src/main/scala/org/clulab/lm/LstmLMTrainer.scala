package org.clulab.lm

import com.typesafe.config.ConfigFactory
import org.clulab.sequences.LstmUtils
import org.clulab.sequences.LstmUtils.initializeDyNet
import org.clulab.struct.Counter
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.io.Source

class LstmLMTrainer {

}

object LstmLMTrainer {
  val logger:Logger = LoggerFactory.getLogger(classOf[LstmLMTrainer])

  val MIN_UNK_TAG_FREQ_RATIO  = 0.000001
  val MIN_UNK_WORD_FREQ_RATIO = 0.00000001

  protected def generateKnownWords(trainFileName: String): (Set[String], Int) = {
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
    for(c <- counts.keySet) {
      if(counts.getCount(c) > totalCounts * MIN_UNK_WORD_FREQ_RATIO) {
        knownWords += c
      }
    }
    logger.debug(s"Found ${knownWords.size} not unknown words.")

    // add the virtual characters we need
    knownWords += LstmUtils.UNK_WORD
    knownWords += LstmUtils.EOS_WORD

    (knownWords.toSet, sentCount)
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
      val (knownChars, totalSentCount) = generateKnownWords(trainFileName)
    }
  }
}
