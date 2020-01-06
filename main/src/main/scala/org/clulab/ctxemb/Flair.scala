package org.clulab.ctxemb

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.sequences.LstmUtils._
import org.clulab.struct.Counter
import org.clulab.utils.Configured
import org.slf4j.{Logger, LoggerFactory}

import scala.io.Source
import edu.cmu.dynet.{ComputationGraph, Dim, ExpressionVector, LookupParameter, LstmBuilder, ParameterCollection, RMSPropTrainer, RnnBuilder}

import scala.collection.mutable
import Flair._
import org.clulab.sequences.LstmCrf.logger

/**
 * Implementation of the FLAIR language model
 */
class Flair {

  var model:FlairParameters = _

  /**
   * Trains the LM from the text in this file
   * The file must contain a sentence per line,
   *   with the white spaces between tokens normalized to a single space
   * @param trainFileName
   */
  def train(trainFileName:String): Unit = {
    // build the set of known characters
    val (knownChars, totalSentCount) = generateKnownCharacters(trainFileName)
    val c2i = knownChars.toArray.zipWithIndex.toMap
    val sentPct = totalSentCount / 100
    model = mkParams(c2i)
    val trainer = new RMSPropTrainer(model.parameters)

    // train the fw and bw character LSTMs on all sentences in training
    val source = Source.fromFile(trainFileName)
    var sentCount = 0
    var fwCummulativeLoss = 0.0
    var bwCummulativeLoss = 0.0
    var numTagged = 0
    for(sentence <- source.getLines()) {
      ComputationGraph.renew()
      val characters = sentence.toCharArray

      //
      // left-to-right prediction
      //
      val fwIn = Array(BOS_CHAR) ++ characters
      val fwOut = toIds(characters ++ Array(EOS_CHAR), c2i)

      // predict
      val fwEmissionScores = emissionScoresAsExpressions(fwIn)
      val fwLoss = sentenceLossGreedy(fwEmissionScores, fwOut)
      fwCummulativeLoss += fwLoss.value().toFloat

      // backprop
      ComputationGraph.backward(fwLoss)
      trainer.update()

      //
      // right-to-left prediction
      //
      val reverse = characters.reverse
      val bwIn = Array(EOS_CHAR) ++ reverse
      val bwOut = toIds(reverse ++ Array(BOS_CHAR), c2i)

      //
      // reporting
      //
      sentCount += 1
      numTagged += characters.length + 1
      if(sentCount % sentPct == 0) {
        val pct = ((sentCount.toDouble * 100.0) / totalSentCount.toDouble).ceil.toInt
        logger.debug(s"Processed $pct% of the data.")
        logger.info("Forward cummulative loss: " + fwCummulativeLoss / numTagged)
      }
    }
    source.close()
  }

  protected def mkParams(c2i:Map[Char, Int]): FlairParameters = {
    val i2c = fromIndexToChar(c2i)

    val parameters = new ParameterCollection()
    val charLookupParameters = parameters.addLookupParameters(c2i.size, Dim(CHAR_EMBEDDING_SIZE))
    val charFwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)
    val charBwBuilder = new LstmBuilder(CHAR_RNN_LAYERS, CHAR_EMBEDDING_SIZE, CHAR_RNN_STATE_SIZE, parameters)

    new FlairParameters(c2i, i2c, parameters,
      charLookupParameters, charFwBuilder, charBwBuilder)
  }

  protected def generateKnownCharacters(trainFileName: String): (Set[Char], Int) = {
    logger.debug(s"Counting characters in file $trainFileName...")
    val counts = new Counter[Char]()
    val source = Source.fromFile(trainFileName)
    var sentCount = 0
    for(line <- source.getLines()) {
      for(c <- line.toCharArray) {
        counts.incrementCount(c)
      }
      sentCount += 1
    }
    source.close()
    logger.debug("Counting completed.")
    logger.debug(s"Found ${counts.size} characters.")
    var totalCounts = 0.0
    for(c <- counts.keySet) {
      totalCounts += counts.getCount(c)
    }
    val knownChars = new mutable.HashSet[Char]()
    for(c <- counts.keySet) {
      if(counts.getCount(c) > totalCounts * MIN_UNK_FREQ_RATIO) {
        knownChars += c
      }
    }
    logger.debug(s"Found ${knownChars.size} not unknown characters.")
    logger.debug(s"Known characters: ${knownChars.toSeq.sorted.mkString(", ")}")

    // add the virtual characters we need
    knownChars += UNKNOWN_CHAR
    knownChars += BOS_CHAR
    knownChars += EOS_CHAR

    (knownChars.toSet, sentCount)
  }
}

class FlairConfig(config:Config) extends Configured {
  override def getConf: Config = config
}

class FlairParameters (
  val c2i: Map[Char, Int],
  val i2c: Array[Char],
  val parameters: ParameterCollection,
  val charLookupParameters: LookupParameter,
  val charFwRnnBuilder: RnnBuilder,
  val charBwRnnBuilder: RnnBuilder) {

}

object Flair {
  private val logger:Logger = LoggerFactory.getLogger(classOf[Flair])

  val CHAR_RNN_LAYERS = 1
  val CHAR_EMBEDDING_SIZE = 100
  val CHAR_RNN_STATE_SIZE = 2048
  val CLIP_THRESHOLD = 10.0f
  val MIN_UNK_FREQ_RATIO = 0.000001

  val UNKNOWN_CHAR = 0.toChar // placeholder for unknown characters
  val BOS_CHAR = 1.toChar // virtual character indicating beginning of sentence
  val EOS_CHAR = 2.toChar // virtual character indicating end of sentence

  def main(args: Array[String]): Unit = {
    initializeDyNet()

    val configName = "flair"
    val config = new FlairConfig(ConfigFactory.load(configName))

    val lm = new Flair()
    lm.train(config.getArgString("flair.train", None))
  }
}
