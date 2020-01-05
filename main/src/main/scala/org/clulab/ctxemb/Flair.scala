package org.clulab.ctxemb

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.sequences.LstmUtils._
import org.clulab.struct.Counter
import org.clulab.utils.Configured
import org.slf4j.{Logger, LoggerFactory}

import scala.io.Source

import Flair._

/**
 * Implementation of the FLAIR language model
 */
class Flair {

  /**
   * Trains the LM from the text in this file
   * The file must contain a sentence per line,
   *   with the white spaces between tokens normalized to a single space
   * @param trainFileName
   */
  def train(trainFileName:String): Unit = {
    val charCounts = countCharacters(trainFileName)
  }

  protected def countCharacters(trainFileName: String): Counter[Char] = {
    logger.debug(s"Counting characters in file $trainFileName...")
    val counts = new Counter[Char]()
    val source = Source.fromFile(trainFileName)
    for(line <- source.getLines()) {
      for(c <- line.toCharArray) {
        counts.incrementCount(c)
      }
    }
    source.close()
    logger.debug("Counting completed.")
    logger.debug(s"Found ${counts.size} characters.")
    val sortedChars = counts.toSeq.sortBy(- _._2)
    for(c <- sortedChars) {
      println(s"[${c._1}] ${c._2}")
    }
    counts
  }
}

class FlairConfig(config:Config) extends Configured {
  override def getConf: Config = config
}

object Flair {
  private val logger:Logger = LoggerFactory.getLogger(classOf[Flair])

  def main(args: Array[String]): Unit = {
    initializeDyNet()

    val configName = "flair"
    val config = new FlairConfig(ConfigFactory.load(configName))

    val lm = new Flair()
    lm.train(config.getArgString("flair.train", None))
  }
}
