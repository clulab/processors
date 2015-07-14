package edu.arizona.sista.swirl2

import edu.arizona.sista.learning.Classifier
import edu.arizona.sista.processors.Document
import edu.arizona.sista.utils.StringUtils._
import org.slf4j.LoggerFactory

import ArgumentClassifier._

/**
 * Identifies the arguments in SR frames
 * User: mihais
 * Date: 7/13/15
 */
class ArgumentClassifier {
  var classifier:Classifier[String, String] = null

  def train(trainPath:String): Unit = {
    val reader = new Reader
    val doc = reader.load(trainPath)

    computeArgStats(doc)
  }

  def computeArgStats(doc:Document): Unit = {
    // TODO
  }
}

object ArgumentClassifier {
  val logger = LoggerFactory.getLogger(classOf[ArgumentClassifier])

  def main(args:Array[String]): Unit = {
    val props = argsToProperties(args)
    val ac = new ArgumentClassifier

    if(props.containsKey("train")) {
      ac.train(props.getProperty("train"))
    }
  }
}