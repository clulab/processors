package edu.arizona.sista.swirl2

import edu.arizona.sista.learning._
import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils._
import org.slf4j.LoggerFactory

import ArgumentClassifier._

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * Identifies the arguments in SR frames
 * User: mihais
 * Date: 7/13/15
 */
class ArgumentClassifier {
  lazy val featureExtractor = new ArgumentFeatureExtractor
  var classifier:Classifier[String, String] = null

  def train(trainPath:String): Unit = {
    val reader = new Reader
    val doc = reader.load(trainPath)

    computeArgStats(doc)

    val dataset = createDataset(doc)
    dataset.removeFeaturesByFrequency(FEATURE_THRESHOLD)
    classifier = new LogisticRegressionClassifier[String, String]()
    //classifier = new LinearSVMClassifier[String, String]()
    classifier.train(dataset)
  }

  def test(testPath:String): Unit = {
    val reader = new Reader
    val doc = reader.load(testPath)

    val output = new ListBuffer[(String, String)]
    for(s <- doc.sentences) {
      val outEdges = s.semanticRoles.get.outgoingEdges
      for (pred <- s.words.indices if isPred(pred, s)) {
        val args = outEdges(pred).map(_._1).toSet
        for(arg <- s.words.indices) {
          val goldLabel = (args.contains(arg)) match {
            case true => POS_LABEL
            case false => NEG_LABEL
          }
          val scores = classify(s, arg, pred)
          val predLabel = (scores.getCount(POS_LABEL) >= POS_THRESHOLD) match {
            case true => POS_LABEL
            case false => NEG_LABEL
          }
          output += new Tuple2(goldLabel, predLabel)
        }
      }
    }

    BinaryScorer.score(output, POS_LABEL)
  }

  def classify(sent:Sentence, arg:Int, pred:Int):Counter[String] = {
    val datum = mkDatum(sent, arg, pred, NEG_LABEL)
    val s = classifier.scoresOf(datum)
    s
  }

  def createDataset(doc:Document): Dataset[String, String] = {
    val dataset = new BVFDataset[String, String]()
    val random = new Random(0)
    var sentCount = 0
    for(s <- doc.sentences) {
      val outEdges = s.semanticRoles.get.outgoingEdges
      for(pred <- s.words.indices if isPred(pred, s)) {
        val args = outEdges(pred).map(_._1).toSet
        for(arg <- s.words.indices) {
          if(args.contains(arg)) {
            dataset += mkDatum(s, arg, pred, POS_LABEL)
          } else {
            // down sample negatives
            if(random.nextDouble() < DOWNSAMPLE_PROB) {
              dataset += mkDatum(s, arg, pred, NEG_LABEL)
            }
          }
        }
      }
      sentCount += 1
      if(sentCount % 1000 == 0)
        logger.debug(s"Processed $sentCount/${doc.sentences.length} sentences...")
    }
    dataset
  }

  def isPred(position:Int, s:Sentence):Boolean = {
    val oes = s.semanticRoles.get.outgoingEdges
    position < oes.length && oes(position) != null && oes(position).nonEmpty
  }

  def mkDatum(sent:Sentence, arg:Int, pred:Int, label:String): BVFDatum[String, String] = {
    new BVFDatum[String, String](label, featureExtractor.mkFeatures(sent, arg, pred))
  }

  def computeArgStats(doc:Document): Unit = {
    val posStats = new Counter[String]()
    var count = 0
    for(s <- doc.sentences) {
      val g = s.semanticRoles.get
      for(i <- g.outgoingEdges.indices) {
        for(a <- g.outgoingEdges(i)) {
          val pos = s.tags.get(a._1)
          posStats.incrementCount(pos)
          count += 1
        }
      }
    }
    logger.info("Arguments by POS tag: " + posStats.sorted)
    logger.info("Total number of arguments: " + count)
  }
}

object ArgumentClassifier {
  val logger = LoggerFactory.getLogger(classOf[ArgumentClassifier])

  val POS_LABEL = "+"
  val NEG_LABEL = "-"

  val FEATURE_THRESHOLD = 10
  val DOWNSAMPLE_PROB = 0.25
  val POS_THRESHOLD = 0.50

  def main(args:Array[String]): Unit = {
    val props = argsToProperties(args)
    val ac = new ArgumentClassifier

    if(props.containsKey("train")) {
      ac.train(props.getProperty("train"))
    }

    if(props.containsKey("test")) {
      ac.test(props.getProperty("test"))
    }
  }
}