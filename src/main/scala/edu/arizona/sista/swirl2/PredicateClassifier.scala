package edu.arizona.sista.swirl2

import edu.arizona.sista.learning._
import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.struct.Counter
import org.slf4j.LoggerFactory

import edu.arizona.sista.utils.StringUtils._
import PredicateClassifier._

import scala.collection.mutable.ListBuffer

/**
 * Identifies the predicates in SR frames
 * User: mihais
 * Date: 5/28/15
 */
class PredicateClassifier {
  lazy val featureExtractor = new PredicateFeatureExtractor
  var classifier:Classifier[String, String] = null

  def train(trainPath:String): Unit = {
    val reader = new Reader
    val doc = reader.load(trainPath)

    computePredStats(doc)

    val dataset = createDataset(doc)
    classifier = new LogisticRegressionClassifier[String, String]()
    //classifier = new LinearSVMClassifier[String, String]()
    classifier.train(dataset)
  }

  def test(testPath:String): Unit = {
    val reader = new Reader
    val doc = reader.load(testPath)

    val output = new ListBuffer[(String, String)]
    for(s <- doc.sentences;
        i <- s.words.indices) {
      output += new Tuple2(goldLabel(s, i), classify(s, i))
    }

    score(output.toList)
  }

  def score(output:Seq[(String, String)]): Unit = {
    var predicted = 0
    var total = 0
    var correct = 0
    for(o <- output) {
      if(o._1 == POS_LABEL) total += 1
      if(o._2 == POS_LABEL) {
        predicted += 1
        if(o._1 == o._2) correct += 1
      }
    }

    val p = correct.toDouble / predicted.toDouble
    val r = correct.toDouble / total.toDouble
    val f1 = 2 * p * r / (p + r)

    logger.info(s"Precision: $p ($correct/$predicted)")
    logger.info(s"Recall: $r ($correct/$total)")
    logger.info(s"F1: $f1")

  }

  def classify(sent:Sentence, position:Int):String = {
    if(! validForPredicate(sent, position))
      return NEG_LABEL

    val datum = mkDatum(sent, position, NEG_LABEL)
    val s = classifier.scoresOf(datum).getCount(POS_LABEL)

    if(s > -1) POS_LABEL
    else NEG_LABEL
  }

  def createDataset(doc:Document): Dataset[String, String] = {
    val dataset = new BVFDataset[String, String]()
    val labelStats = new Counter[String]()

    for(s <- doc.sentences;
        i <- s.words.indices) {
      if(validForPredicate(s, i)) {
        val label = goldLabel(s, i)
        labelStats.incrementCount(label)
        dataset += mkDatum(s, i, label)
      }
    }
    logger.info("Label statistics for training examples: " + labelStats)
    dataset
  }

  def validForPredicate(sent:Sentence, position:Int):Boolean =
    VALID_POS_PATTERN.findFirstIn(sent.tags.get(position)).isDefined

  def goldLabel(sentence:Sentence, position:Int):String = {
    val outgoing = sentence.semanticRoles.get.outgoingEdges

    if (position >= outgoing.length)
      return NEG_LABEL

    outgoing.nonEmpty match {
      case true => POS_LABEL
      case _ => NEG_LABEL
    }
  }

  def mkDatum(sent:Sentence, position:Int, label:String): BVFDatum[String, String] = {
    new BVFDatum[String, String](label, featureExtractor.mkFeatures(sent, position))
  }

  def computePredStats(doc:Document): Unit = {
    val posStats = new Counter[String]()
    for(s <- doc.sentences) {
      val g = s.semanticRoles.get
      for(i <- g.outgoingEdges.indices) {
        if(g.outgoingEdges(i).nonEmpty) {
          val pos = s.tags.get(i)
          posStats.incrementCount(pos.substring(0, 2))
        }
      }
    }
    logger.info("Predicates by POS tag: " + posStats)
  }
}

object PredicateClassifier {
  val logger = LoggerFactory.getLogger(classOf[PredicateClassifier])

  val VALID_POS_PATTERN = """NN|VB|JJ""".r
  val POS_LABEL = "+"
  val NEG_LABEL = "-"

  def main(args:Array[String]): Unit = {
    val props = argsToProperties(args)
    val pc = new PredicateClassifier

    if(props.containsKey("train")) {
      pc.train(props.getProperty("train"))
    }

    if(props.containsKey("test")) {
      pc.test(props.getProperty("test"))
    }
  }
}
