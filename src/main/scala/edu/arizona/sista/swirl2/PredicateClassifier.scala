package edu.arizona.sista.swirl2

import java.io._

import edu.arizona.sista.learning._
import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.Files
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
      val g = goldLabel(s, i)
      val scores = classify(s, i)
      val p = (scores.getCount(POS_LABEL) >= POS_THRESHOLD) match {
        case true => POS_LABEL
        case false => NEG_LABEL
      }
      output += new Tuple2(g, p)
    }

    BinaryScorer.score(output.toList, POS_LABEL)
  }

  def classify(sent:Sentence, position:Int):Counter[String] = {
    val datum = mkDatum(sent, position, NEG_LABEL)
    val s = classifier.scoresOf(datum)
    s
  }

  def createDataset(doc:Document): Dataset[String, String] = {
    val dataset = new BVFDataset[String, String]()
    val labelStats = new Counter[String]()

    for(s <- doc.sentences;
        i <- s.words.indices) {
      val label = goldLabel(s, i)
      labelStats.incrementCount(label)
      dataset += mkDatum(s, i, label)
    }
    logger.info("Label statistics for training examples: " + labelStats)
    dataset
  }

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

  def saveTo(w:Writer): Unit = {
    classifier.saveTo(w)
  }
}

object PredicateClassifier {
  val logger = LoggerFactory.getLogger(classOf[PredicateClassifier])

  val POS_LABEL = "+"
  val NEG_LABEL = "-"
  val POS_THRESHOLD = 0.25 // lower this to boost recall

  def main(args:Array[String]): Unit = {
    val props = argsToProperties(args)
    var pc = new PredicateClassifier

    if(props.containsKey("train")) {
      pc.train(props.getProperty("train"))
      if(props.containsKey("model")) {
        val os = new PrintWriter(new BufferedWriter(new FileWriter(props.getProperty("model"))))
        pc.saveTo(os)
        os.close()
      }
    }

    if(props.containsKey("test")) {
      if(props.containsKey("model")) {
        val is = new BufferedReader(new FileReader(props.getProperty("model")))
        pc = loadFrom(is)
        is.close()
      }
      pc.test(props.getProperty("test"))
    }
  }

  def loadFrom(r:java.io.Reader):PredicateClassifier = {
    val pc = new PredicateClassifier
    val reader = Files.toBufferedReader(r)

    val c = LiblinearClassifier.loadFrom[String, String](reader)
    pc.classifier = c

    pc
  }
}
