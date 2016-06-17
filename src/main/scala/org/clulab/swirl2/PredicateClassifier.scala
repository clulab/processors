package org.clulab.swirl2

import java.io._

import org.clulab.learning._
import org.clulab.processors.{Sentence, Document}
import org.clulab.struct.Counter
import org.clulab.utils.Files
import org.slf4j.LoggerFactory

import org.clulab.utils.StringUtils._
import PredicateClassifier._

import scala.collection.mutable.ListBuffer

/**
 * Identifies the predicates in SR frames
 * User: mihais
 * Date: 5/28/15
 */
class PredicateClassifier {
  lazy val featureExtractor = new PredicateFeatureExtractor

  var classifier:Option[Classifier[String, String]] = None
  var lemmaCounts:Option[Counter[String]] = None

  def train(trainPath:String): Unit = {
    val reader = new Reader
    val doc = reader.load(trainPath)

    computePredStats(doc)

    lemmaCounts = Some(Utils.countLemmas(doc, PredicateFeatureExtractor.UNKNOWN_THRESHOLD))
    featureExtractor.lemmaCounts = lemmaCounts

    var dataset = createDataset(doc)
    dataset = dataset.removeFeaturesByFrequency(FEATURE_THRESHOLD)
    //dataset = dataset.removeFeaturesByInformationGain(0.75)
    classifier = Some(new LogisticRegressionClassifier[String, String]())
    //classifier = Some(new RFClassifier[String, String](numTrees = 10, howManyFeaturesPerNode = featuresPerNode, nilLabel = Some(NEG_LABEL)))
    //classifier = Some(new LinearSVMClassifier[String, String]())
    classifier.get.train(dataset)
  }

  def featuresPerNode(total:Int):Int = (total * 0.66).toInt

  def test(testPath:String): Unit = {
    val reader = new Reader
    val doc = reader.load(testPath)

    val output = new ListBuffer[(String, String)]
    for(s <- doc.sentences;
        i <- s.words.indices) {
      val g = goldLabel(s, i)
      val scores = classify(s, i)
      val p = scores.getCount(POS_LABEL) >= POS_THRESHOLD match {
        case true => POS_LABEL
        case false => NEG_LABEL
      }
      output += new Tuple2(g, p)
    }

    BinaryScorer.score(output.toList, NEG_LABEL)
  }

  def classify(sent:Sentence, position:Int):Counter[String] = {
    if(filter(sent, position)) {
      val datum = mkDatum(sent, position, NEG_LABEL)
      val s = classifier.get.scoresOf(datum)
      //println(s"Scores for datum: $s")
      s
    } else {
      val s = new Counter[String]
      s.setCount(NEG_LABEL, 1.0)
      s
    }
  }

  def filter(s:Sentence, i:Int):Boolean = {
    val tag = s.tags.get(i)
    if(tag.startsWith("NN") || tag.startsWith("VB")) true
    else false
  }

  def createDataset(doc:Document): Dataset[String, String] = {
    val dataset = new BVFDataset[String, String]()
    val labelStats = new Counter[String]()

    for(s <- doc.sentences;
        i <- s.words.indices) {
      if(filter(s, i)) {
        val label = goldLabel(s, i)
        labelStats.incrementCount(label)
        dataset += mkDatum(s, i, label)
      }
    }
    logger.info("Label statistics for training examples: " + labelStats)
    dataset
  }

  def goldLabel(sentence:Sentence, position:Int):String = {
    val outgoing = sentence.semanticRoles.get.outgoingEdges

    if (position >= outgoing.length)
      return NEG_LABEL

    outgoing(position).nonEmpty match {
      case true => POS_LABEL
      case _ => NEG_LABEL
    }
  }

  def mkDatum(sent:Sentence, position:Int, label:String): BVFDatum[String, String] = {
    new BVFDatum[String, String](label, featureExtractor.mkFeatures(sent, position))
  }

  def computePredStats(doc:Document): Unit = {
    val posStats = new Counter[String]()
    var tokenCount = 0
    for(s <- doc.sentences) {
      tokenCount += s.words.length
      val g = s.semanticRoles.get
      for(i <- g.outgoingEdges.indices) {
        if(g.outgoingEdges(i).nonEmpty) {
          val pos = s.tags.get(i)
          posStats.incrementCount(pos.substring(0, 2))
        }
      }
    }
    logger.info(s"Found ${doc.sentences.length} sentences with $tokenCount tokens.")
    logger.info("Predicates by POS tag: " + posStats)
  }

  def saveTo(w:Writer): Unit = {
    lemmaCounts.foreach { x =>
      x.saveTo(w)
      //logger.debug("Saved the lemma dictionary.")
    }
    classifier.foreach { x =>
      x.saveTo(w)
      //logger.debug("Saved the classifier.")
    }
  }
}

object PredicateClassifier {
  val logger = LoggerFactory.getLogger(classOf[PredicateClassifier])

  val POS_LABEL = "+"
  val NEG_LABEL = "-"
  val POS_THRESHOLD = 0.50 // lower this to boost recall
  val FEATURE_THRESHOLD = 2

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

    val lc = Counter.loadFrom[String](reader)
    logger.debug(s"Successfully loaded lemma count hash for the predicate classifier, with ${lc.size} keys.")
    val c = LiblinearClassifier.loadFrom[String, String](reader)
    logger.debug(s"Successfully loaded the predicate classifier.")
    pc.classifier = Some(c)
    pc.lemmaCounts = Some(lc)
    pc.featureExtractor.lemmaCounts = pc.lemmaCounts

    pc
  }
}
