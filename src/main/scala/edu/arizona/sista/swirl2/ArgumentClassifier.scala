package edu.arizona.sista.swirl2

import java.io._

import edu.arizona.sista.learning._
import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.Files
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

    var dataset = createDataset(doc)
    dataset = dataset.removeFeaturesByFrequency(FEATURE_THRESHOLD)
    //classifier = new LogisticRegressionClassifier[String, String]()
    classifier = new LinearSVMClassifier[String, String]()
    //classifier = new RandomForestClassifier(numTrees = 100)
    //classifier = new PerceptronClassifier[String, String](epochs = 5)
    classifier.train(dataset)
  }

  def test(testPath:String): Unit = {
    val reader = new Reader
    val doc = reader.load(testPath)
    val distHist = new Counter[Int]()

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
          var predLabel = NEG_LABEL
          if(VALID_ARG_POS.findFirstIn(s.tags.get(arg)).isDefined) {
            val scores = classify(s, arg, pred)
            predLabel = (scores.getCount(POS_LABEL) >= scores.getCount(NEG_LABEL)) match {
              case true => POS_LABEL
              case false => NEG_LABEL
            }
          }
          if(goldLabel == POS_LABEL && predLabel != POS_LABEL) {
            distHist.incrementCount(math.abs(arg - pred))
            if(math.abs(arg - pred) < 3) {
              println(s"Missed argument ${s.words(arg)}($arg) for predicate ${s.words(pred)}($pred):")
              println( s"""Sentence: ${s.words.mkString(", ")}""")
              val datum = mkDatum(s, arg, pred, NEG_LABEL)
              println("Datum: " + datum.features.mkString(", "))
              println("Dependencies:\n" + s.dependencies.get)
              println()
            }

          }
          output += new Tuple2(goldLabel, predLabel)
        }
      }
    }

    BinaryScorer.score(output, POS_LABEL)
    logger.debug(s"Distance histogram for missed arguments: $distHist")
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
    var droppedCands = 0
    for(s <- doc.sentences) {
      val outEdges = s.semanticRoles.get.outgoingEdges
      for(pred <- s.words.indices if isPred(pred, s)) {
        val args = outEdges(pred).map(_._1).toSet
        for(arg <- s.words.indices) {
          if(VALID_ARG_POS.findFirstIn(s.tags.get(arg)).isDefined) {
            if (args.contains(arg)) {
              dataset += mkDatum(s, arg, pred, POS_LABEL)
            } else {
              // down sample negatives
              if (random.nextDouble() < DOWNSAMPLE_PROB) {
                dataset += mkDatum(s, arg, pred, NEG_LABEL)
              }
            }
          } else {
            droppedCands += 1
          }
        }
      }
      sentCount += 1
      if(sentCount % 1000 == 0)
        logger.debug(s"Processed $sentCount/${doc.sentences.length} sentences...")
    }
    logger.debug(s"Dropped $droppedCands candidate arguments.")
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
          if(pos.length < 2) posStats.incrementCount(pos)
          else posStats.incrementCount(pos.substring(0, 2))
          count += 1
        }
      }
    }
    logger.info("Arguments by POS tag: " + posStats.sorted)
    logger.info("Total number of arguments: " + count)
  }

  def saveTo(w:Writer): Unit = {
    classifier.saveTo(w)
  }
}

object ArgumentClassifier {
  val logger = LoggerFactory.getLogger(classOf[ArgumentClassifier])

  val POS_LABEL = "+"
  val NEG_LABEL = "-"

  val FEATURE_THRESHOLD = 3
  val DOWNSAMPLE_PROB = 0.50
  val POS_THRESHOLD = 0.50

  val VALID_ARG_POS = "NN|IN|PR|JJ|TO|RB|VB|MD|WD|CD|\\$|WP|DT".r

  def main(args:Array[String]): Unit = {
    val props = argsToProperties(args)
    var ac = new ArgumentClassifier

    if(props.containsKey("train")) {
      ac.train(props.getProperty("train"))

      if(props.containsKey("model")) {
        val os = new PrintWriter(new BufferedWriter(new FileWriter(props.getProperty("model"))))
        ac.saveTo(os)
        os.close()
      }
    }

    if(props.containsKey("test")) {
      if(props.containsKey("model")) {
        val is = new BufferedReader(new FileReader(props.getProperty("model")))
        ac = loadFrom(is)
        is.close()
      }

      ac.test(props.getProperty("test"))
    }
  }

  def loadFrom(r:java.io.Reader):ArgumentClassifier = {
    val ac = new ArgumentClassifier
    val reader = Files.toBufferedReader(r)

    val c = LiblinearClassifier.loadFrom[String, String](reader)
    ac.classifier = c

    ac
  }
}