package edu.arizona.sista.swirl2

import java.io._

import edu.arizona.sista.learning._
import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.struct.{DirectedGraphEdgeIterator, DirectedGraph, Counter}
import edu.arizona.sista.utils.Files
import edu.arizona.sista.utils.StringUtils._
import org.slf4j.LoggerFactory

import ArgumentClassifier._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

/**
  * Identifies the arguments in SR frames
  * User: mihais
  * Date: 7/13/15
  */
class ArgumentClassifier {
  lazy val featureExtractor = new ArgumentFeatureExtractor("vectors.txt")

  var classifier:Option[Classifier[String, String]] = None
  var lemmaCounts:Option[Counter[String]] = None

  def train(trainPath:String): Unit = {
    val datasetFileName = "swirl2_argument_classification_dataset.ser"
    val useSerializedDataset = true
    var dataset:Dataset[String, String] = null

    if(useSerializedDataset && new File(datasetFileName).exists()) {
      // read the dataset from the serialized file
      logger.info(s"Reading dataset from $datasetFileName...")
      val is = new ObjectInputStream(new FileInputStream(datasetFileName))
      dataset = is.readObject().asInstanceOf[Dataset[String, String]]
      val lc = is.readObject().asInstanceOf[Counter[String]]
      lemmaCounts = Some(lc)
      featureExtractor.lemmaCounts = lemmaCounts
      is.close()

    } else {
      // generate the dataset online
      val reader = new Reader
      var doc = reader.load(trainPath)
      computeArgStats(doc)

      lemmaCounts = Some(Utils.countLemmas(doc, ArgumentFeatureExtractor.UNKNOWN_THRESHOLD))
      featureExtractor.lemmaCounts = lemmaCounts

      logger.debug("Constructing dataset...")
      dataset = createDataset(doc)
      doc = null // the raw data is no longer needed
      logger.debug("Finished constructing dataset.")

      // now save it for future runs
      if(useSerializedDataset) {
        logger.info(s"Writing dataset to $datasetFileName...")
        val os = new ObjectOutputStream(new FileOutputStream(datasetFileName))
        os.writeObject(dataset)
        os.writeObject(lemmaCounts.get)
        os.close()
      }
    }

    dataset = dataset.removeFeaturesByFrequency(FEATURE_THRESHOLD)
    //dataset = dataset.removeFeaturesByInformationGain(0.05)
    classifier = Some(new LogisticRegressionClassifier[String, String](C = 1000))
    //classifier = Some(new LinearSVMClassifier[String, String]())
    //classifier = Some(new RFClassifier[String, String](numTrees = 10, maxTreeDepth = 100, howManyFeaturesPerNode = featuresPerNode))
    //classifier = Some(new PerceptronClassifier[String, String](epochs = 5))

    classifier.get match {
      case rfc:RFClassifier[String, String] =>
        val counterDataset = dataset.toCounterDataset
        dataset = null
        rfc.train(counterDataset)
      case oc:Classifier[String, String] =>
        oc.train(dataset)
    }
  }

  def featuresPerNode(total:Int):Int = RFClassifier.featuresPerNodeTwoThirds(total)// (10.0 * math.sqrt(total.toDouble)).toInt

  def test(testPath:String): Unit = {
    val reader = new Reader
    val doc = reader.load(testPath)
    printDoc(doc)
    val distHist = new Counter[Int]()

    var totalCands = 0
    val output = new ListBuffer[(String, String)]
    for(s <- doc.sentences) {
      println("Working on this sentence:")
      printSentence(s)

      val outEdges = s.semanticRoles.get.outgoingEdges
      for (pred <- s.words.indices if isPred(pred, s)) {
        val args = outEdges(pred).map(_._1).toSet
        val history = new ArrayBuffer[Int]()
        for(arg <- s.words.indices) {
          val goldLabel = args.contains(arg) match {
            case true => POS_LABEL
            case false => NEG_LABEL
          }
          var predLabel = NEG_LABEL
          if(ValidCandidate.isValid(s, arg, pred)) {
            val scores = classify(s, arg, pred, history)
            predLabel = scores.getCount(POS_LABEL) >= scores.getCount(NEG_LABEL) match {
              case true => POS_LABEL
              case false => NEG_LABEL
            }
            if(predLabel == POS_LABEL) {
              // println(s"Found arg: $pred -> $arg")
              history += arg
            }
          }
          if(goldLabel == POS_LABEL && predLabel != POS_LABEL) {
            println(s"Missed argument: $pred (${s.words(pred)}) -> $arg (${s.words(arg)})")
            distHist.incrementCount(math.abs(arg - pred))

            // debug output
            /*
            if(math.abs(arg - pred) < 3) {
              println(s"Missed argument ${s.words(arg)}($arg) for predicate ${s.words(pred)}($pred):")
              println( s"""Sentence: ${s.words.mkString(", ")}""")
              val datum = mkDatum(s, arg, pred, NEG_LABEL)
              println("Datum: " + datum.features.mkString(", "))
              println("Dependencies:\n" + s.dependencies.get)
              println()
            }
            */

          } else if(goldLabel != POS_LABEL && predLabel == POS_LABEL) {
            println(s"Spurious argument: $pred (${s.words(pred)}) -> $arg (${s.words(arg)})")
          }
          output += new Tuple2(goldLabel, predLabel)
          totalCands += 1
        }
      }
    }

    BinaryScorer.score(output, POS_LABEL)
    logger.debug(s"Total number of candidates investigated: $totalCands")
    logger.debug(s"Distance histogram for missed arguments: $distHist")
  }

  def printDoc(doc:Document): Unit = {
    var sentenceCount = 0
    for (sentence <- doc.sentences) {
      println("Sentence #" + sentenceCount + ":")
      printSentence(sentence: Sentence)
      sentenceCount += 1
    }
  }

  def printSentence(sentence:Sentence) {
    println("Tokens: " + sentence.words.zip(sentence.tags.get).zipWithIndex.mkString(" "))
    sentence.stanfordBasicDependencies.foreach(dependencies => {
      println("Syntactic dependencies:")
      val iterator = new DirectedGraphEdgeIterator[String](dependencies)
      while (iterator.hasNext) {
        val dep = iterator.next
        // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
        println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
      }
    })
    sentence.syntacticTree.foreach(tree => {
      println("Constituent tree: " + tree.toStringDepth(showHead = false))
      // see the edu.arizona.sista.struct.Tree class for more information
      // on syntactic trees, including access to head phrases/words
    })
    sentence.semanticRoles.foreach(printGraph("Semantic dependencies:", sentence, _))
    println("\n")
  }

  def printGraph(header:String, s:Sentence, graph:DirectedGraph[String]) {
    println("Semantic dependencies:")
    val iterator = new DirectedGraphEdgeIterator[String](graph)
    while (iterator.hasNext) {
      val dep = iterator.next
      println(s" head:${dep._1} (${s.words(dep._1)}) modifier:${dep._2} (${s.words(dep._2)}) label:${dep._3}")
    }
  }

  def classify(sent:Sentence, arg:Int, pred:Int, history:ArrayBuffer[Int]):Counter[String] = {
    val datum = mkDatum(sent, arg, pred, history, NEG_LABEL)
    val s = classifier.get.scoresOf(datum)
    s
  }

  def createDataset(doc:Document): Dataset[String, String] = {
    val dataset = new RVFDataset[String, String]()
    val random = new Random(0)
    var sentCount = 0
    var droppedCands = 0
    var done = false
    for(s <- doc.sentences if ! done) {
      val outEdges = s.semanticRoles.get.outgoingEdges
      for(pred <- s.words.indices if isPred(pred, s)) {
        val history = new ArrayBuffer[Int]
        val args = outEdges(pred).map(_._1).toSet
        for(arg <- s.words.indices) {
          if(ValidCandidate.isValid(s, arg, pred)) {
            if (args.contains(arg)) {
              dataset += mkDatum(s, arg, pred, history, POS_LABEL)
              history += arg
            } else {
              // down sample negatives
              if (random.nextDouble() < DOWNSAMPLE_PROB) {
                dataset += mkDatum(s, arg, pred, history, NEG_LABEL)
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

      if(MAX_TRAINING_DATUMS > 0 && dataset.size > MAX_TRAINING_DATUMS)
        done = true
    }
    logger.debug(s"Dropped $droppedCands candidate arguments.")
    dataset
  }

  def isPred(position:Int, s:Sentence):Boolean = {
    val oes = s.semanticRoles.get.outgoingEdges
    position < oes.length && oes(position) != null && oes(position).nonEmpty
  }

  def mkDatum(sent:Sentence, arg:Int, pred:Int, history:ArrayBuffer[Int], label:String): RVFDatum[String, String] = {
    new RVFDatum[String, String](label, featureExtractor.mkFeatures(sent, arg, pred, history))
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

object ArgumentClassifier {
  val logger = LoggerFactory.getLogger(classOf[ArgumentClassifier])

  val FEATURE_THRESHOLD = 2
  val DOWNSAMPLE_PROB = 0.50
  val MAX_TRAINING_DATUMS = 0 // 0 means all data

  val POS_LABEL = "+"
  val NEG_LABEL = "-"

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

    val lc = Counter.loadFrom[String](reader)
    logger.debug(s"Successfully loaded lemma count hash for the argument classifier, with ${lc.size} keys.")
    val c = LiblinearClassifier.loadFrom[String, String](reader)
    logger.debug(s"Successfully loaded the argument classifier.")

    ac.classifier = Some(c)
    ac.lemmaCounts = Some(lc)
    ac.featureExtractor.lemmaCounts = ac.lemmaCounts

    ac
  }
}