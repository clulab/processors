package org.clulab.discourse.rstparser

import org.slf4j.LoggerFactory
import org.clulab.utils.{Files, StringUtils}
import org.clulab.processors.Document
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import EDUClassifier._
import org.clulab.struct.Counter
import org.clulab.learning._
import java.io._
import Utils._

/**
 * Detects EDU boundaries
 * User: mihais
 * Date: 4/12/14
 */
class EDUClassifier {
  var classifier:Classifier[String, String] = null
  var scaleRanges:ScaleRange[String] = null

  def isTrained:Boolean = classifier != null

  def saveTo(w:Writer) {
    classifier.saveTo(w)
    scaleRanges.saveTo(w)
  }

  /**
   * Trains using all .dis in the given directory
   */
  def train(trees:List[(DiscourseTree, Document)], corpusStats:CorpusStats) {
    logger.debug("Constructing dataset...")
    scaleRanges = null
    val dataset = constructDataset(trees)

    logger.debug("Scaling dataset...")
    scaleRanges = Datasets.svmScaleDataset(dataset)

    logger.debug("Training...")
    classifier = new PerceptronClassifier[String, String](epochs = 5)
    //classifier = new LinearSVMClassifier[String, String]()
    //classifier = new LibSVMClassifier[String, String](PolynomialKernel)
    //classifier = new LogisticRegressionClassifier[String, String]()
    //classifier = new RandomForestClassifier[String, String](numTrees = 20)
    //classifier = new BaggingClassifier[String, String](svmFactory, 10, new Random(1))

    classifier.train(dataset)
  }

  /** Tests the standalone segmenter for intra-sentence classification */
  def test (trees:List[(DiscourseTree, Document)]) {

    val tokenStats = new Counter[Boolean]()
    val tokens = extractEDUTokens(trees, tokenStats)
    logger.info("Found " + tokens.size + " EDU tokens in TESTING: " +
      tokenStats.getCount(true) + " +, " +
      tokenStats.getCount(false) + " -.")

    val datums = mkDatums(tokens)
    val output = new ListBuffer[(String, String)]
    var i = 0
    for(datum <- datums) {
      val token = tokens(i)
      val l = classOf(datum)
      output += new Tuple2(datum.label, l)

      if(datum.label == POS && l == NEG) {
        assert(token.eduStart)
        println("MISSED THIS TOKEN:")
        report(token, "FN")
      } else if(datum.label == NEG && l == POS) {
        assert(! token.eduStart)
        println("FALSE POSITIVE:")
        report(token, "FP")
      }

      i += 1
    }
    val (p, r, f, correct, predicted, total) = f1(output.toList)
    logger.info(s"P = $p + ($correct / $predicted)")
    logger.info(s"R = $r + ($correct / $total)")
    logger.info(s"F1 = $f")

  }

  def report(token:EDUToken, errType:String) {
    val doc = token.doc
    val sent = doc.sentences(token.position.sentence)
    val offset = token.position.token
    val context = 10

    print("...")
    for(i <- scala.math.max(0, offset - context) until scala.math.min(offset + context, sent.size)) {
      print(" ")
      if(i == offset) print("[[")
      print(sent.words(i))
      if(i == offset) print("]]")
    }
    println("...")
    println("Incoming dependencies for token:")
    val inc = deps(sent).incomingEdges
    if(offset < inc.size) {
      for (d <- inc(offset)) {
        println("\t" + sent.words(d._1) + "\t" + d._2)
      }
    }
    if(offset < deps(sent).outgoingEdges.size) {
      val (_, top) = featureExtractor.pathToRoot(offset, deps(sent).incomingEdges)
      val leftMost = deps(sent).outgoingEdges(offset).size == 0
      println(errType + "\tleftmost:" + top + "|" + leftMost)
    }
  }

  /**
   * Extracts all document tokens, identifying if they are the beginning of an intra-sentence EDU or not.
   * It skips the first tokens in each sentence because they are not useful during classification (they always begin an EDU).
   * @param trees Extract tokens from all these trees
   * @return All tokens to be used during classification
   */
  private def extractEDUTokens(trees:List[(DiscourseTree, Document)], tokenStats:Counter[Boolean]):Array[EDUToken] = {
    val tokens = new ListBuffer[EDUToken]
    for(tree <- trees) {
      // match known connectives in this document
      val connectives = ConnectiveMatcher.matchConnectives(tree._2)
      val docTokens = new ListBuffer[EDUToken]

      // find positive examples
      val starts = new mutable.HashSet[TokenOffset]()
      findEDUStarts(tree._1, starts)
      for(s <- starts) {
        docTokens += new EDUToken(s, tree._2, connectives, true)
        tokenStats.incrementCount(true)
      }

      // find negative examples
      val doc = tree._2
      for(si <- 0 until doc.sentences.size) {
        val sent = doc.sentences(si)
        // skip the last token in a sentence: it can never be the start of an EDU
        for(ti <- 0 until sent.words.size - 1) {
          val p = new TokenOffset(si, ti)
          if(! starts.contains(p)) {
            docTokens += new EDUToken(p, doc, connectives, false)
            tokenStats.incrementCount(false)
          }
        }
      }

      // sort candidates in sentence/token order in this document
      tokens ++= docTokens.toList.sortWith(boundarySort)
    }

    //printEDUBoundaries(tokens)
    //eduBoundaryStats(tokens)
    tokens.toArray
  }

  def boundarySort(e1:EDUToken, e2:EDUToken):Boolean = {
    if(e1.position.sentence < e2.position.sentence) return true
    if(e1.position.sentence > e2.position.sentence) return false
    if(e1.position.token < e2.position.token) return true
    false
  }

  def eduBoundaryStats(tokens:Iterable[EDUToken]) {
    val posCounts = new Counter[String]()
    val decileCounts = new Counter[Int]()
    for(t <- tokens) {
      if(t.eduStart) {
        val s = t.doc.sentences(t.position.sentence)
        if(s.tags.isDefined) {
          val crt = s.tags.get(t.position.token)
          posCounts.incrementCount(crt)
        }
        decileCounts.incrementCount(Utils.toDecile(t.position.token, s.size))
      }
    }

    println("HISTOGRAM OF POS TAGS:")
    val tags = posCounts.sorted
    for(t <- tags) println(t._1 + "\t" + t._2)

    println("HISTOGRAM OF DECILE COUNTS:")
    val decs = decileCounts.sorted
    for(d <- decs) println(d._1 + "\t" + d._2)

  }

  def printEDUBoundaries(tokens:Iterable[EDUToken]) {
    for(t <- tokens) {
      if(t.eduStart) {
        val s = t.doc.sentences(t.position.sentence)
        val crt = s.words(t.position.token)
        val crtTag = s.tags.get(t.position.token)
        val prev = s.words(t.position.token - 1)
        var next = "END"
        if(t.position.token < s.size - 1)
          next = s.words(t.position.token + 1)
        println("... " + prev + " " + crt + " " + next + " ...")
        if(next == "END") {
          print("ENDSENT: ")
          for(w <- s.words) print(w + " ")
          println()
        }
        if(crtTag == ":") {
          print("COLONSENT: ")
          for(w <- s.words) print(w + " ")
          println()
        }
      }
    }
  }

  def findEDUStarts(tree:DiscourseTree, starts:mutable.HashSet[TokenOffset]) {
    if(tree.isTerminal) {
      val s = tree.firstToken
      // we only care about EDUs that are intra sentence, so we ignore EDUs that start at the beginning of sentence
      if(s.token != 0) {
        starts.add(s)
      }
    } else {
      for(c <- tree.children) {
        findEDUStarts(c, starts)
      }
    }
  }

  private def mkDataset(tokens:Array[EDUToken]):Dataset[String, String] = {
    val dataset = new RVFDataset[String, String]()
    for(i <- 0 until tokens.size) {
      val token = tokens(i)
      val datum = mkDatum(token)
      dataset += datum
    }
    dataset
  }

  private def mkDatums(tokens:Array[EDUToken]):Iterable[Datum[String, String]] = {
    val datums = new ListBuffer[Datum[String, String]]
    for(i <- 0 until tokens.size) {
      val token = tokens(i)
      val datum = mkDatum(token)
      datums += datum
    }
    datums.toList
  }

  def mkDatum(token:EDUToken):Datum[String, String] = {
    var label = NEG
    if(token.eduStart) label = POS
    val feats = featureExtractor.mkFeatures(token.position, token.doc, token.connectives)

    if(scaleRanges != null) {
      val scaledFeats = Datasets.svmScaleDatum(feats, scaleRanges)
      new RVFDatum[String, String](label, scaledFeats)
    } else {
      new RVFDatum[String, String](label, feats)
    }
  }

  def classOf(datum:Datum[String, String]) = classifier.classOf(datum)

  def constructDataset(trees:List[(DiscourseTree, Document)]):Dataset[String, String] = {
    // find all intra-sentence tokens that can be EDU boundaries
    val tokenStats = new Counter[Boolean]()
    val tokens = extractEDUTokens(trees, tokenStats)
    logger.info("Found " + tokens.size + " EDU tokens: " +
      tokenStats.getCount(true) + " +, " +
      tokenStats.getCount(false) + " -.")
    //for(t <- tokens) println(t.position.sentence + "\t" + t.position.token + "\t" + t.eduStart)

    // make the actual dataset with positive and negative examples
    val dataset = mkDataset(tokens)
    dataset
  }

  def f1(output:Iterable[(String, String)]):(Double, Double, Double, Int, Int, Int) = {
    var total = 0
    var pred = 0
    var correct = 0
    for(o <- output) {
      val gold = o._1
      val sys = o._2
      if(gold == POS) total += 1
      if(sys == POS) {
        pred += 1
        if(sys == gold) correct += 1
      }
    }
    val p = correct.toDouble / pred.toDouble
    val r = correct.toDouble / total.toDouble
    val f = 2 * p * r / (p + r)
    (p, r, f, correct, pred, total)
  }

  def simpleF1(output:Iterable[(String, String)]):Double = f1(output)._3

  def featureSelectionIncremental(trees:List[(DiscourseTree, Document)], corpusStats:CorpusStats) {
    val dataset = constructDataset(trees)
    val featureGroups = Utils.findFeatureGroups(":", dataset.featureLexicon)
    logger.debug(s"Found ${featureGroups.size} feature groups:")
    for(f <- featureGroups.keySet) {
      logger.debug(s"Group $f containing ${featureGroups.get(f).get.size} features.")
    }

    val chosenGroups = Datasets.incrementalFeatureSelection(
      dataset, Utils.svmFactory, simpleF1, featureGroups)
    logger.info(s"Selected ${chosenGroups.size} feature groups: " + chosenGroups)
  }

  def featureSelectionByInformativeness(trees:List[(DiscourseTree, Document)], corpusStats:CorpusStats) {
    val dataset = constructDataset(trees)
    val chosenFeatures = Datasets.featureSelectionByInformativeness(dataset, Utils.svmFactory, simpleF1)
  }

  def featureSelectionByFrequency(trees:List[(DiscourseTree, Document)], corpusStats:CorpusStats) {
    val dataset = constructDataset(trees)
    val chosenFeatures = Datasets.featureSelectionByFrequency(dataset, Utils.svmFactory, simpleF1)
  }
}

class EDUToken (val position:TokenOffset, val doc:Document, val connectives:Array[Array[String]], val eduStart:Boolean)

object EDUClassifier {
  val logger = LoggerFactory.getLogger(classOf[EDUClassifier])
  val featureExtractor = new EDUFeatureExtractor

  val POS = "+"
  val NEG = "-"

  def loadFrom(r:java.io.Reader):EDUClassifier = {
    val edu = new EDUClassifier
    val reader = Files.toBufferedReader(r)

    val c = PerceptronClassifier.loadFrom[String, String](reader)
    val sr = ScaleRange.loadFrom[String](reader)

    edu.classifier = c
    edu.scaleRanges = sr
    edu
  }

}
