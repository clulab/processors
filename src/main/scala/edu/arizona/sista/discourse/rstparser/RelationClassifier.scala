package edu.arizona.sista.discourse.rstparser

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import edu.arizona.sista.discourse.rstparser.Utils._
import edu.arizona.sista.learning._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.utils.StringUtils
import org.slf4j.LoggerFactory
import RelationClassifier._

/**
 *
 * User: mihais
 * Date: 6/12/14
 */
class RelationClassifier(var withNuclearity:Boolean = true) {
  val featureExtractor = new RelationFeatureExtractor(PREFIXES)
  var classifier:Classifier[String, String]  = null
  var scaleRanges:ScaleRange[String] = null
  var corpusStats:CorpusStats = null

  def saveTo(os:ObjectOutputStream, saveCorpusStats:Boolean = false) {
    if(saveCorpusStats) os.writeObject(corpusStats)
    os.writeObject(withNuclearity)
    os.writeObject(classifier)
    os.writeObject(scaleRanges)
  }

  /** Trains the relation classifier, assuming gold segmentation and structure */
  def train(trees:List[(DiscourseTree, Document)], cs:CorpusStats) {
    logger.debug("Creating dataset...")
    scaleRanges = null
    corpusStats = cs
    val dataset = mkDataset(trees)
    logger.debug("Scaling dataset...")
    scaleRanges = Datasets.svmScaleDataset(dataset, lower = LOWER, upper = UPPER)

    classifier = new LogisticRegressionClassifier[String, String]()

    logger.debug("Training the LABEL classifier...")
    classifier.train(dataset)
    classifier match {
      case c:LiblinearClassifier[String, String] => printTopWeights(c)
      case _ => // nothing
    }
  }

  def acc(output:Iterable[(String, String)]):Double = {
    val total = output.size.toDouble
    var correct = 0
    for(o <- output) if(o._1 == o._2) correct += 1
    correct.toDouble / total
  }

  def featureSelectionIncremental(trees:List[(DiscourseTree, Document)], corpusStats:CorpusStats) {
    logger.debug("Creating dataset...")
    val dataset = mkDataset(trees)

    val featureGroups = Utils.findFeatureGroups(":", dataset.featureLexicon)
    logger.debug(s"Found ${featureGroups.size} feature groups:")
    for(f <- featureGroups.keySet) {
      logger.debug(s"Group $f containing ${featureGroups.get(f).get.size} features.")
    }

    val chosenGroups = Datasets.incrementalFeatureSelection(
      dataset, lrFactory, acc, featureGroups,
      addAllBetter = false, numFolds = 3)
    logger.info(s"Selected ${chosenGroups.size} feature groups: " + chosenGroups)
  }

  def mkDataset(trees: List[(DiscourseTree, Document)]):Dataset[String, String] = {
    val labelDataset = new RVFDataset[String, String]()
    val total = trees.size
    var count = 0
    logger.info (s"Generating training corpus from $total documents.")
    for(td <- trees) {
      //println("ENTIRE TREE:\n" + td._1)
      val edus = mkGoldEDUs(td._1, td._2)
      addExamples(labelDataset, td._1, td._2, edus)
      count += 1
      if(count % 10 == 0)
        logger.debug(s"Processed $count documents.")
    }
    labelDataset
  }

  def addExamples(labelDataset:Dataset[String, String],
                  tree: DiscourseTree,
                  doc:Document,
                  edus:Array[Array[(Int, Int)]]) {
    if(tree.isTerminal) return
    assert(tree.children.length == 2)

    val left = tree.children(0)
    val right = tree.children(1)
    val l = mkLabel(tree.relationLabel, tree.relationDirection)
    val d = mkDatum(left, right, doc, edus, l)
    labelDataset += d

    for(c <- tree.children) {
      addExamples(labelDataset, c, doc, edus)
    }
  }

  def mkDatum (left: DiscourseTree,
               right: DiscourseTree,
               doc:Document,
               edus:Array[Array[(Int, Int)]],
               label: String):Datum[String, String] = {
    val feats = featureExtractor.mkFeatures(left, right, doc, edus, corpusStats, label)
    if(scaleRanges != null) {
      val scaledFeats = Datasets.svmScaleDatum(feats, scaleRanges, lower = LOWER, upper = UPPER)
      new RVFDatum[String, String](label, scaledFeats)
    } else {
      new RVFDatum[String, String](label, feats)
    }
  }

  /** Tests the relation classifier, assuming gold segmentation and structure */
  def test(trees:List[(DiscourseTree, Document)]) {
    val scorer = new DiscourseScorer
    val score = new DiscourseScore()

    for(td <- trees) {
      val edus = mkGoldEDUs(td._1, td._2)
      test(td._1, td._2, edus, scorer, score)
    }

    logger.info("LABEL ACCURACY:\n" + score)
  }

  def classOf(datum:Datum[String, String]) = classifier.classOf(datum)

  /** Tests the relation classifier, assuming gold segmentation and structure */
  def test(tree:DiscourseTree,
           doc:Document,
           edus:Array[Array[(Int, Int)]],
           scorer:DiscourseScorer,
           score:DiscourseScore) {
    if(! tree.isTerminal) {
      val d = mkDatum(tree.children(0), tree.children(1), doc, edus, "")
      val l = classifier.classOf(d)
      scorer.score(l, mkLabel(tree.relationLabel, tree.relationDirection), score)

      for(c <- tree.children) {
        test(c, doc, edus, scorer, score)
      }
    }
  }

  def mkLabel(relLabel:String, relDir:RelationDirection.Value):String = {
    if(withNuclearity) {
      relDir match {
        case RelationDirection.LeftToRight => relLabel + ">"
        case RelationDirection.RightToLeft => relLabel + "<"
        case _ => relLabel
      }
    } else relLabel
  }

  def parseLabel(label:String):(String, RelationDirection.Value) = {
    if(withNuclearity) {
      if(label.endsWith(">")) (label.substring(0, label.size - 1), RelationDirection.LeftToRight)
      else if(label.endsWith("<")) (label.substring(0, label.size - 1), RelationDirection.RightToLeft)
      else (label, RelationDirection.None)
    } else (label, RelationDirection.None)
  }
}

object RelationClassifier {
  val logger = LoggerFactory.getLogger(classOf[RelationClassifier])

  val LOWER = -1.0
  val UPPER = +1.0

  val PREFIXES = mkPrefixes("true-dominatinglabel, suffix_L, false-dominatedword, false-dominatingtag, Sentence_span_R, dominatedlabel, toprule, tprefix_R, false-dominatedlabel, prefix_L, tsuffix_L, Sentence_span_L, dominatingword, prefix_R, Sentence_tokens_cover_R, dominatinglabel, suffix_L, num_edus_diff, embedded, dominatedword, Sentence_EDUs_cover_L, true-ancestorlabel, false-dominatingword, false-ancestortag, dominatedlabel, false-dominatingtag, num_edus_diff, samesent-dominates-unk, dominatedtag, true-dominatedtag, dominates-true, false-dominatedtag")

  def mkPrefixes(fs:String): Set[String] = fs.split(",\\s*").map(_.trim).toSet

  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    var cls:RelationClassifier = null

    if(props.containsKey("train")) {
      cls = new RelationClassifier (withNuclearity = true)
      val (trees, corpusStats) = RSTParser.mkTrees(props.getProperty("train"))
      cls.train(trees, corpusStats)
      if(props.containsKey("model")) {
        val os = new ObjectOutputStream(new FileOutputStream(props.getProperty("model")))
        cls.saveTo(os, saveCorpusStats = true)
        os.close()
      }
    }
    if(props.containsKey("test")) {
      val (trees, _) = RSTParser.mkTrees(props.getProperty("test"), makeStats = false)
      if(props.containsKey("model")) {
        val is = new ObjectInputStream(new FileInputStream(props.getProperty("model")))
        cls = loadFrom(is, corpusStats = null)
        is.close()
      }
      cls.test(trees)
    }
    if(props.containsKey("fsel")) {
      val (trees, corpusStats) = RSTParser.mkTrees(props.getProperty("fsel"))
      cls.featureSelectionIncremental(trees, corpusStats)
    }
  }

  def loadFrom(is:ObjectInputStream, corpusStats:CorpusStats):RelationClassifier = {
    var cs = corpusStats
    if(cs == null) cs = is.readObject().asInstanceOf[CorpusStats]
    val wn = is.readObject().asInstanceOf[Boolean]
    val c = is.readObject().asInstanceOf[Classifier[String, String]]
    val sr = is.readObject().asInstanceOf[ScaleRange[String]]

    val r = new RelationClassifier
    r.withNuclearity = wn
    r.corpusStats = cs
    r.classifier = c
    r.scaleRanges = sr

    r
  }
}
