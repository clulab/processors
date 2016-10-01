package org.clulab.discourse.rstparser

import java.io._

import org.clulab.discourse.rstparser.Utils._
import org.clulab.learning._
import org.clulab.processors.Document
import org.clulab.utils.Files
import org.slf4j.LoggerFactory
import RelationClassifier._

/**
 *
 * User: mihais
 * Date: 6/12/14
 */
class RelationClassifier( val prefixes:String,
                          var withNuclearity:Boolean = true) {
  val featureExtractor = new RelationFeatureExtractor(mkPrefixes(prefixes))
  var classifier:Classifier[String, String]  = null
  var scaleRanges:ScaleRange[String] = null
  var corpusStats:CorpusStats = null

  def saveTo(w:Writer, saveCorpusStats:Boolean = false) {
    val p = Files.toPrintWriter(w)
    p.println(prefixes)
    p.println(withNuclearity)
    if(saveCorpusStats) corpusStats.saveTo(p)
    scaleRanges.saveTo(p)
    classifier.saveTo(p)
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

  def featureSelectionIncremental(trees:List[(DiscourseTree, Document)], cs:CorpusStats) {
    assert(trees != null)
    assert(cs != null)
    logger.debug("Creating dataset...")
    scaleRanges = null
    corpusStats = cs
    val dataset = mkDataset(trees)
    logger.debug("Scaling dataset...")
    scaleRanges = Datasets.svmScaleDataset(dataset, lower = LOWER, upper = UPPER)

    val featureGroups = Utils.findFeatureGroups(":", dataset.featureLexicon)
    logger.debug(s"Found ${featureGroups.size} feature groups:")
    for(f <- featureGroups.keySet) {
      logger.debug(s"Group $f containing ${featureGroups.get(f).get.size} features.")
    }

    val chosenGroups = Datasets.incrementalFeatureSelection(
      dataset, lrFactory, acc, featureGroups, numFolds = 3)
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

  // for corenlp
  val CONSTITUENTSYNTAX_PREFIXES = "true-dominatinglabel, suffix_L, false-dominatedword, false-dominatingtag, Sentence_span_R, dominatedlabel, toprule, tprefix_R, false-dominatedlabel, prefix_L, tsuffix_L, Sentence_span_L, dominatingword, prefix_R, Sentence_tokens_cover_R, dominatinglabel, suffix_L, num_edus_diff, embedded, dominatedword, Sentence_EDUs_cover_L, true-ancestorlabel, false-dominatingword, false-ancestortag, dominatedlabel, false-dominatingtag, num_edus_diff, samesent-dominates-unk, dominatedtag, true-dominatedtag, dominates-true, false-dominatedtag"
  // for fastnlp
  val DEPENDENCYSYNTAX_PREFIXES = "suffix_L, Num_edus_R, toprule, Embedded_in_subtree_with_other_EDU_L, Sentence_EDUs_cover_R, deps-dominatedword, tprefix_R, deps-domrel, prefix_L, false-deps-domrel, tsuffix_L, deps-ancestorisroot, Embedded_in_subtree_with_other_EDUL, prefix_R, false-deps-dominatingword, rule-R-d1, deps-ancestortag, Embedded_in_subtree_with_other_EDUR, true-deps-dominatingtag, tprefix_L, deps-domrel, Num_edus_R, true-deps-dominatingtag, deps-dominatedword, rule-L-d1, Num_edus_L, Embedded_in_subtree_with_other_EDUR, deps-dominatedword, tprefix_R, true-deps-domrel, Sentence_tokens_cover_L, rule-R-d1, true-deps-dominatedtag, Len_R, Len_L, deps-ancestorisroot, deps-dominatedtag, deps-dominatedword, Sentence_EDUs_cover_L, embedded, Sentence_tokens_cover_R, deps-dominatedtag, true-deps-dominatedtag, false-deps-dominatingword, false-deps-dominatingword, Inside_sentence_R, Sentence_EDUs_cover_L, Embedded_in_subtree_with_other_EDUL, Sentence_EDUs_cover_R"

  def mkPrefixes(fs:String): Set[String] = fs.split(",\\s*").map(_.trim).toSet

  def loadFrom(ir:java.io.Reader, corpusStats:CorpusStats):RelationClassifier = {
    val reader = Files.toBufferedReader(ir)
    val prefs = reader.readLine()
    val wn = reader.readLine().toBoolean
    var cs = corpusStats
    if(cs == null) cs = CorpusStats.loadFrom[String](reader)
    val sr = ScaleRange.loadFrom[String](reader)
    val c = LiblinearClassifier.loadFrom[String, String](reader)

    val r = new RelationClassifier(prefs, withNuclearity = wn)
    r.corpusStats = cs
    r.classifier = c
    r.scaleRanges = sr

    r
  }
}
