package org.clulab.discourse.rstparser

import org.slf4j.LoggerFactory
import org.clulab.learning._
import org.clulab.processors.Document
import StructureClassifier._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import java.io.Writer
import scala.collection.mutable
import Utils._

/**
 *
 * User: mihais
 * Date: 5/21/14
 */
class StructureClassifier {
  val featureExtractor = new RelationFeatureExtractor(filter = null)
  var classifier:Classifier[String, String]  = null
  var scaleRanges:ScaleRange[String] = null
  var corpusStats:CorpusStats = null

  def saveTo(w:Writer, saveCorpusStats:Boolean = false) {
    if(saveCorpusStats) corpusStats.saveTo(w)
    classifier.saveTo(w)
    scaleRanges.saveTo(w)
  }

  /**
   * Trains using all .dis in the given directory, assuming gold segmentation and gold labels
   */
  def train(trees:List[(DiscourseTree, Document)], cs:CorpusStats) {
    logger.debug("Creating dataset...")
    scaleRanges = null
    corpusStats = cs
    val structDataset = mkDataset(trees)
    logger.debug("Scaling dataset...")
    scaleRanges = Datasets.svmScaleDataset(structDataset, lower = LOWER, upper = UPPER)

    classifier = new PerceptronClassifier[String, String](epochs = 5)

    logger.debug("Training the STRUCT classifier...")
    classifier.train(structDataset)
    classifier match {
      case c:LiblinearClassifier[String, String] => printTopWeights(c)
      case _ => // TODO: add weights for perceptron
    }
  }

  def mkTreeIntraFirst(edus:Array[Array[(Int, Int)]],
                       doc:Document,
                       relationModel:RelationClassifier,
                       verbose:Boolean = false):DiscourseTree = {
    // first, parse within each sentence
    val sentTrees = new ArrayBuffer[DiscourseTree]()
    for (i <- 0 until edus.size) {
      // this is the top tree for this sentence
      val t = mkTree(edus, doc, relationModel, i, i + 1, verbose)
      sentTrees += t
    }

    // continue parsing on top of the trees for each sentence
    parseTreeSeq(sentTrees.toArray, edus, doc, relationModel, verbose)
  }

  def mkTree(edus:Array[Array[(Int, Int)]],
             doc:Document,
             relationModel:RelationClassifier,
             verbose:Boolean = false):DiscourseTree = {
    mkTree(edus, doc, relationModel, 0, edus.size, verbose)
  }

  def mkTree(edus:Array[Array[(Int, Int)]],
             doc:Document,
             relationModel:RelationClassifier,
             startSentence:Int,
             endSentence:Int,
             verbose:Boolean):DiscourseTree = {
    val startSenOffset = startSentence match {
      case -1 => 0
      case _ => startSentence
    }
    val endSenOffset = endSentence match {
      case -1 => edus.length
      case _ => endSentence
    }

    // convert the EDUs to terminal trees
    if (verbose) println(s"Working with ${edus.size} EDUs.")
    val trees = edusToTerminalTrees(doc, edus, startSenOffset, endSenOffset)

    // parse using the above trees as leaves
    parseTreeSeq(trees, edus, doc, relationModel, verbose)
  }

  def parseTreeSeq(treeSeq:Array[DiscourseTree],
                   edus:Array[Array[(Int, Int)]],
                   doc:Document,
                   relationModel:RelationClassifier,
                   verbose:Boolean = false):DiscourseTree = {
    // bottom-up parsing: iteratively link the 2 trees with the highest score
    var trees = treeSeq
    var iteration = 0
    while(trees.size > 1) {
      if(verbose) summarizeTrees(trees, iteration)

      // find the link with the highest score
      var maxScore = Double.MinValue
      var maxPos = -1

      // traversing right-to-left is better for this problem
      // or at least it was, with a simple feature set. now it doesn't seem to make a difference
      for(pos <- trees.size - 2 to 0 by -1) {
        val left = trees(pos)
        val right = trees(pos + 1)
        val d = mkDatum(left, right, doc, edus, NEG)
        val s = classifier.scoresOf(d).getCount(POS)
        if(verbose) println("Datum score: " + s)
        if(s > maxScore) {
          maxScore = s
          maxPos = pos
        }
      }
      if(verbose) println(s"Maximum score of $maxScore seen at position $maxPos")

      // create a new tree merging those two nodes
      val children = new Array[DiscourseTree](2)
      children(0) = trees(maxPos)
      children(1) = trees(maxPos + 1)
      val d = relationModel.mkDatum(trees(maxPos), trees(maxPos + 1), doc, edus, NEG)
      val ld = relationModel.classOf(d)
      val (label, dir) = relationModel.parseLabel(ld)
      val merge = new DiscourseTree(label, dir, children)

      // adjust the list of candidate trees
      val b = new ArrayBuffer[DiscourseTree]()
      b ++= trees.slice(0, maxPos)
      b += merge
      b ++= trees.slice(maxPos + 2, trees.size)
      trees = b.toArray

      iteration += 1
    }
    if(verbose) summarizeTrees(trees, iteration)

    trees.head
  }

  def mkTreeBaseline(edus:Array[Array[(Int, Int)]], doc:Document, verbose:Boolean = false):DiscourseTree = {
    // convert the EDUs to terminal trees
    if(verbose) println(s"Working with ${edus.size} EDUs.")
    var trees = edusToTerminalTrees(doc, edus, 0, edus.size)

    // bottom-up parsing: iteratively link the 2 trees with the highest baseline score
    var iteration = 0
    while(trees.size > 1) {
      if(verbose) summarizeTrees(trees, iteration)

      // find the link with the highest score
      var maxScore = Double.MinValue
      var maxPos = -1

      // traversing right-to-left is better for this problem
      for(pos <- trees.size - 2 to 0 by -1){
        val left = trees(pos)
        val right = trees(pos + 1)
        val s = baselineScore(left, right, doc, edus)
        if(s > maxScore) {
          maxScore = s
          maxPos = pos
        }
      }
      if(verbose) println(s"Maximum score of $maxScore seen at position $maxPos")

      // create a new tree merging those two nodes
      val children = new Array[DiscourseTree](2)
      children(0) = trees(maxPos)
      children(1) = trees(maxPos + 1)
      val merge = new DiscourseTree(POS, RelationDirection.None, children)

      // adjust the list of candidate trees
      val b = new ArrayBuffer[DiscourseTree]()
      b ++= trees.slice(0, maxPos)
      b += merge
      b ++= trees.slice(maxPos + 2, trees.size)
      trees = b.toArray

      iteration += 1
    }

    trees.head
  }

  def baselineScore(left:DiscourseTree,
                    right:DiscourseTree,
                    doc:Document,
                    edus:Array[Array[(Int, Int)]]):Double = {
    //
    // score values:
    // 3: they belong to the same S* subtree
    // 2: they belong to the same sentence
    // 1: otherwise
    //

    if(left.firstSentence == right.lastSentence) {
      val leftSubtree = findSubtree(left, doc)
      val rightSubtree = findSubtree(right, doc)
      if(sameSubtree(leftSubtree, rightSubtree))
        return 3
      return 2
    }

    1
  }

  def summarizeTrees(trees:Iterable[DiscourseTree], iteration:Int) {
    println(s"Iteration #$iteration:")
    var pos = 0
    for(t <- trees) {
      println(s"\tT$pos:\t${t.firstToken} ${t.lastToken} ${t.relationLabel}")
      pos += 1
    }
  }

  def mkDataset(trees: List[(DiscourseTree, Document)]):Dataset[String, String] = {
    val structDataset = new RVFDataset[String, String]
    val total = trees.size
    var count = 0
    logger.info (s"Generating training corpus from $total documents.")
    for(td <- trees) {
      //println("ENTIRE TREE:\n" + td._1)
      val edus = mkGoldEDUs(td._1, td._2)
      addPositiveAndNegativeExamples1(structDataset, td._1, td._2, edus)
      count += 1
      if(count % 10 == 0)
        logger.debug(s"Processed $count documents.")
    }
    structDataset
  }

  def addPositiveAndNegativeExamples1(structDataset:Dataset[String, String],
                                      tree: DiscourseTree,
                                      doc:Document,
                                      edus:Array[Array[(Int, Int)]]) {
    if(tree.isTerminal) return
    assert(tree.children.length == 2)

    val lefts = new ArrayBuffer[DiscourseTree]()
    extractRightMost(tree.children(0), lefts)
    val rights = new ArrayBuffer[DiscourseTree]()
    extractLeftMost(tree.children(1), rights)

    for(i <- 0 until lefts.length) {
      for(j <- 0 until rights.length) {
        var l = NEG
        if(i == 0 && j == 0) l = POS
        val left = lefts(i)
        val right = rights(j)
        val d = mkDatum(left, right, doc, edus, l)
        structDataset += d
      }
    }

    for(c <- tree.children) {
      addPositiveAndNegativeExamples1(structDataset, c, doc, edus)
    }
  }

  def edusToTerminalTrees(doc:Document,
                          edus:Array[Array[(Int, Int)]],
                          startSen:Int,
                          endSen:Int):Array[DiscourseTree] = {
    var treesBuffer = new ListBuffer[DiscourseTree]
    for (i <- startSen until endSen) {
      for(j <- 0 until edus(i).size) {
        val edu = edus(i)(j)
        treesBuffer += new DiscourseTree(i, edu._1, edu._2, doc, j)
      }
    }
    treesBuffer.toArray
  }

  def addPositiveAndNegativeExamples2(dataset:Dataset[String, String],
                                      tree: DiscourseTree,
                                      doc:Document,
                                      edus:Array[Array[(Int, Int)]]) {
    val v = false
    if(v) println("Using tree: " + tree)
    // extract the gold discourse dependencies
    val discourseDeps = new mutable.HashSet[(Int, Int, Int, Int)]
    extractDiscourseDependencies(tree, discourseDeps)
    if(v) println("Gold dependencies: " + discourseDeps)

    // convert the EDUs to terminal trees
    var trees = edusToTerminalTrees(doc, edus, 0, edus.size)

    // bottom-up parsing: iteratively try to link every two trees
    while(trees.size > 1) {
      if(v) {
        println("Using trees:")
        for (i <- 0 until trees.size)
          println(s"Tree #$i: ${trees(i)}")
      }

      // create +/- examples
      var posCount = 0
      var negCount = 0
      for(pos <- 0 until trees.size - 1) {
        val left = trees(pos)
        val right = trees(pos + 1)
        val dep = new Tuple4(
          left.firstSentence, left.firstEDU,
          right.lastSentence, right.lastEDU)
        if(v) println("Searching for dependency " + dep)
        var label = NEG
        if (discourseDeps.contains(dep)) {
          label = POS
          posCount += 1
          if(v) println("\tthis is a +")
        } else {
          negCount += 1
          if(v) println("\tthis is a -")
        }

        val d = mkDatum(left, right, doc, edus, label)
        dataset += d
      }

      // create a new tree merging those nodes in a gold dependency
      var mergedTrees = new ArrayBuffer[DiscourseTree]()
      var pos = 0
      var mergeCount = 0
      while(pos < trees.size) {
        if(isDep(trees, pos, discourseDeps)) {
          val children = new Array[DiscourseTree](2)
          children(0) = trees(pos)
          children(1) = trees(pos + 1)
          val merge = new DiscourseTree(POS, RelationDirection.None, children)
          mergedTrees += merge
          pos += 2
          mergeCount += 1
        } else {
          mergedTrees += trees(pos)
          pos += 1
        }
      }

      trees = mergedTrees.toArray
      if(v) println(s"merged $mergeCount trees with $posCount positive examples and $negCount negative examples.")
      if(mergeCount == 0)
        throw new RuntimeException("ERROR: we must always merge some trees in training...")
    }
  }

  def isDep(trees:Array[DiscourseTree], pos:Int, discourseDeps:mutable.HashSet[(Int, Int, Int, Int)]):Boolean = {
    if(pos >= trees.size - 1) return false

    val left = trees(pos)
    val right = trees(pos + 1)
    val dep = new Tuple4(
      left.firstSentence, left.firstEDU,
      right.lastSentence, right.lastEDU)

    if(discourseDeps.contains(dep)) true
    else false
  }

  def extractDiscourseDependencies(tree: DiscourseTree, deps:mutable.HashSet[(Int, Int, Int, Int)]) {
    if(! tree.isTerminal) {
      assert(tree.children.length == 2)
      val c1 = tree.children(0)
      val c2 = tree.children(1)
      deps += new Tuple4(
        c1.firstSentence, c1.firstEDU,
        c2.lastSentence, c2.lastEDU)

      for(c <- tree.children) {
        extractDiscourseDependencies(c, deps)
      }
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
}

object StructureClassifier {
  val logger = LoggerFactory.getLogger(classOf[StructureClassifier])

  val POS = "+"
  val NEG = "-"

  val LOWER = -1.0
  val UPPER = +1.0

  def loadFrom(r:java.io.Reader, corpusStats:CorpusStats):StructureClassifier = {
    var cs = corpusStats
    if(cs == null) cs = CorpusStats.loadFrom[String](r)
    val sc = PerceptronClassifier.loadFrom[String, String](r)
    val sr = ScaleRange.loadFrom[String](r)

    val c = new StructureClassifier
    c.classifier = sc
    c.scaleRanges = sr
    c.corpusStats = cs
    c
  }

}
