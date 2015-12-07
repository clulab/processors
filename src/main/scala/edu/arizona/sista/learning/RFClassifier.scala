package edu.arizona.sista.learning

import java.io.{Writer, Serializable}

import edu.arizona.sista.struct.{Lexicon, Counter}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import RFClassifier._

import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.Random

/**
  * An in-house implementation of random forests
  * User: mihais
  * Date: 11/23/15
  */
class RFClassifier[L, F](numTrees:Int = 100,
                         maxTreeDepth:Int = 0,
                         numThreads:Int = 0,
                         trainBagPct:Double = 0.66) extends Classifier[L, F] with Serializable {
  var trees:Option[Array[RFTree]] = None

  var verbose = false

  /** Feature lexicon */
  private var featureLexicon:Option[Lexicon[F]] = None

  /** Label lexicon */
  private var labelLexicon:Option[Lexicon[L]] = None

  /**
    * Trains a classifier, using only the datums specified in indices
    * indices is useful for bagging
    */
  override def train(dataset: Dataset[L, F], indices: Array[Int]): Unit = {
    train(dataset.toCounterDataset, indices)
  }

  /**
    * Trains a classifier using a CounterDataset (better to compute feature utility)
    */
  def train(dataset: CounterDataset[L, F], indices: Array[Int]): Unit = {
    if(numThreads < 0) throw new RuntimeException("ERROR: numThreads must be >= 0!")
    if(numTrees < 1) throw new RuntimeException("ERROR: numTrees must be >= 1!")

    labelLexicon = Some(dataset.labelLexicon)
    featureLexicon = Some(dataset.featureLexicon)

    logger.debug(s"Training on a dataset containing ${dataset.size} datums, with ${dataset.labelLexicon.size} labels.")

    //
    // compute the feature thresholds
    //
    val thresholds = computeFeatureThresholds(dataset)

    //
    // construct the bags, one per tree
    //
    val bags = new ArrayBuffer[RFJob[L, F]]()
    val bagSize = math.ceil(trainBagPct * indices.length).toInt
    val randomSeed = new Random(RANDOM_SEED)
    for(i <- 0 until numTrees) {
      bags += mkBag(dataset, indices, thresholds, bagSize, randomSeed, i)
    }
    logger.debug(s"Constructed ${bags.size} bag(s), each containing $bagSize datums.")

    //
    // the actual tree building
    //
    logger.debug("Beginning tree building...")
    numThreads match {
      case 0 => // use as many threads as possible
        val parBags = bags.toSet.par
        trees = Some(parBags.map(buildTree).toArray)
      case 1 => // sequential run in the same thread
        trees = Some(bags.map(buildTree).toArray)
      case _ => // use a specific number of threads
        val parBags = bags.toSet.par
        parBags.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numThreads))
        trees = Some(parBags.map(buildTree).toArray)
    }

    if(verbose) {
      logger.debug(s"Label lexicon:\n${labelLexicon.get}")
      logger.debug(s"Feature lexicon:\n${featureLexicon.get}")
      logger.debug(s"Done building ${trees.get.length} trees:")
      for (tree <- trees.get) {
        logger.debug(s"Tree:\n$tree")
      }
    }
  }

  def computeFeatureThresholds(dataset:CounterDataset[L, F]): Array[Array[Double]] = {
    logger.debug("Computing feature thresholds...")

    // store all seen feature values
    val featureValues = new Array[mutable.HashSet[Double]](featureLexicon.get.size)
    for(f <- featureValues.indices)
      featureValues(f) = new mutable.HashSet[Double]()
    for(i <- dataset.indices) {
      val c = dataset.featuresCounter(i)
      for(f <- c.keySet) {
        featureValues(f) += c.getCount(f)
      }
    }
    for(f <- featureValues.indices)
      featureValues(f) += 0

    // compute thresholds
    val thresholds = new Array[Array[Double]](featureValues.length)
    var thresholdCount = 0
    for(f <- featureValues.indices) {
      val sortedValues = featureValues(f).toArray.sorted
      assert(sortedValues.length > 1)
      val featThresholds = new ArrayBuffer[Double]()
      for(i <- 0 until sortedValues.length - 1) {
        featThresholds += (sortedValues(i) + sortedValues(i + 1)) / 2.0
        thresholdCount += 1
      }
      // TODO: if the feature has more than MAX_THRESHOLDS thresholds, use quantiles instead
      thresholds(f) = featThresholds.toArray
    }

    /*
    val thresholds = new ArrayBuffer[Array[Double]]()
    var thresholdCount = 0
    for(f <- dataset.featureLexicon.indices) {
      assert(f == thresholds.size)
      var values = new mutable.HashSet[Double]()
      for(i <- dataset.indices) {
        val c = dataset.featuresCounter(i)
        if(c.contains(f)) {
          values += c.getCount(f)
        }
      }
      if(values.nonEmpty && ! values.contains(0.0)) {
        values += 0.0
      }

      //logger.debug(s"Feature ${dataset.featureLexicon.get(f)}: $values")

      val sortedValues = values.toArray.sorted
      assert(sortedValues.length > 1)
      val featThresholds = new ArrayBuffer[Double]()
      for(i <- 0 until sortedValues.length - 1) {
        featThresholds += (sortedValues(i) + sortedValues(i + 1)) / 2.0
        thresholdCount += 1
      }
      // TODO: if the feature has more than MAX_THRESHOLDS thresholds, use quantiles instead
      thresholds += featThresholds.toArray
    }
    */

    logger.debug("Finished computing feature thresholds.")
    logger.debug(s"Found $thresholdCount thresholds for ${thresholds.length} features.")

    if(verbose) {
      for (f <- thresholds.indices) {
        logger.debug(s"Feature [${featureLexicon.get.get(f)}]: ${thresholds(f).toList}")
      }
    }

    thresholds.toArray
  }

  var treeCount = 0

  def buildTree(job:RFJob[L, F]):RFTree = {
    if(verbose) logger.debug(s"Starting build tree using job: $job")
    val t = buildTree(job, Set[(Int, Double)]())

    this.synchronized {
      treeCount += 1
      logger.debug(s"Built $treeCount/$numTrees decision trees of depth $maxTreeDepth.")
    }
    t
  }

  def updateContingencyTables(features:Array[Int],
                              contingencyTables:Array[Array[(Counter[Int], Counter[Int])]],
                              overallLabels:Counter[Int]): Unit = {
    // add labels from all datums where this feature *has value 0* to the left contigency tables (i.e., <= threshold)
    for(f <- features) {
      for(t <- contingencyTables(f).indices) {
        val left = contingencyTables(f)(t)._1
        val right = contingencyTables(f)(t)._2
        val seen = new Counter[Int]
        seen += left
        seen += right
        val diff = overallLabels - seen
        left += diff
      }
    }
  }

  def computeContingencyTables(job:RFJob[L, F], features:Array[Int]): Array[Array[(Counter[Int], Counter[Int])]] = {
    // initialize the threshold contingency tables *only* for the selected features
    val contingencyTables = new Array[Array[(Counter[Int], Counter[Int])]](job.dataset.featureLexicon.size)
    for(f <- features) {
      contingencyTables(f) = new Array[(Counter[Int], Counter[Int])](job.featureThresholds(f).length)
      for(i <- contingencyTables(f).indices) {
        contingencyTables(f)(i) = new Tuple2(new Counter[Int], new Counter[Int])
      }
    }

    // update contingency tables for non-zero features
    for(i <- job.indices) {
      val l = job.dataset.labels(i)
      val fs = job.dataset.featuresCounter(i)
      for(f <- fs.keySet) {
        val tables = contingencyTables(f)
        if(tables != null) {
          val fv = fs.getCount(f)
          updateContingencyTables(contingencyTables(f), l, fv, job.featureThresholds(f))
        }
      }
    }
    // logger.debug("Done computing contingency tables.")
    contingencyTables
  }

  def updateContingencyTables(tables:Array[(Counter[Int], Counter[Int])], label:Int, fv:Double, thresholds:Array[Double]): Unit = {
    assert(tables.length == thresholds.length)
    for(i <- thresholds.indices) {
      if(fv <= thresholds(i)) {
        tables(i)._1.incrementCount(label)
      } else {
        tables(i)._2.incrementCount(label)
      }
    }
  }

  /** Constructs a single decision tree from the given dataset sample */
  def buildTree(job:RFJob[L, F], activeNodes:Set[(Int, Double)]):RFTree = {
    //
    // termination condition: all datums have the same labels in this split
    //
    if(sameLabels(job)) {
      //logger.debug(s"Found termination condition on job: $job")
      return new RFLeaf(job.labelDist)
    }

    // reached maximum depth
    if(maxTreeDepth > 0 && activeNodes.size > maxTreeDepth) {
      return new RFLeaf(job.labelDist)
    }

    //
    // randomly select a subset of features
    //
    val currentFeatureIndices = randomFeatureSelection(
      job.dataset.numFeatures,
      featuresPerNode(job.dataset.numFeatures),
      job.random)


    //
    // compute contingency tables for all selected features and all their thresholds
    //
    // count only the non-zero features
    val contingencyTables = computeContingencyTables(job, currentFeatureIndices)
    // compute the label distribution for this job
    val overallLabels = job.labelCounts
    // update contingency tables for the selected features that had zero values in this job
    updateContingencyTables(currentFeatureIndices, contingencyTables, overallLabels)

    //
    // find feature with highest utility
    //
    var best:Option[(Int, Double, Double)] = None // feature, threshold, utility
    for(f <- currentFeatureIndices) {
      val utility = featureUtility(f, job.featureThresholds(f), contingencyTables(f), activeNodes)
      if(utility.isDefined) {
        if(best.isEmpty || best.get._3 < utility.get._3) {
          best = utility
        }
      }
    }

    //
    // nothing found, take majority class
    //
    if(best.isEmpty) {
      new RFLeaf(job.labelDist)
    }

    //
    // otherwise, construct a non-terminal node on the best split and recurse
    //
    else {
      logger.debug(s"Found split point at feature ${featureLexicon.get.get(best.get._1)} with threshold ${best.get._2} and utility ${best.get._3}.")

      val newActiveNodes = new mutable.HashSet[(Int, Double)]()
      newActiveNodes ++= activeNodes
      newActiveNodes += new Tuple2(best.get._1, best.get._2)
      val newActiveNodesSet = newActiveNodes.toSet
      new RFNonTerminal(best.get._1, best.get._2,
        buildTree(mkLeftJob(job, best.get._1, best.get._2), newActiveNodesSet),
        buildTree(mkRightJob(job, best.get._1, best.get._2), newActiveNodesSet))
    }
  }

  /** Computes the utility of the given feature */
  def featureUtility(feature:Int,
                     thresholds:Array[Double],
                     contingencyTables:Array[(Counter[Int], Counter[Int])],
                     activeNodes:Set[(Int, Double)]): Option[(Int, Double, Double)] = {
    informationGain(feature, thresholds, contingencyTables, activeNodes)
  }

  /** Computes the utility of the given feature using information gain */
  def informationGain(feature:Int,
                      thresholds:Array[Double],
                      contingencyTables:Array[(Counter[Int], Counter[Int])],
                      activeNodes:Set[(Int, Double)]): Option[(Int, Double, Double)] = {
    var bestThreshold:Option[(Double, Double)] = None // threshold, utility
    for(t <- thresholds.indices) {
      val threshold = thresholds(t)
      val contingencyTable = contingencyTables(t)
      if(! activeNodes.contains((feature, threshold))) {
        val utility = informationGainForThreshold(feature, threshold, contingencyTable)
        if(utility.isDefined && (bestThreshold.isEmpty || bestThreshold.get._2 < utility.get)) {
          bestThreshold = Some(threshold, utility.get)
        }
      }
    }
    if(bestThreshold.isEmpty) None
    else Some((feature, bestThreshold.get._1, bestThreshold.get._2))
  }

  /** Computes IG for a given feature and threshold */
  def informationGainForThreshold(feature:Int,
                                  threshold:Double,
                                  contingencyTable:(Counter[Int], Counter[Int])):Option[Double] = {
    val leftCounter = contingencyTable._1
    val rightCounter = contingencyTable._2

    /*
    //
    // separate the job in two lists: smaller or equal, or larger than threshold
    // in each list, compute the entropy for the existing labels
    //
    val leftCounter = new Counter[Int]()
    val rightCounter = new Counter[Int]()
    for(i <- job.indices) {
      if(job.dataset.featuresCounter(i).getCount(feature) <= threshold) { // left branch
        leftCounter.incrementCount(job.dataset.labels(i))
      } else { // right branch
        rightCounter.incrementCount(job.dataset.labels(i))
      }
    }
    */

    // bail out if any of the splits is empty
    if(leftCounter.size == 0 || rightCounter.size == 0) {
      return None
    }

    // we skip the entropy of the parent node because it is constant for all features
    val ig =  - entropy(leftCounter) - entropy(rightCounter)
    Some(ig)
  }

  def entropy(labels:Counter[Int]):Double = {
    var ent = 0.0
    for(label <- labels.keySet) {
      ent -= labels.proportion(label) * log2(labels.proportion(label))
    }
    ent
  }

  def log2(d:Double):Double = math.log(d) / math.log(2)

  /** Randomly picks selectedFeats features between 0 .. numFeats */
  def randomFeatureSelection(numFeats:Int, selectedFeats:Int, random:Random):Array[Int] = {
    var feats = new ArrayBuffer[Int]()
    for(i <- 0 until selectedFeats) {
      feats += random.nextInt(numFeats)
    }
    feats.toArray
  }

  def sameLabels(job:RFJob[L, F]):Boolean = {
    val ls = new mutable.HashSet[Int]()
    for(i <- job.indices) {
      ls += job.dataset.labels(i)
      if(ls.size > 1) return false
    }
    true
  }

  def mkBag(dataset: CounterDataset[L, F],
            indices: Array[Int],
            thresholds: Array[Array[Double]],
            length:Int,
            random:Random,
            offset:Int):RFJob[L, F] = {
    val bagIndices = new ArrayBuffer[Int]()
    for(i <- 0 until length) {
      bagIndices += indices(random.nextInt(indices.length))
    }
    new RFJob[L, F](dataset, bagIndices.toArray, thresholds, new Random(RANDOM_SEED + offset))
  }

  /** Constructs a job from the datums containing values of this feature smaller or equal than the threshold */
  def mkLeftJob(job:RFJob[L, F], feature:Int, threshold:Double):RFJob[L, F] = {
    val newIndices = new ArrayBuffer[Int]
    for(i <- job.indices) {
      if(job.dataset.featuresCounter(i).getCount(feature) <= threshold) {
        newIndices += i
      }
    }
    // shallow copy everything except the new datum indices
    new RFJob[L, F](job.dataset, newIndices.toArray, job.featureThresholds, job.random)
  }

  /** Constructs a job from the datums containing values of this feature larger than the threshold */
  def mkRightJob(job:RFJob[L, F], feature:Int, threshold:Double):RFJob[L, F] = {
    val newIndices = new ArrayBuffer[Int]
    for(i <- job.indices) {
      if(job.dataset.featuresCounter(i).getCount(feature) > threshold) {
        newIndices += i
      }
    }
    // shallow copy everything except the new datum indices
    new RFJob[L, F](job.dataset, newIndices.toArray, job.featureThresholds, job.random)
  }

  /**
    * Returns the scores of all possible labels for this datum
    * Convention: if the classifier can return probabilities, these must be probabilities
    **/
  override def scoresOf(d: Datum[L, F]): Counter[L] = {
    //
    // convert the datum to a counter of Ints, for easier processing
    //
    val fs = d.featuresCounter
    val ifs = new Counter[Int]
    for(f <- fs.keySet) {
      if(featureLexicon.get.contains(f)) {
        ifs.incrementCount(featureLexicon.get.get(f).get, fs.getCount(f))
      }
    }

    //
    // merge the label distributions from the predictions of all trees
    //
    val labels = new Counter[Int]
    for(tree <- trees.get) {
      val labelDist = tree.apply(ifs)
      labels += labelDist
    }
    for(l <- labels.keySet) {
      labels.setCount(l, labels.getCount(l) / trees.get.length.toDouble)
    }

    // convert to labels of type L
    val prettyLabels = new Counter[L]()
    for(l <- labels.keySet) {
      prettyLabels.setCount(labelLexicon.get.get(l), labels.getCount(l))
    }

    prettyLabels
  }

  /** Returns the argmax for this datum */
  override def classOf(d: Datum[L, F]): L = {
    val scores = scoresOf(d)
    scores.sorted.head._1
  }

  /** Saves to writer. Does NOT close the writer */
  override def saveTo(writer: Writer): Unit = {
    throw new RuntimeException("ERROR: saveTo not supported yet!")
  }
}

class RFJob[L, F](
  val dataset:CounterDataset[L, F],
  val indices:Array[Int],
  val featureThresholds:Array[Array[Double]],
  val random:Random) {

  override def toString:String = {
    val b = new StringBuilder
    var first = true
    for(i <- indices) {
      if(! first) b.append(" ")
      b.append(i.toString)
      b.append(":")
      b.append(dataset.labels(i).toString)
      first = false
    }
    b.toString()
  }

  def labelDist:Counter[Int] = {
    val counts = labelCounts
    val proportions = new Counter[Int]
    for(l <- counts.keySet)
      proportions.setCount(l, counts.proportion(l))
    proportions
  }

  def labelCounts:Counter[Int] = {
    val counts = new Counter[Int]
    for(i <- indices)
      counts.incrementCount(dataset.labels(i))
    counts
  }

}

trait RFTree {
  def decision:Option[(Int, Double)]
  def labels:Option[Counter[Int]]
  def left:Option[RFTree]
  def right:Option[RFTree]

  def indent(i:Int):String = {
    val b = new StringBuilder()
    for(j <- 0 until i) b += ' '
    b.toString()
  }

  def toString(int:Int):String

  def apply(datum:Counter[Int]):Counter[Int] = {
    this match {
      case leaf:RFLeaf => labels.get
      case nonTerm:RFNonTerminal =>
        val v = datum.getCount(nonTerm.decision.get._1)
        if(v <= nonTerm.decision.get._2) nonTerm.left.get.apply(datum)
        else nonTerm.right.get.apply(datum)
    }
  }
}

class RFLeaf(ls:Counter[Int]) extends RFTree {
  def decision = None
  def labels = Some(ls)
  def left = None
  def right = None

  override def toString = toString(0)

  override def toString(ind:Int):String = {
    val b = new StringBuilder
    b.append(indent(ind))
    labels.foreach(l => b.append(l.toString))
    b.toString()
  }
}

class RFNonTerminal(f:Int, t:Double, l:RFTree, r:RFTree) extends RFTree {
  def decision = Some(f, t)
  def labels = None
  def left = Some(l)
  def right = Some(r)

  override def toString:String = toString(0)

  override def toString(ind:Int):String = {
    val b = new StringBuilder
    b.append(indent(ind))
    decision.foreach(d => {
      b.append(d._1.toString)
      b.append(" ")
      b.append(d._2.toString)
      b.append("\n")
      left.foreach(l => b.append(l.toString(ind + 2)))
      b.append("\n")
      right.foreach(r => b.append(r.toString(ind + 2)))
    })
    b.toString()
  }
}

object RFClassifier {
  val logger = LoggerFactory.getLogger(classOf[RFClassifier[String, String]])

  val RANDOM_SEED = 1

  /** Decides how many features to use in each node */
  def featuresPerNode(numFeats:Int):Int = {
    numFeats
    // math.sqrt(numFeats).toInt
  }
}