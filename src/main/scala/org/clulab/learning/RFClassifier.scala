package org.clulab.learning

import java.io.{Writer, Serializable}

import org.clulab.struct.{Lexicon, Counter}
import org.clulab.utils.MathUtils
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
                         maxTreeDepth:Int = 20, // 0 means unlimited tree depth
                         trainBagPct:Double = 0.66, // how much data to use per tree
                         utilityTooSmallThreshold:Double = 0, // 0 means no utility is too small
                         splitTooSmallPct:Double = 0.0, // 0 means no split is too small
                         numThreads:Int = 0, // 0 means maximum parallelism: use all cores available
                         howManyFeaturesPerNode: Int => Int = RFClassifier.featuresPerNodeSqrt, // how many features to use per node, as a function of total feature count
                         nilLabel:Option[L] = None)
  extends Classifier[L, F] with Serializable {
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
      bags += mkBag(dataset, indices, thresholds, bagSize,
        RFClassifier.entropy(RFClassifier.labelCounts(indices, dataset)), randomSeed, i)
    }
    logger.debug(s"Constructed ${bags.size} bag(s), each containing $bagSize datums")
    /*
    for(i <- bags.indices) {
      logger.debug(s"Label counts inside bag #$i: ${bags(i).labelCounts}.")
    }
    */

    //
    // the actual tree building
    //
    logger.debug("Beginning tree building...")
    numThreads match {
      case 0 => // use as many threads as possible
        val parBags = bags.toSet.par
        trees = Some(parBags.map(buildTreeMain).toArray)
      case 1 => // sequential run in the same thread
        trees = Some(bags.map(buildTreeMain).toArray)
      case _ => // use a specific number of threads
        val parBags = bags.toSet.par
        parBags.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numThreads))
        trees = Some(parBags.map(buildTreeMain).toArray)
    }

    if(verbose) {
      logger.debug(s"Label lexicon:\n${labelLexicon.get}")
      logger.debug(s"Feature lexicon:\n${featureLexicon.get}")
    }
    logger.debug(s"Done building ${trees.get.length} trees.")
    if(verbose) {
      for (tree <- trees.get) {
        logger.debug(s"Tree:\n${tree.toPrettyString[L, F](0, featureLexicon.get, labelLexicon.get)}")
      }
    }
  }

  /**
    * Computes the value thresholds for all features in this dataset
    *
    * @param dataset The dataset
    * @return An array of thresholds (Double) for each feature in the dataset; feature indices are used for indexing
    */
  def computeFeatureThresholds(dataset:CounterDataset[L, F]): Array[Array[Double]] = {
    logger.debug("Computing feature thresholds...")

    // store all seen feature values
    val featureValues = new Array[Counter[Double]](featureLexicon.get.size)
    for(f <- featureValues.indices)
      featureValues(f) = new Counter[Double]()
    for(i <- dataset.indices) {
      val c = dataset.featuresCounter(i)
      for(f <- c.keySet) {
        val v = c.getCount(f)
        if(v != 0) { // 0s are added explicitly below
          featureValues(f).incrementCount(c.getCount(f))
        }
      }
    }
    // because we have a sparse representation, 0 values must be explicitly added for all features
    for(f <- featureValues.indices)
      featureValues(f).incrementCount(0)

    // compute thresholds
    val thresholds = new Array[Array[Double]](featureValues.length)
    var thresholdCount = 0
    for(f <- featureValues.indices) {
      val featThresholds = new ArrayBuffer[Double]()

      if(featureValues(f).size > RFClassifier.QUANTILE_THRESHOLD) {
        // too many feature values; use quantiles instead
        val quantileValues = quantiles(featureValues(f), RFClassifier.QUANTILE_THRESHOLD)
        featThresholds ++= quantileValues
        thresholdCount += quantileValues.length

      } else {
        val sortedValues = featureValues(f).sorted(descending = false).map(_._1)
        if(sortedValues.length > 1) {
          for (i <- 0 until sortedValues.length - 1) {
            featThresholds += (sortedValues(i) + sortedValues(i + 1)) / 2.0
            thresholdCount += 1
          }
        } else {
          // this happens when a feature only appears with a value of 0 in the dataset
          // that means we can't use it for any decision making...
          // So the corresponding array of thresholds will have a size of 0
        }
     }

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

  /** Computes binCount-1 quantile values, such that the sequence of values is split into binCount bins */
  def quantiles(values:Counter[Double], binCount:Int):Array[Double] = {
    val sortedUniq = values.sorted(descending = false)
    val sorted = new ArrayBuffer[Double]()
    for(su <- sortedUniq) {
      for(i <- 0 until su._2.toInt) {
        sorted += su._1
      }
    }

    val qs = new Array[Double](binCount - 1)
    for(i <- qs.indices) {
      val pos = sorted.length.toDouble * (i + 1).toDouble / binCount.toDouble
      if(pos == pos.floor) {
        qs(i) = (sorted(pos.toInt) + sorted(pos.toInt - 1)) / 2.0
      } else {
        qs(i) = sorted(pos.floor.toInt)
      }
    }
    qs
  }

  var treeCount = 0

  def buildTreeMain(job:RFJob[L, F]):RFTree = {
    // if(verbose) logger.debug(s"Starting build tree using job: $job")
    val t = buildTree(job)
    val pt = t // prune(t)
    pt.weight = 1.0 // job.oobAccuracy(pt)

    this.synchronized {
      treeCount += 1
      logger.debug(s"Built $treeCount/$numTrees decision trees of depth $maxTreeDepth.") // Current tree accuracy is ${pt.weight}")
    }
    pt
  }

  def prune(tree:RFTree):RFTree = {
    tree match {
      case nt:RFNonTerminal =>
        nt.l = prune(nt.l)
        nt.r = prune(nt.r)

        if(nt.left.get.isLeaf && nt.right.get.isLeaf &&
          nt.left.get.sameLabels(nt.right.get)) {
          println("Pruned 1 node")
          new RFLeaf(nt.left.get.mergeLabels(nt.right.get))
        } else {
          nt
        }
      case _ => tree
    }
  }

  def printContingencyTables(tables:Array[Array[(Counter[Int], Counter[Int])]], thresholds:Array[Array[Double]]) {
    println(s"Tables for ${tables.length} features.")
    for(f <- tables.indices) {
      if(tables(f) != null) {
        println(s"Tables for feature $f")
        for (t <- tables(f).indices) {
          println(s"\tThreshold ${thresholds(f)(t)}:")
          println(s"\t\tSMALLER: ${tables(f)(t)._1}")
          println(s"\t\tGREATER: ${tables(f)(t)._2}")
        }
      }
    }
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

  /**
    * Computes the contingency tables for all given features and dataset partition
    * For each feature and possible threshold (hence the double array),
    *   we store a distribution of datum labels that are <= than the threshold (_1 in the tuple),
    *   or larger than the threshold (_2 in the tuple)
    * This method does not consider 0 values! See updateContingencyTables for that.
    */
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
    for(i <- job.trainIndices) {
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
  def buildTree(job:RFJob[L, F]):RFTree = {
    //
    // termination condition: all datums have the same labels in this split
    //
    if(sameLabels(job)) {
      //logger.debug(s"Found termination condition due to uniform labels on job")
      return new RFLeaf(job.leafLabels)
    }

    // termination condition: reached maximum depth
    if(maxTreeDepth > 0 && job.activeNodes.size > maxTreeDepth) {
      //logger.debug(s"Found termination condition at depth ${activeNodes.size}")
      return new RFLeaf(job.leafLabels)
    }

    // termination condition: the remaining dataset is too small
    if(splitTooSmallPct > 0 && job.trainIndices.length < splitTooSmallPct * job.dataset.size) {
      //logger.debug(s"Found termination condition due to dataset too small: ${job.indices.length}")
      return new RFLeaf(job.leafLabels)
    }

    //
    // randomly select a subset of features from the features present in this partition
    //
    val currentFeatureIndices = randomFeatureSelection(
      job.features, job.dataset.featureLexicon.size, job.random)
    // logger.debug(s"Will work with ${currentFeatureIndices.length} features in this node.")


    //
    // compute contingency tables for all selected features and all their thresholds
    //
    // count only the non-zero features
    val contingencyTables = computeContingencyTables(job, currentFeatureIndices)
    // compute the label distribution for this job
    val overallLabels = job.labelCounts
    // update contingency tables for the selected features that had zero values in this job
    updateContingencyTables(currentFeatureIndices, contingencyTables, overallLabels)
    //printContingencyTables(contingencyTables, job.featureThresholds)

    //
    // find feature with highest utility
    //
    var best:Option[Utility] = None
    for(f <- currentFeatureIndices) {
      val utility = featureUtility(f, job.featureThresholds(f), contingencyTables(f), job.activeNodes, job.currentUtility)
      /*
      if(utility.isDefined)
        logger.debug(s"Utility for feature ${featureLexicon.get.get(f)} and threshold ${utility.get._2} is ${utility.get._3}")
      else
        logger.debug(s"Feature ${featureLexicon.get.get(f)} has no utility!")
      */

      if(verbose && utility.isDefined) {
        println("Current utility:")
        debugUtility(utility.get, job)
      }

      if(utility.isDefined) {
        if(best.isEmpty || best.get.value < utility.get.value) {
          best = utility
          if(verbose) println("CHOSEN NEW BEST!")
        }
      }
    }

    //
    // nothing found, take majority class
    //
    if(best.isEmpty) {
      // logger.debug("No useful feature found.")
      new RFLeaf(job.leafLabels)
    }

    //
    // otherwise, construct a non-terminal node on the best split and recurse
    //
    else {
      if(verbose) {
        println("BEST OVERALL:")
        debugUtility(best.get, job)
      }

      //logger.debug(s"Found split point at feature ${featureLexicon.get.get(best.get._1)} with threshold ${best.get._2} and utility ${best.get._3}.")

      val newActiveNodes = new mutable.HashSet[(Int, Double)]()
      newActiveNodes ++= job.activeNodes
      newActiveNodes += new Tuple2(best.get.feature, best.get.threshold)
      val newActiveNodesSet = newActiveNodes.toSet
      new RFNonTerminal(best.get.feature, best.get.threshold,
        buildTree(mkLeftJob(job, best.get.feature, best.get.threshold, best.get.leftChildValue, newActiveNodesSet)),
        buildTree(mkRightJob(job, best.get.feature, best.get.threshold, best.get.rightChildValue, newActiveNodesSet)))
    }
  }

  def debugUtility(utility:Utility, job:RFJob[L, F]): Unit = {
    println("UTILITY DEBUG:")
    println("Using dataset:")
    job.printDataset()
    println(s"Using feature ${utility.feature} with threshold ${utility.threshold}")
    println(s"Contingency table for this feature+threshold is: SMALLER: ${utility.leftCounter}, GREATER: ${utility.rightCounter}")
    println(s"Overall utility: ${utility.value}")
    println(s"Parent utility: ${utility.parentValue}")
    println(s"Utility of left child: ${utility.leftChildValue}")
    println(s"Utility of right child: ${utility.rightChildValue}")
  }

  /** Computes the utility of the given feature */
  def featureUtility(feature:Int,
                     thresholds:Array[Double],
                     contingencyTables:Array[(Counter[Int], Counter[Int])],
                     activeNodes:Set[(Int, Double)],
                     currentUtility:Double): Option[Utility] = {
    informationGain(feature, thresholds, contingencyTables, activeNodes, currentUtility)
  }

  /** Computes the utility of the given feature using information gain */
  def informationGain(feature:Int,
                      thresholds:Array[Double],
                      contingencyTables:Array[(Counter[Int], Counter[Int])],
                      activeNodes:Set[(Int, Double)],
                      currentEntropy:Double): Option[Utility] = {
    var bestThreshold:Option[Utility] = None
    for(t <- thresholds.indices) {
      val threshold = thresholds(t)
      val contingencyTable = contingencyTables(t)
      if(! activeNodes.contains((feature, threshold))) {
        val ig = informationGainForThreshold(feature, threshold, contingencyTable, currentEntropy)
        if(ig.isDefined && (bestThreshold.isEmpty || bestThreshold.get.value < ig.get.value)) {
          // println("Found new best IG: " + ig.get.value)
          bestThreshold = ig
        }
      }
    }
    bestThreshold
  }

  /** Computes IG for a given feature and threshold */
  def informationGainForThreshold(feature:Int,
                                  threshold:Double,
                                  contingencyTable:(Counter[Int], Counter[Int]),
                                  currentEntropy:Double):Option[Utility] = {
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

    // bail out if any of the splits is empty; in this case IG doesn't change
    if(leftCounter.getTotal == 0 || rightCounter.getTotal == 0) {
      //logger.debug("\tEmpty splits in contingency tables!")
      return None
    }

    val leftWeight = leftCounter.getTotal / (leftCounter.getTotal + rightCounter.getTotal)
    val rightWeight = rightCounter.getTotal / (leftCounter.getTotal + rightCounter.getTotal)
    val leftEntropy = entropy(leftCounter)
    val rightEntropy = entropy(rightCounter)
    val value =  currentEntropy - (leftWeight * leftEntropy) - (rightWeight * rightEntropy)

    if(value < utilityTooSmallThreshold) {
      // if the change in entropy if too small; bail out
      // this is a simple form of regularization
      //logger.debug("\tUtility too small!")
      return None
    }

    Some(Utility(feature, threshold, value, currentEntropy, leftEntropy, rightEntropy, leftCounter, rightCounter))
  }

  /** Randomly picks selectedFeats features between 0 .. numFeats */
  def randomFeatureSelection(presentFeatures:Set[Int], numFeats:Int, random:Random):Array[Int] = {
    val featCount = math.min(howManyFeaturesPerNode(numFeats), presentFeatures.size)
    val randomizedFeats = MathUtils.randomize(presentFeatures.toArray, random)
    var feats = new ArrayBuffer[Int]()
    for(i <- 0 until featCount) {
      feats += randomizedFeats(i)
    }
    feats.toArray
  }

  def sameLabels(job:RFJob[L, F]):Boolean = {
    val ls = new mutable.HashSet[Int]()
    for(i <- job.trainIndices) {
      ls += job.dataset.labels(i)
      if(ls.size > 1) return false
    }
    true
  }

  def mkBag(dataset: CounterDataset[L, F],
            indices: Array[Int],
            thresholds: Array[Array[Double]],
            trainIndicesLength:Int,
            entropy:Double,
            random:Random,
            offset:Int):RFJob[L, F] = {
    val bagIndices = new ArrayBuffer[Int]()
    val oobIndices = new ArrayBuffer[Int]()

    if(trainIndicesLength == indices.length) {
      // just copy all indices in this situation
      for(i <- indices) {
        bagIndices += i
      }
    } else {
      // proper sampling with replacement
      for (i <- 0 until trainIndicesLength) {
        bagIndices += indices(random.nextInt(indices.length))
      }

      // the remaining indices form the OOB dataset
      val uniqueBagIndices = bagIndices.toSet
      for(i <- indices.indices) {
        if(! uniqueBagIndices.contains(indices(i))) {
          oobIndices += indices(i)
        }
      }

      /*
      // sampling wo/ replacement
      val randomized = MathUtils.randomize(indices, random)
      for (i <- 0 until trainIndicesLength) {
        bagIndices += indices(i)
      }
      for(i <- trainIndicesLength until indices.length) {
        oobIndices += indices(i)
      }
      */

    }
    new RFJob[L, F](dataset, bagIndices.toArray, oobIndices.toArray, Set[(Int, Double)](), nilLabel, thresholds, entropy, new Random(RANDOM_SEED + offset))
  }

  /** Constructs a job from the datums containing values of this feature smaller or equal than the threshold */
  def mkLeftJob(job:RFJob[L, F], feature:Int, threshold:Double, entropy:Double, activeNodes:Set[(Int, Double)]):RFJob[L, F] = {
    val newIndices = new ArrayBuffer[Int]
    for(i <- job.trainIndices) {
      if(job.dataset.featuresCounter(i).getCount(feature) <= threshold) {
        newIndices += i
      }
    }
    // shallow copy everything except the new datum indices
    new RFJob[L, F](job.dataset, newIndices.toArray, job.oobIndices, activeNodes, job.nilLabel, job.featureThresholds, entropy, job.random)
  }

  /** Constructs a job from the datums containing values of this feature larger than the threshold */
  def mkRightJob(job:RFJob[L, F], feature:Int, threshold:Double, entropy:Double, activeNodes:Set[(Int, Double)]):RFJob[L, F] = {
    val newIndices = new ArrayBuffer[Int]
    for(i <- job.trainIndices) {
      if(job.dataset.featuresCounter(i).getCount(feature) > threshold) {
        newIndices += i
      }
    }
    // shallow copy everything except the new datum indices
    new RFJob[L, F](job.dataset, newIndices.toArray, job.oobIndices, activeNodes, job.nilLabel, job.featureThresholds, entropy, job.random)
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

    if(verbose) {
      logger.debug(s"Classifying datum: $ifs.")
    }

    //
    // merge the label distributions from the predictions of all trees
    //
    val labels = new Counter[Int]
    var treeIndex = 0
    for(tree <- trees.get) {
      val labelDist = tree.apply(ifs)
      labels += labelDist * tree.weight
      if(verbose) {
        logger.debug(s"Label distribution from tree #$treeIndex: $labelDist")
        logger.debug(s"Tree:\n$tree")
      }
      treeIndex += 1
    }
    if(verbose) logger.debug(s"Overall label distribution: $labels")

    // convert to labels of type L and proportions
    val prettyLabels = new Counter[L]()
    for(l <- labels.keySet) {
      prettyLabels.setCount(labelLexicon.get.get(l), labels.proportion(l))
    }
    if(verbose) logger.debug(s"Pretty labels: $prettyLabels")

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

// (Int, Double, Double, Double, Double)] = None // feature, threshold, utility, left entropy, right entropy
case class Utility (feature:Int, // index of this feature in the feature lexicon
                    threshold:Double, // threshold used for this computation
                    value:Double, // overall utility value if the split at this threshold is taken
                    parentValue:Double, // utility of the node to be split
                    leftChildValue:Double, // utility of the left child after split
                    rightChildValue:Double, // utility of the right child after split
                    leftCounter:Counter[Int], // label distribution for datums with feature <= threshold
                    rightCounter:Counter[Int]) // label distribution for datums with feature > threshold


class RFJob[L, F](
                   val dataset:CounterDataset[L, F],
                   val trainIndices:Array[Int],
                   val oobIndices:Array[Int],
                   val activeNodes:Set[(Int, Double)],
                   val nilLabel:Option[L],
                   val featureThresholds:Array[Array[Double]],
                   val currentUtility:Double,
                   val random:Random) {

  override def toString:String = {
    val b = new StringBuilder
    var first = true
    for(i <- trainIndices) {
      if(! first) b.append(" ")
      b.append(i.toString)
      b.append(":")
      b.append(dataset.labels(i).toString)
      first = false
    }
    b.toString()
  }

  def printDataset(): Unit = {
    for(i <- trainIndices) {
      println(s"label:${dataset.labels(i)}\tfeatures:${dataset.featuresCounter(i)}")
    }
  }

  def labelDist:Counter[Int] = {
    val counts = labelCounts
    val proportions = new Counter[Int]
    for(l <- counts.keySet)
      proportions.setCount(l, counts.proportion(l))
    proportions
  }

  def leafLabels = labelDist

  def labelCounts:Counter[Int] = RFClassifier.labelCounts[L, F](trainIndices, dataset)

  def features:Set[Int] = {
    val feats = new mutable.HashSet[Int]()
    for(i <- trainIndices) {
      feats ++= dataset.featuresCounter(i).keySet
    }
    feats.toSet
  }

  def oobAccuracy(tree:RFTree):Double = {
    val labels = new ArrayBuffer[(Int, Int)] // gold, pred
    for(i <- oobIndices.indices) {
      val prediction = tree.apply(dataset.featuresCounter(oobIndices(i))).sorted.head._1
      labels += new Tuple2(dataset.labels(oobIndices(i)), prediction)
    }

    if(nilLabel.isEmpty) accuracy(labels)
    else f1(labels, dataset.labelLexicon.get(nilLabel.get).get)
  }

  private def accuracy(labels:Seq[(Int, Int)]):Double = {
    var correct = 0
    for(l <- labels)
      if(l._1 == l._2)
        correct += 1
    correct.toDouble / labels.size.toDouble
  }

  private def f1(labels:Seq[(Int, Int)], nilLabel:Int):Double = {
    var correct = 0
    var predicted = 0
    var total = 0
    for(l <- labels) {
      if(l._1 != nilLabel) total += 1
      if(l._2 != nilLabel) {
        predicted += 1
        if(l._1 == l._2)
          correct += 1
      }
    }
    var p = 0.0
    if(predicted != 0) p = correct.toDouble / predicted.toDouble
    var r = 0.0
    if(total != 0) r = correct.toDouble / total.toDouble
    var f1 = 0.0
    if(p != 0.0 && r != 0.0)
      f1 = 2 * p * r / (p + r)
    logger.debug(s"P $p, R $r, F1 $f1")
    f1
  }
}

trait RFTree {
  def decision:Option[(Int, Double)]
  def labels:Option[Counter[Int]]
  def left:Option[RFTree]
  def right:Option[RFTree]
  var weight:Double = 1.0

  def isLeaf = right.isEmpty && left.isEmpty

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

  def sameLabels(other:RFTree):Boolean = {
    //val s1 = labels.get.sorted.map(_._1).mkString("-")
    //val s2 = other.labels.get.sorted.map(_._1).mkString("-")
    val s1 = labels.get.sorted.head._1
    val s2 = other.labels.get.sorted.head._1
    s1 == s2
  }

  def mergeLabels(other:RFTree):Counter[Int] = {
    labels.get + other.labels.get
  }

  def toPrettyString[L, F](ind:Int, featureLexicon:Lexicon[F], labelLexicon:Lexicon[L]):String
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

  override def toPrettyString[L, F](ind:Int, featureLexicon:Lexicon[F], labelLexicon:Lexicon[L]):String = {
    val b = new StringBuilder
    b.append(indent(ind))
    labels.foreach(l => {
      for(lk <- l.keySet) {
        b.append(s"(${labelLexicon.get(lk)} ${l.getCount(lk)}) ")
      }
    })
    b.toString()
  }

}

class RFNonTerminal(f:Int, t:Double, var l:RFTree, var r:RFTree) extends RFTree {
  def decision = Some(f, t)

  def labels = None

  def left = Some(l)

  def right = Some(r)

  override def toString: String = toString(0)

  override def toString(ind: Int): String = {
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

  override def toPrettyString[L, F](ind: Int, featureLexicon: Lexicon[F], labelLexicon: Lexicon[L]): String = {
    val b = new StringBuilder
    b.append(indent(ind))
    decision.foreach(d => {
      b.append(featureLexicon.get(d._1).toString)
      b.append(" ")
      b.append(d._2.toString)
      b.append("\n")
      left.foreach(l => b.append(l.toPrettyString(ind + 2, featureLexicon, labelLexicon)))
      b.append("\n")
      right.foreach(r => b.append(r.toPrettyString(ind + 2, featureLexicon, labelLexicon)))
    })
    b.toString()
  }
}

object RFClassifier {
  val logger = LoggerFactory.getLogger(classOf[RFClassifier[String, String]])

  val RANDOM_SEED = 1

  val QUANTILE_THRESHOLD = 10

  /** How many features to use in each node: sqrt(total feature count) */
  def featuresPerNodeSqrt(numFeats:Int):Int = {
    math.sqrt(numFeats).toInt
  }

  /** How many features to use in each node: 2/3 * (total feature count) */
  def featuresPerNodeTwoThirds(numFeats:Int):Int = {
    (2.0 * numFeats / 3.0).toInt
  }

  /** Use all features in each node */
  def featuresPerNodeAll(numFeats:Int):Int = numFeats

  def entropy(labels:Counter[Int]):Double = {
    var ent = 0.0
    for(label <- labels.keySet) {
      if(labels.getCount(label) > 0)
        ent -= labels.proportion(label) * log2(labels.proportion(label))
    }
    ent
  }

  def labelCounts[L, F](indices:Array[Int], dataset:CounterDataset[L, F]):Counter[Int] = {
    val counts = new Counter[Int]
    for(i <- indices)
      counts.incrementCount(dataset.labels(i))
    counts
  }

  def log2(d:Double):Double = math.log(d) / math.log(2)
}
