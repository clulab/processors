package edu.arizona.sista.learning

import java.io.{Writer, Serializable}

import edu.arizona.sista.struct.Counter
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
class RFClassifier[L, F](numTrees:Int = 100, numThreads:Int = 0) extends Classifier[L, F] with Serializable {
  var trees:Option[Array[RFTree]] = None

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

    logger.debug(s"Training on a dataset containing ${dataset.size} datums, with ${dataset.labelLexicon.size} labels.")

    //
    // compute the feature thresholds
    //
    val thresholds = computeFeatureThresholds(dataset)

    //
    // construct the bags, one per tree
    //
    val bags = new ArrayBuffer[RFJob[L, F]]()
    val bagSize = (TRAIN_BAG_PCT * indices.length).toInt
    val randomSeed = new Random(RANDOM_SEED)
    for(i <- 0 until numTrees) {
      bags += mkBag(dataset, indices, thresholds, bagSize, randomSeed)
    }
    logger.debug(s"Constructed ${bags.size} bag(s), each containing $bagSize datums.")

    // the actual tree building
    logger.debug("Beginning tree building...")
    val bagThreads = bags.toSet.par
    if(numThreads != 0)
      bagThreads.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numThreads))
    trees = Some(bagThreads.map(buildTree).toArray)
    logger.debug(s"Done building ${trees.get.length} trees.")

    System.exit(1) // TODO: remove
  }

  def computeFeatureThresholds(dataset:CounterDataset[L, F]): Array[Array[Double]] = {
    logger.debug("Computing feature thresholds...")
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
      val sortedValues = values.toArray.sorted
      assert(sortedValues.length > 0)
      val featThresholds = new ArrayBuffer[Double]()
      if(sortedValues.length == 1) {
        featThresholds += sortedValues.head / 2.0
        thresholdCount += 1
      } else {
        for(i <- 0 until sortedValues.length - 1) {
          featThresholds += (sortedValues(i) + sortedValues(i + 1)) / 2.0
          thresholdCount += 1
        }
      }
      // TODO: if the feature has more than MAX_THRESHOLDS thresholds, use quantiles instead
      thresholds += featThresholds.toArray
    }
    logger.debug("Finished computing feature thresholds.")
    logger.debug(s"Found $thresholdCount thresholds for ${thresholds.length} features.")
    thresholds.toArray
  }

  def buildTree(job:RFJob[L, F]):RFTree = buildTree(job, Set[(Int, Double)]())

  /** Constructs a single decision tree from the given dataset sample */
  def buildTree(job:RFJob[L, F], activeNodes:Set[(Int, Double)]):RFTree = {
    //
    // termination condition: all datums have the same labels in this split
    //
    if(sameLabels(job)) {
      return new RFLeaf(job.dataset.labels(job.indices.head))
    }

    //
    // randomly select a subset of features
    //
    val currentFeatureIndices = randomFeatureSelection(
      job.dataset.numFeatures,
      featuresPerNode(job.dataset.numFeatures),
      job.random)

    //
    // find feature with highest utility
    //
    var best:Option[(Int, Double, Double)] = None // feature, threshold, utility
    for(f <- currentFeatureIndices) {
      val utility = featureUtility(f, job, activeNodes)
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
      new RFLeaf(majorityClass(job))
    }

    //
    // otherwise, construct a non-terminal node on the best split and recurse
    //
    else {
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
  def featureUtility(feature:Int, job:RFJob[L, F], activeNodes:Set[(Int, Double)]): Option[(Int, Double, Double)] = {
    informationGain(feature, job, activeNodes)
  }

  /** Computes the utility of the given feature using information gain */
  def informationGain(feature:Int, job:RFJob[L, F], activeNodes:Set[(Int, Double)]): Option[(Int, Double, Double)] = {
    None // TODO: implement this next
  }

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

  def majorityClass(job:RFJob[L, F]):Int = {
    val ls = new Counter[Int]()
    for(i <- job.indices) {
      ls.incrementCount(job.dataset.labels(i))
    }
    ls.sorted.head._1
  }

  def mkBag(dataset: CounterDataset[L, F],
            indices: Array[Int],
            thresholds: Array[Array[Double]],
            length:Int,
            random:Random):RFJob[L, F] = {
    val bagIndices = new ArrayBuffer[Int]()
    for(i <- 0 until length) {
      bagIndices += indices(random.nextInt(indices.length))
    }
    new RFJob[L, F](dataset, bagIndices.toArray, thresholds, new Random(RANDOM_SEED))
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
    throw new RuntimeException("ERROR: scoresOf not supported yet!")
  }

  /** Returns the argmax for this datum */
  override def classOf(d: Datum[L, F]): L = {
    throw new RuntimeException("ERROR: classOf not supported yet!")
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
  val random:Random
)

trait RFTree {
  def decision:Option[(Int, Double)]
  def label:Option[Int]
  def left:Option[RFTree]
  def right:Option[RFTree]
}

class RFLeaf(l:Int) extends RFTree {
  def decision = None
  def label = Some(l)
  def left = None
  def right = None
}

class RFNonTerminal(f:Int, t:Double, l:RFTree, r:RFTree) extends RFTree {
  def decision = Some(f, t)
  def label = None
  def left = Some(l)
  def right = Some(r)
}

object RFClassifier {
  val logger = LoggerFactory.getLogger(classOf[RFClassifier[String, String]])

  val RANDOM_SEED = 1
  val TRAIN_BAG_PCT = 0.66

  /** Decides how many features to use in each node */
  def featuresPerNode(numFeats:Int):Int = {
    math.sqrt(numFeats).toInt
  }
}