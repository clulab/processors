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
  val randomSeed = new Random(RANDOM_SEED)
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

    // compute the feature thresholds
    val thresholds = computeFeatureThresholds(dataset)

    // construct the bags, one per tree
    val bags = new ArrayBuffer[RFJob[L, F]]()
    val bagSize = (TRAIN_BAG_PCT * indices.length).toInt
    for(i <- 0 until numTrees) {
      bags += mkBag(dataset, indices, thresholds, bagSize)
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
      thresholds += featThresholds.toArray
    }
    logger.debug("Finished computing feature thresholds.")
    logger.debug(s"Found $thresholdCount thresholds for ${thresholds.length} features.")
    thresholds.toArray
  }

  def buildTree(job:RFJob[L, F]):RFTree = {
    // TODO: implement this
    throw new RuntimeException("implement me!")
  }

  def mkBag(dataset: Dataset[L, F],
            indices: Array[Int],
            thresholds: Array[Array[Double]],
            length:Int):RFJob[L, F] = {
    val bagIndices = new ArrayBuffer[Int]()
    for(i <- 0 until length) {
      bagIndices += indices(randomSeed.nextInt(indices.length))
    }
    new RFJob[L, F](dataset, bagIndices.toArray, thresholds)
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
  val dataset:Dataset[L, F],
  val indices:Array[Int],
  val featureThresholds:Array[Array[Double]]
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
}