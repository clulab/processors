package org.clulab.learning

import java.io._
import org.slf4j.LoggerFactory
import scala.collection.mutable.ArrayBuffer
import org.clulab.struct.Counter
import org.clulab.struct.Counters.dotProduct
import org.clulab.struct.Lexicon
import org.clulab.utils.{StringUtils, MathUtils}
import java.util.Properties
import scala.Serializable
import scala.util.Random
import Datasets._

/**
 * Perceptron classifier for ranking, in primal mode
 * Includes averaging, hard margin, burn-in iterations
 * User: mihais
 * Date: 12/10/13
 */
class PerceptronRankingClassifier[F] (
                                       val epochs:Int = 2,
                                       val burnInIterations:Int = 0,
                                       val marginRatio:Double = 1.0) extends RankingClassifier[F] with Serializable {
  def this(props:Properties) =
    this(
      StringUtils.getInt(props, "epochs", 2),
      StringUtils.getInt(props, "burnInIterations", 0),
      StringUtils.getDouble(props, "marginRatio", 1.0))

  var featureLexicon:Lexicon[F] = null
  var weights:Array[Double] = null
  var survivedIterations:Int = 0
  var avgWeights:Array[Double] = null
  var totalQueries:Int = 0
  var totalUpdates:Int = 0
  var updatesPerEpoch:Int = 0
  var margin:Double = 0.0

  override def train(dataset:RankingDataset[F], spans:Option[Iterable[(Int, Int)]] = None) {
    val indices = mkTrainIndices(dataset.size, spans)
    totalQueries = indices.length
    featureLexicon = Lexicon(dataset.featureLexicon)
    weights = new Array[Double](featureLexicon.size)
    avgWeights = new Array[Double](featureLexicon.size)
    totalUpdates = 0

    val avgLen = computeAverageVectorLength(dataset, indices)
    logger.debug("Average vector length in training: " + avgLen)
    if(marginRatio > 0) margin = marginRatio * avgLen * avgLen

    val random = new Random(1)
    var converged = false
    for(epoch <- 1 to epochs if ! converged) {
      val randomizedIndices = MathUtils.randomize(indices, random)
      updatesPerEpoch = 0
      logger.debug(s"Starting epoch #$epoch")
      for(qi <- randomizedIndices) {
        val labels = dataset.labels(qi)
        for(di1 <- 0 until labels.length) {
          val l1 = labels(di1)
          val d1 = dataset.featuresCounter(qi, di1)
          for(di2 <- di1 + 1 until labels.length) {
            val l2 = labels(di2)
            if(l1 > l2) {
              val d2 = dataset.featuresCounter(qi, di2)
              update(d1, d2)
            } else if(l1 < l2) {
              val d2 = dataset.featuresCounter(qi, di2)
              update(d2, d1)
            }
          }
        }
      }
      totalUpdates += updatesPerEpoch
      logger.debug(s"Epoch $epoch completed with $updatesPerEpoch updates.")
      if(updatesPerEpoch == 0) converged = true
    }
  }

  def computeAverageVectorLength(dataset:RankingDataset[F], indices:Array[Int]):Double = {
    var sum = 0.0
    var count = 0
    for(qi <- indices) {
      val labels = dataset.labels(qi)
      for(di <- 0 until labels.length) {
        val d = dataset.featuresCounter(qi, di)
        sum += math.sqrt(dotProduct(d, d))
        count += 1
      }
    }
    sum / count.toDouble
  }

  def update(better:Counter[Int], worse:Counter[Int]) {
    if(dotProduct(weights, better) - dotProduct(weights, worse) <= margin) {
      addToAvg()

      updateWeights(better, 1.0)
      updateWeights(worse, -1.0)
      survivedIterations = 0
      updatesPerEpoch += 1
    } else {
      survivedIterations += 1
    }
  }

  def addToAvg() {
    if(survivedIterations > 0 && totalUpdates + updatesPerEpoch > burnInIterations) {
      var i = 0
      val mult = survivedIterations.toDouble * totalQueries.toDouble
      while(i < weights.length) {
        avgWeights(i) += weights(i) * mult
        i += 1
      }
    }
  }

  def updateWeights(v:Counter[Int], w:Double) {
    for(i <- v.keySet) {
      if(i < weights.length) weights(i) += (v.getCount(i) * w)
    }
  }

  override def scoresOf(queryDatums:Iterable[Datum[Int, F]]):Iterable[Double] = {
    val scores = new ArrayBuffer[Double]()
    for(d <- queryDatums) {
      val c = d.featuresCounter
      val s = datumDotProduct(c)
      scores += s
    }
    scores.toArray
  }

  def datumDotProduct(c:Counter[F]):Double = {
    var sum = 0.0
    for(f <- c.keySet) {
      val i = featureLexicon.get(f)
      if(i.isDefined) {
        sum += c.getCount(f) * avgWeights(i.get)
      }
    }
    sum
  }

  override def saveTo(fileName:String) {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os.writeObject(this)
    os.close()
  }

  override def displayModel(pw:PrintWriter) {
    pw.println("Perceptron weights:")
    for(i <- 0 until avgWeights.length) {
      pw.println(s"\t#$i: ${featureLexicon.get(i)} => ${avgWeights(i)}")
    }
  }
}

object PerceptronRankingClassifier {
  val logger = LoggerFactory.getLogger(classOf[PerceptronRankingClassifier[String]])

  def loadFrom[F](fileName:String):PerceptronRankingClassifier[F] = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[PerceptronRankingClassifier[F]]
    is.close()
    c
  }
}
