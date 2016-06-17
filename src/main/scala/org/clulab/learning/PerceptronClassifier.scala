package org.clulab.learning

import org.clulab.struct.Counter
import java.io._
import org.slf4j.LoggerFactory
import java.util.Properties
import org.clulab.utils.{Files, MathUtils, StringUtils}
import org.clulab.struct.Lexicon
import org.clulab.struct.Counters._
import PerceptronClassifier.logger
import scala.collection.mutable.ArrayBuffer
import scala.Serializable
import scala.util.Random

/**
 * Multiclass perceptron classifier, in primal mode
 * Includes averaging, hard margin, burn-in iterations
 * User: mihais
 * Date: 12/15/13
 */
class PerceptronClassifier[L, F] (
                             val epochs:Int = 2,
                             val burnInIterations:Int = 0,
                             val marginRatio:Double = 1.0) extends Classifier[L, F] with Serializable {
  def this(props:Properties) =
    this(
      StringUtils.getInt(props, "epochs", 2),
      StringUtils.getInt(props, "burnInIterations", 0),
      StringUtils.getDouble(props, "marginRatio", 1.0))

  private var featureLexicon:Lexicon[F] = null
  private var labelLexicon:Lexicon[L] = null
  private var margin:Double = 0.0

  /** Latest vector for each class label */
  private var weights:Array[Array[Double]] = null
  /** Number of survived iterations by the latest vector, for each class */
  private var survivedIterations:Array[Int] = null
  /** Average vector for each class label */
  private var avgWeights:Array[Array[Double]] = null
  /** Total number of datums in training */
  private var totalDatums = 0

  private var totalUpdates:Array[Int] = null
  private var updatesPerEpoch:Array[Int] = null

  def train(dataset:Dataset[L, F], indices:Array[Int]) {
    featureLexicon = Lexicon(dataset.featureLexicon)
    labelLexicon = Lexicon(dataset.labelLexicon)
    logger.debug(s"Training a model for ${labelLexicon.size} labels and ${featureLexicon.size} features.")

    totalDatums = indices.size
    weights = new Array[Array[Double]](labelLexicon.size)
    avgWeights = new Array[Array[Double]](labelLexicon.size)
    survivedIterations = new Array[Int](labelLexicon.size)
    totalUpdates = new Array[Int](labelLexicon.size)
    updatesPerEpoch = new Array[Int](labelLexicon.size)
    for(i <- 0 until labelLexicon.size) {
      weights(i) = new Array[Double](featureLexicon.size)
      avgWeights(i) = new Array[Double](featureLexicon.size)
    }

    val avgLen = computeAverageVectorLength(dataset, indices)
    logger.debug(s"Average vector length for training dataset with ${dataset.size} datums: $avgLen")
    if(marginRatio > 0) margin = marginRatio * avgLen * avgLen

    val random = new Random(1)
    var converged = false
    for(epoch <- 1 to epochs if ! converged) {
      val randomizedIndices = MathUtils.randomize(indices, random)
      for(i <- 0 until labelLexicon.size)
        updatesPerEpoch(i) = 0
      logger.debug(s"Starting epoch #$epoch")

      for(di <- randomizedIndices) {
        val label = dataset.labels(di)
        val d = dataset.featuresCounter(di)
        update(label, d)
      }

      for(i <- 0 until labelLexicon.size)
        totalUpdates(i) += updatesPerEpoch(i)
      logger.debug(s"Epoch $epoch completed with ${updatesPerEpoch.toList} updates.")

      var sumUpdates = 0
      updatesPerEpoch.foreach(sumUpdates += _)
      if(sumUpdates == 0) converged = true
    }

    /*
    for(i <- 0 until labelLexicon.size) {
      println(s"Weights for label #$i")
      for(j <- 0 until avgWeights(i).length) {
        println(s"\tFeature #$j: ${avgWeights(i)(j)}")
      }
    }
    */
  }

  private def update(goldLabel:Int, datum:Counter[Int]) {
    // compute the scores for all class labels
    val predictions = new ArrayBuffer[(Int, Double)](labelLexicon.size)
    for(i <- 0 until labelLexicon.size) {
      predictions += new Tuple2(i, dotProduct(weights(i), datum))
    }

    // sort predictions in descending order of scores
    val sortedPredictions = predictions.sortBy(- _._2).toArray

    // update if the top prediction is different from the gold label
    if(sortedPredictions(0)._1 != goldLabel) {
      // negative updates for all labels scored higher than gold
      var i = 0
      while(sortedPredictions(i)._1 != goldLabel) {
        val l = sortedPredictions(i)._1
        addToAvg(l)
        updateWeights(l, datum, -1.0)
        survivedIterations(l) = 0
        updatesPerEpoch(l) += 1
        i += 1
      }

      // positive update for gold
      updateWeights(goldLabel, datum, +1.0)
      survivedIterations(goldLabel) = 0
      updatesPerEpoch(goldLabel) += 1
      i += 1

      // increment survival count for all other labels
      while(i < sortedPredictions.length) {
        survivedIterations(sortedPredictions(i)._1) += 1
        i += 1
      }
    }

    // negative updates of correct prediction but small margin delta with next
    else if(sortedPredictions(0)._2 - sortedPredictions(1)._2 < margin) {
      // positive update for gold
      updateWeights(goldLabel, datum, +1.0)
      survivedIterations(goldLabel) = 0
      updatesPerEpoch(goldLabel) += 1

      // negative updates for all labels too close to gold
      var i = 1
      while(i < sortedPredictions.length && sortedPredictions(0)._2 - sortedPredictions(i)._2 < margin) {
        val l = sortedPredictions(i)._1
        addToAvg(l)
        updateWeights(l, datum, -1.0)
        survivedIterations(l) = 0
        updatesPerEpoch(l) += 1
        i += 1
      }

      // increment survival count for all other labels
      while(i < sortedPredictions.length) {
        survivedIterations(sortedPredictions(i)._1) += 1
        i += 1
      }
    }

    else {
      // everything is fine
      // increment survival count for all other labels
      var i = 0
      while(i < sortedPredictions.length) {
        survivedIterations(sortedPredictions(i)._1) += 1
        i += 1
      }
    }
  }

  private def addToAvg(label:Int) {
    if(survivedIterations(label) > 0 && totalUpdates(label) + updatesPerEpoch(label) > burnInIterations) {
      var i = 0
      val mult = survivedIterations(label).toDouble / totalDatums.toDouble
      while(i < weights(label).length) {
        avgWeights(label)(i) += weights(label)(i) * mult
        i += 1
      }
    }
  }

  private def updateWeights(label:Int, datum:Counter[Int], weight:Double) {
    val lw = weights(label)
    for(i <- datum.keySet) {
      if(i < lw.length) lw(i) += (datum.getCount(i) * weight)
    }
  }

  private def computeAverageVectorLength(dataset:Dataset[L, F], indices:Array[Int]):Double = {
    var sum = 0.0
    var count = 0
    for(qi <- indices) {
      val d = dataset.featuresCounter(qi)

      sum += math.sqrt(dotProduct(d, d))
      //println ("d: " + d + " \tsum:" + sum)
      count += 1
    }
    //println ("computeAverageVectorLength: sum:" + sum + " \tcount:" + count )

    sum / count.toDouble
  }

  def classOf(d:Datum[L, F]): L = {
    var bestLabel = 0
    var bestScore = Double.MinValue
    val f = d.featuresCounter
    for(i <- 0 until labelLexicon.size) {
      val score = datumDotProduct(i, f)
      if(score > bestScore) {
        bestScore = score
        bestLabel = i
      }
    }
    labelLexicon.get(bestLabel)
  }

  def scoresOf(d:Datum[L, F]): Counter[L] = {
    val c = new Counter[L]()
    val f = d.featuresCounter
    for(i <- 0 until labelLexicon.size) {
      val score = datumDotProduct(i, f)
      val l = labelLexicon.get(i)
      c.setCount(l, score)
    }
    c
  }

  private def datumDotProduct(label:Int, c:Counter[F]):Double = {
    var sum = 0.0
    for(f <- c.keySet) {
      val i = featureLexicon.get(f)
      if(i.isDefined) {
        sum += c.getCount(f) * avgWeights(label)(i.get)
      }
    }
    sum
  }

  def saveTo(w:Writer) {
    // only need to save avgWeights and lexicons here!
    val writer = Files.toPrintWriter(w)
    featureLexicon.saveTo(writer)
    labelLexicon.saveTo(writer)
    writer.println(avgWeights.size)
    for(i <- 0 until avgWeights.size) {
      writer.println(avgWeights(i).mkString(" "))
    }
  }

  def displayWeights(pw:PrintWriter) {
    println ("displayWeights")

    pw.println ("Perceptron Classifier Average Weights")
    for (i <- 0 until labelLexicon.size) {
      pw.print("label: " + labelLexicon.get(i) + " \t")
      for (j <- 0 until avgWeights(i).size) {
        pw.print (featureLexicon.get(j) + ":" + avgWeights(i)(j) + " \t")
      }
      pw.println ("")
    }
    pw.println
  }
}

object PerceptronClassifier {
  val logger = LoggerFactory.getLogger(classOf[PerceptronClassifier[String, String]])

  def loadFrom[L, F](fileName:String):PerceptronClassifier[L, F] = {
    val r = new BufferedReader(new FileReader(fileName))
    val c = loadFrom[L, F](r)
    r.close()
    c
  }

  def loadFrom[L, F](r:Reader):PerceptronClassifier[L, F] = {
    val reader = Files.toBufferedReader(r)
    val c = new PerceptronClassifier[L, F]()
    c.featureLexicon = Lexicon.loadFrom[F](reader)
    c.labelLexicon = Lexicon.loadFrom[L](reader)
    val vectorCount = reader.readLine().toInt
    c.avgWeights = new Array[Array[Double]](vectorCount)
    for(i <- 0 until vectorCount) {
      c.avgWeights(i) = reader.readLine().split("\\s+").map(_.toDouble)
    }
    c
  }
}
