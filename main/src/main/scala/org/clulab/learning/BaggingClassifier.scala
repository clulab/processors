package org.clulab.learning

import org.clulab.struct.Counter

import scala.util.Random
import org.slf4j.LoggerFactory
import BaggingClassifier._
import java.io._

import org.clulab.utils.Serializer

/**
 * Classifier that implements bagging over another Classifier
 * Created by dfried, mihais
 * Date: 4/25/14
 */
class BaggingClassifier[L, F] (val baseClassifierFactory: () => Classifier[L,F],
                               val N:Int,
                               val random:Random = null) extends Classifier[L, F] {

  val classifiers = new Array[Classifier[L, F]](N)

  /** Trains the classifier on the given dataset */
  override def train(dataset:Dataset[L, F], indices:Array[Int]): Unit = {
    for(i <- 0 until N) {
      logger.debug(s"Training classifier #$i...")
      val sampleIndices = sampleWithReplacement(indices, 1.0)
      classifiers(i) = baseClassifierFactory()
      classifiers(i).train(dataset, sampleIndices)
    }
  }

  /** Samples with replacement from the given array */
  private def sampleWithReplacement(in:Array[Int], proportion:Double):Array[Int] = {
    val max = (proportion * in.length.toDouble).toInt
    val out = new Array[Int](max)
    for(i <- 0 until max) {
      out(i) = in(random.nextInt(in.length))
    }
    out.sorted
  }

  /** Returns the argmax for this datum */
  override def classOf(d:Datum[L, F]): L = {
    val labelCounts = new Counter[L]
    for(c <- classifiers) {
      val l = c.classOf(d)
      labelCounts.incrementCount(l)
    }
    labelCounts.sorted.head._1
  }

  /**
   * Returns the scores of all possible labels for this datum
   * Convention: if the classifier can return probabilities, these must be probabilities
   **/
  override def scoresOf(d:Datum[L, F]): Counter[L] = {
    val summed = new Counter[L]()
    for(c <- classifiers) {
      val scores = c.scoresOf(d)
      for(k <- scores.keySet) {
        summed.incrementCount(k, scores.getCount(k))
      }
    }
    summed / N
  }

  /** Saves the current model to a file */
  override def saveTo(writer:Writer): Unit = { throw new RuntimeException("ERROR: saving to Writer not supported yet!") }

  override def saveTo(fn:String): Unit = {
    Serializer.save(this, fn)
  }
}

object BaggingClassifier {
  val logger = LoggerFactory.getLogger(classOf[BaggingClassifier[String, String]])

  def loadFrom[L, F](fileName:String):BaggingClassifier[L, F] = {
    Serializer.load(fileName)
  }
}
