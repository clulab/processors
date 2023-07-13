package org.clulab.learning

import org.clulab.struct.Counter
import org.clulab.learning.Datasets._

import java.io._
import scala.util.Using

/**
 * Trait for iid classification
 * For reranking problems, see RankingClassifier
 * User: mihais
 * Date: 11/17/13
 */
trait Classifier[L, F] {
  /**
   * Trains the classifier on the given dataset
   * spans is useful during cross validation
   */
  def train(dataset:Dataset[L, F], spans:Option[Iterable[(Int, Int)]] = None): Unit = {
    val indices = mkTrainIndices(dataset.size, spans)
    train(dataset, indices)
  }

  /**
   * Trains a classifier, using only the datums specified in indices
   * indices is useful for bagging
   */
  def train(dataset:Dataset[L, F], indices:Array[Int]): Unit

  /** Returns the argmax for this datum */
  def classOf(d:Datum[L, F]): L

  /**
   * Returns the scores of all possible labels for this datum
   * Convention: if the classifier can return probabilities, these must be probabilities
   **/
  def scoresOf(d:Datum[L, F]): Counter[L]

  /** Saves the current model to a file */
  def saveTo(fileName:String): Unit = {
    Using.resource(new BufferedWriter(new FileWriter(fileName))) { bw =>
      saveTo(bw)
    }
  }

  /** Saves to writer. Does NOT close the writer */
  def saveTo(writer:Writer): Unit
}

