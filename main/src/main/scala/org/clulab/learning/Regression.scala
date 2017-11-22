package org.clulab.learning

import java.io._

import org.clulab.learning.Datasets._

/**
  * Trait for regression
  * Adapted from Classifier trait
  * User: mihais, danebell
  * Date: 11/15/17
  */
trait Regression[F] {
  def train(dataset: RegDataset[F]) {
    train(dataset, dataset.indices.toArray)
  }

  /**
    * Trains the classifier on the given dataset
    * spans is useful during cross validation
    */
  def train(dataset: RegDataset[F], spans: Option[Iterable[(Int, Int)]]) {
    val indices = mkTrainIndices(dataset.size, spans)
    train(dataset, indices)
  }

  /**
    * Trains a classifier, using only the datums specified in indices
    * indices is useful for bagging
    */
  def train(dataset: RegDataset[F], indices:Array[Int])

  /**
    * Returns the score for this datum
    * NB: This is not necessarily a probability
    **/
  def scoreOf(d:Datum[Double, F]): Double

  /** Saves the current model to a file */
  def saveTo(fileName:String) {
    val bw = new BufferedWriter(new FileWriter(fileName))
    saveTo(bw)
    bw.close()
  }

  /** Saves to writer. Does NOT close the writer */
  def saveTo(writer:Writer)
}

