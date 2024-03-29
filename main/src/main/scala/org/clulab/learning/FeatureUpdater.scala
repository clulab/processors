package org.clulab.learning

/**
 * Allows traversal of a dataset (or ranking dataset)'s features and values, and also destructive updates of the values. Useful for finding the range of values and then rescaling them. Analogous to iterator with destructive updates
 * Created by dfried on 5/27/14.
 */
trait FeatureUpdater[F, V] extends Iterable[(F, V)] {

  /**
   * Traverse the dataset's features and values.
   */
  // On Scala 2.13, this is already included.
  // def foreach[U](fn: ((F, Double)) => U): Unit

  /**
   * Destructively modify all feature values using a function of the feature and value
   */
  def updateAll(fn: ((F, V)) => V): Unit
}

