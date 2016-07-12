package org.clulab.learning

/**
 * Allows traversal of a dataset (or ranking dataset)'s features and values, and also destructive updates of the values. Useful for finding the range of values and then rescaling them. Analogous to iterator with destructive updates
 * Created by dfried on 5/27/14.
 */
trait FeatureUpdater[F, V] extends Traversable[(F, V)] {
  /**
   * Destructively modify all feature values using a function of the feature and value
   */
  def updateAll(fn: ((F, V)) => V): Unit
}

