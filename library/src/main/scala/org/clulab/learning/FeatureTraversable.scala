package org.clulab.learning

/**
 * Analogous to iterable -- defines a method for geting the feature traverser and updater
 * Created by dfried on 5/27/14.
 */
trait FeatureTraversable[F, V] {
  /**
   * Get the feature updater for this class, allowing iterating and updating feature values
   */
  def featureUpdater: FeatureUpdater[F, V]
}
