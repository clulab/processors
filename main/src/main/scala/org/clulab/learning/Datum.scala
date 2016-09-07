package org.clulab.learning

import org.clulab.struct.Counter

/**
 * Trait for ML datums. L indicates the type of the label; F indicates the type of the feature
 * User: mihais
 * Date: 4/23/13
 */
trait Datum[L, F] {
  val label:L

  def features:Iterable[F]

  def featuresCounter:Counter[F]

  override def toString:String = {
    val os = new StringBuilder
    os.append("LABEL:" + label)
    os.append(" FEATURES:")
    val c = featuresCounter
    val keys = c.keySet
    var first = true
    for(key <- keys) {
      if(! first) os.append(", ")
      os.append(key)
      os.append(":")
      os.append(c.getCount(key))
      first = false
    }
    os.toString()
  }
}

/**
 * Datum that contains only binary- (or Int) valued features
 * @param label
 * @param features
 * @tparam L
 * @tparam F
 */
class BVFDatum[L, F](
  val label:L,
  val features:Iterable[F]) extends Datum[L, F] {

  def featuresCounter:Counter[F] = {
    val c = new Counter[F]
    for(f <- features) {
      c.incrementCount(f)
    }
    c
  }

  override def equals(other:Any):Boolean = {
    other match {
      case that:BVFDatum[L, F] => label == that.label && features == that.features
      case _ => false
    }
  }

  override def hashCode = features.hashCode()
}

/**
 * Datum that contains real-valued features
 * @param label
 * @param featuresCounter
 * @tparam L
 * @tparam F
 */
class RVFDatum[L, F](
  val label:L,
  val featuresCounter:Counter[F]) extends Datum[L, F] {

  def features = featuresCounter.keySet

  def getFeatureCount(f:F) = featuresCounter.getCount(f)

  override def equals(other:Any):Boolean = {
    other match {
      case that:RVFDatum[L, F] => label == that.label && featuresCounter == that.featuresCounter
      case _ => false
    }
  }
}

/**
 * Datum that contains real-valued features and kernelized representation
 * @param label
 * @param featuresCounter
 * @param kernel
 * @tparam L
 * @tparam F
 */
class RVFKDatum[L, F](
  label:L,
  featuresCounter:Counter[F],
  val kernel:String) extends RVFDatum[L, F](label, featuresCounter) {

  override def equals(other:Any):Boolean = {
    other match {
      case that:RVFKDatum[L, F] => label == that.label && featuresCounter == that.featuresCounter && kernel == that.kernel
      case _ => false
    }
  }
}
