package org.clulab.scaladynet.parameters

import org.clulab.scaladynet.utils.Dim
import org.clulab.scaladynet.vectors.FloatVector

import scala.collection.mutable

class LookupParameter(dim: Dim) {
  protected val map: mutable.Map[Long, FloatVector] = mutable.Map.empty

  def initialize(index: Long, values: FloatVector): Unit = map(index) = values

  def dim(): Dim = dim
}
