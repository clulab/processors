package org.clulab.scaladynet.parameters

import org.clulab.scaladynet.utils.Dim

class ParameterCollection() {
  def gradientL2Norm(): Float = ???
  def resetGradient(): Unit = ???
  def addLookupParameters(n: Long, d: Dim): LookupParameter = ???
  def addParameters(d: Dim, scale: Float = 0.0f): Parameter = ???
}
