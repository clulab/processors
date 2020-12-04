package org.clulab.scaladynet.parameters

import org.clulab.scaladynet.utils.Dim

import scala.collection.mutable
import scala.collection.mutable

class ParameterCollection() {
  protected var lookupParameters: mutable.Buffer[LookupParameter] = mutable.Buffer.empty
  protected var parameters: mutable.Buffer[Parameter] = mutable.Buffer.empty

  def gradientL2Norm(): Float = ???

  def resetGradient(): Unit = ???

  def addLookupParameters(n: Long, dim: Dim): LookupParameter = {
    val lookupParameter = new LookupParameter(dim)

    lookupParameters += lookupParameter
    lookupParameter
  }

  def addParameters(d: Dim, scale: Float = 0.0f): Parameter = {
    val parameter = new Parameter()

    parameters += parameter
    parameter
  }

  // internal
  def add_subcollection(sub_name: String): ParameterCollection = {
    null
  }

  def add_parameters(dim: Dim): Parameter = null

  def add_parameters(dim: Dim, init: ParameterInitConst): Parameter = null
}
