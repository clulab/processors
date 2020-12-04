package org.clulab.scaladynet.parameters

import org.clulab.scaladynet.parameters.init.ParameterInit
import org.clulab.scaladynet.parameters.init.ParameterInitConst
import org.clulab.scaladynet.parameters.init.ParameterInitGlorot
import org.clulab.scaladynet.parameters.init.ParameterInitUniform
import org.clulab.scaladynet.utils.Dim

import scala.collection.mutable

class ParameterCollection(name: String = "/", parentOpt: Option[ParameterCollection] = None) {
  protected var lookupParameters: mutable.Buffer[LookupParameter] = mutable.Buffer.empty
//  protected var parameters: mutable.Buffer[Parameter] = mutable.Buffer.empty

  def gradientL2Norm(): Float = ???

  def resetGradient(): Unit = ???

  def addLookupParameters(n: Long, dim: Dim): LookupParameter = {
    val lookupParameter = new LookupParameter(dim)

    lookupParameters += lookupParameter
    lookupParameter
  }

  def addParameters(d: Dim, scale: Float = 0.0f): Parameter = add_parameters(d, scale)

  // internal

  val name_cntr: mutable.HashMap[String, Int] = mutable.HashMap.empty
  val collec_name_cntr: mutable.HashMap[String, Int] = mutable.HashMap.empty

  def add_subcollection(sub_name: String): ParameterCollection = {
    val new_name = s"$name$sub_name"
    val idx = collec_name_cntr.getOrElse(new_name, 0)
    val suffix = if (idx > 0 || sub_name.isEmpty) "_/" else "/"

    collec_name_cntr.update(new_name, idx + 1)
    new ParameterCollection(s"$new_name$suffix", Some(this))
  }

  def add_parameters(d: Dim, scale: Float = 0f, p_name: String = ""): Parameter = {
    if (scale == 0f)
      add_parameters(d, ParameterInitGlorot(), p_name)
    else
      add_parameters(d, ParameterInitUniform(scale), p_name)
  }

  def add_parameters(dim: Dim, init: ParameterInit): Parameter =
    add_parameters(dim, init, "")

  def add_parameters(dim: Dim, init: ParameterInit, p_name: String): Parameter = {
    val new_name = s"$name$p_name"
    val idx = name_cntr.getOrElse(p_name, 0)
    val suffix = if (idx > 0 || p_name.isEmpty) "_" + idx else ""

    name_cntr.update(p_name, idx + 1)
    new Parameter(dim, s"$new_name$suffix", init)
  }
}
