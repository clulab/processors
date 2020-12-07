package org.clulab.scaladynet.builders

import org.clulab.scaladynet.ComputationGraph
import org.clulab.scaladynet.expressions.Expression
import org.clulab.scaladynet.parameters.Parameter
import org.clulab.scaladynet.parameters.ParameterCollection
import org.clulab.scaladynet.parameters.init.ParameterInitConst
import org.clulab.scaladynet.utils.Dim

class GruBuilder(layers: Int, input_dim: Int, hidden_dim: Int, model: ParameterCollection) extends RnnBuilder {
  protected val local_model: ParameterCollection = model.add_subcollection("gru-builder")
  protected val params: Seq[Seq[Parameter]] = Range(0, layers.toInt).map { i: Int =>
    val layer_input_dim: Int = if (i == 0) input_dim else hidden_dim
    // z
    val p_x2z: Parameter = local_model.add_parameters(Dim(hidden_dim, layer_input_dim))
    val p_h2z: Parameter = local_model.add_parameters(Dim(hidden_dim, hidden_dim))
    val p_bz:  Parameter = local_model.add_parameters(Dim(hidden_dim), ParameterInitConst(0f))
    // r
    val p_x2r: Parameter = local_model.add_parameters(Dim(hidden_dim, layer_input_dim))
    val p_h2r: Parameter = local_model.add_parameters(Dim(hidden_dim, hidden_dim))
    val p_br:  Parameter = local_model.add_parameters(Dim(hidden_dim), ParameterInitConst(0f))
    // h
    val p_x2h: Parameter = local_model.add_parameters(Dim(hidden_dim, layer_input_dim))
    val p_h2h: Parameter = local_model.add_parameters(Dim(hidden_dim), ParameterInitConst(0f))
    val p_bh:  Parameter = local_model.add_parameters(Dim(hidden_dim), ParameterInitConst(0f))

    Seq(p_x2z, p_h2z, p_bz, p_x2r, p_h2r, p_br, p_x2h, p_h2h, p_bh)
  }

  var param_vars: Seq[Seq[Expression]] = Seq.empty

  protected def new_graph_impl(cg: ComputationGraph, update: Boolean): Unit = {
    param_vars = params.map { ps: Seq[Parameter] =>
      ps.map(Expression.const_parameter(cg, _))
    }
    _cg = Some(cg)
  }

  protected def start_new_sequence_impl(h_0: Seq[Expression]): Unit = {
    // h.clear
    // h0 = h_0

  }


}
