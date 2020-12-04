package org.clulab.scaladynet.builders

import org.clulab.scaladynet.ComputationGraph
import org.clulab.scaladynet.expressions.Expression
import org.clulab.scaladynet.parameters.Parameter
import org.clulab.scaladynet.parameters.ParameterCollection
import org.clulab.scaladynet.parameters.ParameterInitConst
import org.clulab.scaladynet.utils.Dim

import scala.collection.mutable
import scala.collection.mutable

// See VanillaLSTMBuilder
class LstmBuilder(layers: Long, input_dim: Long, hidden_dim: Long, model: ParameterCollection) extends RnnBuilder {
  protected val local_model: ParameterCollection = model.add_subcollection("vanilla-lstm-builder")
  protected val params: Seq[Seq[Parameter]] = Range(0, layers.toInt).map { i: Int =>
    val layer_input_dim: Long = if (i == 0) input_dim else hidden_dim
    // i
    val p_x2i: Parameter = local_model.add_parameters(Dim(hidden_dim * 4, layer_input_dim))
    val p_h2i: Parameter = local_model.add_parameters(Dim(hidden_dim * 4, hidden_dim))
    val p_bi:  Parameter = local_model.add_parameters(Dim(hidden_dim * 4), ParameterInitConst(0f))

    Seq(p_x2i, p_h2i, p_bi)
  }

  var param_vars: Seq[Seq[Expression]] = Seq.empty

  protected def new_graph_impl(cg: ComputationGraph, update: Boolean): Unit = {
    param_vars = params.map { ps: Seq[Parameter] =>
      ps.map(Expression.const_parameter(cg, _))
    }
    _cg = Some(cg)
  }

  protected def start_new_sequence_impl(hinit: Seq[Expression]): Unit = {
    // h.clear
    // c.clear
    // has_initial_state = false
  }

}
