package org.clulab.scaladynet.nodes

import org.clulab.scaladynet.ComputationGraph
import org.clulab.scaladynet.utils.Tensor

abstract class Node(args: Seq[Int] = Seq.empty) {

  def this(arg: Int) = this(Seq(arg)) // Probably needs seq of this size, not with this value

  protected var cgOpt: Option[ComputationGraph] = None

  protected def supports_multibatch(): Boolean = false

  def forward(xs: Seq[Tensor], fx: Tensor): Unit = {
    if (supports_multibatch() || fx.d.batch_elems() == 1)
      forward_impl(xs, fx)
    else {
      val xs_elems: Seq[Tensor] = xs.map(_.batch_elem(0))
      val xs_ptrs: Seq[Tensor] = xs_elems
      val xs_sizes: Seq[Int] = xs.map(_.d.size())
      val fx_elem: Tensor = Tensor(fx.batch_elem(0))
      val fx_size: Int = fx_elem.d.size
      forward_impl(xs_ptrs, fx_elem)
      Range(1, fx.d.batch_elems()).foreach { b =>
        Range(0, xs.size).foreach { i =>
          if (xs(i).d.bd > 1)
            xs_elems(i).offset += xs_sizes(i)
        }
        fx_elem.offset += fx_size
        forward_impl(xs_ptrs, fx_elem)
      }
    }
  }

// internal

  def forward_impl(xs: Seq[Tensor], fx: Tensor): Unit = ???

  def forward_dev_impl(xs: Seq[Tensor], fx: Tensor): Unit

  def arity: Int = args.size

  def get_cg: Option[ComputationGraph] = cgOpt

  def set_cg(cg: ComputationGraph): Unit = cgOpt = Option(cg)
}
