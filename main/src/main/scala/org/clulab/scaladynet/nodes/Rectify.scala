package org.clulab.scaladynet.nodes

import org.clulab.scaladynet.utils.Dim
import org.clulab.scaladynet.utils.Tensor

class Rectify(i: Int) extends Node(i) {

  def dim_forward(xs: Seq[Dim]): Dim = {
    ???
  }

  def forward_dev_impl(xs: Seq[Tensor], fx: Tensor): Unit = {
    assert(xs.size == 2, "Failed dimension check in CwiseSum::forward (+)")
    var i: Int = 0
    // Lots and lots of math to fill in.
  }
}

object Rectify {

  def apply(i: Int): Rectify = new Rectify(i)
}


