package org.clulab.scaladynet.nodes

import org.clulab.scaladynet.utils.Dim
import org.clulab.scaladynet.utils.Tensor

class CwiseSum(left: Int, right: Int) extends Node {

  def dim_forward(xs: Seq[Dim]): Dim = {
    ???
  }

  def forward_dev_impl(xs: Seq[Tensor], fx: Tensor): Unit = {
    assert(xs.size == 2, "Failed dimension check in CwiseSum::forward (+)")
    var i: Int = 0
    // Lots and lots of math to fill in.
  }
}

object CwiseSum {

  def apply(left: Int, right: Int): CwiseSum = new CwiseSum(left, right)
}
