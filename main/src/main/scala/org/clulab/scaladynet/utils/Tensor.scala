package org.clulab.scaladynet.utils

import org.clulab.scaladynet.vectors.FloatVector

class Tensor(val d: Dim, v: Seq[Float], var offset: Int) {

  // Sub tensor at batch `b`
  def batch_elem(b: Int /* Batch id */): Tensor = {
    if (d.batch_elems() == 1)
      this
    else {
      assert(b < d.batch_elems())
      val bsize = d.batch_size()
      val new_d = Dim(d, 1)

      Tensor(new_d, v, bsize * b)
    }
  }

  def batch_elems(): Seq[Tensor] = ???

  def toFloat(): Float = v(offset)

  def toVector(): FloatVector = FloatVector(v, offset)
}

object Tensor {

  def apply(d: Dim = Dim()): Tensor = new Tensor(d, Seq.empty, 0)

  def apply(d: Dim, v: Seq[Float], offset: Int): Tensor = new Tensor(d, v, offset)

  def apply(other: Tensor): Tensor = ???
}
