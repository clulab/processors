package org.clulab.sequences

import edu.cmu.dynet._
import edu.cmu.dynet.Expression._

object RNNToy {
  def main(args:Array[String]): Unit = {
    Initialize.initialize(Map("random-seed" -> 1l))
    val parameters = new ParameterCollection()
    val p = parameters.addParameters(Dim(2, 2), ParameterInit.uniform(-1, 1))

    println("matrix as vector:")
    println(p.values().toVector())

    val e = parameter(p)
    val ep = pick(pick(e, 1), 0)
    println(ep.value().toFloat())

    val ev = new ExpressionVector()
    ev.add(input(1))
    ev.add(input(2))
    val lse = logSumExp(ev)
    println(lse.value().toVector())

    val p2 = parameters.addParameters(Dim (2, 1), ParameterInit.uniform(-1, 1))
    println(p2.values().toVector())

    p2.values().toVector()(0) = 0
    p2.values().toVector()(1) = 0
    println(p2.values().toVector())

    println("LookupParameter:")
    val pc = new ParameterCollection()
    val lup: LookupParameter = pc.addLookupParameters(5, Dim(1))
    val ex = Expression.lookup(lup, 2)
    val res = ex.value().toFloat()
    println(res)

    val newValues: Seq[Float] = Array(5.0F).toSeq
    lup.initialize(2, newValues)
    val ex2 = Expression.lookup(lup, 2)
    val res2 = ex2.value().toFloat()
    println(res2)

  }
}
