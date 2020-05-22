package org.clulab.utils

import org.clulab.utils.MathUtils._
import org.scalatest._

class TestMathUtils extends FlatSpec with Matchers {
  val seq = Seq(0.64819654, 0.31665825, 0.95268787, 0.12137638, 0.12971271)

  it should "work with Seq" in {
    math.abs(seq.mean() - 0.4337263) should be < 0.000001
    math.abs(seq.variance() - 0.1038020) should be < 0.000001
  }

  it should "not produce negative mean or variance" in {
    seq.mean() should be >= 0.0
    seq.variance() should be >= 0.0
  }


  val array: Array[Double] = Array(0.28776269, 0.47035501, 0.17410317, 0.26174649, 0.90075753)

  it should "work with Array" in {
    math.abs(array.mean() - 0.4189449) should be < 0.000001
    math.abs(array.variance() - 0.0673307) should be < 0.000001
  }

  it should "work with any numeric collection" in {
    val coll = Array(1, 2, 3, 4, 5)
    math.abs(coll.map(_.toFloat).mean() - 3) should be < 0.000001
    math.abs(coll.map(_.toFloat).variance() - 2) should be < 0.000001
    math.abs(coll.map(_.toLong).mean() - 3) should be < 0.000001
    math.abs(coll.map(_.toLong).variance() - 2) should be < 0.000001
    math.abs(coll.map(_.toDouble).mean() - 3) should be < 0.000001
    math.abs(coll.map(_.toDouble).variance() - 2) should be < 0.000001
  }

  val array2 = Array.fill(1000000) { 100000 }

  it should "be reasonably resilient to overflow" in {
    math.abs(array2.mean() - 100000) should be < 0.000001
    array2.variance() should be < 0.000001
  }
}
