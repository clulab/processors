package org.clulab.dynet

/** Some really basic vector math that happens outside of DyNet */
object ArrayMath {
  def argmax(vector:Array[Float]):Int = {
    var bestValue = Float.MinValue
    var bestArg = 0
    for(i <- vector.indices) {
      if(vector(i) > bestValue) {
        bestValue = vector(i)
        bestArg = i
      }
    }
    bestArg
  }

  def sum(v1:Array[Float], v2:Array[Float]): Array[Float] = {
    assert(v1.length == v2.length)
    val s = new Array[Float](v1.length)
    for(i <- v1.indices) {
      s(i) = v1(i) + v2(i)
    }
    s
  }

  def toFloatArray(doubles: Array[Double]): Array[Float] = {
    val floats = new Array[Float](doubles.length)
    for (i <- doubles.indices) {
      floats(i) = doubles(i).toFloat
    }
    floats
  }
}
