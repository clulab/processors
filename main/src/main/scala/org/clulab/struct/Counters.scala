package org.clulab.struct

/**
 * Operations on Counter(s)
 * User: mihais
 * Date: 3/18/13
 */

object Counters {
  def cosine[T](c1:Counter[T], c2:Counter[T]):Double = {
    var dotProduct = 0.0
    var lsq1 = 0.0
    var lsq2 = 0.0

    for (key <- c1.keySet) {
      val count1 = c1.getCount(key)
      if (count1 != 0.0) {
        lsq1 += count1 * count1
        val count2 = c2.getCount(key)
        if (count2 != 0.0) {
          dotProduct += count1 * count2
        }
      }
    }

    for (key <- c2.keySet) {
      val count2 = c2.getCount(key)
      if (count2 != 0.0) {
        lsq2 += count2 * count2
      }
    }

    if (lsq1 != 0.0 && lsq2 != 0.0) {
      return dotProduct / (math.sqrt(lsq1) * math.sqrt(lsq2))
    }
    0.0
  }

  def dotProduct[T](c1:Counter[T], c2:Counter[T]):Double = {
    var product = 0.0
    for (key <- c1.keySet) {
      val count1 = c1.getCount(key)
      if (count1 != 0.0) {
        val count2 = c2.getCount(key)
        if (count2 != 0.0) {
          product += count1 * count2
        }
      }
    }
    product
  }

  def dotProduct(c1:Array[Double], c2:Counter[Int]):Double = {
    var product = 0.0
    for(i <- c2.keySet) {
      if (i < c1.size) product += c1(i) * c2.getCount(i)
    }
    product
  }

  // TEST
  def dotProductOnlyPositive(c1:Array[Double], c2:Counter[Int]):Double = {
    var product = 0.0
    for(i <- c2.keySet) {
      if (i < c1.size) {
        val prod = c1(i) * c2.getCount(i)
        if (prod > 0) product += prod
      }
    }
    product
  }

}
