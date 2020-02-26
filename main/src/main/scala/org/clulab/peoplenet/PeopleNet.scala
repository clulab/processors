package org.clulab.peoplenet

import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer

/**
 * Builds the GMap input for a network of people linked by cosine similarity of property vectors
 */
object PeopleNet {
  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("testuair2.csv")
    val it = source.getLines()
    // skip the first line; these are the 4 coarse categories
    it.next()

    // skip the second line for now; these are the 34 fine-grained categories
    it.next()

    //
    // Construct the array of people and their property vectors
    //
    val names = new ArrayBuffer[String]()
    val vectors = new ArrayBuffer[Array[Double]]()

    while(it.hasNext) {
      val line = it.next()
      val tokens = line.split(",")
      assert (tokens.length == 35)
      names += tokens(0)
      val vector = new Array[Double](34)
      for(i <- vector.indices) {
        vector(i) = tokens(i + 1).toDouble
        if(vector(i) > 0) {
          // cosine similarity assume that higher is better, so
          // I am changing the weights such that 5 is the highest, 1 is the lowest
          vector(i) = 6 - vector(i)
        }
      }
      vectors += vector
    }
    source.close()
    println(s"Found ${names.length} people.")

    //
    // Just debugging info
    //
    for(i <- names.indices) {
      for(j <- i + 1 until names.length) {
        val cs = cosineSim(vectors(i), vectors(j))
        println(s"Cosine similarity between ${names(i)} and ${names(j)} is $cs.")
      }
    }

    //
    // Save 1 GMap file for each threshold
    // In each setting, we create a GMap link only if cossim is larger than the threshold
    //
    saveGmap("gmap", names.toArray, vectors.toArray, 0.1)
    saveGmap("gmap", names.toArray, vectors.toArray, 0.2)
    saveGmap("gmap", names.toArray, vectors.toArray, 0.3)
    saveGmap("gmap", names.toArray, vectors.toArray, 0.4)
    saveGmap("gmap", names.toArray, vectors.toArray, 0.5)
    saveGmap("gmap", names.toArray, vectors.toArray, 0.6)
    saveGmap("gmap", names.toArray, vectors.toArray, 0.7)
    saveGmap("gmap", names.toArray, vectors.toArray, 0.8)
    saveGmap("gmap", names.toArray, vectors.toArray, 0.9)
  }

  def saveGmap(gmapFilename:String,
               names:Array[String],
               vectors:Array[Array[Double]],
               threshold:Double): Unit = {
    val pw = new PrintWriter(s"${gmapFilename}_$threshold.dot")
    pw.println("graph {")
    for(i <- names.indices) {
      pw.println(s"""  "${names(i)}";""")
    }
    pw.println()
    for(i <- names.indices) {
      for(j <- i + 1 until names.length) {
        val cs = cosineSim(vectors(i), vectors(j))
        if(cs > threshold) {
          pw.println(s"""  "${names(i)}" -- "${names(j)}" [len=${cs}];""")
        }
      }
    }
    pw.println("}")
    pw.close()
  }

  /** Cosine similarity between two vectors */
  def cosineSim(v1: Array[Double], v2: Array[Double]) =
    dotProd(v1, v2) / (len(v1) * len(v2))

  /** Dot product between two vectors */
  def dotProd(v1: Array[Double], v2: Array[Double]): Double = {
    assert(v1.length == v2.length)
    var p = 0.0
    for(i <- v1.indices) {
      p += v1(i) * v2(i)
    }
    p
  }

  /** L2 norm of a vector */
  def len(v:Array[Double]): Double = {
    var l = 0.0
    for(i <- v.indices) {
      l += v(i) * v(i)
    }
    math.sqrt(l)
  }
}
