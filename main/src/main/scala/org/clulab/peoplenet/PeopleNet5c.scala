package org.clulab.peoplenet

import java.io.PrintWriter

import org.clulab.struct.Counter

import scala.collection.mutable.ArrayBuffer

/**
 * Same thing as PeopleNet but it operates over sparse property vectors
 * Sparse vectors are stored using our own Counter class
 */
object PeopleNet5c {
  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("UAIR5c.csv")
    val it = source.getLines()
    // skip the first line; these are the 5 scores
    it.next()

    val names = new ArrayBuffer[String]()
    val vectors = new ArrayBuffer[Counter[String]]()

    while(it.hasNext) {
      val line = it.next()
      println(line)
      val tokens = line.split(",")

      names += tokens(0)
      val v = new Counter[String]()
      for(i <- 1 until 6) {
        v.setCount(tokens(i), (6 - i).toDouble)
      }
      vectors += v
    }
    source.close()
    println(s"Found ${names.length} people.")

    for(i <- names.indices) {
      for(j <- i + 1 until names.length) {
        val cs = cosineSim(vectors(i), vectors(j))
        println(s"Cosine similarity between ${names(i)} and ${names(j)} is $cs.")
      }
    }

    saveGmap("gmap5c", names.toArray, vectors.toArray, 0.1)
    saveGmap("gmap5c", names.toArray, vectors.toArray, 0.2)
    saveGmap("gmap5c", names.toArray, vectors.toArray, 0.3)
    saveGmap("gmap5c", names.toArray, vectors.toArray, 0.4)
    saveGmap("gmap5c", names.toArray, vectors.toArray, 0.5)
    saveGmap("gmap5c", names.toArray, vectors.toArray, 0.6)
    saveGmap("gmap5c", names.toArray, vectors.toArray, 0.7)
    saveGmap("gmap5c", names.toArray, vectors.toArray, 0.8)
    saveGmap("gmap5c", names.toArray, vectors.toArray, 0.9)
  }

  def saveGmap(gmapFilename:String,
               names:Array[String],
               vectors:Array[Counter[String]],
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

  def cosineSim(v1: Counter[String], v2: Counter[String]): Double =
    v1.dotProduct(v2) / (v1.l2Norm * v2.l2Norm)
}
