package org.clulab.tmp

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object Aitchinson extends App {
    val EPS:Double = 0.01d
    println(s"eps = $EPS")

    val factorSimplexes = new HashMap[Int, ArrayBuffer[Array[Double]]]()
    var lineCount = 0
    for (line <- io.Source.fromFile("data_cleaned.txt").getLines) {
        if(line.trim.nonEmpty) {
            lineCount += 1
            val tokens = line.split("\t+")
            assert (tokens.length == 2)
            val id = tokens(0).toInt
            val coordTokens = tokens(1).substring(1, tokens(1).length - 1).split(',')
            //println(coordTokens.mkString(" "))
            val coords = mkArray(coordTokens)
            //println(coords.mkString(" "))

            if(factorSimplexes.contains(id)) {
                factorSimplexes(id) += coords
            } else {
                factorSimplexes += id -> new ArrayBuffer[Array[Double]]()
                factorSimplexes(id) += coords
            }
        }
    }
    println(s"Read $lineCount lines.")

    val prior3 = norm(Array(1d, 1d, 1d))
    val prior4 = norm(Array(1d, 1d, 1d, 1d))

    val avgDists = new HashMap[Int, Double]
    for(id <- factorSimplexes.keys) {
        var avg = 0d
        for(coord <- factorSimplexes(id)) {
            val aDist = aitchinson(coord, if(coord.length == 3) prior3 else prior4)
            avg += aDist
        }
        avg = avg / factorSimplexes(id).size
        avgDists += id -> avg
    }

    val sortedFactors = avgDists.toSeq.sortBy(- _._2)
    println(sortedFactors.mkString("\n"))


    //val a1 = norm(Array(10d, 10d, 0d))
    //println(aitchinson(a1, prior3))

    def norm(v: Array[Double]): Array[Double] = {
        //println(s"eps = $EPS")
        var sum = 0d
        val n = new Array[Double](v.length)
        for(i <- v.indices) {
            if(v(i) == 0) {
                //println(s"setting value at $i to $EPS")
                n(i) = EPS // Aitchinson doesn't like 0s
            }
            else n(i) = v(i)
            sum += n(i)
        }
        for(i <- n.indices) { // normalize to unit
            n(i) = n(i) / sum
        }
        n
    }

    def aitchinson(v1: Array[Double], v2: Array[Double]): Double = {
        //println(s"v1 = ${v1.mkString(" ")}")
        //println(s"v2 = ${v2.mkString(" ")}")
        assert (v1.length == v2.length)
        val d = v1.length

        var sum = 0d
        for(i <- 0 until d) {
            for(j <- 0 until d) {
                val dist = math.pow(math.log(v1(i)/v1(j)) - math.log(v2(i)/v2(j)), 2)
                sum += dist
                //println(s"\t$i $j: $dist")
            }
        }
        //println(s"sum = $sum")

        math.sqrt(sum.toDouble/(2.0d * d))
    }

    def mkArray(tokens: Array[String]): Array[Double] = {
        val b = new ArrayBuffer[Double]()
        for(t <- tokens) {
            var v = t.toDouble
            b += v
        }
        val a = norm(b.toArray)

        println(s"tokens = ${tokens.mkString(" ")}")
        println(s"normed = ${a.mkString(" ")}")

        a
    }
}