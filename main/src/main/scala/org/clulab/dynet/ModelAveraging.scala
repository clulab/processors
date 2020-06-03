package org.clulab.dynet

import java.io.PrintWriter
import java.text.DecimalFormat

import scala.collection.mutable.ArrayBuffer

/**
 * Averages the parameter weights from multiple DyNet model files
 */
object ModelAveraging extends App {
  if(args.length < 2) {
    throw new RuntimeException("ERROR: need at least two models to average!")
  }

  val individualModelFileNames = args.slice(0, args.length - 1)
  val outputModelFileName = args.last

  val out = new PrintWriter(outputModelFileName)
  val lines = new Array[Iterator[String]](individualModelFileNames.length)
  for(i <- individualModelFileNames.indices) {
    lines(i) = io.Source.fromFile(individualModelFileNames(i)).getLines()
  }

  while(lines(0).hasNext) {
    val crtLines = new Array[String](lines.length)
    for(i <- lines.indices) {
      crtLines(i) = lines(i).next()
    }

    if(crtLines(0).startsWith("#Parameter#") ||
      crtLines(0).startsWith("#LookupParameter#") ||
      crtLines(0).trim.isEmpty) {
      out.println(crtLines(0))
    } else {
      out.println(avg(crtLines))
    }
  }

  out.close()

  def avg(lines: Array[String]): String = {
    // convert to vectors
    val vectors = new Array[Array[Double]](lines.length)
    for(i <- lines.indices) {
      vectors(i) = toDoubleArray(lines(i))
      if(i > 0) assert(vectors(i).length == vectors(0).length)
    }

    // compute the avg vector
    val avgVector = new Array[Float](vectors(0).length)
    for(i <- avgVector.indices) {
      var sum = 0d
      for(j <- vectors.indices) {
        sum += vectors(j)(i)
      }
      sum /= vectors.length.toDouble
      avgVector(i) = sum.toFloat
    }

    // convert avg vector to string
    val b = new StringBuilder
    val numFormat = new DecimalFormat("0.00000000E00")
    for(i <- avgVector.indices) {
      //if(i > 0) b.append(" ")
      val num = avgVector(i)
      var numFormatted = numFormat.format(avgVector(i))

      // DyNet uses "e" for exponent
      numFormatted = numFormatted.replace('E', 'e')

      // DyNet explicitly includes the "+" sign
      if(! numFormatted.startsWith("+") && ! numFormatted.startsWith("-")) {
        numFormatted = "+" + numFormatted
      }

      // Dynet uses "e+00" for no exponent
      numFormatted = numFormatted.replace("e00", "e+00")

      b.append(numFormatted)
      b.append(" ")
    }

    b.toString()
  }

  def toDoubleArray(line: String): Array[Double] = {
    val tokens = line.split("\\s+")
    val v = new ArrayBuffer[Double]
    for(token <- tokens) {
      v += token.toDouble
    }
    v.toArray
  }
}
