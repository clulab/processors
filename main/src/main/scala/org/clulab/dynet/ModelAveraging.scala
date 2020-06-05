package org.clulab.dynet

import java.io.{File, PrintWriter}
import java.text.DecimalFormat

import org.apache.commons.io.FileUtils

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

  //
  // generate the .rnn file
  //
  val out = new PrintWriter(outputModelFileName + ".rnn")
  val lines = new Array[Iterator[String]](individualModelFileNames.length)
  for(i <- individualModelFileNames.indices) {
    lines(i) = io.Source.fromFile(individualModelFileNames(i) + ".rnn").getLines()
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

  //
  // generate the .x2i file
  // all the .x2i files should be the same, so just copy one
  //
  val origX2i = new File(individualModelFileNames(0) + ".x2i")
  val avgX2i = new File(outputModelFileName + ".x2i")
  FileUtils.copyFile(origX2i, avgX2i)
  assert(avgX2i.exists())

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
    val expNoSign = "e[0-9]".r

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

      // Dynet requires the plus sign on positive exponents, e.g., "e+01" rather than "e01"
      val expNoSignMatch = expNoSign.findFirstMatchIn(numFormatted)
      if(expNoSignMatch.nonEmpty) {
        numFormatted =
          numFormatted.substring(0, expNoSignMatch.get.start) +
          "e+" +
          numFormatted.substring(expNoSignMatch.get.start + 1)
      }

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
