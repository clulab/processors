package org.clulab.processors.clu.syntax

import java.io.{BufferedReader, FileReader, FileWriter, PrintWriter}

import org.maltparser.concurrent.ConcurrentUtils

import scala.collection.mutable.ArrayBuffer

/**
  * Reverts word order in a CoNLL-X treebank from left-to-right to right-to-left
  * This is needed to provide support for right-to-left dependency parsers
  * User: mihais
  * Date: 8/4/17
  */
object ReverseTreebank {
  def main(args:Array[String]) {
    val inputFile = args(0)
    val outputFile = s"$inputFile.righttoleft"

    println(s"Creating reverted treebank in file: $outputFile...")
    val reader = new BufferedReader(new FileReader(inputFile))
    val writer = new PrintWriter(new FileWriter(outputFile))

    var done = false
    while(! done){
      val leftToRight = ConcurrentUtils.readSentence(reader)
      if(leftToRight.isEmpty) {
        done = true
      } else {
        val rightToLeft = revertSentence(leftToRight)
        for(line <- rightToLeft)
          writer.println(line)
        writer.println()
      }
    }

    reader.close()
    writer.close()
    println(s"Reverted treebank saved to: $outputFile.")
  }

  def revertSentence(leftToRight:Array[String]):Array[String] = {
    val leftToRightWoComments = new ArrayBuffer[String]()
    for(s <- leftToRight) {
      if(! s.trim.startsWith("#"))
        leftToRightWoComments += s
    }

    val size = leftToRightWoComments.length
    val raw = leftToRightWoComments.reverse
    val rightToLeft = new ArrayBuffer[String]
    for(i <- raw.indices) {
      val tokens = raw(i).split("\\s+")
      if (tokens.length < 5) {
        throw new RuntimeException(s"ERROR: invalid line #$i: ${raw(i)}!")
      }
      val reverted = new Array[String](tokens.length)
      for (j <- tokens.indices) {
        if (Set(0, 6, 8).contains(j) && isNumber(tokens(j)) && tokens(j) != "0") {
          reverted(j) = (size + 1 - tokens(j).toInt).toString
        } else {
          reverted(j) = tokens(j)
        }
      }
      rightToLeft += reverted.mkString("\t")
    }
    rightToLeft.toArray
  }

  private def isNumber(text:String):Boolean = {
    for(i <- 0 until text.length) {
      if(! Character.isDigit(text.charAt(i)))
        return false
    }
    true
  }
}
