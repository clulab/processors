package org.clulab.processors.clu.syntax

import java.io.{BufferedReader, FileReader, FileWriter, PrintWriter}

import org.maltparser.concurrent.ConcurrentUtils

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
    val size = leftToRight.length
    val raw = leftToRight.reverse
    val rightToLeft = new Array[String](size)
    for(i <- raw.indices) {
      val tokens = raw(i).split("\\s+")
      assert(tokens.length >= 5)
      val reverted = new Array[String](tokens.length)
      for(j <- tokens.indices) {
        if(Set(0, 6, 8).contains(j) && tokens(j) != "_" && tokens(j) != "0") {
          reverted(j) = (size + 1 - tokens(j).toInt).toString
        } else {
          reverted(j) = tokens(j)
        }
      }
      rightToLeft(i) = reverted.mkString("\t")
    }
    rightToLeft
  }
}
