package org.clulab.dynet

import java.io.PrintWriter

/** Converts the standard CoNLLU syntactic dependency format to Metal */
object CoNLLUToMetal {
  def main(args: Array[String]): Unit = {
    val in = io.Source.fromFile(args(0))
    val headsPw = new PrintWriter(args(1) + ".heads")
    val labelsPw = new PrintWriter(args(1) + ".labels")

    for(line <- in.getLines()) {
      if (line.trim.isEmpty) {
        headsPw.println()
        labelsPw.println()
      } else {
        val tokens = line.split("\\s+")
        assert(tokens.length == 10)

        val offset = tokens(0).toInt - 1 // our positions start at 0
        val word = tokens(1)
        val posTag = tokens(4)
        val absHeadPosition = tokens(6).toInt - 1 // our positions start at 0
        val relativeHeadDist =
          if(absHeadPosition == -1) 0 // we encode root position as 0
          else absHeadPosition - offset
        val depLabel = tokens(7)

        headsPw.println(s"$word\t$posTag\t_\t$relativeHeadDist")
        labelsPw.println(s"$word\t$posTag\t_\t$depLabel\t$absHeadPosition")
      }
    }

    in.close()
    headsPw.close()
    labelsPw.close()
  }
}
