package org.clulab.dynet

import java.io.PrintWriter

/**
 * Converts Robert's CoNLLY format (for syntactic dependencies, from his LREC 2020 paper) to Metal
 */
object CoNLLYToMetal {
  def main(args: Array[String]): Unit = {
    val in = io.Source.fromFile(args(0))
    val headsPw = new PrintWriter(args(1) + ".heads")
    val labelsPw = new PrintWriter(args(1) + ".labels")

    var position = 0
    for(line <- in.getLines()) {
      if(line.trim.isEmpty) {
        headsPw.println()
        labelsPw.println()
        position = 0
      } else {
        val tokens = line.split("\\s+")
        assert(tokens.length == 4)

        val word = tokens(0)
        val relativeHeadDist = tokens(1).toInt
        val depLabel = tokens(2)
        val posTag = tokens(3)

        headsPw.println(s"$word\t$posTag\t_\t$relativeHeadDist")

        val headPosition = {
          if(relativeHeadDist == 0) {
            -1
          } else {
            position + relativeHeadDist
          }
        }

        labelsPw.println(s"$word\t$posTag\t_\t$depLabel\t$headPosition")

        position += 1
      }
    }

    in.close()
    headsPw.close()
    labelsPw.close()
  }
}
