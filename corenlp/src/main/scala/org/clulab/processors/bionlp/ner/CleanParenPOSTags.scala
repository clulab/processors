package org.clulab.processors.bionlp.ner

import java.io.PrintWriter

import scala.io.Source._

/**
  * Makes sure that POS tags for parens follow the Treebank conventions (they do
  */
object CleanParenPOSTags {
  val WORD_POSITION = 0
  val TAG_POSITION = 1

  def main(args: Array[String]): Unit = {
    val source = fromFile(args(0))
    val pw = new PrintWriter(args(0) + ".cleanparens")
    for(line <- source.getLines()) {
      val tokens = line.split("\\s+")
      if(tokens.length == 0) {
        pw.println()
      } else {
        if(tokens(WORD_POSITION) == "(" || tokens(WORD_POSITION) == "[" || tokens(WORD_POSITION) == "{")
          tokens(TAG_POSITION) = "-LRB-"
        else if(tokens(WORD_POSITION) == ")" || tokens(WORD_POSITION) == ")" || tokens(WORD_POSITION) == "}")
          tokens(TAG_POSITION) = "-RRB-"
        pw.println(tokens.mkString(" "))
      }
    }
    pw.close()
    source.close()
  }
}
