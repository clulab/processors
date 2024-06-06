package org.clulab.processors.hexatagging

import org.clulab.sequences.Row

object DepsToTagsApp {
  def main(args: Array[String]): Unit = {
    var sent = Array(
      Row(Array("this", "", "", "nsubj", "3")),
      Row(Array("is", "", "", "cop", "3")),
      Row(Array("a", "", "", "det", "3")),
      Row(Array("test", "", "", "root", "-1"))
    )
    generateAndPrint(sent)

    sent = Array(
      Row(Array("she", "", "", "nsubj", "1")),
      Row(Array("reads", "", "", "root", "-1")),
      Row(Array("fascinating", "", "", "amod", "4")),
      Row(Array("great", "", "", "amod", "4")),
      Row(Array("papers", "", "", "dobj", "1")),
    )
    generateAndPrint(sent)
  }

  def generateAndPrint(sent:Array[Row]): Unit = {
    val (termTags, nonTermTags) = DepsToTags.generateHexaTags(sent)
    println(s"term tags: ${termTags.mkString(", ")}")
    println(s"nonterm tags: ${nonTermTags.mkString(", ")}")
  }
}
