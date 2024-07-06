package org.clulab.processors

import org.clulab.sequences.ColumnReader
import org.clulab.struct.DirectedGraph
import org.clulab.processors.clu.BalaurProcessor

/**
  * Evaluates UAS and LAS for the parser included in a processor
  */
object EvaluateProcessorParser extends App {
  val TEST_FILE_NAME = "data/en/deps/universal/wsj/test.labels"
  val sents = ColumnReader.readColumns(TEST_FILE_NAME)
  val proc = new BalaurProcessor()

  var errorCount = 0
  var wordCount = 0
  var correctHead = 0
  var correctLabel = 0
  for(sent <- sents) {
    if(sent.length > 1) {
      val words = sent.map(_.get(0)).toArray
      // println("Words: " + words.mkString(" "))
      val doc = proc.mkDocumentFromTokens(Seq(words))
      try {
        proc.annotate(doc)
        val deps = doc.sentences(0).universalBasicDependencies.get
        val sentLen = doc.sentences(0).size
        val (heads, labels) = convertDeps(deps, sentLen)

        // UAS and LAS computation
        wordCount += sentLen
        for(i <- heads.indices) {
          if(heads(i) == sent(i).get(4).toInt) {
            correctHead += 1
            if(labels(i) == sent(i).get(3)) {
              correctLabel += 1
            }
          }
        }

      } catch {
        case e: Throwable => errorCount += 1
      }
    }
  }
  println(s"Found $errorCount problematic sentences out of ${sents.size}.")
  println(s"UAS: $correctHead/$wordCount = ${correctHead.toFloat/wordCount}")
  println(s"LAS: $correctLabel/$wordCount = ${correctLabel.toFloat/wordCount}")

  /** Creates the array of heads and labels for all tokens in a sentence */
  private def convertDeps(deps: DirectedGraph[String], length: Int): (Array[Int], Array[String]) = {
    val heads = new Array[Int](length)
    val labels = new Array[String](length)

    for(root <- deps.roots) {
      heads(root) = -1
      labels(root) = "root"
    }

    for(edge <- deps.edges) {
      heads(edge.destination) = edge.source
      labels(edge.destination) = edge.relation
    }

    (heads, labels)
  }
}
