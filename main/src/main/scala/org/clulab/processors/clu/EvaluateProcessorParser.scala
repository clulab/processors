package org.clulab.processors.clu

import org.clulab.sequences.ColumnReader

/**
  * Evaluates UAS and LAS for the parser included in a processor
  */
object EvaluateProcessorParser extends App {
  val TEST_FILE_NAME = "dynet/en/deps/universal/wsj/test.labels"
  val sents = ColumnReader.readColumns(TEST_FILE_NAME)
  val proc = new BalaurProcessor()

  var errorCount = 0
  for(sent <- sents) {
    if(sent.length > 1) {
      val words = sent.map(_.get(0)).toArray
      // println("Words: " + words.mkString(" "))
      val doc = proc.mkDocumentFromTokens(Seq(words))
      try {
        proc.annotate(doc)
      } catch {
        case e: Throwable => errorCount += 1
      }
    }
  }
  println(s"Found $errorCount problematic sentences out of ${sents.size}.")
}
