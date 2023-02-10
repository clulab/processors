package org.clulab.processors.clu

object CaseRestorer extends App {
  val proc = new CluProcessor()
  //val doc = proc.mkDocument("mr. scruggs -- who is arguing the case with his son zach")
  val doc = proc.mkDocumentFromTokens("<YOUR ARRAY OF TOKENS>")
  proc.restoreCase(doc)
  for(sent <- doc.sentences) {
    println(s"${sent.words.mkString(" ")}")
  }
}
