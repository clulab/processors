package org.clulab.sequences

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Shell

class LexiconNERShell(val lexiconNer: LexiconNER) extends Shell {
  val proc: CluProcessor = new CluProcessor()

  override def work(text: String): Unit = {
    val doc = proc.mkDocument(text)
    for (sent <- doc.sentences) {
      val labels = lexiconNer.find(sent)
      println(labels.mkString(", "))
    }
  }
}

object LexiconNERShell extends App {
  Utils.initializeDyNet()
  val kb = args(0) // pass the KB file name as the first argument
  val lexiconNer = LexiconNER(Seq(kb))
  val shell = new LexiconNERShell(lexiconNer)
  shell.shell()
}
