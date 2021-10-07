package org.clulab.sequences

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.ReloadableProcessor
import org.clulab.utils.ReloadableShell

class LexiconNERShell(val lexiconNer: LexiconNER) extends ReloadableShell {
  val proc = new ReloadableProcessor(() => new CluProcessor(), true)

  override def work(text: String): Unit = {
    val doc = proc.get.mkDocument(text)
    for (sent <- doc.sentences) {
      val labels = lexiconNer.find(sent)
      println(labels.mkString(", "))
    }
  }

  override def reload(): Unit = {
    println("The processor is reloading...")
    proc.reload()
  }
}

object LexiconNERShell extends App {
  Utils.initializeDyNet()
  val kb = args(0) // pass the KB file name as the first argument
  val lexiconNer = LexiconNER(Seq(kb))
  val shell = new LexiconNERShell(lexiconNer)
  shell.shell()
}
