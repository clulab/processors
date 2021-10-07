package org.clulab.numeric

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.ReloadableProcessor
import org.clulab.utils.ReloadableShell

class NumericEntityRecognizerShell extends ReloadableShell {
  Utils.initializeDyNet()

  val proc = new ReloadableProcessor(() => new CluProcessor(), true)
  val ner = new NumericEntityRecognizer()

  /** The actual work, including printing out the output */
  def work(text: String): Unit = {
    val doc = proc.get.annotate(text)
    val mentions = ner.extractFrom(doc)

    setLabelsAndNorms(doc, mentions)
    displayMentions(mentions, doc)
  }

  def reload(): Unit = {
    println("The processor is reloading...")
    proc.reload()
  }
}

object NumericEntityRecognizerShell extends App {
  new NumericEntityRecognizerShell().shell()
}
