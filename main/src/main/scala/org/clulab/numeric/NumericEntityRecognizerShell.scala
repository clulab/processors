package org.clulab.numeric

import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Shell

class NumericEntityRecognizerShell extends Shell {
  val proc = new CluProcessor()
  val ner = new NumericEntityRecognizer()

  /** The actual work, including printing out the output */
  override def work(text: String): Unit = {
    val doc = proc.annotate(text)
    ner.extractFrom(doc)
  }
}

object NumericEntityRecognizerShell extends App {
  val sh = new NumericEntityRecognizerShell()
  sh.shell()
}
