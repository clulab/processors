package org.clulab.numeric

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.ReloadableShell

class ReloadableNer(protected var ner: NumericEntityRecognizer) {

  def get: NumericEntityRecognizer = ner

  def reload(path: String): Unit = ner = ner.reloaded(path)
}

class NumericEntityRecognizerShell(pathOpt: Option[String]) extends ReloadableShell {
  Utils.initializeDyNet()

  val proc = new CluProcessor()
  var ner = new ReloadableNer(NumericEntityRecognizer())

  /** The actual work, including printing out the output */
  def work(text: String): Unit = {
    val doc = proc.annotate(text)
    val mentions = ner.get.extractFrom(doc)

    setLabelsAndNorms(doc, mentions)
    displayMentions(mentions, doc)
  }

  def reload(): Unit = {
    if (pathOpt.isDefined) {
      println("The numeric entity recognizer is reloading...")
      try {
        ner.reload(pathOpt.get)
      }
      catch {
        case throwable: Throwable =>
          println(s"Rules could not be reloaded from ${pathOpt.get}!")
          throwable.printStackTrace
      }
    }
    else
      println("No file was specified, possibly on the command line, from which to read rules!")
  }
}

object NumericEntityRecognizerShell extends App {
  // args(0) can optionally be a path from which to reload rules.
  new NumericEntityRecognizerShell(args.lift(0)).shell()
}
