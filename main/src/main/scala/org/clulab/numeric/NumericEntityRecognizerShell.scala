package org.clulab.numeric

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.ReloadableProcessor
import org.clulab.utils.ReloadableShell

import java.io.File

class ReloadableNumericProcessor(ruleDirOpt: Option[String]) extends ReloadableProcessor(() => new CluProcessor(), true) {

  override def get: CluProcessor = super.get.asInstanceOf[CluProcessor]

  override def reload(): Unit = {
    val cluProcessor = this.get
    val numericEntityRecognizer = cluProcessor
        .numericEntityRecognizer
        .reloaded(new File(ruleDirOpt.get))

    processorOpt = Some(cluProcessor.copy(numericEntityRecognizerOptOpt = Some(Some(numericEntityRecognizer))))
  }
}

class NumericEntityRecognizerShell(ruleDirOpt: Option[String]) extends ReloadableShell {
  Utils.initializeDyNet()

  val proc = new ReloadableNumericProcessor(ruleDirOpt)

  /** The actual work, including printing out the output */
  def work(text: String): Unit = {
    val doc = proc.get.annotate(text)
    val mentions = proc.get.numericEntityRecognizer.extractFrom(doc)

    setLabelsAndNorms(doc, mentions)
    displayMentions(mentions, doc)
  }

  def reload(): Unit = {
    if (ruleDirOpt.isDefined) {
      println("The numeric entity recognizer is reloading...")
      try {
        proc.reload()
      }
      catch {
        case throwable: Throwable =>
          println(s"Rules could not be reloaded from ${ruleDirOpt.get}!")
          throwable.printStackTrace
      }
    }
    else
      println("No directory was specified, possibly on the command line, from which to reload rules!")
  }
}

object NumericEntityRecognizerShell extends App {
  // args(0) can optionally be a directory from which to reload rules.
  new NumericEntityRecognizerShell(args.lift(0)).shell()
}
