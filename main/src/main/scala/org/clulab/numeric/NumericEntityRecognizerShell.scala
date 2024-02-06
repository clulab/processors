package org.clulab.numeric

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.utils.ReloadableProcessor
import org.clulab.utils.ReloadableShell

import java.io.File

class ReloadableNumericProcessor(ruleDirOpt: Option[String]) extends ReloadableProcessor(() => new BalaurProcessor(), true) {

  override def get: BalaurProcessor = {
    val processor = super.get.asInstanceOf[BalaurProcessor]

    // Other code will run without this check, but no Mentions will be produced,
    // which would lead to confusion.
    assert(processor.numericEntityRecognizerOpt.isDefined)
    processor
  }

  override def reload(): Unit = {
    val balaurProcessor = this.get
    val numericEntityRecognizerOpt = balaurProcessor
        .numericEntityRecognizerOpt
        .map(_.reloaded(new File(ruleDirOpt.get)))
    val numericEntityRecognizerOptOpt = numericEntityRecognizerOpt.map(Option(_))

    processorOpt = Some(balaurProcessor.copy(numericEntityRecognizerOptOpt = numericEntityRecognizerOptOpt))
  }
}

class NumericEntityRecognizerShell(ruleDirOpt: Option[String]) extends ReloadableShell {
  val proc = new ReloadableNumericProcessor(ruleDirOpt)

  /** The actual work, including printing out the output */
  def work(text: String): Unit = {
    val doc = proc.get.annotate(text)
    val mentions = proc.get.numericEntityRecognizerOpt.map(_.extractFrom(doc)).getOrElse(Seq.empty)

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
