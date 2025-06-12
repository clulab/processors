package org.clulab.processors.apps

import org.clulab.numeric.NumericUtils
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

    processorOpt = Some(balaurProcessor.copy(numericEntityRecognizerOpt = numericEntityRecognizerOpt))
  }
}

class NumericEntityRecognizerShell(ruleDirOpt: Option[String]) extends ReloadableShell {
  val proc = new ReloadableNumericProcessor(ruleDirOpt)

  /** The actual work, including printing out the output */
  def work(text: String): Unit = {
    val doc = proc.get.annotate(text)
    // This gets the same numericEntityRecognizer already used in the annotation
    // so that the mentions, since thrown away, can be recalculated.
    val mentions = proc.get.numericEntityRecognizerOpt.map(_.extractFrom(doc)).getOrElse(Seq.empty)

    // The doc should already have been annotated two lines above.
    NumericUtils.displayMentions(mentions, doc)
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
