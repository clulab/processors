package org.clulab.processors.apps

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.utils.{Sourcer, Timers}

import scala.util.Using

object TokenClassifierTimerApp extends App {
  val fileName = args.lift(0).getOrElse("../corpora/sentences/sentences.txt")

  val processor = {
    val processor = new BalaurProcessor()
    processor.annotate("This is just to prime the pump.")
    processor
  }
  val lines = {
    Using.resource(Sourcer.sourceFromFilename(fileName)) { source =>
      val lines = source.getLines().take(100).toArray
      lines
    }
  }
  val elapsedTimer = Timers.getOrNew("Elapsed")

  elapsedTimer.time {
    lines.zipWithIndex.foreach { case (line, index) =>
      println(s"$index $line")
      if (index != 1382) {
        val words = line.split(" ")
        val document = processor.mkDocumentFromTokens(Array(words.toIndexedSeq))

        processor.annotate(document)
      }
    }
  }
  Timers.summarize()
}
