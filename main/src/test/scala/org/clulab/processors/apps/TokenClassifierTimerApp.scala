package org.clulab.processors.apps

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.{Sourcer, Timers}


object TokenClassifierTimerApp extends App {
  val fileName = args.lift(0).getOrElse("../sentences.txt")

  Utils.initializeDyNet()
  val processor = {
    val processor = new CluProcessor()
    processor.annotate("This is just to prime the pump.")
    processor
  }
  val lines = {
    val source = Sourcer.sourceFromFilename(fileName)
    val lines = source.getLines.take(100).toArray

    source.close
    lines
  }
  val elapsedTimer = Timers.getOrNew("Elapsed")

  elapsedTimer.time {
    lines.zipWithIndex.foreach { case (line, index) =>
      println(s"$index $line")
      if (index != 1382) {
        val words = line.split(" ")
        val document = processor.mkDocumentFromTokens(Array(words.toIterable).toIterable)

        processor.annotate(document)
      }
    }
  }
  Timers.summarize
}
