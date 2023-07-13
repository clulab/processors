package org.clulab.processors.apps

import org.clulab.processors.clu.CluProcessor
import org.clulab.scala.Using._
import org.clulab.utils.FileUtils

object ExtractSentencesApp extends App {
  val directoryName = args.lift(0).getOrElse("../corpora/Doc16k/txt")
  val fileName = args.lift(1).getOrElse("sentences.txt")

  val files = FileUtils.findFiles(directoryName, ".txt")
  val processor = new CluProcessor()
  var count = 0

  Using.resource(FileUtils.printWriterFromFile(fileName)) { printWriter =>
    files.foreach { file =>
      val text = FileUtils.getTextFromFile(file)
      val document = processor.mkDocument(text, keepText = true)

      document.sentences.foreach { sentence =>
        val words = sentence.words.mkString(" ")

        printWriter.println(words)
        count += 1
        if (count >= 10000)
          throw new RuntimeException("There are too many sentences!")
      }
    }
  }
}
