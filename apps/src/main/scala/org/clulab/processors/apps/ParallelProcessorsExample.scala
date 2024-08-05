package org.clulab.processors.apps

import org.clulab.processors.Document
import org.clulab.processors.Processor
import org.clulab.processors.clu.BalaurProcessor
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.{FileUtils, StringUtils, ThreadUtils, Timer}

import java.io.File
import java.io.PrintWriter
import scala.util.Using

object ParallelProcessorsExample {

  def mainWithCallback(args: Array[String])(callback: (File, String) => Unit): Unit = {

    def printDocument(document: Document, printWriter: PrintWriter): Unit = document.prettyPrint(printWriter)

    val inputDir = args(0)
    val outputDir = args(1)
    val extension = args(2)
    val threads = args(3).toInt
    val parallel = args.lift(4).contains("true")

    val files = FileUtils.findFiles(inputDir, extension)
    val serFiles = files.sortBy(-_.length)
    val parFiles = ThreadUtils.parallelize(serFiles, threads)
    val documentSerializer = new DocumentSerializer

    val startupTimer = new Timer("This is how long it takes to start up")
    startupTimer.start()

    val processor: Processor = new BalaurProcessor()
    processor.annotate("I am happy to join with you today in what will go down in history as the greatest demonstration for freedom in the history of our nation.")
    startupTimer.stop()
    println(startupTimer.toString)

    val label =
      if (parallel) s"$threads threads processing ${parFiles.size} files in parallel"
      else s"1 threads processing ${parFiles.size} files in serial"
    val timer = new Timer(label)
    timer.start()

    (if (parallel) parFiles else serFiles).iterator.foreach { file =>
      println(s"Processing ${file.getName}...")

      val text = FileUtils.getTextFromFile(file)
      val outputFile = new File(outputDir + "/" + file.getName)
      val document = try {
        val document = processor.annotate(text)
        document
      }
      catch {
        case throwable: Throwable =>
          println(s"Threw exception for ${file.getName}")
          throw throwable
      }
      val printedDocument = StringUtils.viaPrintWriter { printWriter =>
        printDocument(document, printWriter)
      }
      val savedDocument = documentSerializer.save(document)
      val outputDocument = printedDocument + savedDocument

      callback(outputFile, outputDocument)
    }

    timer.stop()
    println(timer.toString)
  }

  def run(args: Array[String]): Unit = {

    mainWithCallback(args) { case (file: File, contents: String) =>
      Using.resource(new PrintWriter(file)) { printWriter =>
        printWriter.println(contents)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    run(Array(
      FileUtils.getSubprojectDir("./corenlp/src/test/resources/documents"),
      ".",
      "txt",
      "8",
      "false"
    ))
  }
}
