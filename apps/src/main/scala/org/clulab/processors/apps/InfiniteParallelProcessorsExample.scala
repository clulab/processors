package org.clulab.processors.apps

import org.clulab.processors.Document
import org.clulab.processors.Processor
import org.clulab.processors.clu.{BalaurProcessor, DocumentPrettyPrinter}
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.{FileUtils, StringUtils, ThreadUtils, Timer}

import java.io.File
import java.io.PrintWriter
import scala.collection.compat._
import scala.collection.parallel.ParSeq
import scala.util.Using

object InfiniteParallelProcessorsExample {

  class ProcessorProvider(reuseProcessor: Boolean) {
    protected val processorOpt: Option[Processor] =
        Option.when(reuseProcessor)(new BalaurProcessor())

    def newOrReusedProcessor: Processor =
        if (reuseProcessor) processorOpt.get
        else new BalaurProcessor()
  }

  def mainWithCallback(args: Array[String])(callback: (File, String) => Unit): Unit = {
    val inputDir = args(0)
    val outputDir = args(1)
    val extension = args(2)
    val threads = args(3).toInt
    val reuseProcessor = args.lift(4).map(_ == "true").getOrElse(false)

    val files = FileUtils.findFiles(inputDir, extension)
    val parFiles = ThreadUtils.parallelize(files, threads)
    val documentSerializer = new DocumentSerializer

    def processFiles(parFiles: ParSeq[File], processor: Processor): Unit = {
      parFiles.foreach { file =>
        println(s"Processing ${file.getName}...")

        val text = FileUtils.getTextFromFile(file)
        val outputFile = new File(outputDir + "/" + file.getName)
        val document = processor.annotate(text)
        val printedDocument = StringUtils.viaPrintWriter { printWriter =>
          new DocumentPrettyPrinter(printWriter).print(document)
        }
        val savedDocument = documentSerializer.save(document)
        val outputDocument = printedDocument + savedDocument

        callback(outputFile, outputDocument)
      }
    }

    val processorProvider = new ProcessorProvider(reuseProcessor)
    val untimed = processorProvider.newOrReusedProcessor.annotate("I am happy to join with you today in what will go down in history as the greatest demonstration for freedom in the history of our nation.")

    val timer = new Timer(s"$threads threads processing ${parFiles.size} files")
    timer.start()

    var done = false

    // In the debugger you can change done to true in order to stop looping and check memory.
    while (!done) {
      processFiles(parFiles, processorProvider.newOrReusedProcessor)
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
      "2",
      "false"
    ))
  }
}
