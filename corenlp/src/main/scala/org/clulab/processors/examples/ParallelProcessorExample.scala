package org.clulab.processors.examples

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.FilenameFilter
import java.io.PrintWriter
import java.io.StringWriter
import java.nio.charset.StandardCharsets
import org.clulab.dynet.Utils
import org.clulab.processors.Document
import org.clulab.processors.Processor
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.ThreadUtils
import org.clulab.utils.Timer
import org.clulab.utils.Timers

import scala.io.Source

object ParallelProcessorExample {

  def mainWithCallback(args: Array[String])(callback: (File, String) => Unit): Unit = {
    val utf8: String = StandardCharsets.UTF_8.toString

    def findFiles(collectionDir: String, extension: String): Seq[File] = {
      val dir = new File(collectionDir)
      val filter = new FilenameFilter {
        def accept(dir: File, name: String): Boolean = name.endsWith(extension)
      }

      val result = Option(dir.listFiles(filter))
          .getOrElse( {
            val here = new File(".").getAbsolutePath
            println(s"Can't find $collectionDir from $here.")
            throw new FileNotFoundException(collectionDir)
          })
      result
    }

    def printDocument(document: Document, printWriter: PrintWriter): Unit = document.prettyPrint(printWriter)

    val inputDir = args(0)
    val outputDir = args(1)
    val extension = args(2)
    val threads = args(3).toInt

    val files = findFiles(inputDir, extension)
    val parFiles = ThreadUtils.parallelize(files, threads)

    Utils.initializeDyNet()

//    val processor: Processor = new FastNLPProcessorWithSemanticRoles()
    val processor: Processor = new FastNLPProcessor()
//    val processor: Processor = new CluProcessor()

    val documentSerializer = new DocumentSerializer

    val untimed = processor.annotate("I am happy to join with you today in what will go down in history as the greatest demonstration for freedom in the history of our nation.")
    Timers.clear()
    val timer = new Timer(s"$threads threads processing ${parFiles.size} files")
    timer.start()
    
    files.foreach { file =>
      println(s"Processing ${file.getName}...")

      val text = {
        val source = Source.fromFile(file, utf8)
        val text = source.mkString

        source.close
        text
      }

      val outputFile = new File(outputDir + "/" + file.getName)
      val document = processor.annotate(text)
Timers.summarize()
      1.to(10).foreach { index =>
        processor.annotate(text)
        Timers.summarize()
      }

      val printedDocument = {
        val stringWriter = new StringWriter
        val printWriter = new PrintWriter(stringWriter)

        printDocument(document, printWriter)
        printWriter.close()

        val result = stringWriter.toString
        result
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
      // Print these to a file for analysis.
      val printWriter = new PrintWriter(new BufferedOutputStream(new FileOutputStream(file)))

      printWriter.println(contents)
      printWriter.close
    }
  }

  def main(args: Array[String]): Unit = {
    import org.clulab.fatdynet.utils.Utils

    Utils.startup()
    run(args)
    Utils.shutdown()
  }
}
