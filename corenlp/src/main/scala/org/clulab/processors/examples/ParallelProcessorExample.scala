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
import org.clulab.serialization.DocumentSerializer

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.ForkJoinTasks
import scala.collection.parallel.ParSeq
import scala.io.Source

object ParallelProcessorExample {

  class Timer(val description: String) {
    var elapsedTime: Option[Long] = None
    var startTime: Option[Long] = None

    def time[R](block: => R): R = {
      val t0 = System.currentTimeMillis()
      val result: R = block // call-by-name
      val t1 = System.currentTimeMillis()

      elapsedTime = Some(t1 - t0)
      result
    }

    def start(): Unit = {
      val t0 = System.currentTimeMillis()

      startTime = Some(t0)
    }

    def stop(): Unit = {
      if (startTime.isDefined) {
        val t1 = System.currentTimeMillis()

        elapsedTime = Some(t1 - startTime.get)
      }
    }

    override def toString: String = {
      if (elapsedTime.isDefined)
        s"\tTime\t$description\t${diffToString(elapsedTime.get)}"
      else if (startTime.isDefined)
        s"\tStart\t$description\t${startTime.get}\tms"
      else
        s"\tTimer\t$description"
    }

    def diffToString(diff: Long): String = {
      val days = (diff / (1000 * 60 * 60 * 24)) / 1
      val hours = (diff % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60)
      val mins = (diff % (1000 * 60 * 60)) / (1000 * 60)
      val secs = (diff % (1000 * 60)) / 1000
      val msecs = (diff % (1000 * 1)) / 1

      f"$days:$hours%02d:$mins%02d:$secs%02d.$msecs%03d"
    }
  }

  def mainWithCallback(args: Array[String])(callback: (File, String) => Unit): Unit = {
    val utf8: String = StandardCharsets.UTF_8.toString

    def findFiles(collectionDir: String, extension: String): Seq[File] = {
      val dir = new File(collectionDir)
      val filter = new FilenameFilter {
        def accept(dir: File, name: String): Boolean = name.endsWith(extension)
      }

      val result = Option(dir.listFiles(filter))
          .getOrElse(throw new FileNotFoundException(collectionDir))
      result
    }

    def parallelize[T](seq: Seq[T], threads: Int): ParSeq[T] = {
      val forkJoinPoolConstructor = {
        // Get something of the right type.
        val defaultForkJoinPool = ForkJoinTasks.defaultForkJoinPool
        // Find the constructor.
        defaultForkJoinPool.getClass.getConstructor(classOf[Int])
      }

      def newForkJoinPool(threads: Int) = {
        // Invoke the constructor.
        forkJoinPoolConstructor.newInstance(threads.asInstanceOf[Integer])

        // For the record, this is the standard version
        //new ForkJoinPool(threads)
      }

      val forkJoinPool = newForkJoinPool(threads)
      val forkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)
      val parSeq = seq.par

      parSeq.tasksupport = forkJoinTaskSupport
      parSeq
    }

    def printDocument(document: Document, printWriter: PrintWriter) {
      document.prettyPrint(printWriter)
    }

    val inputDir = args(0)
    val outputDir = args(1)
    val extension = args(2)
    val threads = args(3).toInt

    val files = findFiles(inputDir, extension)
    // Parallelizing by file results in a quick crash.
    val parFiles = parallelize(files, threads)

    Utils.initializeDyNet()

    val processor: Processor = new CluProcessor()
    val documentSerializer = new DocumentSerializer

    val untimed = processor.annotate("Once upon a time there were three bears.")

    val timer = new Timer(s"$threads threads processing ${parFiles.size} files")
    timer.start()

    parFiles.foreach { file =>
      println(s"Processing ${file.getName}...")

      val text = {
        val source = Source.fromFile(file, utf8)
        val text = source.mkString

        source.close
        text
      }

      val outputFile = new File(outputDir + "/" + file.getName)
      val document = processor.annotate(text)
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

  def main(args: Array[String]): Unit = {

    mainWithCallback(args) { case (file: File, contents: String) =>
      // Print these to a file for analysis.
      val printWriter = new PrintWriter(new BufferedOutputStream(new FileOutputStream(file)))

      printWriter.println(contents)
      printWriter.close
    }
  }
}
