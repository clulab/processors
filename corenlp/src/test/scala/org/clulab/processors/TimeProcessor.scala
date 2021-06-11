package org.clulab.processors

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.utils.FileUtils
import org.clulab.utils.Timer
import org.clulab.utils.Timers

object TimeProcessor {

  def run(args: Array[String]): Unit = {
    val inputDir = FileUtils.getSubprojectDir("./corenlp/src/test/resources/documents")
    val extension = "txt"
    val files = FileUtils.findFiles(inputDir, extension)

    Utils.initializeDyNet()

//    val processor: Processor = new FastNLPProcessorWithSemanticRoles()
    //    val processor: Processor = new FastNLPProcessor()
        val processor: Processor = new CluProcessor()

    processor.annotate("I am happy to join with you today in what will go down in history as the greatest demonstration for freedom in the history of our nation.")
    Timers.clear()
    val timer = new Timer(s"processing ${files.size} files")
    timer.start()

    files.foreach { file =>
      println(s"Processing ${file.getName}...")

      val text = FileUtils.getTextFromFile(file)
      Timers.summarize()
      1.to(10).foreach { index =>
        processor.annotate(text)
        Timers.summarize()
        println
      }
    }

    timer.stop()
    println(timer.toString)
  }

  def main(args: Array[String]): Unit = {
    run(args)
  }
}
