package org.clulab.processors

import scala.collection.immutable.ListMap
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import java.io.{File, PrintWriter}

import jline.console.ConsoleReader
import jline.console.history.FileHistory
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor

/**
  * A simple interactive shell
  * User: mihais
  * Date: 3/13/14
  * Last Modified: Fix compiler warning: remove redundant match case clause.
 */
object ProcessorShell extends App {

  val history = new FileHistory(new File(System.getProperty("user.home"), ".processorshellhistory"))
  sys addShutdownHook {
    history.flush() // flush file before exiting
  }

  val reader = new ConsoleReader
  reader.setHistory(history)

  val commands = ListMap(
    ":help" -> "show commands",
    ":core" -> "use CoreNLPProcessor",
    ":fast" -> "use FastNLPProcessor",
    ":clu" -> "use CluProcessor",
    ":exit" -> "exit system"
  )

  // create the processor
  lazy val core: Processor = new CoreNLPProcessor() // this uses the slower constituent parser
  lazy val fast: Processor = new FastNLPProcessor() // this uses the faster dependency parser
  lazy val clu: Processor = new CluProcessor()

  var proc = core
  reader.setPrompt("(core)>>> ")
  println("\nWelcome to the ProcessorShell!")
  printCommands()

  var running = true
  val printWriter = new PrintWriter(System.out)

  while (running) {
    reader.readLine match {
      case ":help" =>
        printCommands()

      case ":core" =>
        reader.setPrompt("(core)>>> ")
        println("Preparing CoreNLPProcessor...\n")
        proc = core
        proc.annotate("initialize me!")

      case ":fast" =>
        reader.setPrompt("(fast)>>> ")
        println("Preparing FastNLPProcessor...\n")
        proc = fast
        proc.annotate("initialize me!")

      case ":clu" =>
        reader.setPrompt("(clu)>>> ")
        println("Preparing CluProcessor...\n")
        Utils.initializeDyNet()
        proc = clu
        proc.annotate("initialize me!")

      case ":exit" | null =>
        running = false

      case text =>
        val doc = proc.annotate(text)
        doc.prettyPrint(printWriter)
        printWriter.flush()
    }
  }

  // manual terminal cleanup
  reader.getTerminal.restore()
  reader.shutdown()


  /** summarize available commands */
  def printCommands(): Unit = {
    println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println()
  }
}
