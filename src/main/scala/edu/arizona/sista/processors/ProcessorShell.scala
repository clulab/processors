package edu.arizona.sista.processors

import scala.collection.immutable.ListMap
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import java.io.File
import jline.console.ConsoleReader
import jline.console.history.FileHistory

/**
 * A simple interactive shell
 * User: mihais
 * Date: 3/13/14
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
    ":bio" -> "use BioNLPProcessor",
    ":fast" -> "use FastNLPProcessor",
    ":exit" -> "exit system"
  )

  // create the processor
  val core: Processor = new CoreNLPProcessor(withDiscourse = false) // this uses the slow but better discourse parser
  val fast: Processor = new FastNLPProcessor(useMalt = false) // this uses the fast but slightly worse discourse parser
  val bio: Processor = new BioNLPProcessor(withDiscourse = false, removeFigTabReferences = true)

  var proc = core
  reader.setPrompt("(core)>>> ")
  println("Loading CoreNLPProcessor...\n")
  proc.annotate("blah")

  println("\nWelcome to the ProcessorShell!")
  printCommands()

  var running = true

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

      case ":bio" =>
        reader.setPrompt("(bio)>>> ")
        println("Preparing BioNLPProcessor...\n")
        proc = bio
        proc.annotate("initialize me!")

      case ":exit" | null =>
        running = false

      case text =>
        val doc = proc.annotate(text)
        ProcessorExample.printDoc(doc)
    }
  }

  // manual terminal cleanup
  reader.getTerminal.restore()
  reader.shutdown()


  // summarize available commands
  def printCommands(): Unit = {
    println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println()
  }
}
