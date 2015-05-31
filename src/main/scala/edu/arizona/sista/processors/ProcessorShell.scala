package edu.arizona.sista.processors

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
object ProcessorShell {

  val history = new FileHistory(new File(System.getProperty("user.home"), ".processorshellhistory"))
  sys addShutdownHook {
    history.flush() // flush file before exiting
  }

  val reader = new ConsoleReader
  reader.setPrompt(">>> ")
  reader.setHistory(history)

  val commands = Map(
    ":help" -> "show commands",
    ":exit" -> "exit system"
  )

  def main(args:Array[String]) {
    // create the processor
    //val proc:Processor = new CoreNLPProcessor(withDiscourse = false) // this uses the slow but better discourse parser
    //val proc:Processor = new FastNLPProcessor(useMalt = false) // this uses the fast but slightly worse discourse parser
    val proc:Processor = new BioNLPProcessor(withDiscourse = false, removeFigTabReferences = true)


    println("Loading processor...\n")
    proc.annotate("blah")
    
    println("\nWelcome to the ProcessorShell!")
    printCommands()

    var running = true

    while (running) {
      reader.readLine match {
        case ":help" =>
          printCommands()

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

  }

  // summarize available commands
  def printCommands(): Unit = {
    println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println()
  }
}
