package org.clulab.utils

import java.io.{File, PrintWriter}

import jline.console.ConsoleReader
import jline.console.history.FileHistory

import scala.collection.immutable.ListMap

/**
 * An interactive shell to be used to test various NLP components
 */
abstract class Shell {
  /** Initialize the NLP component needed for the work method */
  def initialize(): Unit
  /** The actual work, including printing out the output */
  def work(text:String):Unit

  protected val printWriter = new PrintWriter(System.out)

  val commands = ListMap(
    ":help" -> "show commands",
    ":exit" -> "exit system"
  )

  def shell() {
    val history = new FileHistory(new File(System.getProperty("user.home"), ".shellhistory"))
    sys addShutdownHook {
      history.flush() // flush file before exiting
    }

    val reader = new ConsoleReader
    reader.setHistory(history)

    initialize()

    reader.setPrompt("(shell)>>> ")
    printWriter.println("\nWelcome to the shell!")
    printCommands()
    printWriter.flush()

    var running = true
    while (running) {
      reader.readLine match {
        case ":help" =>
          printCommands()
          printWriter.flush()

        case ":exit" | null =>
          running = false

        case text =>
          if(text.trim.nonEmpty) {
            try {
              work(text)
              printWriter.flush()
            } catch {
              case e:Throwable =>
                printWriter.println("Processing failed with the following error:")
                e.printStackTrace()
            }
          }
      }
    }

    // manual terminal cleanup
    reader.getTerminal.restore()
    reader.shutdown()
  }

  /** Summarizes available commands */
  def printCommands(): Unit = {
    printWriter.println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      printWriter.println(s"\t$cmd\t=> $msg")
    printWriter.println()
  }
}
