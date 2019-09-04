package org.clulab.sequences

import java.io.File

import jline.console.ConsoleReader
import jline.console.history.FileHistory

import scala.collection.immutable.ListMap

object MTLShell {
  val commands: Map[String, String] = ListMap(
    ":help" -> "show commands",
    ":exit" -> "exit system"
  )

  def shell(mtl:LstmCrfMtl) {
    val history = new FileHistory(new File(System.getProperty("user.home"), ".mtlshellhistory"))
    sys addShutdownHook {
      history.flush() // flush file before exiting
    }

    val reader = new ConsoleReader
    reader.setHistory(history)

    reader.setPrompt("(mtl)>>> ")
    println("\nWelcome to the MTL shell!")
    printCommands()

    var running = true
    while (running) {
      reader.readLine match {
        case ":help" =>
          printCommands()

        case ":exit" | null =>
          running = false

        case text =>
          if(text.trim.nonEmpty) {
            try {
              val words = text.split("\\s+")
              val labels = mtl.predictJointly(words)
              print(words, labels)
            } catch {
              case e:Throwable =>
                println("Processing failed with the following error:")
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
    println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println()
  }

  /** Prints one document */
  def print(words:Array[String], labels:Array[Array[String]]): Unit = {
    println(s"Input words: ${words.mkString(", ")}")
    for(tid <- labels.indices) {
      println(s"Labels for task #$tid: ${labels(tid).mkString(", ")}")
    }
    println()
  }
}
