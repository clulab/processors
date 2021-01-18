package org.clulab.utils

import java.io.File

/**
 * Line reader to use in different environments.  The CliReader works on the command line
 * and with sbt and supports history.  However, it doesn't work well in the IntelliJ
 * or Eclipse IDEs (as least not with Windows).  For those, use the IdeReader.  To switch
 * between the two, add a command line argument to get the IdeReader rather than changing
 * the code.
 */

abstract class LineReader {
  def readLine(): String
  def readLineOpt(): Option[String]
  def setPrompt(prompt: String): Unit
}

class CliReader(prompt: String, parentProperty: String, child: String) extends LineReader {
  import jline.console.ConsoleReader
  import jline.console.history.FileHistory

  val reader = new ConsoleReader()
  val history = new FileHistory(new File(System.getProperty(parentProperty), child))

  reader.setPrompt(prompt)
  reader.setHistory(history)
  sys addShutdownHook {
    reader.getTerminal.restore()
    reader.shutdown()
    history.flush() // flush file before exiting
  }

  override def readLine(): String = reader.readLine

  override def readLineOpt(): Option[String] = Option(readLine())

  override def setPrompt(prompt: String): Unit = reader.setPrompt(prompt)
}

class IdeReader(protected var prompt: String) extends LineReader {
  import java.util.Scanner

  protected val reader = new Scanner(System.in)

  override def readLine(): String = {
    print(prompt)
    Console.flush()
    reader.nextLine
  }

  override def readLineOpt(): Option[String] = Option(readLine())

  override def setPrompt(prompt: String): Unit = this.prompt = prompt
}
