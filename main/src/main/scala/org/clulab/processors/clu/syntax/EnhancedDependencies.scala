package org.clulab.processors.clu.syntax

import java.io.File

import org.clulab.struct.{DirectedGraph, DirectedGraphEdgeIterator}
import jline.console.ConsoleReader
import jline.console.history.FileHistory
import org.clulab.processors.Document
import org.clulab.processors.clu.CluProcessor

import scala.collection.immutable.ListMap

/**
  * Converts Stanford basic dependencies to collapsed ones
  * This follows the rules from http://universaldependencies.org/u/overview/enhanced-syntax.html
  *   (but applied to Stanford deps rather than universal ones)
  * User: mihais
  * Date: 8/1/17
  */
object EnhancedDependencies {
  def generateEnhancedDependencies(dg:DirectedGraph[String]): DirectedGraph[String] = {
    // TODO
    dg
  }

  val commands = ListMap(
    ":help" -> "show commands",
    ":exit" -> "exit system"
  )

  def shell() {
    val history = new FileHistory(new File(System.getProperty("user.home"), ".enhanceddepshistory"))
    sys addShutdownHook {
      history.flush() // flush file before exiting
    }

    val reader = new ConsoleReader
    reader.setHistory(history)

    lazy val proc = new CluProcessor()
    reader.setPrompt("(clu)>>> ")
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
          printDeps(doc)
      }
    }

    // manual terminal cleanup
    reader.getTerminal.restore()
    reader.shutdown()
  }

  /** summarize available commands */
  def printCommands(): Unit = {
    println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println()
  }

  def printDeps(doc:Document) {
    var sentenceCount = 0
    for (sentence <- doc.sentences) {
      println("Sentence #" + sentenceCount + ":")
      println("Tokens: " + sentence.words.mkString(" "))
      sentence.stanfordBasicDependencies.foreach(dependencies => {
        println("Basic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })

      sentenceCount += 1
      println("\n")
    }
  }

  def main(args:Array[String]): Unit = {
    shell()
  }
}
