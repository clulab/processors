package org.clulab.processors.clu

import java.io.File

import jline.console.ConsoleReader
import jline.console.history.FileHistory
import org.clulab.processors.Document
import org.clulab.struct.DirectedGraphEdgeIterator

import scala.collection.immutable.ListMap

/**
  * An interactive shell for CluProcessor
  * User: mihais
  * Date: 8/2/17
  */
object CluShell {
  val commands = ListMap(
    ":help" -> "show commands",
    ":exit" -> "exit system"
  )

  def shell() {
    val history = new FileHistory(new File(System.getProperty("user.home"), ".clushellhistory"))
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
          if(text.trim.nonEmpty) {
            try {
              val doc = proc.annotate(text)
              print(doc)
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
  def print(doc:Document) {
    var sentenceCount = 0
    for (sentence <- doc.sentences) {
      println("Sentence #" + sentenceCount + ":")
      val indices = 0 until sentence.size
      println("Tokens: " + sentence.words.zip(indices).mkString(" "))
      println("Tags: " + sentence.tags.get.zip(indices).mkString(" "))
      
      sentence.stanfordBasicDependencies.foreach(dependencies => {
        println("Basic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })

      sentence.stanfordCollapsedDependencies.foreach(dependencies => {
        println("Enhanced dependencies:")
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
