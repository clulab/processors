package org.clulab.sequences

import java.io.File

import jline.console.ConsoleReader
import jline.console.history.FileHistory
import org.clulab.processors.Sentence

/**
  * Simple shell for sequence taggers
  * Created by mihais on 6/7/17.
  */
object SequenceTaggerShell {
  def shell[L, F](tagger:SequenceTagger[L, F]): Unit = {
    val history = new FileHistory(new File(System.getProperty("user.home"), ".seqshellhistory"))
    sys addShutdownHook {
      history.flush() // flush file before exiting
    }

    val reader = new ConsoleReader
    reader.setHistory(history)

    var running = true
    while (running) {
      reader.setPrompt(">> ")
      reader.readLine match {
        case ":exit" | null =>
          running = false

        case text =>
          parse(text, tagger)
      }
    }

    // manual terminal cleanup
    reader.getTerminal.restore()
    reader.shutdown()
  }

  def parse[L, F](text:String, tagger:SequenceTagger[L, F]) {
    val sent = mkSent(text)
    println("Tokens: " + sent.words.mkString(", "))
    val labels = tagger.classesOf(sent)
    println("Labels: " + labels.mkString(", "))
  }

  def mkSent(text:String): Sentence = {
    val tokens = text.split("\\s+")
    val startOffsets = new Array[Int](tokens.length) // the offsets do not matter here
    val endOffsets = new Array[Int](tokens.length)
    new Sentence(tokens, startOffsets, endOffsets)
  }

}
