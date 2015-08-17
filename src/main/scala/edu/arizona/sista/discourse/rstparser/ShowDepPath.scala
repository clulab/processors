package edu.arizona.sista.discourse.rstparser

import scala.io.StdIn
import edu.arizona.sista.processors.{Sentence, Document, Processor}
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import Utils._

/**
 * Displays dependency path to root for a given word. Debugging only.
 * User: mihais
 * Date: 4/22/14
 */
object ShowDepPath {
  def main(args:Array[String]) {
    // create the processor
    val proc:Processor = new FastNLPProcessor()

    while(true) {
      print("> ")
      var text = StdIn.readLine()
      val doc = proc.annotate(text)
      showPaths(doc)
    }
  }

  def showPaths(doc:Document) {
    for(s <- doc.sentences) {
      showPath(s)
    }
  }

  def showPath(s:Sentence) {
    println("WORDS: " + s.words.mkString(" "))
    println("DEPENDENCIES:")
    val in = deps(s).incomingEdges
    for(i <- in.indices) {
      print("\t" + i + "(" + s.words(i) + "):")
      for(e <- in(i)) {
        print(" (" + e._1 + ", " + e._2 + ")")
      }
      println()
    }

    println("PATHS TO ROOT:")
    for(i <- in.indices) {
      println("\t" + i + "(" + s.words(i) + "): " + pathToRoot(i, in))
    }
  }

  def pathToRoot(start:Int, in:Array[Array[(Int, String)]]):String = {
    val os = new StringBuilder
    var root = false
    var pos = start
    while(! root) {
      if(in(pos).isEmpty) {
        root = true
      } else {
        os.append(in(pos)(0)._2)
        os.append(" ")
        pos = in(pos)(0)._1
      }
    }
    os.toString().trim
  }
}
