package edu.arizona.sista.processors

import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import scala.io.Source
import scala.collection.mutable.ListBuffer

/**
 * A simple example for CoreNLPProcessor usage. Assumes each line in a file is a standalone document.
 * User: mihais
 * Date: 9/14/13
 */
object ProcessorExample1 {
  def main(args:Array[String]) {
    val proc = new CoreNLPProcessor()
    val lines = Source.fromFile(args(0)).getLines.toList
    var count = 0
    for(l <- lines) {
      val doc = proc.annotate(l)
      count += 1
      println(s"Processed line $count")
    }
  }
}
