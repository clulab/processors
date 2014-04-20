package edu.arizona.sista.processors

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import scala.io.Source

/**
 *
 * User: mihais
 * Date: 4/16/14
 */
object ProcessorFileExample {
  val proc = new FastNLPProcessor()

  def main(args:Array[String]) {
    val text = Source.fromFile(args(0), "ISO-8859-1").getLines().mkString(" ")
    proc.annotate(text)
  }
}
