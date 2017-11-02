package org.clulab.processors.examples

import org.clulab.processors.fastnlp.FastNLPProcessor

import scala.io.Source

/**
 *
 * User: mihais
 * Date: 4/16/14
 */
object ProcessorFileExample {
  val proc = new FastNLPProcessor()

  def main(args:Array[String]) {
    val text = Source.fromFile(args(0), "UTF-8").getLines().mkString(" ")
    proc.annotate(text)
  }
}
