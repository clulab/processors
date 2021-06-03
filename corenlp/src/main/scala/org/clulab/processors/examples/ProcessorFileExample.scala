package org.clulab.processors.examples

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.utils.FileUtils

import scala.io.Source

/**
 *
 * User: mihais
 * Date: 4/16/14
 */
object ProcessorFileExample {
  val proc = new FastNLPProcessor()

  def main(args:Array[String]) {
    val text = FileUtils.getTextFromFile(args(0))
    proc.annotate(text)
  }
}
