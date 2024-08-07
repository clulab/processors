package org.clulab.processors.apps

import org.clulab.utils.FileUtils

import scala.io.Source
import org.clulab.processors.clu.BalaurProcessor

/**
 *
 * User: mihais
 * Date: 4/16/14
 */
object ProcessorsFileExample {
  val proc = new BalaurProcessor()

  def main(args:Array[String]): Unit = {
    val text = FileUtils.getTextFromFile(args(0))
    proc.annotate(text)
  }
}
