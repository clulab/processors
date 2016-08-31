package org.clulab.discourse.rstparser

import scala.io.StdIn
import org.clulab.processors.fastnlp.FastNLPProcessor
import Utils._
import com.typesafe.scalalogging.LazyLogging

/**
 * Extracts the head word of a span using dependency syntax
 * User: mihais
 * Date: 8/19/14
 */
object CheckDepHead extends LazyLogging {
  def main(args:Array[String]) {
    val proc = new FastNLPProcessor()
    while(true) {
      print("> ")
      var text = StdIn.readLine()
      val doc = proc.annotate(text)
      val offsets = StdIn.readLine().split("\\s+")
      logger.info(deps(doc.sentences(0)).toString)

      val start = offsets(0).toInt
      val end = offsets(1).toInt
      val (h, p, l) = Utils.findSyntacticHeadFromDependencies(deps(doc.sentences(0)), start, end)
      logger.info(s"Head = $h")
      logger.info(s"Parent = $p")
    }
  }
}
