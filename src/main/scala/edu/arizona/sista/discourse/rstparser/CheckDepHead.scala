package edu.arizona.sista.discourse.rstparser

import scala.io.StdIn
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import Utils._

/**
 * Extracts the head word of a span using dependency syntax
 * User: mihais
 * Date: 8/19/14
 */
object CheckDepHead {
  def main(args:Array[String]) {
    val proc = new FastNLPProcessor()
    while(true) {
      print("> ")
      var text = StdIn.readLine()
      val doc = proc.annotate(text)
      val offsets = StdIn.readLine().split("\\s+")
      println(deps(doc.sentences(0)))

      val start = offsets(0).toInt
      val end = offsets(1).toInt
      val (h, p, l) = Utils.findSyntacticHeadFromDependencies(deps(doc.sentences(0)), start, end)
      println(s"Head = $h")
      println(s"Parent = $p")
    }
  }
}
