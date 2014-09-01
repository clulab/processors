package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

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
      var text = Console.readLine()
      val doc = proc.annotate(text)
      val offsets = Console.readLine().split("\\s+")
      println(doc.sentences(0).dependencies.get)

      val start = offsets(0).toInt
      val end = offsets(1).toInt
      val (h, p, l) = Utils.findSyntacticHeadFromDependencies(doc.sentences(0).dependencies.get, start, end)
      println(s"Head = $h")
      println(s"Parent = $p")
    }
  }
}
