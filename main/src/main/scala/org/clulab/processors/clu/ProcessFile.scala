package org.clulab.processors.clu

import org.clulab.dynet.Utils

object ProcessFile extends App {
  val fileName = args(0)
  lazy val proc = new CluProcessor()
  val text = io.Source.fromFile(fileName).mkString
  //println(text)

  Utils.initializeDyNetForInference()
  val doc = proc.mkDocument(text)
  proc.annotate(doc)
}
