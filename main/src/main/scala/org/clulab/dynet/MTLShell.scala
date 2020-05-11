package org.clulab.dynet

import org.clulab.utils.Shell

class MTLShell(val mtl:LstmCrfMtl) extends Shell {
  override def initialize(): Unit = {
    // nothing to do. we receive an initialize MTL object
  }

  override def work(text: String): Unit = {
    val words = text.split("\\s+")
    val labels = mtl.predictJointly(words)
    print(words, labels)
  }

  /** Prints one document */
  def print(words:Array[String], labels:Array[Array[String]]): Unit = {
    println(s"Input words: ${words.mkString(", ")}")
    for(tid <- labels.indices) {
      println(s"Labels for task #$tid: ${labels(tid).mkString(", ")}")
    }
    println()
  }
}
