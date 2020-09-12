package org.clulab.processors.clu

import java.io.PrintWriter

import org.clulab.dynet.Utils
import org.clulab.utils.Shell

/**
  * An interactive shell for CluProcessor
  * User: mihais
  * Date: 8/2/17
  */
class CluShell extends Shell {
  var proc: CluProcessor = _

  override def initialize(): Unit = {
    proc = new CluProcessor()
  }

  override def work(text: String): Unit = {
    val doc = proc.annotate(text)
    doc.prettyPrint(printWriter)
  }
}

object CluShell {
  def main(args:Array[String]): Unit = {
    Utils.initializeDyNet(train = false)
    val sh = new CluShell
    sh.shell()
  }
}
