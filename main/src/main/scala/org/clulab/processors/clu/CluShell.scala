package org.clulab.processors.clu

import org.clulab.dynet.Utils
import org.clulab.utils.Shell

import java.io.PrintWriter

/**
  * An interactive shell for CluProcessor
  * User: mihais
  * Date: 8/2/17
  */
class CluShell extends Shell {
  val printWriter = new PrintWriter(System.out, true)
  val proc: CluProcessor = new CluProcessor()

  override def work(text: String): Unit = {
    val doc = proc.annotate(text)
    doc.prettyPrint(printWriter)
  }
}

object CluShell {
  def main(args: Array[String]): Unit = {
    Utils.initializeDyNet()
    val sh = new CluShell
    sh.shell()
  }
}
