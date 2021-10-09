package org.clulab.processors.clu

import org.clulab.dynet.Utils
import org.clulab.utils.ReloadableProcessor
import org.clulab.utils.Shell

import java.io.PrintWriter

/**
  * An interactive shell for CluProcessor
  * User: mihais
  * Date: 8/2/17
  */
class CluShell extends Shell {
  Utils.initializeDyNet()

  val printWriter = new PrintWriter(System.out, true)
  val proc = new ReloadableProcessor(() => new CluProcessor(), true)

  def work(text: String): Unit = {
    val doc = proc.get.annotate(text)
    doc.prettyPrint(printWriter)
  }

  // We inherit now just from Shell, so no reloading is performed.
  def reload(): Unit = {
    println("The processor is reloading...")
    proc.reload()
  }
}

object CluShell {
  def main(args: Array[String]): Unit = {
    new CluShell().shell()
  }
}
