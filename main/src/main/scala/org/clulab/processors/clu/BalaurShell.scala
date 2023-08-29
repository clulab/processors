package org.clulab.processors.clu

import org.clulab.utils.ReloadableProcessor
import org.clulab.utils.Shell

import java.io.PrintWriter

/**
  * An interactive shell for BalaurProcessor
  * User: mihais
  * Date: 7/5/2023
  */
class BalaurShell extends Shell {
  val printWriter = new PrintWriter(System.out, true)
  val proc = new ReloadableProcessor(() => new BalaurProcessor(), true)

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

object BalaurShell {
  def main(args: Array[String]): Unit = {
    new BalaurShell().shell()
  }
}
