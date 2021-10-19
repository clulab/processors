package org.clulab.processors

import org.clulab.serialization.json4StopGraph.JSONSerializer
import org.clulab.utils.Menu

class ProcessorJsonShell extends ProcessorShell {
  val serializer = new JSONSerializer(printWriter)

  override def work(text: String): Unit = {
    val doc = proc.get.annotate(text)
    serializer.serialize(doc)
    printWriter.flush()
  }

  override def mkMenu(): Menu = super.mkMenu.withGreeting("Welcome to the ProcessorJsonShell!")
}

object ProcessorJsonShell extends App {
  new ProcessorJsonShell().shell()
}
