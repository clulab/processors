package org.clulab.processors.apps

import org.clulab.serialization.json4StopGraph.JSONSerializer
import org.clulab.utils.Menu

class ProcessorsJsonShell extends ProcessorsShell {
  val serializer = new JSONSerializer(printWriter)

  override def work(text: String): Unit = {
    val doc = proc.get.annotate(text)
    serializer.serialize(doc)
    printWriter.flush()
  }

  override def mkMenu(): Menu = super.mkMenu().withGreeting("Welcome to the ProcessorJsonShell!")
}

object ProcessorsJsonShell extends App {
  new ProcessorsJsonShell().shell()
}
