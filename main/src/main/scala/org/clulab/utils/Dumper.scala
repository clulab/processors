package org.clulab.utils

import edu.cmu.dynet.ComputationGraph

import java.util.concurrent.atomic.AtomicInteger

object Dumper {
  val count = new AtomicInteger(0)

  def dump(suffix: String  = ""): Unit = {
    val index = count.incrementAndGet()
    val txtFilename = s"dump-$index$suffix.txt"
    val vizFilename = s"dump-$index$suffix.viz"

    ComputationGraph.dump(txtFilename, true, true, false)
    ComputationGraph.printGraphVizToFile(vizFilename)
  }
}
