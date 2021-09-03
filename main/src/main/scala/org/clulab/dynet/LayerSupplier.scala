package org.clulab.dynet

import edu.cmu.dynet.ComputationGraph
import org.clulab.fatdynet.utils.Synchronizer
import org.clulab.utils.Dumper

import java.util.function.Supplier

class LayersSupplier(val referenceLayers: IndexedSeq[Layers]) extends Supplier[IndexedSeq[Layers]] {
  var debug = false

  Dumper.dump("-ref")

  def checkForNaN(): Unit = {
    if (debug)
      if (ComputationGraph.hasNaN)
        println("Where did this come from?")
  }

  // Does this need to be Synchronizer.withComputationGraph?
  override def get(): IndexedSeq[Layers] = Synchronizer.withComputationGraph("LayersSupplier.get()") {
    println(s"Start cloning layers on thread ${Thread.currentThread.getId}")
    checkForNaN()
    val result = referenceLayers.map(_.clone())
    checkForNaN()
    println(s" Stop cloning layers on thread ${Thread.currentThread.getId}")

    Dumper.dump("-copy")
    checkForNaN()
    result
  }
}
