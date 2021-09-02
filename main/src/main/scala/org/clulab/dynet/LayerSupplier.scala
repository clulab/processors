package org.clulab.dynet

import edu.cmu.dynet.ComputationGraph
import org.clulab.fatdynet.utils.Synchronizer

import java.util.function.Supplier

class LayersSupplier(val referenceLayers: IndexedSeq[Layers]) extends Supplier[IndexedSeq[Layers]] {

  // Does this need to be Synchronizer.withComputationGraph?
  override def get(): IndexedSeq[Layers] = Synchronizer.withComputationGraph("LayersSupplier.get()") {
    println(s"Start cloning layers on thread ${Thread.currentThread.getId}")
    val result = referenceLayers.map(_.clone())
    println(s" Stop cloning layers on thread ${Thread.currentThread.getId}")

    // Now test in some way that current computation graph is same as original one?
//    ComputationGraph.g
    result
  }
}
