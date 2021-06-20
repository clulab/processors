package org.clulab.dynet

import java.util.function.Supplier

class LayersSupplier(val referenceLayers: IndexedSeq[Layers]) extends Supplier[IndexedSeq[Layers]] {
  override def get(): IndexedSeq[Layers] = {
    referenceLayers.map(_.clone())
  }
}
