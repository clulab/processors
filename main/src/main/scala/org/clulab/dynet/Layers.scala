package org.clulab.dynet

import com.typesafe.config.Config
import org.clulab.struct.Counter

/**
 * A sequence of layers that implements a complete NN architecture for sequence modeling
 */
class Layers (val initialLayer: Option[InitialLayer],
              val intermediateLayers: IndexedSeq[IntermediateLayer],
              val finalLayer: Option[FinalLayer]) {

}

object Layers {
  def apply(config: Config,
            paramPrefix: String,
            wordCounter: Counter[String],
            labelCounterOpt: Option[Counter[String]]): Layers = {
    null // TODO
  }
}
