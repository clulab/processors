package org.clulab.fatdynet.utils

import edu.cmu.dynet._

object Transducer {

  def transduce(builder: RnnBuilder, inputs: Seq[Expression]): Seq[Expression] = {
    builder.startNewSequence()
    inputs.map { input =>
      builder.addInput(input)
    }
  }
}
