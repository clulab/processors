package org.clulab.processors.apps

import org.clulab.numeric.EvalTimeNorm
import org.clulab.processors.clu.BalaurProcessor

object EvalTimeNormApp extends App {
  val proc = new BalaurProcessor()

  EvalTimeNorm.run(proc)
}
