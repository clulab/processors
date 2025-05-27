package org.clulab.processors.apps

import org.clulab.numeric.EvalTimeNorm
import org.clulab.processors.clu.BalaurProcessor

object EvalTimeNormApp extends App {
  val proc = new BalaurProcessor()
  val timeNormEvalDir = "/org/clulab/numeric/TimeNormEvalSet"
  val testFile = "WorldModelersDatesRangesTimex.csv"

  EvalTimeNorm.run(proc, timeNormEvalDir, testFile)
}
