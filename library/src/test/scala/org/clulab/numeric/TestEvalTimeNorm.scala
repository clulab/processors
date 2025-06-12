package org.clulab.numeric

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.utils.Test

class TestEvalTimeNorm extends Test {

  behavior of "temporal parser"

  it should "not degrade in performance" in {
    val timeNormEvalDir = "/org/clulab/numeric/TimeNormEvalSet"
    val testFile = "WorldModelersDatesRangesTimex.csv"
    val seasonPath = "/org/clulab/numeric/custom/SEASON.tsv"
    val expectedFscore = 0.85
    val proc = new BalaurProcessor(seasonPathOpt = Some(seasonPath))
    val actualFscore = EvalTimeNorm.run(proc, timeNormEvalDir, testFile)

    actualFscore should be >= expectedFscore
  }
}
