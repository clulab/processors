package org.clulab.numeric

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.utils.Test

class TestEvalTimeNorm extends Test {

  behavior of "temporal parser"

  it should "not degrade in performance" in {
    val expectedFscore = 0.85
    val proc = new BalaurProcessor(seasonPathOpt = Some("/org/clulab/numeric/custom/SEASON.tsv"))
    val ner = NumericEntityRecognizer(seasonPath = "/org/clulab/numeric/custom/SEASON.tsv")
    val actualFscore = EvalTimeNorm.test(proc, ner)
    actualFscore should be >= expectedFscore
  }

}

