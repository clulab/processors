package org.clulab.numeric
import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestEvalTimeNorm extends FlatSpec with Matchers{

  behavior of "temporal parser"

  it should "not degrade in performance" in {
    // Initialize before the CluProcessor is constructed even though EvalTimeNorm will do it again.
    Utils.initializeDyNet()
    val expectedFscore = 0.85
    val proc = new CluProcessor(seasonPathOpt = Some("/org/clulab/numeric/custom/SEASON.tsv"))
    val ner = NumericEntityRecognizer(seasonPath = "/org/clulab/numeric/custom/SEASON.tsv")
    val actualFscore = EvalTimeNorm.test(proc, ner)
    actualFscore should be >= expectedFscore
  }

}

