package org.clulab.numeric
import org.clulab.processors.clu.CluProcessor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestEvalTimeNorm extends AnyFlatSpec with Matchers{

  behavior of "temporal parser"

  it should "not degrade in performance" in {
    val expectedFscore = 0.85
    val proc = new CluProcessor(seasonPathOpt = Some("/org/clulab/numeric/custom/SEASON.tsv"))
    val ner = NumericEntityRecognizer(seasonPath = "/org/clulab/numeric/custom/SEASON.tsv")
    val actualFscore = EvalTimeNorm.test(proc, ner)
    actualFscore should be >= expectedFscore
  }

}

