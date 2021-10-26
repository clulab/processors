package org.clulab.numeric
import org.scalatest.{FlatSpec, Matchers}

class TestEvalTimeNorm extends FlatSpec with Matchers{

  behavior of "temporal parser"

  it should "not degrade in performance" in {
    val expectedFscore = 0.81
    val actualFscore = EvalTimeNorm.test()
    actualFscore should be >= expectedFscore
  }

}

