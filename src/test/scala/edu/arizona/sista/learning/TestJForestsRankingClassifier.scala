package edu.arizona.sista.learning

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import java.io._
import scala.collection.mutable.ArrayBuffer
import junit.framework.Assert

/**
 *
 * User: mihais
 * Date: 11/26/13
 */
class TestJForestsRankingClassifier extends AssertionsForJUnit {
  @Test def testClassifier() {
    val dataset = RVFRankingDataset.mkDatasetFromSvmRankFormat(
      "src/main/resources/edu/arizona/sista/learning/ranking_train.txt.gz")

    val cls = new JForestsRankingClassifier[String](
      workingDir = ".",
      keepIntermediateFiles = false,
      trainFraction = 0.80)
    cls.train(dataset)

    val queries = RVFRankingDataset.mkDatumsFromSvmRankFormat(
      "src/main/resources/edu/arizona/sista/learning/ranking_test.txt.gz")

    /*
    // this is only needed if offline evaluation is desired
    val ps = new PrintStream(new FileOutputStream(cls.workingDir + File.separator + "ensemble.txt"))
    ps.println(cls.ensemble.get)
    ps.close()

    RVFRankingDataset.saveToSvmRankFormat(
      queries,
      cls.featureLexicon.get,
      cls.workingDir + File.separator + "test_ready.txt")
    cls.convertToBin("test_ready.txt")
    */

    // evaluate using our API
    val queryScores = new ArrayBuffer[Array[Double]]()
    for(query <- queries) {
      val scores = cls.scoresOf(query)
      queryScores += scores.toArray
    }

    // we should do better the 54% P@1 on this dataset
    val p = RankerEvaluator.score(queries, queryScores.toArray)
    println("P@1 = " + p)
    Assert.assertTrue(p >= 0.54)

    // cls.saveTo("./model.TestJForestsRankingClassifier")
  }
}
