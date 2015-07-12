package edu.arizona.sista.learning

import org.scalatest._
import scala.collection.mutable.ArrayBuffer

/**
 *
 * User: mihais
 * Date: 11/26/13
 */
class TestJForestsRankingClassifier extends FlatSpec with Matchers {
  "JForestsRankingClassifier" should "have an accuracy > 0.54 in this dataset" in {
    val dataset = RVFRankingDataset.mkDatasetFromSvmRankFormat(
      "src/test/resources/edu/arizona/sista/learning/ranking_train.txt.gz")

    val cls = new JForestsRankingClassifier[String](
      workingDir = ".",
      keepIntermediateFiles = false,
      trainFraction = 0.80)
    cls.train(dataset)

    val queries = RVFRankingDataset.mkDatumsFromSvmRankFormat(
      "src/test/resources/edu/arizona/sista/learning/ranking_test.txt.gz")

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
    //println("P@1 = " + p)
    p should be >= 0.54

    // cls.saveTo("./model.TestJForestsRankingClassifier")
  }
}
