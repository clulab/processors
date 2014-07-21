package edu.arizona.sista.learning

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.collection.mutable.ArrayBuffer
import junit.framework.Assert
import java.io.{OutputStreamWriter, PrintWriter}

/**
 *
 * User: mihais
 * Date: 12/10/13
 */
class TestPerceptronRankingClassifier extends AssertionsForJUnit {
  @Test def testClassifier() {
    val dataset = RVFRankingDataset.mkDatasetFromSvmRankFormat(
      "src/test/resources/edu/arizona/sista/learning/ranking_train.txt.gz")

    val classifier = new PerceptronRankingClassifier[String](epochs = 1)
    classifier.train(dataset)

    val pw = new PrintWriter(new OutputStreamWriter(System.out))
    classifier.displayModel(pw)
    pw.flush()

    val queries = RVFRankingDataset.mkDatumsFromSvmRankFormat(
      "src/test/resources/edu/arizona/sista/learning/ranking_test.txt.gz")
    val queryScores = new ArrayBuffer[Array[Double]]()
    for(query <- queries) {
      val scores = classifier.scoresOf(query)
      queryScores += scores.toArray
    }

    // we should do better the 50% P@1 on this dataset
    val p = RankerEvaluator.score(queries, queryScores.toArray)
    println("P@1 = " + p)
    Assert.assertTrue(p >= 0.50)
  }
}
