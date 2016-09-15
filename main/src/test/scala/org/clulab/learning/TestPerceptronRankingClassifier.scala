package org.clulab.learning

import org.scalatest._
import scala.collection.mutable.ArrayBuffer
import java.io.{OutputStreamWriter, PrintWriter}

/**
 *
 * User: mihais
 * Date: 12/10/13
 */
class TestPerceptronRankingClassifier extends FlatSpec with Matchers {
  "PerceptronRankingClassifier" should "have a P@1 > .50 on this dataset" in {
    val dataset = RVFRankingDataset.mkDatasetFromSvmRankResource("org/clulab/learning/ranking_train.txt.gz")

    val classifier = new PerceptronRankingClassifier[String](epochs = 1)
    classifier.train(dataset)

    val pw = new PrintWriter(new OutputStreamWriter(System.out))
    classifier.displayModel(pw)
    pw.flush()

    val queries = RVFRankingDataset.mkDatumsFromSvmRankResource("org/clulab/learning/ranking_test.txt.gz")
    val queryScores = new ArrayBuffer[Array[Double]]()
    for(query <- queries) {
      val scores = classifier.scoresOf(query)
      queryScores += scores.toArray
    }

    // we should do better the 50% P@1 on this dataset
    val p = RankerEvaluator.score(queries, queryScores.toArray)
    //println("P@1 = " + p)
    p should be > 0.50
  }
}
