package org.clulab.learning

import java.io.{File, PrintWriter}

import org.scalatest._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.sys.process._

object NeedsExternalBinary extends Tag("NeedsExternalBinary")

/**
  * Tests training svm_rank; needs the svm_rank_classify in the $PATH!
  * User: mihais
  * Date: 4/25/13
  * Last Modified: Fix compiler issue: import scala.io.Source.
  */
class TestSVMRankingClassifier extends FlatSpec with Matchers {

  "SVMRankingClassifier" should "perform similarly to the command line svm_rank_classify" taggedAs (NeedsExternalBinary) in {
    val dataset = new BVFRankingDataset[String]

    val qid1 = new ListBuffer[Datum[Int, String]]
    qid1 += new BVFDatum[Int, String](2, List("good", "great"))
    qid1 += new BVFDatum[Int, String](1, List("bad", "awful"))
    dataset += qid1.toList

    val qid2 = new ListBuffer[Datum[Int, String]]
    qid2 += new BVFDatum[Int, String](2, List("good"))
    qid2 += new BVFDatum[Int, String](1, List("great"))
    dataset += qid2.toList

    //println("Dataset feature lexicon is:\n" + dataset.featureLexicon)
    // normally, keepIntermediateFiles = false
    // also, set the working directory to something meaningful in your application
    val classifier = new SVMRankingClassifier[String](".", keepIntermediateFiles = true)
    classifier.train(dataset)

    val qid3 = new ListBuffer[Datum[Int, String]]
    qid3 += new BVFDatum[Int, String](2, List("good", "bad"))
    qid3 += new BVFDatum[Int, String](1, List("great"))
    val scores = classifier.scoresOf(qid3)
    //println("Scores: " + scores)

    //
    // the code before is just for debug purposes
    // we do not do this in real life
    //

    // let's make sure we get the same values as svm_rank_classify
    val pw = new PrintWriter("./test.dat")
    classifier.mkTestFile(pw, qid3, 1)
    pw.close()
    val exitCode = "svm_rank_classify ./test.dat ./model.dat ./predictions".!
    exitCode should be (0)

    val svmRankClassifyScores =
      Source.fromFile("./predictions").getLines().map(l => l.toDouble).toArray
    val ourScores = scores.toArray
    svmRankClassifyScores(0) should be (ourScores(0))
    svmRankClassifyScores(1) should be (ourScores(1))
    //println("Scores match!")

    //
    // let's verify that we get the same scores after serialization
    //
    classifier.saveTo("./model.ser")
    val serCls:SVMRankingClassifier[String] = SVMRankingClassifier.loadFrom("./model.ser")
    val serScores = serCls.scoresOf(qid3).toArray
    serScores(0) should be (ourScores(0))
    serScores(1) should be (ourScores(1))
    //println("Scores after serialization match!")

    // delete all files generated here
    new File("./model.dat").delete()
    new File("./train.dat").delete()
    new File("./test.dat").delete()
    new File("./predictions").delete()
    new File("./model.ser").delete()
  }

  it should "perform better than 0.50 P@1 on this dataset" taggedAs (NeedsExternalBinary) in {
    val dataset = RVFRankingDataset.mkDatasetFromSvmRankResource("org/clulab/learning/ranking_train.txt.gz")

    val classifier = new SVMRankingClassifier[String](".", keepIntermediateFiles = true)
    classifier.train(dataset)

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

    // delete all files generated here
    new File("./model.dat").delete()
    new File("./train.dat").delete()
  }
}
