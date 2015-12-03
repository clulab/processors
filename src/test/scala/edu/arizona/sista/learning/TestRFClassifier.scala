package edu.arizona.sista.learning

import org.scalatest.{Matchers, FlatSpec}

/**
  *
  * User: mihais
  * Date: 11/29/15
  */
class TestRFClassifier extends FlatSpec with Matchers {
  "RFClassifier" should "work with BVFDataset" in {
    val classifier = new RFClassifier[String, String](numTrees = 1, numThreads = 1)
    val dataset = new BVFDataset[String, String]()

    val d1 = new BVFDatum[String, String]("+", List("good", "great"))
    val d2 = new BVFDatum[String, String]("-", List("bad", "awful"))
    val d3 = new BVFDatum[String, String]("-", List("good", "horrible"))
    dataset += d1
    dataset += d2
    dataset += d3

    classifier.train(dataset)

  }
}
