package org.clulab.learning

import org.scalatest._

/**
 *
 * User: mihais
 * Date: 12/10/13
 */
class TestRandomForestClassifier extends FlatSpec with Matchers {
  "RFClassifier" should "work with BVFDataset" in {
    val classifier = new RFClassifier[String, String](numTrees = 1, trainBagPct = 1.0, howManyFeaturesPerNode = RFClassifier.featuresPerNodeAll)
    val dataset = new BVFDataset[String, String]()

    val d1 = new BVFDatum[String, String]("+", List("good", "good", "great"))
    val d2 = new BVFDatum[String, String]("-", List("good", "good", "bad", "awful"))
    val d3 = new BVFDatum[String, String]("-", List("good", "horrible"))
    dataset += d1
    dataset += d2
    dataset += d3

    classifier.verbose = true
    classifier.train(dataset)

    classOf(classifier, List("good", "horrible")) should be ("-")
    classOf(classifier, List("bad", "horrible")) should be ("-")
    classOf(classifier, List("good", "great")) should be ("+")
  }

  "RandomForestClassifier" should "have an accuracy > .95 on this dataset" in {
    //val classifier = new RandomForestClassifier[Int, String](numTrees = 50, maxTreeDepth = 0)
    val classifier = new RFClassifier[Int, String]

    var dataset = RVFDataset.mkDatasetFromSvmLightResource("org/clulab/learning/classification_train.txt.gz").asInstanceOf[Dataset[Int, String]]
    dataset = dataset.removeFeaturesByInformationGain(0.75)
    classifier.train(dataset)

    val datums = RVFDataset.mkDatumsFromSvmLightResource("org/clulab/learning/classification_test.txt.gz")
    var total = 0
    var correct = 0
    for(datum <- datums) {
      val l = classifier.classOf(datum)
      //println(s"prediction: $l")
      if(l == datum.label) correct += 1
      total += 1
    }
    val acc = correct.toDouble / total.toDouble
    println("Accuracy: " + acc)
    acc should be >= 0.955
  }

  def classOf(c:RFClassifier[String, String], feats:List[String]):String = {
    val d = new BVFDatum[String, String]("?", feats)
    val l = c.classOf(d)
    println(s"Class of $feats: $l")
    l
  }

}
