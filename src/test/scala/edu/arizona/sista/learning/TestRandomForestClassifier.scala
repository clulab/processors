package edu.arizona.sista.learning

import org.scalatest._

/**
 *
 * User: mihais
 * Date: 12/10/13
 */
class TestRandomForestClassifier extends FlatSpec with Matchers {
  "RandomForestClassifier" should "have an accuracy > .96 on this dataset" in {
    val classifier = new RandomForestClassifier[Int, String](
      numTrees = 1000,
      featureSampleRatio = -0.50,
      maxTreeDepth = 0)

    // MS: let's disable this; it takes forever
    /*
    val dataset = RVFDataset.mkDatasetFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_train.txt.gz")
    classifier.train(dataset)

    val datums = RVFDataset.mkDatumsFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_test.txt.gz")
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
    acc should be > 0.96
    */
  }
}
