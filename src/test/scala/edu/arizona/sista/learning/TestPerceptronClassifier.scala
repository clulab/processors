package edu.arizona.sista.learning

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import junit.framework.Assert._

/**
 *
 * User: mihais
 * Date: 12/15/13
 */
class TestPerceptronClassifier extends AssertionsForJUnit {
  @Test def testClassifier() {
    val classifier = new PerceptronClassifier[Int, String](
      epochs = 10,
      marginRatio = 1.0)
    val dataset = RVFDataset.mkDatasetFromSvmLightFormat("src/main/resources/edu/arizona/sista/learning/classification_train.txt.gz")
    classifier.train(dataset)

    val datums = RVFDataset.mkDatumsFromSvmLightFormat("src/main/resources/edu/arizona/sista/learning/classification_test.txt.gz")
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
    assertTrue(acc > 0.97)
  }
}
