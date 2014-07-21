package edu.arizona.sista.learning

import java.io.File

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
    val dataset = RVFDataset.mkDatasetFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_train.txt.gz")
    classifier.train(dataset)

    val datums = RVFDataset.mkDatumsFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_test.txt.gz")
    val acc = computeAcc(datums, classifier)
    println("Accuracy: " + acc)
    assertTrue(acc > 0.97)

    // make sure scores are the same after saving/loading
    val file = File.createTempFile("model", "dat")
    println(s"Saving classifier to $file")
    file.deleteOnExit()
    classifier.saveTo(file.getAbsolutePath)
    val loadedClassifier = PerceptronClassifier.loadFrom[Int, String](file.getAbsolutePath)
    val newAcc = computeAcc(datums, loadedClassifier)
    assertTrue(acc == newAcc)
  }

  def computeAcc(datums:Iterable[Datum[Int, String]], classifier:Classifier[Int, String]) = {
    var total = 0
    var correct = 0
    for(datum <- datums) {
      val l = classifier.classOf(datum)
      //println(s"prediction: $l")
      if(l == datum.label) correct += 1
      total += 1
    }
    val acc = correct.toDouble / total.toDouble
    acc
  }
}
