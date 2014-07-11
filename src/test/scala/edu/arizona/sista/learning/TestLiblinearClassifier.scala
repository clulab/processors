package edu.arizona.sista.learning

import org.scalatest.junit.AssertionsForJUnit
import junit.framework.Assert._
import org.junit.Test
import edu.arizona.sista.struct.Counter
import de.bwaldvogel.liblinear._

/**
 *
 * User: mihais
 * Date: 11/17/13
 */
class TestLiblinearClassifier extends AssertionsForJUnit {
  @Test def testRVFClassifier() {
    val classifier = new LiblinearClassifier[String, String](bias = false)
    val dataset = new RVFDataset[String, String]()

    val d1 = mkRVFDatum("+", List("good", "great", "good"))
    val d2 = mkRVFDatum("-", List("bad", "awful"))
    val d3 = mkRVFDatum("~", List("meh", "soso"))

    dataset += d1
    dataset += d2
    dataset += d3
    classifier.train(dataset)

    val dn = mkRVFDatum("+", List("good", "great", "bad", "new"))
    assertTrue(classifier.classOf(d1) == d1.label)
    assertTrue(classifier.classOf(d2) == d2.label)
    assertTrue(classifier.classOf(dn) == dn.label)

    val probs = classifier.scoresOf(dn)
    println("Probability for class +: " + probs.getCount("+"))
    println("Probability for class -: " + probs.getCount("-"))
    println("Probability for class ~: " + probs.getCount("~"))
    assertTrue(probs.getCount("+") > probs.getCount("-"))
    assertTrue(probs.getCount("-") > probs.getCount("~"))
  }

  @Test def testBVFClassifier() {
    val classifier = new LiblinearClassifier[String, String](bias = false)
    val dataset = new BVFDataset[String, String]()

    //
    // good:1
    // great:2
    // bad:3
    // awful:4
    // meh:5
    // soso:6
    //
    // +:0
    // -:1
    // ~:2
    //
    val d1 = new BVFDatum[String, String]("+", List("good", "great", "good"))
    val d2 = new BVFDatum[String, String]("-", List("bad", "awful"))
    val d3 = new BVFDatum[String, String]("~", List("meh", "soso"))

    dataset += d1
    dataset += d2
    dataset += d3
    classifier.train(dataset)

    val dn = new BVFDatum[String, String]("+", List("good", "great", "bad", "new"))
    assertTrue(classifier.classOf(d1) == d1.label)
    assertTrue(classifier.classOf(d2) == d2.label)
    assertTrue(classifier.classOf(dn) == dn.label)

    val probs = classifier.scoresOf(dn)
    println("Probability for class +: " + probs.getCount("+"))
    println("Probability for class -: " + probs.getCount("-"))
    println("Probability for class ~: " + probs.getCount("~"))
    assertTrue(probs.getCount("+") > probs.getCount("-"))
    assertTrue(probs.getCount("-") > probs.getCount("~"))

    //
    // let's make sure we get the same scores if we use liblinear directly
    //
    val ld1 = new Array[Feature](2)
    ld1(0) = new FeatureNode(1, 2)
    ld1(1) = new FeatureNode(2, 1)
    val ld2 = new Array[Feature](2)
    ld2(0) = new FeatureNode(3, 1)
    ld2(1) = new FeatureNode(4, 1)
    val ld3 = new Array[Feature](2)
    ld3(0) = new FeatureNode(5, 1)
    ld3(1) = new FeatureNode(6, 1)
    val y = new Array[Double](3)
    y(0) = 0
    y(1) = 1
    y(2) = 2
    val problem = new Problem
    problem.l = 3
    problem.n = 6
    problem.y = y
    problem.x = new Array[Array[Feature]](3)
    problem.x(0) = ld1
    problem.x(1) = ld2
    problem.x(2) = ld3
    problem.bias = -1
    val parameter = new Parameter(classifier.solverType, classifier.C, classifier.eps)
    val model = Linear.train(problem, parameter)
    val ldn = new Array[Feature](4)
    ldn(0) = new FeatureNode(1, 1)
    ldn(1) = new FeatureNode(2, 1)
    ldn(2) = new FeatureNode(3, 1)
    ldn(3) = new FeatureNode(10, 1)
    val lprobs = new Array[Double](model.getNrClass)
    Linear.predictProbability(model, ldn, lprobs)
    val probabilities = new Counter[String]
    for(i <- 0 until model.getNrClass) {
      probabilities.setCount(dataset.labelLexicon.get(model.getLabels()(i)), lprobs(i))
    }
    // here we check that our wrapper and liblinear get the same scores
    assertTrue(probs.getCount("+") == probabilities.getCount("+"))
    assertTrue(probs.getCount("-") == probabilities.getCount("-"))
    assertTrue(probs.getCount("~") == probabilities.getCount("~"))

    //
    // Let's analyze the classifier learned weights here
    //
    val weights = classifier.getWeights()
    for(l <- weights.keys) {
      println("Feature weights for label " + l)
      val sorted = weights.get(l).get.sorted
      for(fv <- sorted) {
        println("\t" + fv._1 + " " + fv._2)
      }
    }
    assertTrue(weights.get("+").get.getCount("good") > weights.get("+").get.getCount("great"))
    assertTrue(weights.get("+").get.getCount("great") > weights.get("+").get.getCount("bad"))
    assertTrue(weights.get("+").get.getCount("great") > weights.get("+").get.getCount("meh"))
  }

  @Test def testWeightsTwoLabels() {
    val classifier = new LiblinearClassifier[String, String](bias = false)
    val dataset = new BVFDataset[String, String]()

    val d1 = new BVFDatum[String, String]("+", List("good", "great", "good"))
    val d2 = new BVFDatum[String, String]("-", List("bad", "awful"))

    dataset += d1
    dataset += d2
    classifier.train(dataset)

    val weights = classifier.getWeights()
    for(l <- weights.keys) {
      println("Feature weights for label " + l)
      val sorted = weights.get(l).get.sorted
      for(fv <- sorted) {
        println("\t" + fv._1 + " " + fv._2)
      }
    }
    assertTrue(weights.get("+").get.getCount("good") > weights.get("+").get.getCount("great"))
    assertTrue(weights.get("+").get.getCount("great") > weights.get("+").get.getCount("bad"))
    assertTrue(weights.get("+").get.getCount("great") > weights.get("+").get.getCount("awful"))
    assertTrue(weights.get("-").get.getCount("great") < weights.get("-").get.getCount("awful"))
    assertTrue(weights.get("-").get.getCount("good") < weights.get("-").get.getCount("awful"))
  }

  private def mkRVFDatum(label:String, features:List[String]):RVFDatum[String, String] = {
    val c = new Counter[String]
    for(f <- features) c.incrementCount(f)
    new RVFDatum[String, String](label, c)
  }

  @Test def testLRClassifier() {
    val classifier = new LogisticRegressionClassifier[Int, String](bias = false)
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

  @Test def testSVMClassifier() {
    val classifier = new LinearSVMClassifier[Int, String](bias = false)
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
