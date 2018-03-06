package org.clulab.learning

import java.io.File

import org.scalatest._
import org.clulab.struct.Counter
import de.bwaldvogel.liblinear._

import org.clulab.TestUtils._

/**
 *
 * User: mihais
 * Date: 11/17/13
 */
class TestLiblinearClassifier extends FlatSpec with Matchers {
  "LiblinearClassifier" should "work with RVFDataset" in {
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
    classifier.classOf(d1) should be(d1.label)
    classifier.classOf(d2) should be(d2.label)
    classifier.classOf(dn) should be(dn.label)

    val probs = classifier.scoresOf(dn)
    //println("Probability for class +: " + probs.getCount("+"))
    //println("Probability for class -: " + probs.getCount("-"))
    //println("Probability for class ~: " + probs.getCount("~"))
    probs.getCount("+") should be > probs.getCount("-")
    probs.getCount("-") should be > probs.getCount("~")

    // make sure scores are the same after saving/loading
    val file = File.createTempFile("model", "dat")
    //println(s"Saving classifier to $file")
    file.deleteOnExit()
    classifier.saveTo(file.getAbsolutePath)
    val loadedClassifier = LiblinearClassifier.loadFrom[String, String](file.getAbsolutePath)
    val newProbs = loadedClassifier.scoresOf(dn)
    probs.getCount("+") should be(newProbs.getCount("+"))
    probs.getCount("-") should be(newProbs.getCount("-"))
  }

  it should "work with BVFDataset" in {
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
    classifier.classOf(d1) should be (d1.label)
    classifier.classOf(d2) should be (d2.label)
    classifier.classOf(dn) should be (dn.label)

    val probs = classifier.scoresOf(dn)
    //println("Probability for class +: " + probs.getCount("+"))
    //println("Probability for class -: " + probs.getCount("-"))
    //println("Probability for class ~: " + probs.getCount("~"))
    probs.getCount("+") should be > probs.getCount("-")
    probs.getCount("-") should be > probs.getCount("~")

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
    probs.getCount("+") should be(probabilities.getCount("+"))
    probs.getCount("-") should be(probabilities.getCount("-"))
    probs.getCount("~") should be(probabilities.getCount("~"))

    //
    // Let's analyze the classifier learned weights here
    //
    val weights = classifier.getWeights()
    /*
    for(l <- weights.keys) {
      println("Feature weights for label " + l)
      val sorted = weights.get(l).get.sorted
      for(fv <- sorted) {
        println("\t" + fv._1 + " " + fv._2)
      }
    }
    */
    weights.get("+").get.getCount("good") should be > weights.get("+").get.getCount("great")
    weights.get("+").get.getCount("great") should be > weights.get("+").get.getCount("bad")
    weights.get("+").get.getCount("great") should be > weights.get("+").get.getCount("meh")
  }

  it should "work with a two-label dataset" in {
    val classifier = new LiblinearClassifier[String, String](bias = false)
    val dataset = new BVFDataset[String, String]()

    val d1 = new BVFDatum[String, String]("+", List("good", "great", "good"))
    val d2 = new BVFDatum[String, String]("-", List("bad", "awful"))

    dataset += d1
    dataset += d2
    classifier.train(dataset)

    val weights = classifier.getWeights()
    /*
    for(l <- weights.keys) {
      println("Feature weights for label " + l)
      val sorted = weights.get(l).get.sorted
      for(fv <- sorted) {
        println("\t" + fv._1 + " " + fv._2)
      }
    }
    */
    weights.get("+").get.getCount("good") should be > weights.get("+").get.getCount("great")
    weights.get("+").get.getCount("great") should be > weights.get("+").get.getCount("bad")
    weights.get("+").get.getCount("great") should be > weights.get("+").get.getCount("awful")
    weights.get("-").get.getCount("great") should be < weights.get("-").get.getCount("awful")
    weights.get("-").get.getCount("good") should be < weights.get("-").get.getCount("awful")
  }

  "LogisticRegressionClassifier" should "have an accuracy > .97 in this dataset" in {
    val classifier = new LogisticRegressionClassifier[Int, String](bias = false)
    val dataset = RVFDataset.mkDatasetFromSvmLightResource("org/clulab/learning/classification_train.txt.gz")
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
    //println("Accuracy: " + acc)
    acc should be > 0.97
  }

  "LinearSVMClassifier" should "have an accuracy > .97 in this dataset" in {
    val classifier = new LinearSVMClassifier[Int, String](bias = false)
    val dataset = RVFDataset.mkDatasetFromSvmLightResource("org/clulab/learning/classification_train.txt.gz")
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
    //println("Accuracy: " + acc)
    acc should be > 0.97
  }
}
