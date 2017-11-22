package org.clulab.learning

import java.io.File

import org.scalatest._
import de.bwaldvogel.liblinear._

import org.clulab.TestUtils._

/**
  * Adapted from TestLiblinearClassifier
  * User: mihais, danebell
  * Date: 11/20/17
  */
class TestLiblinearRegression extends FlatSpec with Matchers {

  "LiblinearRegression" should "work with RVFRegDataset" in {
    val regression = new LiblinearRegression[String](bias = false)
    val dataset = new RVFRegDataset[String]()

    val d1 = mkRVFDatum(0.91, List("good", "great", "good"))
    val d2 = mkRVFDatum(0.13, List("bad", "awful"))
    val d3 = mkRVFDatum(0.44, List("meh", "soso"))

    dataset += d1
    dataset += d2
    dataset += d3
    regression.train(dataset, dataset.indices.toArray)

    val dn = mkRVFDatum(0.8, List("good", "great", "bad", "new"))
    regression.scoreOf(d1) should be > regression.scoreOf(d2)
    regression.scoreOf(d1) should be > regression.scoreOf(d3)
    regression.scoreOf(d3) should be > regression.scoreOf(d2)
    regression.scoreOf(dn) should be > regression.scoreOf(d2)

    val prob = regression.scoreOf(dn)

    // make sure scores are the same after saving/loading
    val file = File.createTempFile("model", "dat")
    //println(s"Saving classifier to $file")
    file.deleteOnExit()
    regression.saveTo(file.getAbsolutePath)
    val loadedClassifier = LiblinearRegression.loadFrom[String](file.getAbsolutePath)
    val newProb = loadedClassifier.scoreOf(dn)
    newProb should be (prob)
  }

  it should "work with BVFDataset" in {
    val regression = new LiblinearRegression[String](bias = false)
    val dataset = new BVFRegDataset[String]()

    //
    // good:1
    // great:2
    // bad:3
    // awful:4
    // meh:5
    // soso:6
    //
    val d1 = new BVFDatum[Double, String](0.91, List("good", "great", "good"))
    val d2 = new BVFDatum[Double, String](0.13, List("bad", "awful"))
    val d3 = new BVFDatum[Double, String](0.44, List("meh", "soso"))

    dataset += d1
    dataset += d2
    dataset += d3
    regression.train(dataset)

    val dn = mkRVFDatum(0.8, List("good", "great", "bad", "new"))
    regression.scoreOf(d1) should be > regression.scoreOf(d2)
    regression.scoreOf(d1) should be > regression.scoreOf(d3)
    regression.scoreOf(d3) should be > regression.scoreOf(d2)
    regression.scoreOf(dn) should be > regression.scoreOf(d2)

    val prob = regression.scoreOf(dn)

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
    y(0) = 0.91
    y(1) = 0.13
    y(2) = 0.44
    val problem = new Problem
    problem.l = 3
    problem.n = 6
    problem.y = y
    problem.x = new Array[Array[Feature]](3)
    problem.x(0) = ld1
    problem.x(1) = ld2
    problem.x(2) = ld3
    problem.bias = -1
    val parameter = new Parameter(regression.solverType, regression.C, regression.eps)
    val model = Linear.train(problem, parameter)
    val ldn = new Array[Feature](4)
    ldn(0) = new FeatureNode(1, 1)
    ldn(1) = new FeatureNode(2, 1)
    ldn(2) = new FeatureNode(3, 1)
    ldn(3) = new FeatureNode(10, 1)
    val newProb = Linear.predict(model, ldn)
    // here we check that our wrapper and liblinear get the same scores
    prob should be (newProb)

    //
    // Let's analyze the classifier learned weights here
    //
    val weights = regression.getWeights()
    /*
    for(l <- weights.keys) {
      println("Feature weights for label " + l)
      val sorted = weights.get(l).get.sorted
      for(fv <- sorted) {
        println("\t" + fv._1 + " " + fv._2)
      }
    }
    */
    weights.getCount("good") should be > weights.getCount("great")
    weights.getCount("great") should be > weights.getCount("bad")
    weights.getCount("great") should be > weights.getCount("meh")
  }

  "LinearSVMRegression" should "have a mse < .021 in this dataset" in {
    val regression = new LinearSVMRegression[String](bias = false)
    val dataset = RVFRegDataset.mkRegDatasetFromSvmLightResource("org/clulab/learning/regression_train.txt.gz")
    regression.train(dataset)
    val datums = RVFRegDataset.mkDatumsFromSvmLightResource("org/clulab/learning/regression_test.txt.gz")
    val scores = for (datum <- datums) yield {
      val err = datum.label - regression.scoreOf(datum)
      err * err
    }
    val mse = scores.sum / scores.toSeq.length.toDouble
    mse should be < 0.09
  }

  "LinearSVMRegressionDual" should "have a mse < .021 in this dataset" in {
    val regression = new LinearSVMRegressionDual[String](bias = false)
    val dataset = RVFRegDataset.mkRegDatasetFromSvmLightResource("org/clulab/learning/regression_train.txt.gz")
    regression.train(dataset)
    val datums = RVFRegDataset.mkDatumsFromSvmLightResource("org/clulab/learning/regression_test.txt.gz")
    val scores = for (datum <- datums) yield {
      val err = datum.label - regression.scoreOf(datum)
      err * err
    }
    val mse = scores.sum / scores.toSeq.length.toDouble
    mse should be < 0.021
  }

  "L1LinearSVMRegression" should "have a mse < .02 in this dataset" in {
    val regression = new L1LinearSVMRegression[String](bias = false)
    val dataset = RVFRegDataset.mkRegDatasetFromSvmLightResource("org/clulab/learning/regression_train.txt.gz")
    regression.train(dataset)
    val datums = RVFRegDataset.mkDatumsFromSvmLightResource("org/clulab/learning/regression_test.txt.gz")
    val scores = for (datum <- datums) yield {
      val err = datum.label - regression.scoreOf(datum)
      err * err
    }
    val mse = scores.sum / scores.toSeq.length.toDouble
    mse should be < 0.02
  }

}
