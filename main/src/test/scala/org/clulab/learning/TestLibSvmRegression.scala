package org.clulab.learning

import org.scalatest._
import libsvm._

import org.clulab.TestUtils._

/**
  * User: mihais, dfried, danebell
  * Date: 11/20/17
  */
class TestLibSvmRegression extends FlatSpec with Matchers {
  "LibSVMRegression" should "work with RVFDataset" in {
    val regression = new LibSvmRegression[String](kernelType = LinearKernel)
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
  }

  it should "work with BVFDataset" in {
    val regression = new LibSvmRegression[String](kernelType = LinearKernel)
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

    val dn = new BVFDatum[Double, String](0.8, List("good", "great", "bad", "new"))
    regression.scoreOf(d1) should be > regression.scoreOf(d2)
    regression.scoreOf(d1) should be > regression.scoreOf(d3)
    regression.scoreOf(d3) should be > regression.scoreOf(d2)
    regression.scoreOf(dn) should be > regression.scoreOf(d2)

    val prob = regression.scoreOf(dn)

    //
    // let's make sure we get the same scores if we use libsvm directly
    //
    val ld1 = new Array[svm_node](2)
    ld1(0) = new svm_node { index = 1; value =  2 }
    ld1(1) = new svm_node { index = 2; value =  1 }
    val ld2 = new Array[svm_node](2)
    ld2(0) = new svm_node { index = 3; value =  1 }
    ld2(1) = new svm_node { index = 4; value =  1 }
    val ld3 = new Array[svm_node](2)
    ld3(0) = new svm_node { index = 5; value =  1 }
    ld3(1) = new svm_node { index = 6; value =  1 }
    val y = new Array[Double](3)
    y(0) = 0.91
    y(1) = 0.13
    y(2) = 0.44
    val problem = new svm_problem
    problem.l = 3
    problem.y = y
    problem.x = new Array[Array[svm_node]](3)
    problem.x(0) = ld1
    problem.x(1) = ld2
    problem.x(2) = ld3
    //println("before train")
    val model = svm.svm_train(problem, regression.parameters)
    //println("after train")
    val ldn = new Array[svm_node](4)
    ldn(0) = new svm_node { index = 1; value =  1 }
    ldn(1) = new svm_node { index = 2; value =  1 }
    ldn(2) = new svm_node { index = 3; value =  1 }
    ldn(3) = new svm_node { index = 10; value =  1 }
    val newProb = svm.svm_predict(model, ldn)

    prob should be (newProb)

    //
    // Let's analyze the classifier learned weights here
    //
    /*
    val weights = classifier.getWeights
    for(l <- weights.keys) {
      println("Feature weights for label " + l)
      val sorted = weights.get(l).get.sorted
      for(fv <- sorted) {
        println("\t" + fv._1 + " " + fv._2)
      }
    }
    weights.get("+").get.getCount("good") should be > weights.get("+").get.getCount("great")
    weights.get("+").get.getCount("great") should be > weights.get("+").get.getCount("bad")
    weights.get("+").get.getCount("great") should be > weights.get("+").get.getCount("meh")
    */
  }

  "LibSvmEpsilonRegression" should "have an MSE < 0.02 on this dataset" in {
    val dataset = RVFRegDataset.mkRegDatasetFromSvmLightResource("org/clulab/learning/regression_train.txt.gz")
    val datums = RVFRegDataset.mkDatumsFromSvmLightResource("org/clulab/learning/regression_test.txt.gz")
    for (kernel <- List(LinearKernel, PolynomialKernel, RBFKernel, SigmoidKernel)) {
      val regression = new LibSvmEpsilonRegression[String](kernelType = kernel, degree = 1)
      regression.train(dataset)
      val scores = for (datum <- datums) yield {
        val err = datum.label - regression.scoreOf(datum)
        err * err
      }
      val mse = scores.sum / scores.toSeq.length.toDouble
      mse should be < 0.02
    }
  }

  "LibSvmNuRegression" should "have an MSE < 0.02 on this dataset" in {
    val dataset = RVFRegDataset.mkRegDatasetFromSvmLightResource("org/clulab/learning/regression_train.txt.gz")
    val datums = RVFRegDataset.mkDatumsFromSvmLightResource("org/clulab/learning/regression_test.txt.gz")
    for (kernel <- List(LinearKernel, PolynomialKernel, RBFKernel, SigmoidKernel)) {
      val regression = new LibSvmNuRegression[String](kernelType = kernel, degree = 1)
      regression.train(dataset)
      val scores = for (datum <- datums) yield {
        val err = datum.label - regression.scoreOf(datum)
        err * err
      }
      val mse = scores.sum / scores.toSeq.length.toDouble
      mse should be < 0.02
    }
  }
}
