package org.clulab.learning

import org.scalatest._
import libsvm._

import org.clulab.struct.Counter
import org.clulab.TestUtils._

/**
 *
 * User: mihais, dfried
 * Date: 11/17/13
 */
class TestLibSVMClassifier extends FlatSpec with Matchers {
  "LibSVMClassifier" should "work with RVFDataset" in {
    val classifier = new LibSVMClassifier[String, String](LinearKernel)
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

    //val probs = classifier.scoresOf(dn)
    //println("Probability for class +: " + probs.getCount("+"))
    //println("Probability for class -: " + probs.getCount("-"))
    //println("Probability for class ~: " + probs.getCount("~"))
    //"+ should have higher probability than -", probs.getCount("+") > probs.getCount("-"))
    //"- should have higher probability than ~", probs.getCount("-") > probs.getCount("~"))
  }

  it should "work with BVFDataset" in {
    println("testBVFClassifier")
    val classifier = new LibSVMClassifier[String, String](LinearKernel)

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
    val dataset = new BVFDataset[String, String]()
    val d1 = new BVFDatum[String, String]("+", List("good", "great", "good"))
    val d2 = new BVFDatum[String, String]("-", List("bad", "awful"))
    val d3 = new BVFDatum[String, String]("~", List("meh", "soso"))

    dataset += d1
    dataset += d2
    dataset += d3
    classifier.train(dataset)

    val dn = new BVFDatum[String, String]("+", List("good", "great", "bad", "new"))
    classifier.classOf(d1) should be(d1.label)
    classifier.classOf(d2) should be(d2.label)
    classifier.classOf(dn) should be(dn.label)

    val probs = classifier.scoresOf(dn)
    //println("Probability for class +: " + probs.getCount("+"))
    //println("Probability for class -: " + probs.getCount("-"))
    //println("Probability for class ~: " + probs.getCount("~"))
    //println(s"our probs: ${probs.toShortString}")
    // probs.getCount("+") should be > probs.getCount("-")
    // probs.getCount("-") should be > probs.getCount("~")

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
    y(0) = 0
    y(1) = 1
    y(2) = 2
    val problem = new svm_problem
    problem.l = 3
    problem.y = y
    problem.x = new Array[Array[svm_node]](3)
    problem.x(0) = ld1
    problem.x(1) = ld2
    problem.x(2) = ld3
    //println("before train")
    val model = svm.svm_train(problem, classifier.parameters)
    //println("after train")
    val ldn = new Array[svm_node](4)
    ldn(0) = new svm_node { index = 1; value =  1 }
    ldn(1) = new svm_node { index = 2; value =  1 }
    ldn(2) = new svm_node { index = 3; value =  1 }
    ldn(3) = new svm_node { index = 10; value =  1 }
    val lprobs = new Array[Double](model.nr_class)
    svm.svm_predict_probability(model, ldn, lprobs)
    val probabilities = new Counter[String]
    for(i <- 0 until model.nr_class) {
      probabilities.setCount(dataset.labelLexicon.get(model.label(i)), lprobs(i))
    }
    //println(s"probs calculated by wrapper: ${probs.toShortString}")
    //println(s"probs calculated by  libsvm: ${probs.toShortString}")
    // here we check that our wrapper and libsvm get the same scores
    probs.getCount("+") should be(probabilities.getCount("+"))
    probs.getCount("-") should be(probabilities.getCount("-"))
    probs.getCount("~") should be(probabilities.getCount("~"))

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

  it should "have an accuracy > .60 on this dataset" in {
    for (kernel <- List(LinearKernel, PolynomialKernel, RBFKernel, SigmoidKernel)) {
      val classifier = new LibSVMClassifier[Int, String](kernel, degree=1)
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
      //println(s"$kernel accuracy: $acc")
      acc should be > 0.60 // polynomial has low acc w/ default params
    }
  }
}
