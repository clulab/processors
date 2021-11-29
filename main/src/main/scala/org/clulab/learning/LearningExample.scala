package org.clulab.learning

import org.clulab.struct.Counter

/**
  * Simple example of how to use RVF data and classifiers
  * User: mihais
  * Date: 11/18/15
  */
object LearningExample {
  def main(args:Array[String]): Unit = {
    //
    // Create training dataset
    //
    val dataset = new RVFDataset[String, String]()
    val df1 = new Counter[String]()
    df1.incrementCount("great", 2.0)
    df1.incrementCount("good", 7.0)
    dataset += new RVFDatum[String, String]("positive", df1)
    val df2 = new Counter[String]()
    df2.incrementCount("funny", 2.0)
    df2.incrementCount("good", 1.0)
    df2.incrementCount("bad", 1.0)
    dataset += new RVFDatum[String, String]("positive", df2)
    val df3 = new Counter[String]()
    df3.incrementCount("horrible", 2.0)
    df3.incrementCount("good", 1.0)
    df3.incrementCount("bad", 10.0)
    dataset += new RVFDatum[String, String]("negative", df3)

    //
    // Train the classifier
    //
    val classifier = new LogisticRegressionClassifier[String, String]()
    //val classifier = new LinearSVMClassifier[String, String]()
    //val classifier = new PerceptronClassifier[String, String]()
    //eval classifier = new RandomForestClassifier[String, String]()
    classifier.train(dataset)

    //
    // Let's see what the model learns
    //
    val weights = classifier.getWeights()
    println(s"""Weights for the positive class: ${weights.get("positive")}""")
    println(s"""Weights for the negative class: ${weights.get("negative")}""")

    //
    // Predict on new data!
    //
    val df4 = new Counter[String]()
    df4.incrementCount("great", 3.0)
    df4.incrementCount("bad", 2.0)
    val predictedLabel = classifier.classOf(new RVFDatum[String, String]("unknown", df4))
    println(s"The classifier predicted $predictedLabel")

  }
}
