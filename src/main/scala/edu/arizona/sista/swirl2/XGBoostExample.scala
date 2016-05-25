package edu.arizona.sista.swirl2

import ml.dmlc.xgboost4j.java.{DMatrix => JDMatrix}
import ml.dmlc.xgboost4j.scala.{XGBoost, DMatrix}

import scala.collection.mutable

/**
  *
  * User: mihais
  * Date: 5/17/16
  */
object XGBoostExample {
  def main(args: Array[String]): Unit = {
    val trainMax = new DMatrix("train.dat") // "agaricus.txt.train")
    val testMax = new DMatrix("test.dat") // "agaricus.txt.test")

    val params = new mutable.HashMap[String, Any]()
    params += "eta" -> 1.0
    params += "max_depth" -> 4
    params += "silent" -> 1
    params += "objective" -> "binary:logistic"

    val watches = new mutable.HashMap[String, DMatrix]
    watches += "train" -> trainMax
    watches += "test" -> testMax

    val round = 20
    // train a model
    val booster = XGBoost.train(trainMax, params.toMap, round, watches.toMap)
    // predict
    val predicts = booster.predict(testMax)

    val err = eval(predicts, testMax)
    println(s"Error = $err")
  }

  def eval(predicts: Array[Array[Float]], dmat: DMatrix): Float = {
    var error: Float = 0f
    val labels: Array[Float] = dmat.getLabel
    val nrow: Int = predicts.length
    for (i <- 0 until nrow) {
      if (labels(i) == 0.0 && predicts(i)(0) > 0.5) {
        error += 1
      } else if (labels(i) == 1.0 && predicts(i)(0) <= 0.5) {
        error += 1
      }

      // println(s"${labels(i)} => ${predicts(i).mkString(" ")}")
    }
    error / labels.length
  }

}
