package edu.arizona.sista.swirl2

import org.slf4j.LoggerFactory

/**
 * Computes F1 score for binary classification
 * User: mihais
 * Date: 7/14/15
 */
object BinaryScorer {
  val logger = LoggerFactory.getLogger(classOf[PredicateClassifier])

  def score(output:Seq[(String, String)], posLabel:String): Unit = {
    var predicted = 0
    var total = 0
    var correct = 0
    for(o <- output) {
      if(o._1 == posLabel) total += 1
      if(o._2 == posLabel) {
        predicted += 1
        if(o._1 == o._2) correct += 1
      }
    }

    val p = correct.toDouble / predicted.toDouble
    val r = correct.toDouble / total.toDouble
    val f1 = 2 * p * r / (p + r)

    logger.info(s"Precision: $p ($correct/$predicted)")
    logger.info(s"Recall: $r ($correct/$total)")
    logger.info(s"F1: $f1")
  }
}
