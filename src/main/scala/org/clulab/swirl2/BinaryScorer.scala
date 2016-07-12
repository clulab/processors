package org.clulab.swirl2

import org.slf4j.LoggerFactory

/**
 * Computes F1 score for binary classification
 * User: mihais
 * Date: 7/14/15
 */
object BinaryScorer {
  val logger = LoggerFactory.getLogger(classOf[PredicateClassifier])

  def score(output:Seq[(String, String)], negLabel:String): Unit = {
    var predicted = 0
    var total = 0
    var correctLabeled = 0
    var correctUnlabeled = 0
    for(o <- output) {
      if(o._1 != negLabel) total += 1
      if(o._2 != negLabel) {
        predicted += 1
        if(o._1 == o._2)
          correctLabeled += 1
        if(o._1 != negLabel && o._2 != negLabel)
          correctUnlabeled += 1
      }
    }

    val pu = correctUnlabeled.toDouble / predicted.toDouble
    val ru = correctUnlabeled.toDouble / total.toDouble
    val f1u = 2 * pu * ru / (pu + ru)

    val pl = correctLabeled.toDouble / predicted.toDouble
    val rl = correctLabeled.toDouble / total.toDouble
    val f1l = 2 * pl * rl / (pl + rl)

    logger.info(s"Labeled Precision: $pl ($correctLabeled/$predicted)")
    logger.info(s"Labeled Recall: $rl ($correctLabeled/$total)")
    logger.info(s"Labeled F1: $f1l")

    logger.info(s"Unlabeled Precision: $pu ($correctUnlabeled/$predicted)")
    logger.info(s"Unlabeled Recall: $ru ($correctUnlabeled/$total)")
    logger.info(s"Unlabeled F1: $f1u")
  }
}
