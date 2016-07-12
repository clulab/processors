package org.clulab.swirl2

import org.clulab.processors.Document
import org.clulab.struct.Counter
import org.slf4j.LoggerFactory

/**
  * Utilities for the swirl2 project
  * User: mihais
  * Date: 3/20/16
  */
class Utils

object Utils {
  val logger = LoggerFactory.getLogger(classOf[Utils])

  def countLemmas(doc:Document, unknownThreshold:Int): Counter[String] = {
    val lc = new Counter[String]
    for(s <- doc.sentences) {
      for(l <- s.lemmas.get) {
        lc.incrementCount(l)
      }
    }
    var avgLen = 0.0
    for (l <- lc.keySet) {
      avgLen += l.length
    }
    avgLen /= lc.size
    logger.debug(s"Found ${lc.size} unique lemmas in the training dataset, with an avg length of $avgLen.")
    var count = 0
    for(l <- lc.keySet) {
      if(lc.getCount(l) > unknownThreshold)
        count += 1
    }
    logger.debug(s"$count of these lemmas will be kept as such. The rest will mapped to Unknown.")
    lc
  }


}
