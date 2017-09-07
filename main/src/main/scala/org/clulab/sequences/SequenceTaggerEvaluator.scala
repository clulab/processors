package org.clulab.sequences

import org.clulab.processors.Document
import org.clulab.sequences.SequenceTaggerEvaluator._
import org.slf4j.{Logger, LoggerFactory}

/**
  * Implements evaluation of a sequence tagger
  * Created by mihais on 6/8/17.
  */
class SequenceTaggerEvaluator[L, F] {
  def accuracy(tagger:SequenceTagger[L, F], docs:Iterator[Document]): Double = {
    var correct = 0
    var total = 0
    for(doc <- docs; sentence <- doc.sentences) {
      val goldLabels = tagger.labelExtractor(sentence)
      val predLabels = tagger.classesOf(sentence)
      assert(goldLabels.size == predLabels.size)

      total += goldLabels.size
      for(i <- goldLabels.indices)
        if(goldLabels(i) == predLabels(i))
          correct += 1
    }

    val acc = 100.0 * correct.toDouble / total
    logger.info(s"Accuracy = $acc ($correct/$total)")
    acc
  }
}

object SequenceTaggerEvaluator {
  val logger:Logger = LoggerFactory.getLogger(classOf[SequenceTaggerEvaluator[String, String]])
}
