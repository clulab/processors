package org.clulab.sequences

import java.io.PrintWriter

import org.clulab.processors.Document
import org.clulab.sequences.SequenceTaggerEvaluator._
import org.slf4j.{Logger, LoggerFactory}

/**
  * Implements evaluation of a sequence tagger
  * Created by mihais on 6/8/17.
  */
class SequenceTaggerEvaluator[L, F] {
  def accuracy(tagger:SequenceTagger[L, F], docs:Iterator[Document], saveOutput:Boolean = true): Double = {
    val pw:Option[PrintWriter] =
      if(saveOutput) Some(new PrintWriter("output_for_conlleval.txt"))
      else None
    var correct = 0
    var total = 0
    for(doc <- docs; sentence <- doc.sentences) {
      val goldLabels = tagger.labelExtractor(sentence)
      val predLabels = tagger.classesOf(sentence)
      assert(goldLabels.size == predLabels.size)

      for(i <- 0 until sentence.size) {
        val tag =
          if(sentence.tags.isDefined) sentence.tags.get(i)
          else "X"

        if(pw.isDefined) pw.get.println(s"${sentence.words(i)} $tag ${goldLabels(i)} ${predLabels(i)}")
      }
      if(pw.isDefined) pw.get.println()

      total += goldLabels.size
      for(i <- goldLabels.indices)
        if(goldLabels(i) == predLabels(i))
          correct += 1
    }

    if(pw.isDefined) {
      logger.info("Scorable file in the CoNLL format saved to file: output_for_conlleval.txt")
      pw.get.close()
    }
    val acc = 100.0 * correct.toDouble / total
    logger.info(s"Accuracy = $acc ($correct/$total)")
    acc
  }
}

object SequenceTaggerEvaluator {
  val logger:Logger = LoggerFactory.getLogger(classOf[SequenceTaggerEvaluator[String, String]])
}
