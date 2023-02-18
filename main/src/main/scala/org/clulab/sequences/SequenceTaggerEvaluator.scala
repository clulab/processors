package org.clulab.sequences

import org.clulab.processors.Document
import org.clulab.scala.Using._
import org.clulab.sequences.SequenceTaggerEvaluator._
import org.slf4j.{Logger, LoggerFactory}

import java.io.{PrintWriter, StringWriter}

/**
  * Implements evaluation of a sequence tagger
  * Created by mihais on 6/8/17.
  */
class SequenceTaggerEvaluator[L, F] {
  def accuracy(tagger:SequenceTagger[L, F], docs:Iterator[Document], saveOutput:Boolean = true): Double = {
    Using.resource(
      if (saveOutput) new PrintWriter("output_for_conlleval.txt")
      else new PrintWriter(new StringWriter())
    ) { pw =>
      var correct = 0
      var total = 0
      for (doc <- docs; sentence <- doc.sentences) {
        val goldLabels = tagger.labelExtractor(sentence)
        val predLabels = tagger.classesOf(sentence)
        assert(goldLabels.size == predLabels.size)

        for (i <- 0 until sentence.size) {
          val tag =
            if (sentence.tags.isDefined) sentence.tags.get(i)
            else "X"

          pw.println(s"${sentence.words(i)} $tag ${goldLabels(i)} ${predLabels(i)}")
        }
        pw.println()

        total += goldLabels.size
        for (i <- goldLabels.indices)
          if (goldLabels(i) == predLabels(i))
            correct += 1
      }

      if (saveOutput)
        logger.info("Scorable file in the CoNLL format saved to file: output_for_conlleval.txt")

      val acc = 100.0 * correct.toDouble / total
      logger.info(s"Accuracy = $acc ($correct/$total)")
      acc
    }
  }
}

object SequenceTaggerEvaluator {
  val logger:Logger = LoggerFactory.getLogger(classOf[SequenceTaggerEvaluator[String, String]])
}
