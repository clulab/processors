package org.clulab.processors.bionlp

import edu.stanford.nlp.ling.CoreLabel
import org.clulab.processors.Sentence
import org.clulab.processors.clu.bio.BioPOSPostProcessor

/**
  * Fixes some common POS tagging mistakes in the bio domain (in place, using CoreLabel.setTag)
  * User: mihais
  * Date: 2/9/17
  */
class BioNLPPOSTaggerPostProcessor {
  val proc = new BioPOSPostProcessor

  def postprocessCoreLabelTags(tas:Array[CoreLabel]): Unit = {
    val s = toSentence(tas)
    proc.process(s)
    for(i <- s.indices) {
      tas(i).setTag(s.tags.get(i))
    }
  }

  private def toSentence(labels: Array[CoreLabel]): Sentence = {
    val words = new Array[String](labels.length)
    val startOffsets = new Array[Int](labels.length)
    val endOffsets = new Array[Int](labels.length)
    val tags = new Array[String](labels.length)
    for(i <- labels.indices) {
      words(i) = labels(i).word()
      tags(i) = labels(i).tag()
    }
    val s = new Sentence(words, startOffsets, endOffsets)
    s.tags = Some(tags)
    s
  }
}
