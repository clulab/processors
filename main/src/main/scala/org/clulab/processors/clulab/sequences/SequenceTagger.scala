package org.clulab.processors.clulab.sequences

import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Counter
import org.slf4j.LoggerFactory

/**
  * Generic sequence tagger over words implemented using the mallet CRF
  * Author: mihais
  * Date: 3/24/17
  */
abstract class SequenceTagger[L, F] {
  def verbose = false

  def train(docs:Seq[Document]) {
    for(doc <- docs; sentence <- doc.sentences) {
      val labels = labelExtractor(sentence)
      val features = (0 until sentence.size).map(featureExtractor(sentence, _)).toArray
      if(verbose) {
        println("Labels: " + labels.mkString(", "))
        println("Features:")
        for (f <- features)
          println("\tFeatures: " + f)
      }
    }
  }

  def classesOf(sentence: Sentence):Array[L] = {
    // TODO
    null
  }

  /** Abstract method that generates the features for a given sentence */
  def featureExtractor(sentence: Sentence, offset:Int):Counter[F]

  /** Abstract method that extracts the training labels for a given sentence */
  def labelExtractor(sentence:Sentence): Array[L]
}

object SequenceTagger {
  val logger = LoggerFactory.getLogger(classOf[PartOfSpeechTagger])

  
}
