package org.clulab.processors.clulab.sequences

import cc.mallet.pipe.Pipe
import cc.mallet.types.{Alphabet, Instance, LabelAlphabet}
import org.clulab.processors.{Document, Sentence}
import org.slf4j.{Logger, LoggerFactory}

/**
  * Generic sequence tagger over words implemented using the mallet CRF
  * Author: mihais
  * Date: 3/24/17
  */
abstract class SequenceTagger[L, F] {
  def verbose = true

  def train(docs:Seq[Document]) {
    for(doc <- docs; sentence <- doc.sentences) {
      // labels and features for one sentence
      val labels = labelExtractor(sentence)
      val features = (0 until sentence.size).map(featureExtractor(sentence, _)).toArray
      val slf = new SentenceLabelsFeatures(labels, features)
      if(verbose) println(slf)

      // convert to mallet features and labels
      
    }
  }

  def classesOf(sentence: Sentence):Array[L] = {
    // TODO
    null
  }

  /** Abstract method that generates the features for a given sentence */
  def featureExtractor(sentence: Sentence, offset:Int):Set[F]

  /** Abstract method that extracts the training labels for a given sentence */
  def labelExtractor(sentence:Sentence): Array[L]
}

class SentenceLabelsFeatures[L, F] (val labels: Array[L], val features:Array[Set[F]]) {
  override def toString: String = {
    val b = new StringBuilder
    b.append(labels.mkString(", "))
    b.append("\n")
    b.append(features.mkString(", "))
    b.append("\n")
    b.toString()
  }
}

class ToFeatureVector extends Pipe(new Alphabet(), new LabelAlphabet())  {
  override def pipe(carrier: Instance):Instance = {
    // TODO
    null
  }
}

object SequenceTagger {
  val logger:Logger = LoggerFactory.getLogger(classOf[PartOfSpeechTagger])

  
}
