package org.clulab.processors.clu.sequences

import java.io.{File, FileInputStream, InputStream}

import org.clulab.processors.{Document, Sentence}

/**
  * Trait for all sequence taggers
  * User: mihais
  * Date: 8/25/17
  */
trait SequenceTagger[L, F] {
  def train(docs:Iterator[Document])

  def classesOf(sentence: Sentence):List[L]

  /** Abstract method that generates the features for the word at the position offset in the given sentence */
  def featureExtractor(sentence: Sentence, offset:Int):Set[F]

  /** Abstract method that extracts the training labels for a given sentence */
  def labelExtractor(sentence:Sentence): Array[L]

  def save(fn:File)

  def loadFromFile(fn:File) {
    val is = new FileInputStream(fn)
    load(is)
  }

  def loadFromResource(rn:String) {
    val is = SequenceTaggerLoader.getClass.getClassLoader.getResourceAsStream(rn)
    load(is)
  }

  def load(is:InputStream)
}

object SequenceTaggerLoader