package org.clulab.processors.clu.sequences

import java.io.{File, FileInputStream, InputStream}

import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Counter

import scala.collection.mutable

/**
  * Trait for all sequence taggers
  * User: mihais
  * Date: 8/25/17
  */
trait SequenceTagger[L, F] {
  def train(docs:Iterator[Document])

  def classesOf(sentence: Sentence):List[L]

  /** Abstract method that generates the features for the word at the position offset in the given sentence */
  def featureExtractor(features:Counter[F], sentence: Sentence, offset:Int):Set[F]

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

  def addHistoryFeatures(features:Counter[F], order:Int, labels:Seq[L], offset:Int):Unit = {
    var reachedBos = false
    for(o <- 1 to order if ! reachedBos) {
      if(offset - o >= 0) {
        features += mkFeatAtHistory(o, labels(offset - o))
      } else {
        features += mkFeatAtBeginSent(o)
        reachedBos = true
      }
    }

  }
  
  def mkFeatAtHistory(position:Int, label:L):F
  def mkFeatAtBeginSent(position:Int):F
  def mkFeatAtEndSent(position:Int):F
  def mkFeatFirstPass(label:L):F
}

object SequenceTaggerLoader