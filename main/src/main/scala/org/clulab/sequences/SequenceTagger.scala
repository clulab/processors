package org.clulab.sequences

import java.io.{BufferedReader, File, FileInputStream, InputStream}

import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Counter
import org.clulab.utils.Files

/**
  * Trait for all sequence taggers
  * User: mihais
  * Date: 8/25/17
  */
trait SequenceTagger[L, F] extends Tagger[L] {
  def train(docs:Iterator[Document]): Unit

  def classesOf(sentence: Sentence):Array[L]

  /** Abstract method that generates the features for the word at the position offset in the given sentence */
  def featureExtractor(features:Counter[F], sentence: Sentence, offset:Int): Unit

  /** Abstract method that extracts the training labels for a given sentence */
  def labelExtractor(sentence:Sentence): Array[L]

  override def find(sentence: Sentence): Array[L] = classesOf(sentence)

  def save(fn:File): Unit

  def loadFromFile(fn:File): Unit = {
    val is = Files.loadFile(fn)
    load(is)
  }

  def loadFromResource(rn:String): Unit = {
    val is = Files.loadStreamFromClasspath(rn)
    load(is)
  }

  def load(is:BufferedReader): Unit

  def addHistoryFeatures(features:Counter[F], order:Int, labels:Seq[L], offset:Int):Unit = {
    addLeftFeatures(features, order, "", labels, offset)
  }

  def addFirstPassFeatures(features:Counter[F], order:Int, labels:Seq[L], offset:Int):Unit = {
    //addLeftFeatures(features, order, "fp", labels, offset)
    //addRightFeatures(features, order, "fp", labels, offset)
    features += mkFeatAtHistory(0, "fp", labels(offset))
  }

  def addLeftFeatures(features:Counter[F], order:Int, prefix:String, labels:Seq[L], offset:Int):Unit = {
    var reachedBos = false
    for(o <- 1 to order if ! reachedBos) {
      if(offset - o >= 0) {
        features += mkFeatAtHistory(- o, prefix, labels(offset - o))
      } else {
        features += mkFeatAtBeginSent(o, prefix)
        reachedBos = true
      }
    }
  }

  def addRightFeatures(features:Counter[F], order:Int, prefix:String, labels:Seq[L], offset:Int):Unit = {
    var reachedEos = false
    for(o <- 1 to order if ! reachedEos) {
      if(offset + o < labels.size) {
        features += mkFeatAtHistory(o, prefix, labels(offset + o))
      } else {
        features += mkFeatAtEndSent(o, prefix)
        reachedEos = true
      }
    }
  }
  
  def mkFeatAtHistory(position:Int, prefix:String, label:L):F
  def mkFeatAtBeginSent(position:Int, prefix:String):F
  def mkFeatAtEndSent(position:Int, prefix:String):F
}

object SequenceTaggerLoader
