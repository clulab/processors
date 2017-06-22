package org.clulab.processors.clulab.sequences

import java.io.File

import org.clulab.processors.Sentence
import org.clulab.utils.StringUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/**
  * Part of speech tagger 
  * Author: mihais
  * Date: 3/24/17
  */
class PartOfSpeechTagger extends SequenceTagger[String, String] {

  def featureExtractor(sentence: Sentence, offset:Int):Set[String] = {
    val features = new mutable.HashSet[String]()
    val fe = new FeatureExtractor(sentence, allowable, offset, features)

    for(offset <- List(-2, -1, 0, 1, 2)) {
      fe.word(offset)
      fe.lemma(offset)
      fe.casing(offset)
      fe.suffixes(offset, 1, 3)
      fe.allowable(offset)
    }

    // TODO: add bigrams

    features.toSet
  }

  def labelExtractor(sentence:Sentence): Array[String] = {
    // labels are the tags for this task
    assert(sentence.tags.isDefined)
    sentence.tags.get
  }
}

object PartOfSpeechTagger {
  val logger:Logger = LoggerFactory.getLogger(classOf[PartOfSpeechTagger])

  val DEFAULT_MODEL_RESOURCE = "org/clulab/processors/clu/pos.dat"

  def loadFromFile(fn:String): PartOfSpeechTagger = {
    val tagger = new PartOfSpeechTagger
    tagger.loadFromFile(new File(fn))
    tagger
  }

  def loadFromResource(rn:String): PartOfSpeechTagger = {
    val tagger = new PartOfSpeechTagger
    tagger.loadFromResource(rn)
    tagger
  }
  
  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    if(props.containsKey("train")) {
      val doc = ColumnsToDocument.read(props.getProperty("train"), 0, 1)
      val tagger = new PartOfSpeechTagger
      tagger.preprocess(List(doc).iterator)
      tagger.train(List(doc).iterator)

      if(props.containsKey("model")) {
        tagger.save(new File(props.getProperty("model")))
      }
    }

    if(props.containsKey("model")) {
      val tagger = loadFromFile(props.getProperty("model"))

      if(props.containsKey("shell")) {
        SequenceTaggerShell.shell[String, String](tagger)
      } else if(props.containsKey("test")) {
        val doc = ColumnsToDocument.read(props.getProperty("test"), 0, 1)
        (new SequenceTaggerEvaluator[String, String]).accuracy(tagger, List(doc).iterator)
      }
    }

  }

}
