package org.clulab.processors.clu.sequences

import java.io.File

import org.clulab.processors.Sentence
import org.clulab.sequences.{BiMEMMSequenceTagger, ColumnsToDocument, SequenceTaggerEvaluator, SequenceTaggerShell}
import org.clulab.struct.Counter
import org.clulab.utils.StringUtils
import org.slf4j.{Logger, LoggerFactory}

/**
  * Part of speech tagger using a MEMM architecture
  * Author: mihais
  * Date: 3/24/17
  */
class PartOfSpeechTagger() extends BiMEMMSequenceTagger[String, String]() {

  def featureExtractor(features:Counter[String], sentence: Sentence, offset:Int) = {
    val fe = new FeatureExtractor(sentence, offset, features)

    for(offset <- List(-2, -1, 0, 1, 2)) {
      fe.word(offset)
      fe.lemma(offset)
      fe.casing(offset)
      fe.suffixes(offset, 1, 3)
      fe.prefixes(offset, 1, 3)
      fe.features(offset)
    }

    fe.wordBigrams(0, 2)
    fe.wordBigrams(1, 2)
  }

  def labelExtractor(sentence:Sentence): Array[String] = {
    // labels are the tags for this task
    assert(sentence.tags.isDefined)
    sentence.tags.get
  }

  def mkFeatAtHistory(position:Int, prefix:String, label:String):String = s"${prefix}h$position:$label}"
  def mkFeatAtBeginSent(position:Int, prefix:String):String = s"${prefix}h$position:<s>"
  def mkFeatAtEndSent(position:Int, prefix:String):String = s"${prefix}h$position:</s>"
}

object PartOfSpeechTagger {
  val logger:Logger = LoggerFactory.getLogger(classOf[PartOfSpeechTagger])

  def loadFromFile(fn:String): PartOfSpeechTagger = {
    val tagger = new PartOfSpeechTagger
    tagger.loadFromFile(new File(fn))
    tagger
  }

  def loadFromResource(rn:String): PartOfSpeechTagger = {
    val tagger = new PartOfSpeechTagger
    logger.debug(s"Using model for POS tagging: $rn")
    tagger.loadFromResource(rn)
    tagger
  }
  
  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    if(props.containsKey("train")) {
      val doc = ColumnsToDocument.readFromFile(props.getProperty("train"), wordPos = 0, tagPos = 1)
      val tagger = new PartOfSpeechTagger // a single-pass model is sufficient for POS tagging

      if(props.containsKey("order")) {
        tagger.order = props.getProperty("order").toInt
      }

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
        val doc = ColumnsToDocument.readFromFile(props.getProperty("test"))
        new SequenceTaggerEvaluator[String, String].accuracy(tagger, List(doc).iterator)
      }
    }

  }

}
