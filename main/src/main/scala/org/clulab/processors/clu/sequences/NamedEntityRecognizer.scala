package org.clulab.processors.clu.sequences

import java.io.File

import org.clulab.processors.Sentence
import org.clulab.sequences.{BiMEMMSequenceTagger, ColumnsToDocument, SequenceTaggerEvaluator}
import org.clulab.struct.Counter
import org.clulab.utils.StringUtils
import org.slf4j.{Logger, LoggerFactory}

/**
  * NER using a MEMM architecture
  * @author Mihai
  */
class NamedEntityRecognizer extends BiMEMMSequenceTagger[String, String]() {
  def featureExtractor(features:Counter[String], sentence: Sentence, offset:Int) = {
    val fe = new FeatureExtractor(sentence, offset, features)

    for(offset <- List(-2, -1, 0, 1, 2)) {
      fe.word(offset)
      fe.lemma(offset)
      fe.tag(offset)
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
    assert(sentence.entities.isDefined)
    sentence.entities.get
  }

  def mkFeatAtHistory(position:Int, prefix:String, label:String):String = s"${prefix}h$position:$label}"
  def mkFeatAtBeginSent(position:Int, prefix:String):String = s"${prefix}h$position:<s>"
  def mkFeatAtEndSent(position:Int, prefix:String):String = s"${prefix}h$position:</s>"
}

object NamedEntityRecognizer {
  val logger:Logger = LoggerFactory.getLogger(classOf[NamedEntityRecognizer])

  def loadFromFile(fn:String): NamedEntityRecognizer = {
    val tagger = new NamedEntityRecognizer
    tagger.loadFromFile(new File(fn))
    tagger
  }

  def loadFromResource(rn:String): NamedEntityRecognizer = {
    val tagger = new NamedEntityRecognizer
    logger.debug(s"Using model for NER: $rn")
    tagger.loadFromResource(rn)
    tagger
  }

  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    if(props.containsKey("train")) {
      val doc = ColumnsToDocument.readFromFile(props.getProperty("train"),
        wordPos = 0, labelPos = 3,
        ColumnsToDocument.setEntities,
        ColumnsToDocument.annotateLemmmaTags)
      val tagger = new NamedEntityRecognizer

      // how many folds to use in the first pass, for a bi-directional model
      // if undefined, it uses a single pass MEMM
      if(props.containsKey("bi")) {
        tagger.numFoldsFirstPass = props.getProperty("bi").toInt
      }

      // length of label history
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

      if(props.containsKey("test")) {
        val doc = ColumnsToDocument.readFromFile(props.getProperty("test"),
          wordPos = 0, labelPos = 3,
          ColumnsToDocument.setEntities,
          ColumnsToDocument.annotateLemmmaTags)
        new SequenceTaggerEvaluator[String, String].accuracy(tagger, List(doc).iterator)
      }
    }

  }
}
