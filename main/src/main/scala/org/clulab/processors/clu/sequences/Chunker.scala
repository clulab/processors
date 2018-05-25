package org.clulab.processors.clu.sequences

import java.io.File

import org.clulab.processors.Sentence
import org.clulab.sequences.{BiMEMMSequenceTagger, ColumnsToDocument, SequenceTaggerEvaluator}
import org.clulab.struct.Counter
import org.clulab.utils.StringUtils
import org.slf4j.{Logger, LoggerFactory}

/**
  * Generates chunking (shallow syntax) labels
  */
class Chunker() extends BiMEMMSequenceTagger[String, String]() with FirstPassLabelsReader {

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
    assert(sentence.chunks.isDefined)
    sentence.chunks.get
  }

  def mkFeatAtHistory(position:Int, prefix:String, label:String):String = s"${prefix}h$position:$label}"
  def mkFeatAtBeginSent(position:Int, prefix:String):String = s"${prefix}h$position:<s>"
  def mkFeatAtEndSent(position:Int, prefix:String):String = s"${prefix}h$position:</s>"
}

object Chunker {
  val logger:Logger = LoggerFactory.getLogger(classOf[Chunker])

  def loadFromFile(fn:String): Chunker = {
    val tagger = new Chunker
    tagger.loadFromFile(new File(fn))
    tagger
  }

  def loadFromResource(rn:String): Chunker = {
    val tagger = new Chunker
    logger.debug(s"Using model for chunking: $rn")
    tagger.loadFromResource(rn)
    tagger
  }

  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    if(props.containsKey("train")) {
      val doc = ColumnsToDocument.readFromFile(props.getProperty("train"),
        wordPos = 0, labelPos = 2,
        ColumnsToDocument.setChunks,
        ColumnsToDocument.annotateLemmmaTags)
      val tagger = new Chunker

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
          wordPos = 0, labelPos = 2,
          ColumnsToDocument.setChunks,
          ColumnsToDocument.annotateLemmmaTags)
        new SequenceTaggerEvaluator[String, String].accuracy(tagger, List(doc).iterator)
      }
    }

  }

}
