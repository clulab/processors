package org.clulab.processors.clulab.sequences

import java.io.File

import jline.console.ConsoleReader
import jline.console.history.FileHistory
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.utils.StringUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Part of speech tagger 
  * Author: mihais
  * Date: 3/24/17
  */
class PartOfSpeechTagger extends SequenceTagger[String, String] {

  def featureExtractor(sentence: Sentence, offset:Int):Set[String] = {
    val words = sentence.words
    val c = new mutable.HashSet[String]()

    c += words(offset)

    if(Character.isUpperCase(words(offset)(0)))
      c += "*UP*"

    // TODO: add more features here

    c.toSet
  }

  def labelExtractor(sentence:Sentence): Array[String] = {
    // labels are the tags for this task
    assert(sentence.tags.isDefined)
    sentence.tags.get
  }
}

object PartOfSpeechTagger {
  val logger:Logger = LoggerFactory.getLogger(classOf[PartOfSpeechTagger])
  
  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    if(props.containsKey("train")) {
      val doc = ColumnsToDocument.read(props.getProperty("train"), 0, 1)
      val tagger = new PartOfSpeechTagger
      tagger.train(List(doc).iterator)

      if(props.containsKey("model")) {
        tagger.save(new File(props.getProperty("model")))
      }
    }

    if(props.containsKey("shell")) {
      assert(props.containsKey("model"))
      val tagger = new PartOfSpeechTagger
      tagger.load(new File(props.getProperty("model")))
      SequenceTaggerShell.shell[String, String](tagger)
    }
  }


}
