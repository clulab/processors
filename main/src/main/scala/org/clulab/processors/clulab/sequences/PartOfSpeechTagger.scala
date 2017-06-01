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
      val doc = twoColumnToDocument(props.getProperty("train"))
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
      shell(tagger)
    }
  }

  def shell(tagger:PartOfSpeechTagger): Unit = {
    val history = new FileHistory(new File(System.getProperty("user.home"), ".posshellhistory"))
    sys addShutdownHook {
      history.flush() // flush file before exiting
    }

    val reader = new ConsoleReader
    reader.setHistory(history)

    var running = true
    while (running) {
      reader.setPrompt(">> ")
      reader.readLine match {
        case ":exit" | null =>
          running = false

        case text =>
          parse(text, tagger)
      }
    }

    // manual terminal cleanup
    reader.getTerminal.restore()
    reader.shutdown()
  }

  def parse(text:String, tagger:PartOfSpeechTagger): Unit = {
    val sent = mkSent(text)
    println("Tokens: " + sent.words.mkString(", "))
    tagger.classesOf(sent)
  }

  def mkSent(text:String): Sentence = {
    val tokens = text.split("\\s+")
    val startOffsets = new Array[Int](tokens.length)
    val endOffsets = new Array[Int](tokens.length)
    new Sentence(tokens, startOffsets, endOffsets)
  }

  private def in(s:String):String = Processor.internString(s)

  def twoColumnToDocument(fn:String): Document = {
    val source = io.Source.fromFile(fn)
    var words = new ArrayBuffer[String]()
    var startOffsets = new ArrayBuffer[Int]()
    var endOffsets = new ArrayBuffer[Int]()
    var tags = new ArrayBuffer[String]()
    var charOffset = 0
    val sentences = new ArrayBuffer[Sentence]()
    for(line <- source.getLines()) {
      val l = line.trim
      if (l.isEmpty) {
        // end of sentence
        if (words.nonEmpty) {
          val s = new Sentence(words.toArray, startOffsets.toArray, endOffsets.toArray)
          s.tags = Some(tags.toArray)
          sentences += s
          words = new ArrayBuffer[String]()
          startOffsets = new ArrayBuffer[Int]()
          endOffsets = new ArrayBuffer[Int]()
          tags = new ArrayBuffer[String]()
          charOffset += 1
        }
      } else {
        // within the same sentence
        val bits = l.split("\\s+")
        if (bits.length != 2)
          throw new RuntimeException(s"ERROR: invalid line [$l]!")
        words += bits(0)
        tags += in(bits(1))
        startOffsets += charOffset
        charOffset = bits(0).length
        endOffsets += charOffset
        charOffset += 1
      }
    }
    if(words.nonEmpty) {
      val s = new Sentence(words.toArray, startOffsets.toArray, endOffsets.toArray)
      s.tags = Some(tags.toArray)
      sentences += s
    }
    source.close()
    logger.debug(s"Loaded ${sentences.size} sentences from file $fn.")
    new Document(sentences.toArray)
  }
}
