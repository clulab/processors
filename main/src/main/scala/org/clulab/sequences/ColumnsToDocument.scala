package org.clulab.sequences

import java.io.InputStream

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.{Document, Processor, Sentence}
import org.slf4j.{Logger, LoggerFactory}

class ColumnsToDocument

/**
  * Converts the CoNLLX column-based format to our Document by reading only words and POS tags
  * Created by mihais on 6/8/17.
  * Last Modified: Fix compiler issue: import scala.io.Source.
  */
object ColumnsToDocument {
  val logger:Logger = LoggerFactory.getLogger(classOf[ColumnsToDocument])

  val WORD_POS_CONLLX = 1
  val TAG_POS_CONLLX = 4

  val proc = new CluProcessor()

  def readFromFile(fn:String, wordPos:Int = WORD_POS_CONLLX, tagPos:Int = TAG_POS_CONLLX): Document = {
    val source = Source.fromFile(fn)
    readFromSource(source, wordPos, tagPos)
  }

  def readFromStream(stream:InputStream, wordPos:Int = WORD_POS_CONLLX, tagPos:Int = TAG_POS_CONLLX): Document = {
    val source = Source.fromInputStream(stream)
    readFromSource(source, wordPos, tagPos)
  }

  def readFromSource(source:Source, wordPos:Int, tagPos:Int): Document = {
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
        if (bits.length < 2)
          throw new RuntimeException(s"ERROR: invalid line [$l]!")
        words += bits(wordPos)
        tags += in(bits(tagPos))
        startOffsets += charOffset
        charOffset = bits(wordPos).length
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
    logger.debug(s"Loaded ${sentences.size} sentences.")

    val d = new Document(sentences.toArray)
    annotate(d)

    d
  }

  def annotate(doc:Document) {
    proc.lemmatize(doc) // some features use lemmas, which are not available in the CoNLL data
  }

  private def in(s:String):String = Processor.internString(s)
}
