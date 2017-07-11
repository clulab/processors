package org.clulab.processors.clulab.sequences

import java.io.InputStream

import org.clulab.processors.clulab.CluProcessor
import org.clulab.processors.{Document, Processor, Sentence}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class ColumnsToDocument

/**
  * Converts a column CoNLL-like format to our Document
  * Created by mihais on 6/8/17.
  */
object ColumnsToDocument {
  val logger:Logger = LoggerFactory.getLogger(classOf[ColumnsToDocument])

  val proc = new CluProcessor()

  def readFromFile(fn:String, wordPos:Int, tagPos:Int): Document = {
    val source = io.Source.fromFile(fn)
    readFromSource(source, wordPos, tagPos)
  }

  def readFromStream(stream:InputStream, wordPos:Int, tagPos:Int): Document = {
    val source = io.Source.fromInputStream(stream)
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
        if (bits.length != 2)
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
