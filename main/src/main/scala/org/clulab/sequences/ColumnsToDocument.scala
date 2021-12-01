package org.clulab.sequences

import java.io.InputStream

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import org.clulab.processors.clu.{CluProcessor, SpanishCluProcessor, PortugueseCluProcessor}
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
  val WORD_POS_CONLLU = 1
  val TAG_POS_CONLLU = 3

  var proc:Processor = new CluProcessor()
  var prevLang: String = "en"

  def readFromFile(fn:String,
                   wordPos:Int = WORD_POS_CONLLX,
                   labelPos:Int = TAG_POS_CONLLX,
                   setLabels: (Sentence, Array[String]) => Unit,
                   annotate: (Document) => Unit,
                   filterOutContractions:Boolean = false,
                   lang: String = "en"
                  ): Document = {

    // redefine proc acording to the language used
    if (lang != prevLang) {
      if (lang == "pt") {
        println("Using Portuguese processors")
        this.proc = new PortugueseCluProcessor()
      } else if (lang == "es") {
        println("Using Spanish processors")
        this.proc = new SpanishCluProcessor()
      } else {
        println("Using English processors")
        this.proc = new CluProcessor()
      }
      this.prevLang = lang
    }

    val source = Source.fromFile(fn)

    readFromSource(source, wordPos, labelPos, setLabels, annotate, filterOutContractions)
  }

  def readFromStream(stream:InputStream,
                     wordPos:Int = WORD_POS_CONLLX,
                     labelPos:Int = TAG_POS_CONLLX,
                     setLabels: (Sentence, Array[String]) => Unit,
                     annotate: (Document) => Unit,
                     filterOutContractions:Boolean = false,
                     lang: String = "en"): Document = {

    // redefine proc acording to the language used
    if (lang == "pt"){
      println("Using Portuguese processors")
      this.proc = new PortugueseCluProcessor()
    } else if(lang == "es") {
      println("Using Spanish processors")
      this.proc = new SpanishCluProcessor()
    } else {
      println("Using English processors")
      this.proc = new CluProcessor()
    }

    val source = Source.fromInputStream(stream)
    readFromSource(source, wordPos, labelPos, setLabels, annotate, filterOutContractions)
  }

  def readFromSource(source:Source,
                     wordPos:Int,
                     labelPos:Int,
                     setLabels: (Sentence, Array[String]) => Unit,
                     annotate: (Document) => Unit,
                     filterOutContractions:Boolean): Document = {
    var words = new ArrayBuffer[String]()
    var startOffsets = new ArrayBuffer[Int]()
    var endOffsets = new ArrayBuffer[Int]()
    var labels = new ArrayBuffer[String]()
    var charOffset = 0
    val sentences = new ArrayBuffer[Sentence]()
    for(line <- source.getLines()) {
      val l = line.trim
      if (l.isEmpty) {
        // end of sentence
        if (words.nonEmpty) {
          val s = new Sentence(words.toArray, startOffsets.toArray, endOffsets.toArray, words.toArray)
          setLabels(s, labels.toArray)
          sentences += s
          words = new ArrayBuffer[String]()
          startOffsets = new ArrayBuffer[Int]()
          endOffsets = new ArrayBuffer[Int]()
          labels = new ArrayBuffer[String]()
          charOffset += 1
        }
      } else {
        // within the same sentence
        val bits = l.split("\\s+")
        if (bits.length < 2)
          throw new RuntimeException(s"ERROR: invalid line [$l]!")

        //
        // in the Portuguese dataset contractions are preserved, with positions marked with dash, e.g.:
        //   9-10	das	_	_	_	_	_	_	_	_
        // we will skip all these lines, because the solved contractions are always present afterwards, e.g.:
        //   9	de	de	ADP	INDT	_	11	case	_	_
        //   10	as	o	DET	_	Gender=Fem|Number=Plur	11	det	_	_
        //
        val offset = bits(0) // we assume token offsets are always in column 0!
        if(! filterOutContractions || ! offset.contains("-")) {
          words += bits(wordPos)
          labels += in(bits(labelPos))
          startOffsets += charOffset
          charOffset = bits(wordPos).length
          endOffsets += charOffset
          charOffset += 1
        } else {
          // println("Skipped line: " + l)
        }
      }
    }
    if(words.nonEmpty) {
      val s = new Sentence(words.toArray, startOffsets.toArray, endOffsets.toArray, words.toArray)
      s.tags = Some(labels.toArray)
      sentences += s
    }
    source.close()
    logger.debug(s"Loaded ${sentences.size} sentences.")

    val d = new Document(sentences.toArray)
    annotate(d)

    d

  }

  def setTags(s:Sentence, tags:Array[String]): Unit = {
    s.tags = Some(tags)
  }

  def setChunks(s:Sentence, chunks:Array[String]): Unit = {
    s.chunks = Some(chunks)
  }

  def setEntities(s:Sentence, entities:Array[String]): Unit = {
    s.entities = Some(entities)
  }

  def annotateLemmas(doc:Document): Unit = {
    proc.lemmatize(doc) // some features use lemmas, which are not available in the CoNLL data
  }

  def annotateLemmmaTags(doc:Document): Unit = {
    proc.lemmatize(doc)
    proc.tagPartsOfSpeech(doc)
  }

  def annotateNil(doc:Document): Unit = {}

  private def in(s:String):String = Processor.internString(s)
}
