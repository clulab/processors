package org.clulab.processors.apps

import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.processors.clu.BalaurProcessor
import org.clulab.scala.WrappedArrayBuffer._
import org.clulab.utils.WrappedArraySeq
import org.slf4j.{Logger, LoggerFactory}

import java.io.InputStream
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

class ColumnsToDocument

/**
  * Converts the CoNLLX column-based format to our Document by reading only words and POS tags
  * Created by mihais on 6/8/17.
  * Last Modified: Fix compiler issue: import scala.io.Source.
  */
object ColumnsToDocument {
  type LabelSetter = (Sentence, Seq[String]) => Sentence
  type Annotator = (Document) => Document
  val logger:Logger = LoggerFactory.getLogger(classOf[ColumnsToDocument])

  val WORD_POS_CONLLX = 1
  val TAG_POS_CONLLX = 4
  val WORD_POS_CONLLU = 1
  val TAG_POS_CONLLU = 3

  var proc: Processor = new BalaurProcessor()
  var prevLang: String = "en"

  protected def setProcessor(lang: String): Unit = {
    if (lang != prevLang) {
      if (lang == "pt") {
        println("Using Portuguese processors")
        throw new RuntimeException(s"ERROR: language '$lang' not supported!")
        //this.proc = new PortugueseCluProcessor()
      }
      else if (lang == "es") {
        println("Using Spanish processors")
        //this.proc = new SpanishCluProcessor()
        throw new RuntimeException(s"ERROR: language '$lang' not supported!")
      }
      else {
        println("Using English processors")
        this.proc = new BalaurProcessor()
      }
      this.prevLang = lang
    }
  }

  def readFromFile(
    fn: String,
    wordPos: Int = WORD_POS_CONLLX,
    labelPos: Int = TAG_POS_CONLLX,
    setLabels: LabelSetter,
    annotate: Annotator,
    filterOutContractions: Boolean = false,
    lang: String = "en"
  ): Document = {
    setProcessor(lang)
    Using.resource(Source.fromFile(fn)) { source =>
      readFromSource(source, wordPos, labelPos, setLabels, annotate, filterOutContractions)
    }
  }

  def readFromStream(
    stream: InputStream,
    wordPos: Int = WORD_POS_CONLLX,
    labelPos: Int = TAG_POS_CONLLX,
    setLabels: LabelSetter,
    annotate: Annotator,
    filterOutContractions: Boolean = false,
    lang: String = "en"
  ): Document = {
    setProcessor(lang)
    Using.resource(Source.fromInputStream(stream)) { source =>
      readFromSource(source, wordPos, labelPos, setLabels, annotate, filterOutContractions)
    }
  }

  def readFromSource(
    source: Source,
    wordPos: Int,
    labelPos: Int,
    setLabels: LabelSetter,
    annotate: Annotator,
    filterOutContractions:Boolean
  ): Document = {
    val words = new ArrayBuffer[String]()
    val startOffsets = new ArrayBuffer[Int]()
    val endOffsets = new ArrayBuffer[Int]()
    val labels = new ArrayBuffer[String]()
    val sentences = new ArrayBuffer[Sentence]()
    var charOffset = 0

    def mkSentence(): Sentence = {
      val wordsSeq = new WrappedArraySeq(words.toArray).toImmutableSeq
      val unlabeledSentence = new Sentence(wordsSeq, startOffsets, endOffsets, wordsSeq)

      words.clear()
      startOffsets.clear()
      endOffsets.clear()

      val labeledSentence = setLabels(unlabeledSentence, labels.toSeq)

      labels.clear()
      labeledSentence
    }

    for (line <- source.getLines()) {
      val l = line.trim
      if (l.isEmpty) {
        // end of sentence
        if (words.nonEmpty) {
          sentences += mkSentence()
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
        if (!filterOutContractions || ! offset.contains("-")) {
          words += bits(wordPos)
          labels += bits(labelPos)
          startOffsets += charOffset
          charOffset = bits(wordPos).length
          endOffsets += charOffset
          charOffset += 1
        } else {
          // println("Skipped line: " + l)
        }
      }
    }
    if (words.nonEmpty)
      sentences += mkSentence()
    logger.debug(s"Loaded ${sentences.size} sentences.")

    val unannotatedSentence = new Document(sentences)
    val annotatedSentence = annotate(unannotatedSentence)

    annotatedSentence
  }

  def annotateNil(document: Document): Document = document
}
