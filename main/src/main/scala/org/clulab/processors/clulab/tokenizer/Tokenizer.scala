package org.clulab.processors.clulab.tokenizer

import java.io.{BufferedReader, InputStreamReader}
import java.util.zip.GZIPInputStream

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import org.clulab.processors.Sentence

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import Tokenizer._

import scala.util.matching.Regex

/**
  *
  * User: mihais
  * Date: 3/15/17
  */
class Tokenizer {
  /** Tokenization and sentence splitting */
  def tokenize(text:String):Array[Sentence] = {
    val lexer = new OpenDomainLexer(new ANTLRInputStream(text)) // TODO: customize lexer grammar here
    val tokens = new CommonTokenStream(lexer)
    var done = false

    val rawTokens = new ArrayBuffer[RawToken]()

    // raw tokenization, using the antlr grammar
    while(! done) {
      val t = tokens.LT(1)
      if(t.getType == -1) {
        // EOF
        done = true
      } else {
        // info on the current token
        val word = t.getText
        val startOffset = t.getStartIndex
        val endOffset = t.getStopIndex + 1 // antlr is inclusive, we are exclusive

        // add to raw stream
        rawTokens ++= normalizeToken(RawToken(word, startOffset, endOffset))

        // advance to next token in stream
        tokens.consume()
      }
    }

    sentenceSplitting(rawTokens.toArray)
  }

  /** Local normalization of a given token */
  def normalizeToken(raw:RawToken): Seq[RawToken] = {
    //
    // Unlike CoreNLP, we allow single quotes inside words
    // We must separate important linguistic constructs here
    //
    // genitive
    if("""'[sS]$""".r.findFirstIn(raw.text).isDefined) {
      val tokens = new ListBuffer[RawToken]
      tokens += RawToken(raw.text.substring(0, raw.text.length - 2), raw.startOffset, raw.endOffset - 2)
      tokens += RawToken(raw.text.substring(raw.text.length - 2), raw.startOffset + raw.text.length - 2, raw.endOffset)
      return tokens
    }
    // "won't"
    if("""^[wW][oO][nN]'[tT]$""".r.findFirstIn(raw.text).isDefined) {
      val tokens = new ListBuffer[RawToken]
      tokens += RawToken("will", raw.startOffset, 2)
      tokens += RawToken("not", raw.startOffset + 2, raw.endOffset)
      return tokens
    }
    // other words ending with "n't"
    if("""[nN]'[tT]$""".r.findFirstIn(raw.text).isDefined) {
      val tokens = new ListBuffer[RawToken]
      tokens += RawToken(raw.text.substring(0, raw.text.length - 3), raw.startOffset, raw.endOffset - 3)
      tokens += RawToken("not", raw.startOffset + raw.text.length - 3, raw.endOffset)
      return tokens
    }

    List(raw)
  }

  /** Sentence splitting over a stream of tokens */
  def sentenceSplitting(tokens:Array[RawToken]):Array[Sentence] = {
    val sentences = new ArrayBuffer[Sentence]()
    var words = new ArrayBuffer[String]()
    var startOffsets = new ArrayBuffer[Int]()
    var endOffsets = new ArrayBuffer[Int]()

    for(i <- tokens.indices) {
      val crt = tokens(i)

      words += crt.text
      startOffsets += crt.startOffset
      endOffsets += crt.endOffset

      if(EOS.findFirstIn(crt.text).isDefined || isEndOfSentenceAbbreviation(tokens, i)) {
        sentences += Sentence(words.toArray, startOffsets.toArray, endOffsets.toArray)
        words = new ArrayBuffer[String]()
        startOffsets = new ArrayBuffer[Int]()
        endOffsets = new ArrayBuffer[Int]()
      }
    }

    if(words.nonEmpty) {
      sentences += Sentence(words.toArray, startOffsets.toArray, endOffsets.toArray)
    }

    sentences.toArray
  }

  def isEndOfSentenceAbbreviation(tokens:Array[RawToken], offset:Int):Boolean = false // TODO: implement me
}

case class RawToken(text:String, startOffset:Int, endOffset:Int)

object Tokenizer {
  val EOS: Regex = """^[\.!\?]+$""".r

  val IS_ENGLISH_ABBREVIATION: Regex = loadAbbreviations("org/clulab/processors/clulab/tokenizer/english.abbreviations")

  /** Reads all abbreviations and converts them into a single regex for efficiency */
  def loadAbbreviations(rn:String): Regex = {
    val is = Tokenizer.getClass.getClassLoader.getResourceAsStream(rn)
    assert(is != null, s"Failed to find resource $rn in the classpath!")
    val reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(is)))
    val regex = new StringBuilder

    var done = false
    var first = true
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else if(! line.startsWith("#")) { // skip comments
        if(! first) regex.append("|")

        first = false
      }
    }

    reader.close()
    regex.toString.r
  }

  def tokenize(text:String):Array[Sentence] = {
    val tokenizer = new Tokenizer
    tokenizer.tokenize(text)
  }
}
