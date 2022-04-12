package org.clulab.processors.clu.tokenizer

import java.io.{BufferedReader, InputStreamReader}

import org.clulab.processors.Sentence

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

import SentenceSplitter._

trait SentenceSplitter {
  def split(tokens:Array[RawToken], sentenceSplit:Boolean):Array[Sentence]
}

abstract class RuleBasedSentenceSplitter extends SentenceSplitter {
  /**
    * Sentence splitting over a stream of tokens
    * This includes detection of abbreviations as well
    **/
  override def split(tokens:Array[RawToken], sentenceSplit:Boolean):Array[Sentence] = {
    val sentences = new ArrayBuffer[Sentence]()
    var raw = new ArrayBuffer[String]()
    var words = new ArrayBuffer[String]()
    var beginPositions = new ArrayBuffer[Int]()
    var endPositions = new ArrayBuffer[Int]()

    for (i <- tokens.indices) {
      val crt = tokens(i)

      // next and previous tokens. We need these to detect proper ends of sentences
      var next: Option[RawToken] = None
      if (i < tokens.length - 1) next = Some(tokens(i + 1))
      var prev: Option[RawToken] = None
      if (i > 0) prev = Some(tokens(i - 1))

      //
      // we handle end-of-sentence markers (periods, etc.) here
      // this includes detecting if a period belongs to the previous token (if it's an abbreviation)
      // and understanding if this token actually marks the end of a sentence
      //
      if (EOS.findFirstIn(crt.word).isDefined) {
        // found a token that normally indicates end of sentence
        var isEos = sentenceSplit

        // period that probably belongs to an abbreviation and should not be marked as EOS
        if (crt.word == "." && prev.isDefined && isAbbreviation(prev.get.word) && crt.beginPosition == prev.get.endPosition) {
          // found a period that should be attached to the previous abbreviation
          endPositions(endPositions.size - 1) = crt.endPosition
          words(words.size - 1) = words.last + crt.word
          raw(raw.size - 1) = raw.last + crt.raw

          // this is not an end of sentence if the next token does NOT look like the start of a sentence
          // TODO: maybe this should be handled with a binary classifier instead?
          if (isEos && next.isDefined && !isSentStart(next.get.word)) {
            isEos = false
          }
        }

        // regular end-of-sentence marker; treat is a distinct token
        else {
          raw += crt.raw
          words += crt.word
          beginPositions += crt.beginPosition
          endPositions += crt.endPosition
        }

        // found a valid end of sentence; start an empty one
        if (isEos) {
          sentences += Sentence(raw.toArray, beginPositions.toArray, endPositions.toArray, words.toArray)
          raw = new ArrayBuffer[String]()
          words = new ArrayBuffer[String]()
          beginPositions = new ArrayBuffer[Int]()
          endPositions = new ArrayBuffer[Int]()
        }
      }

      // found a period *inside* a token; sometimes this is an EOS
      else if(EOS_FOLLOWEDBY_BULLET.findFirstIn(crt.raw).isDefined &&
        crt.raw.lastIndexOf('.') > 0 &&
        next.isDefined && isSentStart(next.get.word)) {
        //println(s"FOUND EOS INSIDE TOKEN: ${crt.raw}")

        //
        // create the last token from the token fragment before the period, and the period itself
        //
        val dotRawPosition = crt.raw.lastIndexOf('.')
        assert(dotRawPosition > 0)
        val dotWordPosition = crt.word.lastIndexOf('.')
        assert(dotWordPosition > 0)

        raw += crt.raw.substring(0, dotRawPosition)
        words += crt.word.substring(0, dotWordPosition)
        beginPositions += crt.beginPosition
        endPositions += crt.beginPosition + dotRawPosition

        // This is just for the period with length of 1.
        raw += crt.raw.substring(dotRawPosition, dotRawPosition + 1)
        words += crt.word.substring(dotWordPosition, dotWordPosition + 1)
        beginPositions += endPositions.last
        endPositions += beginPositions.last + 1
        val lastPosition = endPositions.last

        //
        // create the current sentence
        //
        sentences += Sentence(raw.toArray, beginPositions.toArray, endPositions.toArray, words.toArray)
        raw = new ArrayBuffer[String]()
        words = new ArrayBuffer[String]()
        beginPositions = new ArrayBuffer[Int]()
        endPositions = new ArrayBuffer[Int]()

        //
        // add the part of the token after the period to the new sentence
        //
        raw += crt.raw.substring(dotRawPosition + 1)
        words += crt.word.substring(dotWordPosition + 1)
        beginPositions += lastPosition
        endPositions += lastPosition + raw.head.length
      }

      else {
        // just a regular token
        raw += crt.raw
        words += crt.word
        beginPositions += crt.beginPosition
        endPositions += crt.endPosition
      }
    }

    // a few words left over at the end
    if (words.nonEmpty) {
      sentences += Sentence(raw.toArray, beginPositions.toArray, endPositions.toArray, words.toArray)
    }

    sentences.toArray
  }

  def isAbbreviation(word:String):Boolean

  def isSentStart(word:String):Boolean
}

/**
  * Splits a sequence of tokens into sentences
  */
class EnglishSentenceSplitter extends RuleBasedSentenceSplitter {

  override def isAbbreviation(word:String):Boolean = {
    IS_ENGLISH_ABBREVIATION.findFirstIn(word).isDefined
  }

  override def isSentStart(word:String):Boolean = {
    IS_ENGLISH_SENTSTART.findFirstIn(word).isDefined
  }
}

/**
  * Splits a sequence of Portuguese tokens into sentences
  */
class PortugueseSentenceSplitter extends RuleBasedSentenceSplitter {

  override def isAbbreviation(word:String):Boolean = {
    IS_PORTUGUESE_ABBREVIATION.findFirstIn(word).isDefined
  }

  override def isSentStart(word:String):Boolean = {
    IS_PORTUGUESE_SENTSTART.findFirstIn(word).isDefined
  }
}

/**
  * Splits a sequence of Spanish tokens into sentences
  */
class SpanishSentenceSplitter extends RuleBasedSentenceSplitter {

  override def isAbbreviation(word:String):Boolean = {
    IS_SPANISH_ABBREVIATION.findFirstIn(word).isDefined
  }

  override def isSentStart(word:String):Boolean = {
    IS_SPANISH_SENTSTART.findFirstIn(word).isDefined
  }
}

object SentenceSplitter {
  val EOS: Regex = """^[\.!\?\s]+$""".r

  val EOS_FOLLOWEDBY_BULLET = """\.\d+$""".r

  val IS_ENGLISH_ABBREVIATION: Regex = loadDictionary("org/clulab/processors/clu/tokenizer/english.abbreviations")
  val IS_ENGLISH_SENTSTART: Regex = loadDictionary("org/clulab/processors/clu/tokenizer/english.sentstarts")
  val IS_PORTUGUESE_ABBREVIATION: Regex = loadDictionary("org/clulab/processors/clu/tokenizer/portuguese.abbreviations")
  val IS_PORTUGUESE_SENTSTART: Regex = loadDictionary("org/clulab/processors/clu/tokenizer/portuguese.sentstarts")
  val IS_SPANISH_ABBREVIATION: Regex = loadDictionary("org/clulab/processors/clu/tokenizer/spanish.abbreviations")
  val IS_SPANISH_SENTSTART: Regex = loadDictionary("org/clulab/processors/clu/tokenizer/spanish.sentstarts")

  /** Reads all words in the given dictionary and converts them into a single disjunction regex for efficiency */
  private def loadDictionary(rn:String): Regex = {
    val is = SentenceSplitter.getClass.getClassLoader.getResourceAsStream(rn)
    assert(is != null, s"Failed to find resource $rn in the classpath!")
    val reader = new BufferedReader(new InputStreamReader(is))
    val regex = new StringBuilder
    regex.append("^(")

    var done = false
    var first = true
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else if(! line.startsWith("#")) { // skip comments
        if(! first) regex.append("|")
        regex.append(normalizeSpecialChars(line.trim))
        first = false
      }
    }

    regex.append(")$")
    reader.close()
    regex.toString.r
  }

  private def normalizeSpecialChars(s:String):String = {
    var n = s.replaceAll("\\.", "\\\\.")
    n = n.replaceAll("\\-", "\\\\-")
    n
  }
}
