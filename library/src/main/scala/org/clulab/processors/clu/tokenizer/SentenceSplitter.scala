package org.clulab.processors.clu.tokenizer

import org.clulab.processors.Sentence
import org.clulab.scala.WrappedArrayBuffer._

import java.io.{BufferedReader, InputStreamReader}
import scala.collection.compat._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.util.Using

import SentenceSplitter._

trait SentenceSplitter {
  def split(tokens:Array[RawToken], sentenceSplit:Boolean, characterOffset: Int = 0):Seq[Sentence]
}

abstract class RuleBasedSentenceSplitter extends SentenceSplitter {
  /**
    * Sentence splitting over a stream of tokens
    * This includes detection of abbreviations as well.
    * The characterOffset is included so that Sentences
    * in a longer text need not be edited afterward.
    **/
  override def split(tokens: Array[RawToken], sentenceSplit: Boolean, characterOffset: Int): Seq[Sentence] = {
    val sentences = new ArrayBuffer[Sentence]()
    var raw = new ArrayBuffer[String]()
    var words = new ArrayBuffer[String]()
    var beginPositions = new ArrayBuffer[Int]()
    var endPositions = new ArrayBuffer[Int]()

    for (i <- tokens.indices) {
      val curr: RawToken = tokens(i)
      // next and previous tokens. We need these to detect proper ends of sentences
      val nextOpt: Option[RawToken] = Option.when(i < tokens.length - 1)(tokens(i + 1))
      val prevOpt: Option[RawToken] = Option.when(i > 0)(tokens(i - 1))

      //
      // we handle end-of-sentence markers (periods, etc.) here
      // this includes detecting if a period belongs to the previous token (if it's an abbreviation)
      // and understanding if this token actually marks the end of a sentence
      //
      if (EOS.findFirstIn(curr.word).isDefined) {
        // found a token that normally indicates end of sentence
        var isEos = sentenceSplit

        // period that probably belongs to an abbreviation and should not be marked as EOS
        if (curr.word == "." && prevOpt.isDefined && isAbbreviation(prevOpt.get.word) && curr.beginPosition == prevOpt.get.endPosition) {
          // found a period that should be attached to the previous abbreviation
          endPositions(endPositions.size - 1) = curr.endPosition + characterOffset
          words(words.size - 1) = words.last + curr.word
          raw(raw.size - 1) = raw.last + curr.raw

          // this is not an end of sentence if the next token does NOT look like the start of a sentence
          // TODO: maybe this should be handled with a binary classifier instead?
          if (isEos && nextOpt.isDefined && !isSentStart(nextOpt.get.word)) {
            isEos = false
          }
        }

        // regular end-of-sentence marker; treat is a distinct token
        else {
          raw += curr.raw
          words += curr.word
          beginPositions += curr.beginPosition + characterOffset
          endPositions += curr.endPosition + characterOffset
        }

        // found a valid end of sentence; start an empty one
        if (isEos) {
          sentences += Sentence(raw, beginPositions, endPositions, words)
          raw = new ArrayBuffer[String]() // TODO: Check whether clear() is sufficient.
          words = new ArrayBuffer[String]()
          beginPositions = new ArrayBuffer[Int]()
          endPositions = new ArrayBuffer[Int]()
        }
      }

      // found a period *inside* a token; sometimes this is an EOS
      else if(EOS_FOLLOWEDBY_BULLET.findFirstIn(curr.raw).isDefined &&
        curr.raw.lastIndexOf('.') > 0 &&
        nextOpt.isDefined && isSentStart(nextOpt.get.word)) {
        //println(s"FOUND EOS INSIDE TOKEN: ${crt.raw}")

        //
        // create the last token from the token fragment before the period, and the period itself
        //
        val dotRawPosition = curr.raw.lastIndexOf('.')
        assert(dotRawPosition > 0)
        val dotWordPosition = curr.word.lastIndexOf('.')
        assert(dotWordPosition > 0)

        raw += curr.raw.substring(0, dotRawPosition)
        words += curr.word.substring(0, dotWordPosition)
        beginPositions += curr.beginPosition + characterOffset
        endPositions += curr.beginPosition + dotRawPosition + characterOffset

        // This is just for the period with length of 1.
        raw += curr.raw.substring(dotRawPosition, dotRawPosition + 1)
        words += curr.word.substring(dotWordPosition, dotWordPosition + 1)
        beginPositions += endPositions.last
        endPositions += beginPositions.last + 1
        val lastPosition = endPositions.last

        //
        // create the current sentence
        //
        sentences += Sentence(raw, beginPositions, endPositions, words)
        raw = new ArrayBuffer[String]()
        words = new ArrayBuffer[String]()
        beginPositions = new ArrayBuffer[Int]()
        endPositions = new ArrayBuffer[Int]()

        //
        // add the part of the token after the period to the new sentence
        //
        raw += curr.raw.substring(dotRawPosition + 1)
        words += curr.word.substring(dotWordPosition + 1)
        beginPositions += lastPosition
        endPositions += lastPosition + raw.head.length
      }

      else {
        // just a regular token
        raw += curr.raw
        words += curr.word
        beginPositions += curr.beginPosition + characterOffset
        endPositions += curr.endPosition + characterOffset
      }
    }

    // a few words left over at the end
    if (words.nonEmpty) {
      sentences += Sentence(raw, beginPositions, endPositions, words)
    }

    sentences
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
    val regex = new StringBuilder
    regex.append("^(")

    Using.resource(new BufferedReader(new InputStreamReader(is))) { reader =>
      var done = false
      var first = true
      while (!done) {
        val line = reader.readLine()
        if (line == null) {
          done = true
        } else if (!line.startsWith("#")) { // skip comments
          if (!first) regex.append("|")
          regex.append(normalizeSpecialChars(line.trim))
          first = false
        }
      }
    }

    regex.append(")$")
    regex.toString.r
  }

  private def normalizeSpecialChars(s:String):String = {
    var n = s.replaceAll("\\.", "\\\\.")
    n = n.replaceAll("\\-", "\\\\-")
    n
  }
}
