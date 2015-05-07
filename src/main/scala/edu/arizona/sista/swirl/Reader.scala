package edu.arizona.sista.swirl

import java.io.File

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.{Document, Processor}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import Reader._

/**
 * Reads a CoNLL formatted file and converts it to our own representation
 * User: mihais
 * Date: 5/5/15
 */
class Reader {
  class CoNLLToken(val word:String, val pos:String, val pred:Int, val frameBits:Array[String]) {
    override def toString:String = word + "/" + pos + "/" + pred  }

  def read(file:File,
           proc:Processor = null,
           verbose:Boolean = false):Document = {
    val source = Source.fromFile(file)
    val sentences = new ArrayBuffer[Array[CoNLLToken]]
    var sentence = new ArrayBuffer[CoNLLToken]
    var tokenCount = 0
    var sentCount = 0
    var hyphCount = 0
    for(l <- source.getLines()) {
      val line = l.trim
      if(line.length > 0) {
        val bits = l.split("\\t")
        assert(bits.size >= 14)
        val token = mkToken(bits)
        sentence += token
        tokenCount += 1
        if(token.pos == "HYPH") hyphCount += 1
      } else {
        // end of sentence
        sentences += collapseHyphens(sentence.toArray, verbose)
        sentence = new ArrayBuffer[CoNLLToken]()
        sentCount += 1
      }
    }
    source.close()
    logger.debug(s"Read $tokenCount tokens, grouped in $sentCount sentences.")
    logger.debug(s"Found $hyphCount hyphens.")
    null
  }

  def mkToken(bits:Array[String]):CoNLLToken = {
    val word = bits(1)
    val pos = bits(4)
    val isPred = bits(13) match {
      case "_" => 0
      case _ => 1
    }
    val frameBits =  bits.slice(14, bits.length)
    new CoNLLToken(word, pos, isPred, frameBits)
  }

  /**
   * Merges tokens that were separated around dashes in CoNLL, to bring tokenization closer to the usual Treebank one
   * We need this because most parsers behave horribly if hyphenated words are tokenized around dashes
   */
  def collapseHyphens(origSentence:Array[CoNLLToken], verbose:Boolean):Array[CoNLLToken] = {
    val sent = new ArrayBuffer[CoNLLToken]()

    var start = 0
    while(start < origSentence.length) {
      val end = findEnd(origSentence, start)
      if(end > start + 1) {
        val token = mergeTokens(origSentence, start, end, verbose)
        sent += token
      } else {
        sent += origSentence(start)
      }
      start = end
    }

    sent.toArray
  }

  def findEnd(sent:Array[CoNLLToken], start:Int):Int = {
    var end = start + 1
    while(end < sent.length) {
      if(sent(end).pos != "HYPH") return end
      else end = end + 2
    }
    sent.length
  }

  def mergeTokens(sent:Array[CoNLLToken], start:Int, end:Int, verbose:Boolean):CoNLLToken = {
    val phrase = sent.slice(start, end)
    val word = phrase.map(_.word).mkString("")
    val pos = phrase.last.pos // this one doesn't really matter; we retag the entire data with our Processor anyway...
    val pred = mergePredicates(phrase, verbose)
    val frameBits = mergeFrames(phrase, verbose)

    if(verbose) {
      //logger.debug("Merging tokens: " + phrase.mkString(" ") + " as: " + word + "/" + isPred)
    }

    new CoNLLToken(word, pos, pred, frameBits)
  }

  def mergePredicates(phrase:Array[CoNLLToken], verbose:Boolean):Int = {
    val l = phrase.map(_.pred).sum

    if(l > 0) {
      if(l > 1) {
        logger.info("Found MULTI PREDICATE in hyphenated phrase: " + phrase.mkString(" "))
      }
      if(verbose) {
        // logger.info("Found hyphenated predicate: " + phrase.mkString(" "))
      }
    }

    l
  }

  def mergeFrames(phrase:Array[CoNLLToken], verbose:Boolean):Array[String] = {
    // TODO
  }
}

object Reader {
  val logger = LoggerFactory.getLogger(classOf[Reader])

  def main(args:Array[String]) {
    val reader = new Reader
    val proc = new FastNLPProcessor(useMalt = false, useBasicDependencies = false)
    val file = new File(args(0))

    reader.read(file, proc, verbose = true)
  }
}
