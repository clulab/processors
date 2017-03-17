package org.clulab.processors.clulab.tokenizer

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import org.clulab.processors.Sentence

import scala.collection.mutable.ArrayBuffer

import Tokenizer._

/**
  *
  * User: mihais
  * Date: 3/15/17
  */
class Tokenizer {
  def tokenize(text:String):Array[Sentence] = {
    //println(s"Tokenizing text: $text")
    val lexer = new OpenDomainLexer(new ANTLRInputStream(text)) // TODO: customize lexer grammar here
    val tokens = new CommonTokenStream(lexer)
    var done = false
    var sentences = new ArrayBuffer[Sentence]()
    var words = new ArrayBuffer[String]()
    var startOffsets = new ArrayBuffer[Int]()
    var endOffsets = new ArrayBuffer[Int]()
    while(! done) {
      val t = tokens.LT(1)
      if(t.getType == -1) {
        // EOF
        done = true
        if(words.size > 0) {
          sentences += Sentence(words.toArray, startOffsets.toArray, endOffsets.toArray)
        }
      } else {
        // info on the current token
        //println(s"${t.getText}\t${t.getStartIndex}\t${t.getStopIndex}\t${t.getType}")
        val word = normalizeToken(t.getText)
        val startOffset = t.getStartIndex
        val endOffset = t.getStopIndex + 1 // antlr is inclusive, we are exclusive

        // add to current sentence
        words += word
        startOffsets += startOffset
        endOffsets += endOffset

        // found a regular end of sentence
        if(EOS.findFirstIn(word).isDefined) {
          sentences += Sentence(words.toArray, startOffsets.toArray, endOffsets.toArray)
          words = new ArrayBuffer[String]()
          startOffsets = new ArrayBuffer[Int]()
          endOffsets = new ArrayBuffer[Int]()
        }

        // advance to next token in stream
        tokens.consume()
      }
    }
    sentences.toArray
  }

  def normalizeToken(t:String):String = {
    // TODO: add token normalizations, e.g., converting Unicode chars to ASCII here
    t
  }
}

object Tokenizer {
  val EOS = """^[\.!\?]+$""".r

  def tokenize(text:String):Array[Sentence] = {
    val tokenizer = new Tokenizer
    tokenizer.tokenize(text)
  }
}
