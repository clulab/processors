package org.clulab.processors.clulab.tokenizer

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import org.clulab.processors.Sentence

/**
  *
  * User: mihais
  * Date: 3/15/17
  */
class Tokenizer {
  def tokenize(text:String):Array[Sentence] = {
    println(s"Tokenizing text: $text")
    val lexer = new OpenDomainLexer(new ANTLRInputStream(text)) // TODO: customize lexer grammar here
    val tokens = new CommonTokenStream(lexer)
    var done = false
    while(! done) {
      val t = tokens.LT(1)
      if(t.getType == -1) {
        // EOF
        done = true
      } else {
        println(s"${t.getText}\t${t.getStartIndex}\t${t.getStopIndex}\t${t.getType}")
        tokens.consume()
      }
    }
    null
  }
}

object Tokenizer {
  def tokenize(text:String):Array[Sentence] = {
    val tokenizer = new Tokenizer
    tokenizer.tokenize(text)
  }
}
