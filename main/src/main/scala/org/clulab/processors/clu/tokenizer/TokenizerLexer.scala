package org.clulab.processors.clu.tokenizer

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

/**
  * Thin wrapper over the Antlr lexer
  * Author: mihais
  * Date: 3/21/17
  */
trait TokenizerLexer {
  def mkLexer(text:String): CommonTokenStream
}

/** Tokenizer using the OpenDomainLexer.g grammar */
class OpenDomainEnglishLexer extends TokenizerLexer {
  override def mkLexer(text: String): CommonTokenStream = {
    val lexer = new OpenDomainLexer(new ANTLRInputStream(text))
    new CommonTokenStream(lexer)
  }
}

/** Tokenizer using the OpenDomainLexer.g grammar */
class OpenDomainPortugueseTokenizerLexer extends TokenizerLexer {
  override def mkLexer(text: String): CommonTokenStream = {
    val lexer = new OpenDomainPortugueseLexer(new ANTLRInputStream(text))
    new CommonTokenStream(lexer)
  }
}

/** Tokenizer using the OpenDomainLexer.g grammar */
class OpenDomainSpanishTokenizerLexer extends TokenizerLexer {
  override def mkLexer(text: String): CommonTokenStream = {
    val lexer = new OpenDomainSpanishLexer(new ANTLRInputStream(text))
    new CommonTokenStream(lexer)
  }
}
