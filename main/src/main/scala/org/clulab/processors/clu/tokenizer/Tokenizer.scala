package org.clulab.processors.clu.tokenizer

import org.clulab.processors.Sentence

import scala.collection.mutable.ArrayBuffer

/** English open domain tokenizer */
class OpenDomainEnglishTokenizer(postProcessor:Option[TokenizerStep]) extends Tokenizer(
  lexer = new OpenDomainEnglishLexer,
  // the postprocessor must go first because it assumes that .word == .raw
  postProcessor.toList ++ Seq(new TokenizerStepContractions, new TokenizerStepNormalization),
  new EnglishSentenceSplitter)

/**
  * Generic tokenizer
  * Author: mihais
  * Date: 3/15/17
  */
class Tokenizer(
  lexer:TokenizerLexer,
  steps:Seq[TokenizerStep],
  sentenceSplitter: SentenceSplitter) {

  /** Tokenization and sentence splitting */
  def tokenize(text:String, sentenceSplit:Boolean = true):Array[Sentence] = {
    val tokens = lexer.mkLexer(text)
    var done = false

    val rawTokens = new ArrayBuffer[RawToken]()

    //
    // raw tokenization, using the antlr grammar
    //
    while(! done) {
      val t = tokens.LT(1)
      if(t.getType == -1) {
        // EOF
        done = true
      } else {
        // info on the current token
        val word = t.getText
        val beginPosition = t.getStartIndex
        val endPosition = t.getStopIndex + 1 // antlr is inclusive on end position, we are exclusive

        // make sure character positions are legit
        assert(beginPosition + word.length == endPosition)

        // add to raw stream
        rawTokens += RawToken(word, beginPosition)

        // advance to next token in stream
        tokens.consume()
      }
    }

    //
    // now apply all the additional non-Antlr steps such as solving contractions, normalization, post-processing
    //
    var postProcessedTokens = rawTokens.toArray
    for(step <- steps) {
      postProcessedTokens = step.process(postProcessedTokens)
    }

    //
    // sentence splitting, including detection of abbreviations
    //
    sentenceSplitter.split(postProcessedTokens, sentenceSplit)
  }
}

