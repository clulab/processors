package org.clulab.processors.clu.tokenizer

import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.Token
import org.clulab.processors.Sentence

import scala.collection.mutable.ArrayBuffer

/** English open domain tokenizer */
class OpenDomainEnglishTokenizer(postProcessor:Option[TokenizerStep] = None) extends Tokenizer(
  lexer = new OpenDomainEnglishLexer,
  // the postprocessor must go first because it assumes that .word == .raw
  postProcessor.toList ++ Seq(
    new TokenizerStepContractions,
    new TokenizerStepNormalization,
    new TokenizerStepHyphens
  ),
  new EnglishSentenceSplitter)

/** Portuguese open domain tokenizer */
class OpenDomainPortugueseTokenizer(postProcessor:Option[TokenizerStep] = None) extends Tokenizer(
  lexer = new OpenDomainPortugueseTokenizerLexer,
  // the postprocessor must go first because it assumes that .word == .raw
  postProcessor.toList ++ Seq(new TokenizerStepPortugueseContractions, new TokenizerStepAccentedNormalization),
  new PortugueseSentenceSplitter)

/** Spanish open domain tokenizer */
class OpenDomainSpanishTokenizer(postProcessor:Option[TokenizerStep] = None) extends Tokenizer(
  lexer = new OpenDomainSpanishTokenizerLexer,
  // the postprocessor must go first because it assumes that .word == .raw
  postProcessor.toList ++ Seq(new TokenizerStepSpanishContractions, new TokenizerStepAccentedNormalization),
  new SpanishSentenceSplitter)

/**
  * Generic tokenizer
  * Author: mihais
  * Date: 3/15/17
  */
class Tokenizer(
  val lexer: TokenizerLexer,
  val steps: Seq[TokenizerStep],
  val sentenceSplitter: SentenceSplitter
) {

  protected def newRawToken(token: Token): RawToken = {
    val word = token.getText
    val beginPosition = token.getStartIndex
    val endPosition = token.getStopIndex + 1 // antlr is inclusive on end position, we are exclusive

    assert(beginPosition + word.length == endPosition)
    RawToken(word, beginPosition)
  }

  protected def readTokens(text: String): Array[RawToken] = {
    val tokens: CommonTokenStream = lexer.mkLexer(text)
    val rawTokenBuffer = new ArrayBuffer[RawToken]()

    def processToken(token: Token): Boolean = {
      if (token.getType == -1) // EOF
        false
      else {
        rawTokenBuffer += newRawToken(token)
        true
      }
    }

    while (processToken(tokens.LT(1)))
      tokens.consume()
    rawTokenBuffer.toArray
  }

  /** Tokenization and sentence splitting */
  def tokenize(text: String, sentenceSplit: Boolean = true): Array[Sentence] = {
    // raw tokenization, using the antlr grammar
    val rawTokens = readTokens(text)
    // now apply all the additional non-Antlr steps such as solving contractions, normalization, post-processing
    val stepTokens = steps.foldLeft(rawTokens) { (rawTokens, step) =>
      step.process(rawTokens)
    }
    // sentence splitting, including detection of abbreviations
    val sentences = sentenceSplitter.split(stepTokens, sentenceSplit)

    sentences
  }
}
