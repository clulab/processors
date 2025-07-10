package org.clulab.processors.clu

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.utils.WrappedArraySeq
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.compat._
import scala.collection.mutable.ArrayBuffer

class DocumentMaker

object DocumentMaker {
  val logger: Logger = LoggerFactory.getLogger(classOf[DocumentMaker])

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument( // TODO: mkDocumentFromText
    tokenizer: Tokenizer,
    text: String,
    keepText: Boolean
  ): Document = {
    val sentences = tokenizer.tokenize(text)
    val textOpt = Option.when(keepText)(text)
    val document = Document(sentences, textOpt)

    document
  }

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences( // TODO: mkDocumentFromTexts
    tokenizer: Tokenizer,
    texts: Iterable[String],
    keepText: Boolean,
    charactersBetweenSentences: Int
  ): Document = {
    val sentenceSep = " " * charactersBetweenSentences
    var characterOffset = 0
    val sentencesArray = texts.map { text =>
      val sentence = tokenizer.tokenize(text, sentenceSplit = false, characterOffset).head // We produce a single sentence here!

      characterOffset = sentence.endOffsets.last + charactersBetweenSentences
      sentence
    }.toArray
    val sentences = WrappedArraySeq(sentencesArray).toImmutableSeq
    val textOpt = Option.when(keepText)(texts.mkString(sentenceSep))
    val document = Document(sentences, textOpt)

    document
  }

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens( // TODO: mkDocumentFromTokenizedTexts
    tokenizedTexts: Iterable[Iterable[String]],
    keepText: Boolean,
    charactersBetweenSentences: Int,
    charactersBetweenTokens: Int
  ): Document = {
    val sentenceSep = " " * charactersBetweenSentences
    val tokenSep = " " * charactersBetweenTokens
    var charOffset = 0
    val text = new StringBuilder
    // Just use one buffer for each but clear them as necessary.
    val startOffsetsBuffer = new ArrayBuffer[Int]()
    val endOffsetsBuffer = new ArrayBuffer[Int]()
    val sentencesArray = tokenizedTexts.map { tokenizedTextIterable =>
      // We are going to need to tokens in an array anyway, so make them now.
      val tokenizedTextArray = tokenizedTextIterable.toArray

      tokenizedTextArray.foreach { token =>
        startOffsetsBuffer += charOffset
        charOffset += token.length
        endOffsetsBuffer += charOffset
        charOffset += charactersBetweenTokens
      }
      // The simple version of this doesn't work if there were no tokens.
      charOffset += charactersBetweenSentences - (if (tokenizedTextArray.nonEmpty) charactersBetweenTokens else 0)

      // Note: NO postprocessing happens in this case, so use it carefully!
      val startOffsets = WrappedArraySeq(startOffsetsBuffer.toArray).toImmutableSeq
      startOffsetsBuffer.clear()
      val endOffsets = WrappedArraySeq(endOffsetsBuffer.toArray).toImmutableSeq
      endOffsetsBuffer.clear()
      val tokens = WrappedArraySeq(tokenizedTextArray).toImmutableSeq
      val sentence = new Sentence(tokens, startOffsets, endOffsets, tokens)

      if (keepText) {
        text.append(tokens.mkString(tokenSep))
        text.append(sentenceSep)
      }
      sentence
    }.toArray
    val sentences = WrappedArraySeq(sentencesArray).toImmutableSeq
    val textOpt = Option.when(keepText)(text.toString)
    val document = Document(sentences, textOpt)

    document
  }
}
