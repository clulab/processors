package org.clulab.processors.clu

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.scala.WrappedArrayBuffer._
import org.clulab.utils.WrappedArraySeq
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.compat._
import scala.collection.mutable.ArrayBuffer

class DocumentMaker

object DocumentMaker {
  val logger:Logger = LoggerFactory.getLogger(classOf[DocumentMaker])

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument(tokenizer:Tokenizer,
                 text:String,
                 keepText:Boolean): Document = {
    val sents = tokenizer.tokenize(text)
    val textOpt = Option.when(keepText)(text)
    val doc = Document(sents, textOpt)

    doc
  }

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences(
    tokenizer: Tokenizer,
    texts: Iterable[String],
    keepText: Boolean,
    charactersBetweenSentences: Int
  ): Document = {
    var characterOffset = 0
    val sentencesArray = texts.map { text =>
      val sentence = tokenizer.tokenize(text, sentenceSplit = false, characterOffset).head // We produce a single sentence here!

      characterOffset = sentence.endOffsets.last + charactersBetweenSentences
      sentence
    }.toArray
    val sentences = WrappedArraySeq(sentencesArray).toImmutableSeq
    val textOpt = Option.when(keepText)(texts.mkString(mkSep(charactersBetweenSentences)))
    val document = Document(sentences, textOpt)

    document
  }

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens(sentences:Iterable[Iterable[String]],
                           keepText:Boolean,
                           charactersBetweenSentences:Int,
                           charactersBetweenTokens:Int): Document = {
    var charOffset = 0
    val sents = new ArrayBuffer[Sentence]()
    val text = new StringBuilder
    for(sentence <- sentences) {
      val startOffsets = new ArrayBuffer[Int]()
      val endOffsets = new ArrayBuffer[Int]()
      for(word <- sentence) {
        startOffsets += charOffset
        charOffset += word.length
        endOffsets += charOffset
        charOffset += charactersBetweenTokens
      }
      // note: NO postprocessing happens in this case, so use it carefully!
      sents += new Sentence(sentence.toSeq, startOffsets, endOffsets, sentence.toSeq)
      charOffset += charactersBetweenSentences - charactersBetweenTokens
      if(keepText) {
        text.append(sentence.mkString(mkSep(charactersBetweenTokens)))
        text.append(mkSep(charactersBetweenSentences))
      }
    }

    val textOpt = Option.when(keepText)(text.toString)
    val doc = Document(sents, textOpt)

    doc
  }

  private def mkSep(size:Int):String = {
    val os = new StringBuilder
    for (_ <- 0 until size) os.append(" ")
    os.toString()
  }
}
