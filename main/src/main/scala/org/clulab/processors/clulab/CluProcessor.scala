package org.clulab.processors.clulab

import org.clulab.processors.clulab.tokenizer.OpenDomainEnglishTokenizer
import org.clulab.processors.{Document, Processor, Sentence}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CluProcessor (val internStrings:Boolean = false) extends Processor {

  lazy val tokenizer = new OpenDomainEnglishTokenizer

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument(text:String, keepText:Boolean = false): Document = {
    val sents = tokenizer.tokenize(text)
    val doc = new Document(sents)
    if(keepText) doc.text = Some(text)
    doc
  }

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences(sentences:Iterable[String],
                              keepText:Boolean = false,
                              charactersBetweenSentences:Int = 1): Document = {
    val sents = new ArrayBuffer[Sentence]()
    for(text <- sentences) {
      sents ++= tokenizer.tokenize(text, sentenceSplit = false) // we produce a single sentence here!
    }
    val doc = new Document(sents.toArray)
    if(keepText) doc.text = Some(sentences.mkString(mkSep(charactersBetweenSentences)))
    doc
  }

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens(sentences:Iterable[Iterable[String]],
                           keepText:Boolean = false,
                           charactersBetweenSentences:Int = 1,
                           charactersBetweenTokens:Int = 1): Document = {
    var charOffset = 0
    var sents = new ArrayBuffer[Sentence]()
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
      sents += new Sentence(sentence.toArray, startOffsets.toArray, endOffsets.toArray)
      charOffset += charactersBetweenSentences
      if(keepText) {
        text.append(sentence.mkString(mkSep(charactersBetweenTokens)))
        text.append(mkSep(charactersBetweenSentences))
      }
    }

    val doc = new Document(sents.toArray)
    if(keepText) doc.text = Some(text.toString)
    doc
  }

  private def mkSep(size:Int):String = {
    val os = new mutable.StringBuilder
    for (i <- 0 until size) os.append(" ")
    os.toString()
  }

  private def in(s:String):String = {
    if (internStrings) Processor.internString(s)
    else s
  }

  /** Part of speech tagging */
  def tagPartsOfSpeech(doc:Document) {
    // TODO
  }

  /** Lematization; modifies the document in place */
  def lemmatize(doc:Document) {
    // TODO
  }

  /** NER; modifies the document in place */
  def recognizeNamedEntities(doc:Document) {
    // TODO
  }

  /** Syntactic parsing; modifies the document in place */
  def parse(doc:Document) { }

  /** Shallow parsing; modifies the document in place */
  def chunking(doc:Document) { }

  /** SRL; modifies the document in place */
  def labelSemanticRoles(doc:Document) { }

  /** Coreference resolution; modifies the document in place */
  def resolveCoreference(doc:Document) { }

  /** Discourse parsing; modifies the document in place */
  def discourse(doc:Document) { }
}