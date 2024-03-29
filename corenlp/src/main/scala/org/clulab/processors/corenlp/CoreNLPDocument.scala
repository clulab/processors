package org.clulab.processors.corenlp

import org.clulab.processors.{Document, Sentence}
import edu.stanford.nlp.pipeline.Annotation
import org.clulab.struct.CorefChains


/**
 * 
 * User: mihais
 * Date: 3/2/13
 */
class CoreNLPDocument(sentences: Array[Sentence]) extends Document(sentences) {

  var annotation:Option[Annotation] = None

  def assimilate(document: CoreNLPDocument, textOpt: Option[String]): CoreNLPDocument = {
    super.assimilate(document, textOpt)
    annotation = document.annotation
    this
  }

  override def copy(sentences: Array[Sentence] = sentences, textOpt: Option[String] = text): CoreNLPDocument =
      new CoreNLPDocument(sentences).assimilate(this, textOpt)

  override def clear(): Unit = {
    //println("Clearing state from document.")
    annotation = None
  }
}

object CoreNLPDocument {

  def apply(sentences: Array[Sentence]) = new CoreNLPDocument(sentences)
  def apply(
    sentences: Array[Sentence],
    coref: Option[CorefChains],
    annotation: Option[Annotation],
    text: Option[String]
  ): CoreNLPDocument = {
    val coreDoc = new CoreNLPDocument(sentences)
    coreDoc.coreferenceChains = coref
    coreDoc.annotation = annotation
    coreDoc
  }

  def apply(
    sentences: Array[Sentence],
    annotation: Annotation
  ): CoreNLPDocument = {
    val coreDoc = new CoreNLPDocument(sentences)
    coreDoc.annotation = Some(annotation)
    coreDoc
  }
}