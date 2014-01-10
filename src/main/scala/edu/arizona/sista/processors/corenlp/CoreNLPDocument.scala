package edu.arizona.sista.processors.corenlp

import edu.arizona.sista.processors.{CorefChains, Sentence, Document}
import edu.stanford.nlp.pipeline.Annotation


/**
 * 
 * User: mihais
 * Date: 3/2/13
 */
class CoreNLPDocument(
  sentences:Array[Sentence],
  coref:Option[CorefChains],
  var annotation:Option[Annotation]) extends Document(sentences, coref) {

  def this(sentences:Array[Sentence], annotation:Option[Annotation]) =
    this(sentences, None, annotation)

  override def clear() {
    //println("Clearing state from document.")
    annotation = None
  }
}
