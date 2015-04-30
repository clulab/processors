package edu.arizona.sista.processors.corenlp

import edu.arizona.sista.discourse.rstparser.DiscourseTree
import edu.arizona.sista.processors.{CorefChains, Sentence, Document}
import edu.stanford.nlp.pipeline.Annotation


/**
 * 
 * User: mihais
 * Date: 3/2/13
 */
class CoreNLPDocument(
  id:Option[String],
  sentences:Array[Sentence],
  coref:Option[CorefChains],
  dtree:Option[DiscourseTree],
  text:Option[String],
  var annotation:Option[Annotation]) extends Document(id, sentences, coref, dtree, text) {

  def this(sentences:Array[Sentence],
           coref:Option[CorefChains],
           dtree:Option[DiscourseTree],
           annotation:Option[Annotation]) =
    this(None, sentences, coref, dtree, None, annotation)

  def this(sentences:Array[Sentence], annotation:Option[Annotation]) =
    this(sentences, None, None, annotation)

  override def clear() {
    //println("Clearing state from document.")
    annotation = None
  }
}
