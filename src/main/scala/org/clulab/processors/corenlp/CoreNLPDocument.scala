package org.clulab.processors.corenlp

import org.clulab.discourse.rstparser.DiscourseTree
import org.clulab.processors.{Document, Sentence}
import edu.stanford.nlp.pipeline.Annotation
import org.clulab.struct.CorefChains


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
