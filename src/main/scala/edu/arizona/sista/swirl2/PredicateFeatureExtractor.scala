package edu.arizona.sista.swirl2

import edu.arizona.sista.processors.Sentence

import scala.collection.mutable.ListBuffer

import PredicateFeatureExtractor._

/**
 * Creates features for the classification of predicates
 * User: mihais
 * Date: 5/28/15
 */
class PredicateFeatureExtractor {
  def mkFeatures(sent:Sentence, position:Int):Seq[String] = {
    val features = new ListBuffer[String]

    for(i <- Range(-2, 3)) {
      features += s"lemma:$i:${lemmaAt(sent, position + i)}"
      features += s"tag:$i:${tagAt(sent, position + i)}"
    }

    features.toList
  }

  def lemmaAt(sent:Sentence, position:Int):String = {
    if(position >= 0 && position < sent.size) sent.lemmas.get(position)
    else PADDING
  }
  def tagAt(sent:Sentence, position:Int):String = {
    if(position >= 0 && position < sent.size) sent.tags.get(position)
    else PADDING
  }
}

object PredicateFeatureExtractor {
  val PADDING = "##"
}
