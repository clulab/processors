package org.clulab.swirl2

import org.clulab.processors.Sentence
import org.clulab.struct.Counter

import scala.collection.mutable.ListBuffer

import PredicateFeatureExtractor._

/**
 * Creates features for the classification of predicates
 * User: mihais
 * Date: 5/28/15
 */
class PredicateFeatureExtractor {
  var lemmaCounts:Option[Counter[String]] = None

  def mkFeatures(sent:Sentence, position:Int):Seq[String] = {
    val features = new ListBuffer[String]

    // unigrams
    for(i <- Range(-2, 3)) {
      // of lemmas
      features += s"lemma:$i:${lemmaAt(sent, position + i)}"
      // of POS tags
      features += s"tag:$i:${tagAt(sent, position + i)}"
    }

    // dependencies
    for(i <- Range(0, 1)) {
      sent.dependencies.foreach(dg => {
        addDepFeats(features, dg.incomingEdges, position, i, "inc")
        addDepFeats(features, dg.outgoingEdges, position, i, "out")
      })
    }

    features.toList
  }

  def addDepFeats(features:ListBuffer[String],
                  deps:Array[Array[(Int, String)]],
                  position:Int,
                  offset:Int,
                  prefix:String) {
    val absPos = position + offset
    if(absPos >= 0 && absPos < deps.length) {
      val ds = deps(absPos)
      if (ds != null) {
        for (d <- ds) {
          features += s"$prefix:$offset:${d._2}"
        }
      }
    }
  }

  def lemmaAt(sent:Sentence, position:Int):String = {
    if(position >= 0 && position < sent.size) {
      val l = sent.lemmas.get(position)
      if(lemmaCounts.isDefined) {
        if(lemmaCounts.get.getCount(l) > UNKNOWN_THRESHOLD) {
          l
        } else {
          UNKNOWN_TOKEN
        }
      } else {
        l
      }
    }
    else PADDING
  }
  def tagAt(sent:Sentence, position:Int):String = {
    if(position >= 0 && position < sent.size) sent.tags.get(position)
    else PADDING
  }
}

object PredicateFeatureExtractor {
  val PADDING = "##"
  val UNKNOWN_THRESHOLD = 5
  val UNKNOWN_TOKEN = "*u*"
}
