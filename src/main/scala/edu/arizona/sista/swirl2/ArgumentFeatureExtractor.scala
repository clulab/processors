package edu.arizona.sista.swirl2

import edu.arizona.sista.processors.Sentence

import scala.collection.mutable.ListBuffer

import ArgumentFeatureExtractor._

/**
 * Creates features for the binary classification of tokens as potential arguments
 * User: mihais
 * Date: 7/14/15
 */
class ArgumentFeatureExtractor {
  def mkFeatures(sent:Sentence, position:Int, pred:Int):Seq[String] = {
    val features = new ListBuffer[String]
    val predLemma = lemmaAt(sent, pred)
    val predTag = tagAt(sent, pred)

    // unigrams
    for (i <- Range(-1, 2)) {
      // of lemmas
      val lemma = lemmaAt(sent, position + i)
      features += s"lemma:$i:$lemma"
      features += s"lemma:$i:$lemma:$predLemma"

      // of POS tags
      val tag = tagAt(sent, position + i)
      features += s"tag:$i:$tag"
      features += s"tag:$i:$tag:$predTag"
    }

    val paths = sent.dependencies.get.shortestPathEdges(pred, position)
    if(paths.nonEmpty) {
      val path = paths.head.toArray
      features += s"path-length:${path.length}"
      val pathLabels = path.map(_._3).mkString("-")
      features += s"path-labels:$pathLabels"
      features += s"path-labels:$predLemma-$pathLabels-${lemmaAt(sent, position)}"
      // println(s"$position\t$pred\t${path.toList}\t$pathLabels")
    } else {
      features += "no-path"
    }

    if (pred == position) {
      features += s"same-token:${tagAt(sent, position)}"
    } else {
      features += s"token-dist:${math.abs(pred - position)}:${pred < position}"
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

object ArgumentFeatureExtractor {
  val PADDING = "##"
}
