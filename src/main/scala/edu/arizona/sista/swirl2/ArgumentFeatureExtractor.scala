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
    for (i <- Range(-2, 3)) {
      // of lemmas
      val lemma = lemmaAt(sent, position + i)
      features += s"lemma:$i:$lemma"
      features += s"lemma:$i:$lemma:$predLemma"

      // of POS tags
      val tag = tagAt(sent, position + i)
      features += s"tag:$i:$tag"
      features += s"tag:$i:$tag:$predTag"
    }

    val path = sent.dependencies.get.shortestPath(pred, position).toArray
    features += s"path-length:${path.length}"
    val posPath = new StringBuilder
    for(i <- 1 until path.length - 1)
      posPath.append(sent.tags.get(path(i)))
    features += s"pos-path:${posPath.toString()}"
    //println(s"$position\t$pred\t${path.toList}\t${posPath.toString()}")

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
