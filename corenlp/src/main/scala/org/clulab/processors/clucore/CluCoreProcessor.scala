package org.clulab.processors.clucore

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.Sentence
import org.clulab.processors.clu.CluProcessor
import org.clulab.struct.DirectedGraph

// TODO: tests (doc date + all labels)

class CluCoreProcessor(config: Config = ConfigFactory.load("cluprocessor"))
  extends CluProcessor(config) {

  val numericEntityRecognizer = new NumericEntityRecognizer

  val outside = "O"
  val empty = ""

  /** Produces NE labels for one sentence by merging the CLU NER with the NumericEntityRecognizer */
  override def nerSentence(words: IndexedSeq[String],
                           tags: IndexedSeq[String], // this are only used by the NumericEntityRecognizer
                           startCharOffsets: IndexedSeq[Int],
                           endCharOffsets: IndexedSeq[Int],
                           docDateOpt: Option[String]): (IndexedSeq[String], Option[IndexedSeq[String]]) = {
    val (cluLabels, _) = super.nerSentence(words, tags, startCharOffsets, endCharOffsets, docDateOpt)
    val (numericLabels, numericNorms) = numericEntityRecognizer.classify(words, tags, startCharOffsets, endCharOffsets, docDateOpt)
    assert(cluLabels.length == numericLabels.length)
    assert(cluLabels.length == numericNorms.length)

    val labels = new Array[String](cluLabels.length)
    val norms = new Array[String](cluLabels.length)

    for(i <- cluLabels.indices) {
      norms(i) = empty
      labels(i) = outside

      if(cluLabels(i) != outside) {
        labels(i) = cluLabels(i)
      } else if(numericLabels(i) != outside) {
        labels(i) = numericLabels(i)
        norms(i) = numericNorms(i)
      }
    }

    (labels, Some(norms))
  }

  override def srlSentence(sent: Sentence, predicateIndexes: IndexedSeq[Int]): DirectedGraph[String] = {
    val words = sent.words
    val tags = sent.tags.get
    // the SRL models were trained using only named (CoNLL) entities, not numeric ones
    val onlyNamedLabels = removeNumericLabels(sent.entities.get)

    srlSentence(words, tags, onlyNamedLabels, predicateIndexes)
  }

  def removeNumericLabels(allLabels: Array[String]): Array[String] = {
    val labels = new Array[String](allLabels.length)
    // TODO: remove numeric labels
    labels
  }
}
