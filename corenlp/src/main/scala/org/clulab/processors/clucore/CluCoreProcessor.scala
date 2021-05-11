package org.clulab.processors.clucore

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.dynet.ConstEmbeddingParameters
import org.clulab.processors.Sentence
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clucore.CluCoreProcessor.{EMPTY, EMPTY_GRAPH, NAMED_LABELS_FOR_SRL, OUTSIDE}
import org.clulab.sequences.LexiconNER
import org.clulab.struct.{DirectedGraph, GraphMap}

import scala.collection.mutable.ArrayBuffer

class CluCoreProcessor(config: Config = ConfigFactory.load("cluprocessor"),
                       val optionalNER: Option[LexiconNER] = None)
  extends CluProcessor(config) {

  val numericEntityRecognizer = new NumericEntityRecognizer

  /** Produces NE labels for one sentence by merging the CLU NER with the NumericEntityRecognizer */
  override def nerSentence(words: Array[String],
                           lemmas: Option[Array[String]],
                           tags: Array[String], // this are only used by the NumericEntityRecognizer
                           startCharOffsets: Array[Int],
                           endCharOffsets: Array[Int],
                           docDateOpt: Option[String],
                           embeddings: ConstEmbeddingParameters): (IndexedSeq[String], Option[IndexedSeq[String]]) = {
    val (cluLabels, _) = super.nerSentence(words, lemmas, tags, startCharOffsets, endCharOffsets, docDateOpt, embeddings)
    val (numericLabels, numericNorms) = numericEntityRecognizer.classify(words, tags, startCharOffsets, endCharOffsets, docDateOpt)
    assert(cluLabels.length == numericLabels.length)
    assert(cluLabels.length == numericNorms.length)

    val optionalNERLabels: Option[Array[String]] = {
      if(optionalNER.isEmpty) {
        None
      } else {
        val sentence = Sentence(
          words,
          startCharOffsets,
          endCharOffsets,
          words,
          Some(tags),
          lemmas = lemmas,
          entities = None,
          norms = None,
          chunks = None,
          tree = None,
          deps = EMPTY_GRAPH,
          relations = None
        )

        Some(optionalNER.get.find(sentence))
      }
    }

    val labels = new Array[String](cluLabels.length)
    val norms = new Array[String](cluLabels.length)

    for(i <- cluLabels.indices) {
      norms(i) = EMPTY
      labels(i) = OUTSIDE

      if(optionalNERLabels.nonEmpty && optionalNERLabels.get(i) != OUTSIDE) {
        labels(i) = optionalNERLabels.get(i)
      } else if(cluLabels(i) != OUTSIDE) {
        labels(i) = cluLabels(i)
      } else if(numericLabels(i) != OUTSIDE) {
        labels(i) = numericLabels(i)
        norms(i) = numericNorms(i)
      }
    }

    (labels, Some(norms))
  }

  override def srlSentence(sent: Sentence,
                           predicateIndexes: IndexedSeq[Int],
                           embeddings: ConstEmbeddingParameters): DirectedGraph[String] = {
    val words = sent.words
    val tags = sent.tags.get
    // the SRL models were trained using only named (CoNLL) entities, not numeric ones
    val onlyNamedLabels = removeNumericLabels(sent.entities.get)

    srlSentence(words, tags, onlyNamedLabels, predicateIndexes, embeddings)
  }

  def removeNumericLabels(allLabels: Array[String]): Array[String] = {
    val labels = new ArrayBuffer[String]
    for(l <- allLabels) {
      if(NAMED_LABELS_FOR_SRL.contains(l)) {
        labels += l
      } else {
        labels += OUTSIDE
      }
    }
    // println("Using labels for SRL: " + labels.mkString(", "))
    labels.toArray
  }
}

object CluCoreProcessor {
  val OUTSIDE = "O"
  val EMPTY = ""

  val EMPTY_GRAPH = new GraphMap

  // These are the NE labels used to train the SRL model
  val NAMED_LABELS_FOR_SRL = Set(
    "B-PER", "I-PER",
    "B-ORG", "I-ORG",
    "B-LOC", "I-LOC",
    "B-MISC", "I-MISC"
  )
}
