
package org.clulab.processors.clucore

import edu.stanford.nlp.ie.regexp.NumberSequenceClassifier
import edu.stanford.nlp.ie.QuantifiableEntityNormalizer
import edu.stanford.nlp.ling.CoreAnnotations.NormalizedNamedEntityTagAnnotation
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel}
import edu.stanford.nlp.util.ArrayCoreMap

import java.util
import java.util.Collections
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.collection.mutable.ArrayBuffer

class NumericEntityRecognizer {
  val numSeqClassifier = new NumberSequenceClassifier()

  def classify(words: IndexedSeq[String],
               tags: IndexedSeq[String],
               startCharOffsets: IndexedSeq[Int],
               endCharOffsets: IndexedSeq[Int],
               docDateOpt: Option[String]): (IndexedSeq[String], IndexedSeq[String]) = {

    assert(words != null && tags != null)
    assert(words.length == tags.length)
    assert(startCharOffsets != null && endCharOffsets != null)
    assert(words.length == startCharOffsets.length)
    assert(words.length == endCharOffsets.length)

    //
    // based on NERClassifierCombiner.classifyWithGlobalInformation() (line 272)
    //

    // create the list of CoreLabels
    val coreLabels = mkCoreLabels(words, tags, startCharOffsets, endCharOffsets)

    // outputs will have NamedEntityTagAnnotation and NormalizedNamedEntityTagAnnotation set
    val outputs: Option[Iterable[CoreLabel]] =
      try {
        // run the numeric classifier
        val outputCoreLabels = recognizeNumberSequences(coreLabels, docDateOpt)

        // generate normalized values
        QuantifiableEntityNormalizer.addNormalizedQuantitiesToEntities(outputCoreLabels, false, true)

        Some(outputCoreLabels.asScala)
      } catch {
        case e: Exception =>
          println("Ignored an exception in SUTimeAPI. This means some numeric expressions were not extracted in the text below:")
          println(words.mkString(" "))
          e.printStackTrace()
          None
        case e: AssertionError =>
          println("Ignored an assertion in SUTimeAPI. This means some numeric expressions were not extracted in the text below:")
          println(words.mkString(" "))
          e.printStackTrace()
          None
      }

    // fetch labels and norms
    mkOutputs(outputs, words.length)
  }

  def mkOutputs(outputs: Option[Iterable[CoreLabel]], length: Int): (IndexedSeq[String], IndexedSeq[String]) = {
    val labels = new ArrayBuffer[String]()
    val norms = new ArrayBuffer[String]()

    if(outputs.isEmpty) {
      for(_ <- 0 until length) {
        labels += "O"
        norms += ""
      }
    } else {
      for (output <- outputs.get) {
        val label = output.get(classOf[CoreAnnotations.NamedEntityTagAnnotation])
        var norm = output.get(classOf[NormalizedNamedEntityTagAnnotation])
        if(norm == null) norm = ""
        //println(output.word() + " " + label + " " + norm)

        TODO: add B- and I-

        labels += label
        norms += norm
      }
    }

    (labels, norms)
  }

  def recognizeNumberSequences(coreLabels: util.List[CoreLabel], docDateOpt: Option[String]): java.util.List[CoreLabel] = {

    //
    // sets AnswerAnnotation, TimexAnnotation
    //
    val outputs =
    if(docDateOpt.isEmpty) {
      numSeqClassifier.classify(coreLabels)
    } else {
      //
      // See: DocDateAnnotator line 116
      // annotation.set(classOf[CoreAnnotations.DocDateAnnotation], foundDocDate)
      //
      val doc = new ArrayCoreMap()
      doc.set(classOf[CoreAnnotations.DocDateAnnotation], docDateOpt.get)
      numSeqClassifier.classifyWithGlobalInformation(coreLabels, doc, null)
    }

    // AnswerAnnotation -> NERAnnotation
    copyAnswerFieldsToNERField(outputs)

    outputs
  }

  /** Mimics NERClassifierCombiner.copyAnswerFieldsToNERField, which is private */
  def copyAnswerFieldsToNERField(l: java.util.List[CoreLabel]): Unit = {
    for(i <- 0 until l.size()) { // TODO: if this is not an ArrayList, this is expensive
      val m = l.get(i)
      m.set(classOf[CoreAnnotations.NamedEntityTagAnnotation], m.get(classOf[CoreAnnotations.AnswerAnnotation]))
      var labelProb = m.get(classOf[CoreAnnotations.AnswerProbAnnotation])
      if(labelProb == null) labelProb = -1.0
      val labelToProb = Collections.singletonMap(m.get(classOf[CoreAnnotations.NamedEntityTagAnnotation]), labelProb)
      m.set(classOf[CoreAnnotations.NamedEntityTagProbsAnnotation], labelToProb)
    }
  }

  def mkCoreLabels(words: Seq[String],
                   tags: Seq[String],
                   startCharOffsets: Seq[Int],
                   endCharOffsets: Seq[Int]): java.util.List[CoreLabel] = {
    val coreLabels = new util.ArrayList[CoreLabel]()

    var tokenOffset = 0
    for(i <- words.indices) {
      val coreLabel = new CoreLabel()
      coreLabel.setWord(words(i))
      coreLabel.setValue(words(i))
      coreLabel.setTag(tags(i))
      coreLabel.setIndex(tokenOffset + 1) // Stanford counts tokens starting from 1
      coreLabel.setSentIndex(1) // only 1 sentence at a time here
      coreLabel.setBeginPosition(startCharOffsets(i))
      coreLabel.setEndPosition(endCharOffsets(i))

      coreLabels.add(coreLabel)
      tokenOffset += 1
    }

    coreLabels
  }
}
