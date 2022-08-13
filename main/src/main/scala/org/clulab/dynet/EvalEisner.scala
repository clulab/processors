package org.clulab.dynet

import org.clulab.dynet.Utils.initializeDyNet
import org.clulab.sequences.ColumnReader
import org.clulab.utils.StringUtils

/** Evaluates the Eisner algorithm as an unlabeled parsing algorithm */
object EvalEisner extends App {
  val props = StringUtils.argsToMap(args)
  initializeDyNet()

  val headsModel = props.getOrElse("heads", throw new RuntimeException("Missing heads model!"))
  val heads = Metal(headsModel)

  val labelsModel = props.getOrElse("labels", throw new RuntimeException("Missing labels model!"))
  val labels = Metal(labelsModel)

  val testFile = props.getOrElse("test", throw new RuntimeException("Missing test file!"))
  val sentences = ColumnReader.readColumns(testFile)
  println(s"Read ${sentences.length} sentences.")
  val reader = new MetalRowReader
  val eisner = new Eisner
  val scoreCountsByLabel = new ScoreCountsByLabel

  for(sentence <- sentences) {
    val annotatedSentences = reader.toAnnotatedSentences(sentence, 0)

    for(as <- annotatedSentences) {
      val annotatedSentence = as._1
      val goldLabels = as._2.map(_.label)
      val constEmbeddings = ConstEmbeddingsGlove.mkConstLookupParams(annotatedSentence.words)
      val preds = eisner.ensembleParser(heads, Some(labels), annotatedSentence, constEmbeddings, 5, 0.6f, true)
      val predLabels = preds.map(_._1.toString)

      val sc = SeqScorer.f1(goldLabels, predLabels)
      scoreCountsByLabel.incAll(sc)
    }
  }

  println(s"Accuracy on ${sentences.length} sentences: ${scoreCountsByLabel.accuracy()}")
  println(s"Precision on ${sentences.length} sentences: ${scoreCountsByLabel.precision()}")
  println(s"Recall on ${sentences.length} sentences: ${scoreCountsByLabel.recall()}")
  println(s"Micro F1 on ${sentences.length} sentences: ${scoreCountsByLabel.f1()}")
  for(label <- scoreCountsByLabel.labels) {
    println(s"\tP/R/F1 for label $label (${scoreCountsByLabel.map(label).gold}): ${scoreCountsByLabel.precision(label)} / ${scoreCountsByLabel.recall(label)} / ${scoreCountsByLabel.f1(label)}")
  }


}
