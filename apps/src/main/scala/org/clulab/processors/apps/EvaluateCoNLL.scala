package org.clulab.processors.apps

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.processors.{Document, Sentence}
import org.clulab.utils.{ColumnsToDocument, WrappedArraySeq}

import java.io.PrintWriter

object EvaluateCoNLL extends App {
  val WORD_POS = 0 // in the column format
  val TAG_POS = 1 // in the column format
  val ANNOTATION_POS = 0 // in the processors annotations produced by forward()

  val fileName = "test.txt"
  val pw = new PrintWriter(fileName + ".conll")

  val proc = new BalaurProcessor()
  val goldDoc = ColumnsToDocument.readFromFile(fileName, WORD_POS, TAG_POS, labelSetter, annotator, filterOutContractions = false)
  val sentences = goldDoc.sentences
  println(s"Read a doc with ${sentences.length} sentences.")
  //System.exit(1)

  for(i <- sentences.indices) {
    val words = sentences(i).words
    if(sentences(i).entities.isDefined) {
      val goldEnts = sentences(i).entities.get
      val annotations = proc.forward(words)(ANNOTATION_POS)
      val predEnts = WrappedArraySeq(annotations.map(_.head._1)).toImmutableSeq
      println("gold: " + goldEnts.mkString(", "))
      println("pred: " + predEnts.mkString(", "))
      if(goldEnts.length == predEnts.length) {
        for (j <- words.indices) {
          pw.println(s"${words(j)} ${goldEnts(j)} ${predEnts(j)}")
        }
        pw.println()
      }
    }
  }
  pw.close()

  def labelSetter(sent: Sentence, labels: Seq[String]): Sentence = {
    sent.copy(entities = Some(labels.toVector)) // must make a copy of labels here, hence the toVector
  }

  def annotator(doc: Document): Document = doc
}
