package org.clulab.numeric

import org.clulab.dynet.Utils
import org.clulab.numeric.mentions.Norm
import org.clulab.processors.clu.CluProcessor

import scala.io.Source

object EvalTimeNorm {
  Utils.initializeDyNet()

  def runEval(proc: CluProcessor, ner: NumericEntityRecognizer,
              testFile: String): Double = {
    val timeNormEvalDir = "/org/clulab/numeric/TimeNormEvalSet"
    val goldStream = getClass.getResourceAsStream(s"$timeNormEvalDir/$testFile")
    val goldLines = Source.fromInputStream(goldStream).getLines()
    // Build a Map with the gold time expressions.
    // The key is the document name. The value is a Seq with the time expressions in the document
    val goldTimex = (for ((goldLine, goldIdx) <- goldLines.toSeq.zipWithIndex) yield {
      goldLine.split(",").map(_.trim) match {
        case Array(docId, startSpan, endSpan, startIntervalStr) =>
          (docId, (startSpan, endSpan, startIntervalStr))
      }
    }).groupBy(t => t._1).mapValues(_.map(_._2))
    // For each docId in goldTimex keys get parse the document and get the number of
    // gold time expressions, predicted time expressions and the intersection
    val valuesPerDocument = for (docId <- goldTimex.keys.toSeq.sorted) yield {
      val gold = goldTimex(docId).toSet
      val docStream = getClass.getResourceAsStream(s"$timeNormEvalDir/$docId/$docId")
      val docText = Source.fromInputStream(docStream).getLines().mkString("\n")
      val doc = proc.annotate(docText)
      val mentions = ner.extractFrom(doc)
      setLabelsAndNorms(doc, mentions)
      val prediction =  mentions.collect{
        case m: Norm if m.neLabel.equals("DATE") || m.neLabel.equals("DATE-RANGE") =>
          (m.startOffset.toString, m.endOffset.toString, m.neNorm)
      }.toSet
      val intersection = prediction.intersect(gold)
      (prediction.size, gold.size, intersection.size)
    }
    // Calculate the overall performance
    val totalValues = valuesPerDocument.reduce((x, y) => (x._1 + y._1, x._2 + y._2, x._3 + y._3))
    val precision = totalValues._3.toFloat / totalValues._1
    val recall = totalValues._3.toFloat / totalValues._2
    val fscore = 2 * precision * recall / (precision + recall)
    printf("Precision: %.3f\n", precision)
    printf("Recall: %.3f\n", recall)
    printf("F1 score: %.3f\n", fscore)
    fscore
  }

  protected def run(): Double = {
    val proc = new CluProcessor() // there are lots of options for this
    val ner = NumericEntityRecognizer()
    test(proc, ner)
  }

  def test(proc: CluProcessor, ner: NumericEntityRecognizer): Double = {
    runEval(proc, ner, "WorldModelersDatesRangesTimex.csv")
  }

  def main(args: Array[String]): Unit = run()
}
