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
    val goldLines = Source.fromInputStream(goldStream).getLines
    // Build a Map with the gold time expressions.
    // The key is tuple with the document name and the document creation time
    // The values is the a Seq with the time expressions (TimeNormScorer.Timex corresponding) in the document
    val goldTimex = (for ((goldLine, goldIdx) <- goldLines.toSeq.zipWithIndex) yield {
      goldLine.split(",").map(_.trim) match {
        case Array(docId, startSpan, endSpan, startIntervalStr) =>
          (docId, (startSpan, endSpan, startIntervalStr))
      }
    }).groupBy(t => t._1).mapValues(_.map(_._2))
    var matchers = 0
    var all_predictions = 0
    var all_gold_labels = 0
    // For each docId in goldTimex keys get the, parse the document and extract the time expressions found
    for (docId <- goldTimex.keys.toSeq.sorted) {
      val docStream = getClass.getResourceAsStream(s"$timeNormEvalDir/$docId/$docId")
      val docText = Source.fromInputStream(docStream).getLines().mkString("\n")
      val doc = proc.annotate(docText)
      val mentions = ner.extractFrom(doc)
      val mention_label = "DATE"
      val mention_label_range = "DATE-RANGE"
      setLabelsAndNorms(doc, mentions)
      val predicted_output = for {mention <- mentions
                                  if mention.isInstanceOf[Norm]
                                  if mention.asInstanceOf[Norm].neLabel == mention_label || mention.asInstanceOf[Norm].neLabel == mention_label_range} yield {
        (mention.startOffset.toString, mention.endOffset.toString, mention.asInstanceOf[Norm].neNorm)
      }
      val intersection = predicted_output.toSet.intersect(goldTimex(docId).toSet)
      matchers += intersection.size
      all_predictions += predicted_output.toSet.size
      all_gold_labels += goldTimex(docId).toSet.size

    }
    val precision = matchers.toFloat / all_predictions.toFloat
    val recall = matchers.toFloat / all_gold_labels.toFloat
    // Calculate the overall performance

    val fscore = 2 * precision * recall / (precision + recall)
    println("precision: ", precision)
    println("recall: ", recall)
    println("f1 score: ", fscore)
    fscore
  }

  protected def run(): Double = {
    val proc = new CluProcessor() // there are lots of options for this
    val ner = NumericEntityRecognizer()
    runEval(proc, ner, "WorldModelersDatesRangesTimex.csv")
  }

  def test(): Double = run()

  def main(args: Array[String]): Unit = run()
}
