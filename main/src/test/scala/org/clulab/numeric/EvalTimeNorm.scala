package org.clulab.numeric

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.numeric.mentions.{DateMention, DateRangeMention, MeasurementMention, Norm}
import org.clulab.utils.Sourcer


object EvalTimeNorm {
  Utils.initializeDyNet()

  def runEval(proc: CluProcessor, ner: NumericEntityRecognizer,
              testFile: String): Double = {
    val timeNormEvalDir = "/Users/zeyzhan/IdeaProjects/processors/main/src/test/resources/EvalTimeNorm"
    //val goldStream = getClass.getResourceAsStream(s"$timeNormEvalDir/$testFile")

    //val goldLines = Source.fromInputStream(goldStream).getLines
    val goldLines = Sourcer.sourceFromFilename(s"$timeNormEvalDir/$testFile").getLines()
    //goldLines.next() // Skip headers
    // Build a Map with the gold time expressions.
    // The key is tuple with the document name and the document creation time
    // The values is the a Seq with the time expressions (TimeNormScorer.Timex corresponding) in the document
    val goldTimex = (for ((goldLine, goldIdx) <- goldLines.toSeq.zipWithIndex) yield {
      goldLine.split(",").map(_.trim) match {
        case Array(docId, startSpan, endSpan, startIntervalStr) =>
          (docId, (startSpan, endSpan, startIntervalStr))
      }
    }).groupBy(t => t._1).mapValues(_.map(_._2))
    println(goldTimex)
    var matchers = 0
    var all_predictions = 0
    var all_gold_labels = 0
    // For each docId in goldTimex keys get the, parse the document and extract the time expressions found
    //val precisionRecalls = for (docId <- goldTimex.keys.toSeq.sorted) yield {
    for (docId <- goldTimex.keys.toSeq.sorted){
      println(docId)
      val docText = Sourcer.sourceFromFilename(s"$timeNormEvalDir/$docId/$docId").getLines().mkString("\n")
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
      val diff = predicted_output.toSet.diff(goldTimex(docId).toSet)
      val diff_2 = goldTimex(docId).toSet.diff(predicted_output.toSet)
      println("set_predictions.diff(set_annotations): ")
      for (d <- diff) println(docText.substring(d._1.toInt, d._2.toInt), d._3, d._1.toInt, d._2.toInt)
      println("set_annotations.diff(set_predictions): ")
      for (d <- diff_2) println(docText.substring(d._1.toInt, d._2.toInt), d._3, d._1.toInt, d._2.toInt)
      matchers += intersection.size
      all_predictions += predicted_output.toSet.size
      all_gold_labels += goldTimex(docId).toSet.size
      println("prediction: ", predicted_output.toSet)
      println("annotation label: ", goldTimex(docId).toSet)
      //println("diff: ", diff)
      //displayMentions(mentions, doc)
    }
    val precision = matchers.toFloat/all_predictions.toFloat
    val recall = matchers.toFloat/all_gold_labels.toFloat
    // Calculate the overall performance

    val fscore = 2 * precision * recall / (precision + recall)
    println("precision: ", precision)
    println("recall: ", recall)
    println("f1 score: ", fscore)
    fscore
  }

  protected def run(): Double = {

    val proc = new CluProcessor() // there are lots of options for this
    val ner = new NumericEntityRecognizer()
    //    val doc = proc.annotate(text)
    //    val mentions = ner.extractFrom(doc)
    //    setLabelsAndNorms(doc, mentions)

    runEval(proc, ner, "WorldModelersDatesRangesTimex.csv")
  }

  def test(): Double = run()

  def main(args: Array[String]): Unit = run()
}