package org.clulab.numeric

import com.typesafe.config.ConfigValueFactory
import org.clulab.dynet.Utils
import org.clulab.processors.Sentence
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.struct.Interval
import org.scalatest.{FlatSpec, Matchers}

import scala.util.matching.Regex
import java.time.LocalDateTime
import scala.collection.JavaConverters._
import scala.io.Source

object EvalTimeNorm {
  Utils.initializeDyNet()

  def runEval(proc: CluProcessor, ner: NumericEntityRecognizer,
              testFile: String): Double = {
    val timeNormEvalDir = "/Users/zeyzhan/IdeaProjects/processors/main/src/test/resources"
    val goldStream = getClass.getResourceAsStream(s"$timeNormEvalDir/$testFile")

    println(goldStream)
    val goldLines = Source.fromInputStream(goldStream).getLines
    goldLines.next() // Skip headers
    // Build a Map with the gold time expressions.
    // The key is tuple with the document name and the document creation time
    // The values is the a Seq with the time expressions (TimeNormScorer.Timex corresponding) in the document
    val goldTimex = (for ((goldLine, goldIdx) <- goldLines.toSeq.zipWithIndex) yield {
      goldLine.split(",").map(_.trim) match {
        case Array(docId, startSpan, endSpan, startIntervalStr, endIntervalStr) =>
          (docId, (startSpan, endSpan, startIntervalStr, endIntervalStr))
      }
    }).groupBy(t => t._1).mapValues(_.map(_._2))


    // For each docId in goldTimex keys get the, parse the document and extract the time expressions found
    val precisionRecalls = for (docId <- goldTimex.keys.toSeq.sorted) yield {
      println(docId)
      val docStream = getClass.getResourceAsStream(s"$timeNormEvalDir$docId.txt")
      val docText = Source.fromInputStream(docStream).getLines().mkString("\n")
      val doc = proc.annotate(docText)
      val mentions = ner.extractFrom(doc)
      setLabelsAndNorms(doc, mentions)
    }

    // Calculate the overall performance

    val fscore = 0.0
    fscore
  }

  protected def run(): Double = {

    val proc = new CluProcessor() // there are lots of options for this
    val ner = new NumericEntityRecognizer()
    //    val doc = proc.annotate(text)
    //    val mentions = ner.extractFrom(doc)
    //    setLabelsAndNorms(doc, mentions)

    runEval(proc, ner, "WorldModelersDates.csv")
  }

  def test(): Double = run()

  def main(args: Array[String]): Unit = run()
}