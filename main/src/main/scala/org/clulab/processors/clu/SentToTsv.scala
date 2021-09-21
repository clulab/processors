package org.clulab.processors.clu

import org.clulab.processors.Sentence
import org.clulab.utils.{FileUtils, StringUtils}

import java.io.PrintWriter

/** Reads, tokenizes, and breaks into sentences a collection of text files */
object SentToTsv {
  val FILTER = "believ|belief|think|thought|accept|consider|conclude|doubt|reject|suppose|suggest|submit|theorize|offer|repudiate|view|sure|certain|convinced|confident|positive|hesitant|credible|convincing|believable|likely|provable|possible|probable|plausible|reasonable|conclusive|unsupportable|questionable|unquestionable|axiomatic|true|false|established|debatable|equivocal|dubious|suspect|incontrovertible|indubitable|skeptical|inconceivable|absurd|unimaginable|unthinkable|preposterous|wish|want|intend|desire|expect|anticipate|predict|look\\s+forward|worried|concerned|uneasy|apprehensive|fatalistic|pessimistic|sanguine|skeptical|hopeful|optimistic|prefer|favor".r

  def filter(sent: Sentence): Boolean = {
    for(w <- sent.words) {
      if(FILTER.findFirstMatchIn(w.toLowerCase()).isDefined) {
        return true
      }
    }
    false
  }

  def main(args: Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)

    val inputDir = props.getProperty("in")
    assert(inputDir != null)
    val outputTsv = props.getProperty("out")
    assert(outputTsv != null)

    val proc = new CluProcessor()
    val tsvWriter = new PrintWriter(outputTsv)
    var filtered = 0
    var total = 0
    for(file <- FileUtils.findFiles(inputDir, ".txt")) {
      val text = FileUtils.getTextFromFile(file)
      val doc = proc.mkDocument(text)
      var sid = 1
      for(sent <- doc.sentences) {
        if(filter(sent)) {
          tsvWriter.println(file.getName + "\t" + sid + "\t" + sent.words.mkString(" "))
          filtered += 1
        }
        sid += 1
        total += 1
      }
    }
    println(s"Scanned $total sentences. Found $filtered sentences.")

  }
}
