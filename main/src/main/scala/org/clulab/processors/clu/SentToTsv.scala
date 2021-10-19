package org.clulab.processors.clu

import org.clulab.processors.Sentence
import org.clulab.utils.{FileUtils, StringUtils}
import scala.util.control.Breaks._
import org.clulab.dynet.Utils.initializeDyNet

import java.io.PrintWriter

/** Reads, tokenizes, and breaks into sentences a collection of text files */
object SentToTsv {
  val FILTER = "believe|belief|think|thought|accept|consider|conclude|doubt|reject|suppose|suggest|submit|theorize|offer|repudiate|view|sure|certain|convinced|confident|positive|hesitant|credible|convincing|believable|likely|provable|possible|probable|plausible|reasonable|conclusive|unsupportable|questionable|unquestionable|axiomatic|true|false|established|debatable|equivocal|dubious|suspect|incontrovertible|indubitable|skeptical|inconceivable|absurd|unimaginable|unthinkable|preposterous|wish|want|intend|desire|expect|anticipate|predict|look\\s+forward|worried|concerned|uneasy|apprehensive|fatalistic|pessimistic|sanguine|skeptical|hopeful|optimistic|prefer|favor".r
  // To be added through further conversation with Allegra
  val MENTAL_FILTER_OUT = ("we think that|we expect that|they think that|they expect that|we assume that|we|https|\\*\\*\\*|\\.\\.|>>|<<|\\+\\+\\+").r
  val remove_references = "references".r

  def filter(sent: Sentence): Boolean = {
    if(sent.words.size < 12 || sent.words.size > 35) {
      // Filter out short sentence and too long sentence (which might be just parse of the whole table.
      return false
    }
    for(w <- sent.words) {
      if(FILTER.findFirstMatchIn(w.toLowerCase()).isDefined) {
        return true
      }
    }
    false
  }

  def filter_for_mental_group(sent_text: String, pos_tags: String): Boolean = {
    if(MENTAL_FILTER_OUT.findFirstMatchIn(sent_text.toLowerCase()).isDefined) {
        return false
    }
    // Make sure it is a complete sentence and not just table information
    pos_tags.contains("VB") && pos_tags.contains("NN")
  }

  def filter_out_references(sent_text: String): Boolean = {
    if(remove_references.findFirstMatchIn(sent_text.toLowerCase()).isDefined) {
      return true
    }
    false
  }

  def main(args: Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)
    val inputDir = props.getProperty("in")
    assert(inputDir != null)
    val outputTsv = props.getProperty("out")
    assert(outputTsv != null)
    initializeDyNet()
    val proc = new CluProcessor()
    val tsvWriter = new PrintWriter(outputTsv)
    var filtered = 0
    var total = 0
    for(file <- FileUtils.findFiles(inputDir, ".txt")) {
      val text = FileUtils.getTextFromFile(file)
      var doc = proc.mkDocument(text)
      doc  = proc.annotate(doc)
      var sid = 1
      for(sent <- doc.sentences) {
        breakable {
          val sent_text = sent.words.mkString(" ").replace("\t", "").replace("\n", "")
          var pos_tags = ""
          sent.tags.foreach(tags => pos_tags = s"POS tags: ${tags.mkString(" ")}")
          if(filter_out_references(sent_text)) {
            // Ignore everything else in the document after the References section
            break
          } else {
            if(filter(sent)) {
              // Additional filter based on mental group's suggestions or other suggestions besides Paul's preliminary work.
              if(filter_for_mental_group(sent_text, pos_tags)) {
                tsvWriter.println(file.getName + "\t" + sid + "\t" + sent_text)
                filtered += 1
                tsvWriter.flush()
              }
            }
            sid += 1
            total += 1
          }
        }
      }
      println(s"Currently Scanned $total sentences. Found $filtered sentences.")
    }
    println(s"Totally Scanned $total sentences. Found $filtered sentences.")

  }
}
