package org.clulab.processors.bio

import org.clulab.processors.Sentence
import org.clulab.processors.clu.SentencePostProcessor

import scala.util.matching.Regex

/**
  * Fixes some common POS tagging mistakes in the bio domain (in place)
  *
  * Note: this class is used by the CRF-based BioNER to cleanup its training data (from BioCreative 2),
  *       through org.clulab.processors.bionlp.BioNLPPOSTaggerPostProcessor.
  *       This means that every time there are changes here, the CRF should be retrained. Tell Mihai.
  * User: mihais
  * Date: 9/23/17
  */
class BioPOSPostProcessor extends SentencePostProcessor {
  override def process(sentence: Sentence) {
    val hyphPat = """(^[a-zA-Z0-9-]+-[A-Z0-9][a-zA-Z0-9-]*)""".r
    val tags = sentence.tags.get

    for(i <- sentence.indices) {
      val text = sentence.words(i)
      val lower = text.toLowerCase()

      text match {
        case aids if lower == "aids" && aids != "AIDS" => tags(i) = "VBZ"
        case mutant if lower == "mutant" => tags(i) = "NN"
        case human if lower == "human" => tags(i) = "NN"

        // make sure that these verbs are treated as verbs with correct POS tag:
        case vb if lower.endsWith("acetylate")      => tags(i) = "VB"
        case vb if lower.endsWith("fanesylate")     => tags(i) = "VB"
        case vb if lower.endsWith("farnesylate")    => tags(i) = "VB"
        case vb if lower.endsWith("glycosylate")    => tags(i) = "VB"
        case vb if lower.endsWith("hydrolyze")      => tags(i) = "VB"
        case vb if lower.endsWith("hydroxylate")    => tags(i) = "VB"
        case vb if lower.endsWith("methylate")      => tags(i) = "VB"
        case vb if lower.endsWith("phosphorylate")  => tags(i) = "VB"
        case vb if lower.endsWith("ribosylate")     => tags(i) = "VB"
        case vb if lower.endsWith("sumoylate")      => tags(i) = "VB"
        case vb if lower.endsWith("translocate")    => tags(i) = "VB"
        case vb if lower.endsWith("ubiquitinate")   => tags(i) = "VB"
        case vb if lower.endsWith("acetylates")     => tags(i) = "VBZ"
        case vb if lower.endsWith("fanesylates")    => tags(i) = "VBZ"
        case vb if lower.endsWith("farnesylates")   => tags(i) = "VBZ"
        case vb if lower.endsWith("glycosylates")   => tags(i) = "VBZ"
        case vb if lower.endsWith("hydrolyzes")     => tags(i) = "VBZ"
        case vb if lower.endsWith("hydroxylates")   => tags(i) = "VBZ"
        case vb if lower.endsWith("methylates")     => tags(i) = "VBZ"
        case vb if lower.endsWith("phosphorylates") => tags(i) = "VBZ"
        case vb if lower.endsWith("ribosylates")    => tags(i) = "VBZ"
        case vb if lower.endsWith("sumoylates")     => tags(i) = "VBZ"
        case vb if lower.endsWith("translocates")   => tags(i) = "VBZ"
        case vb if lower.endsWith("ubiquitinates")  => tags(i) = "VBZ"

        // his and pro are amino acids (pos: nn)
        case aa if aa == "His" => tags(i) = "NN"
        case aa if aa == "Pro" => tags(i) = "NN"

        case _ => ()
      }

      //
      // change VBN to JJ if in between DT and NN
      // e.g.: "XRCC1 is phosphorylated by the co-immunoprecipitated DNA-PK" => "co-immunoprecipitated" should be JJ
      //
      for(i <- sentence.indices) {
        if(i > 0 && i < sentence.size - 1 &&
          tags(i) == "VBN" &&
          tags(i - 1).startsWith("DT") &&
          tags(i + 1).startsWith("NN")) {
          tags(i) = "JJ"
        }
      }

      //
      // parens must be tagged -LRB- and -RRB-
      // this improves parsing a lot!
      //
      for(i <- sentence.indices) {
        val text = sentence.words(i)
        if(LEFT_PARENS.findFirstMatchIn(text).nonEmpty) {
          tags(i) = "-LRB-"
        } else if(RIGHT_PARENS.findFirstMatchIn(text).nonEmpty) {
          tags(i) = "-RRB-"
        }
      }

      //
      // Capitalized hyphenated words at beginning of sentence -> NNP
      // e.g. "K-Ras phosphorylates p53."
      if(sentence.size > 0) {
        val text = sentence.words(0)
        if(hyphPat.findFirstIn(text).nonEmpty)
          tags(0) = "NNP"
      }
    }
  }

  val LEFT_PARENS: Regex = """^(\-LRB\-)|(\-LSB\-)|(-LCB-)|\(|\[|\{$""".r
  val RIGHT_PARENS: Regex = """^(\-RRB\-)|(\-RSB\-)|(-RCB-)|\)|\]|\}$""".r
}
