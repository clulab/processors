package org.clulab.processors.bionlp

import edu.stanford.nlp.ling.CoreLabel

/**
  * Fixes some common POS tagging mistakes in the bio domain (in place, using CoreLabel.setTag)
  * User: mihais
  * Date: 2/9/17
  */
class BioNLPPOSTaggerPostProcessor {
  /**
    * Fixes some common POS tagging mistakes (in place, using CoreLabel.setTag)
    * Note: this function is used by the CRF-based BioNER to cleanup its training data (from BioCreative 2).
    *       This means that everytime there are changes here, the CRF should be retrained. Tell Mihai.
    *
    * @param tas arrays of tokens stored as CoreNLP CoreLabel
    */
  def postprocessCoreLabelTags(tas:Array[CoreLabel]): Unit = {
    val hyphPat = """(^[a-zA-Z0-9-]+-[A-Z0-9][a-zA-Z0-9-]*)""".r

    //
    // some of our would-be verbs are mistagged...
    //
    tas.foreach{ ta =>
      val text = ta.originalText()
      text match {
        case aids if aids.toLowerCase == "aids" && aids != "AIDS" => ta.setTag("VBZ")
        case mutant if mutant.toLowerCase == "mutant" => ta.setTag("NN")

        // Modified by Enrique for context 07/20/15:
        case human if human.toLowerCase == "human" => ta.setTag("NN")

        // make sure that these verbs are treated as verbs with correct POS tag:
        case vb if vb.toLowerCase.endsWith("acetylate")      => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("fanesylate")     => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("glycosylate")    => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("hydrolyze")      => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("hydroxylate")    => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("methylate")      => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("phosphorylate")  => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("ribosylate")     => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("sumoylate")      => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("translocate")    => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("ubiquitinate")   => ta.setTag("VB")
        case vb if vb.toLowerCase.endsWith("acetylates")     => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("fanesylates")    => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("glycosylates")   => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("hydrolyzes")     => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("hydroxylates")   => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("methylates")     => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("phosphorylates") => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("ribosylates")    => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("sumoylates")     => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("translocates")   => ta.setTag("VBZ")
        case vb if vb.toLowerCase.endsWith("ubiquitinates")  => ta.setTag("VBZ")

        // his and pro are amino acids (pos: nn)
        case aa if aa == "His" => ta.setTag("NN")
        case aa if aa == "Pro" => ta.setTag("NN")

        case _ => ()
      }
    }

    //
    // change VBN to JJ if in between DT and NN
    // e.g.: "XRCC1 is phosphorylated by the co-immunoprecipitated DNA-PK" => "co-immunoprecipitated" should be JJ
    //
    for(i <- tas.indices) {
      if(i > 0 && i < tas.length - 1 &&
        tas(i).tag() == "VBN" &&
        tas(i - 1).tag().startsWith("DT") &&
        tas(i + 1).tag().startsWith("NN")) {
        tas(i).setTag("JJ")
      }
    }

    //
    // parens must be tagged -LRB- and -RRB-
    // this improves parsing a lot!
    //
    tas.foreach { ta =>
      val text = ta.originalText()
      text match {
        case "(" => ta.setTag("-LRB-")
        case ")" => ta.setTag("-RRB-")
        case _ =>
      }
    }

    //
    // Capitalized hyphenated words at beginning of sentence -> NNP
    // e.g. "K-Ras phosphorylates p53."
    tas.foreach { ta =>
      val text = ta.originalText()
      text match {
        case hyphen if ta.index == 1 && (hyphPat findFirstIn hyphen).nonEmpty => ta.setTag("NNP")
        case _ => ()
      }
    }
  }
}
