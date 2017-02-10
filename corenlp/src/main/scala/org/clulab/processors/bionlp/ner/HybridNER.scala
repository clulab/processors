package org.clulab.processors.bionlp.ner

import java.util
import java.util.regex.Pattern

import org.clulab.processors.{Document, Sentence}
import HybridNER._
import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.pipeline.Annotation
import org.clulab.struct.MutableNumber
import org.slf4j.LoggerFactory


import scala.collection.JavaConversions._

/**
  * Combines RuleNER and CRFNER into a, surprise surprise, hybrid NER
  * User: mihais
  * Date: 2/9/17
  */
class HybridNER(withCRFNER:Boolean, withRuleNER:Boolean) {
  lazy private val bioNer = CRFNER.load(CRF_MODEL_PATH)
  lazy private val ruleNer = KBLoader.loadAll

  /** Runs the NER, and stores the output in place, in the .entities field in the sentences in the given Document */
  def recognizeNamedEntities(doc:Document, annotation:Option[Annotation]) {
    if(annotation.isEmpty) return
    val verboseMerge = false

    if(withRuleNER) {
      // run the rule-based NER on one sentence at a time
      for(sentence <- doc.sentences) {
        sentence.entities = Some(ruleNer.find(sentence))
      }
    }

    if (withCRFNER) {
      // run the CRF NER on one sentence at a time
      // we are traversing our sentences and the CoreNLP sentences in parallel here!
      val sas = annotation.get.get(classOf[SentencesAnnotation])
      var sentenceOffset = 0
      for (sa: CoreMap <- sas) {
        val ourSentence = doc.sentences(sentenceOffset) // our sentence
        val coreNLPSentence = sa.get(classOf[TokensAnnotation]) // the CoreNLP sentence

        // build the NER input
        val inputSent = mkSent(ourSentence)
        /*
        println("Input sentence:")
        for(i <- 0 until inputSent.size()) {
          val t = inputSent.get(i)
          println("\t" + t.word() + " " + t.tag() + " " + t.lemma())
        }
        */

        // the actual sequence classification
        val bioNEs = bioNer.classify(inputSent).toArray

        if(verboseMerge) {
          if (ourSentence.entities.isDefined)
            println( s"""ENTITIES FROM RULE-NER: ${ourSentence.entities.get.mkString(" ")}""")
          println( s""" ENTITIES FROM CRF-NER: ${bioNEs.mkString(" ")}""")
        }

        // store labels in the CoreNLP annotation for the sentence
        var labelOffset = 0
        for (token <- coreNLPSentence) {
          token.setNER(bioNEs(labelOffset))
          labelOffset += 1
        }

        // BC2 annotations include context information in gene names (e.g., species)
        // Here, we remove context info if they appear at the beginning of gene names,
        //   because they are annotated by our RuleNER, and we want both the context and the gene in our output!
        removeContextFromBeginningOfGenes(bioNEs, ourSentence.entities.get)

        // store labels in our sentence
        // the rule-based NER labels take priority!
        // TODO: change to prefer longest entity here??
        if(ourSentence.entities.isDefined) {
          RuleNER.mergeLabels(ourSentence.entities.get, bioNEs)
        } else {
          ourSentence.entities = Some(bioNEs)
        }

        if(verboseMerge)
          println(s"""ENTITIES AFTER MERGING: ${ourSentence.entities.get.mkString(" ")}""")

        // post-processing
        postProcessNamedEntities(ourSentence)

        // TODO: we should have s.norms as well...

        sentenceOffset += 1
      }
    }
  }

  private def mkSent(sentence:Sentence):util.List[CoreLabel] = {
    val output = new util.ArrayList[CoreLabel]()
    for(i <- 0 until sentence.size) {
      val l = new CoreLabel()
      l.setOriginalText(sentence.words(i))
      l.setWord(sentence.words(i))
      l.setTag(sentence.tags.get(i))
      l.setLemma(sentence.lemmas.get(i))
      output.add(l)
    }
    output
  }


  private def removeContextFromBeginningOfGenes(bc2Labels:Array[String], labelsWithContext:Array[String]): Unit = {
    assert(bc2Labels.length == labelsWithContext.length)
    val gene = "Gene_or_gene_product"
    var i = 0
    while(i < bc2Labels.length) {
      // found species names at the beginning of a gene name!
      if(bc2Labels(i) == s"B-$gene" && beginsContext(labelsWithContext(i))) {
        val contextEnd = findEntityEnd(i, labelsWithContext)
        while(i < contextEnd) {
          // Ok to set the context labels to O here; they will be picked again during the merge of ruleNer and CRF
          bc2Labels(i) = RuleNER.OUTSIDE_LABEL
          i += 1
        }
        if(i < bc2Labels.length && bc2Labels(i) == s"I-$gene") {
          // start the gene name after the species
          bc2Labels(i) = s"B-$gene"
        }
      } else {
        i += 1
      }
    }
  }

  private def beginsContext(label:String):Boolean = {
    if(label == "B-Species" || label == "B-Organ" || label == "B-CellType" || label == "B-CellLine" || label == "B-TissueType") true
    else false
  }

  /**
    * Fix common NER errors
    */
  private def postProcessNamedEntities(sent:Sentence) {
    val seq = sent.entities.get
    val tags = sent.tags.get
    val lemmas = sent.lemmas.get
    val words = sent.words

    //
    // IF: "A ,(optional) and B complex" THEN: make sure "complex" is labeled "O"
    // This pattern is considered a Binding event, and will be modeled by REACH
    //
    var i = 0
    while(i < seq.length) {
      val offset = new MutableNumber[Int](i - 1)
      if(lemmas(i) == "complex" &&
        isEntity(offset, seq) &&
        isCC(offset, tags) &&
        isEntity(offset, seq)) {
        seq(i) = RuleNER.OUTSIDE_LABEL
        seq(findEntityStart(i - 1, seq) - 1) = RuleNER.OUTSIDE_LABEL
      }
      i += 1
    }

    //
    // Single-character entities cannot exist in this domain
    //
    i = 0
    while(i < seq.length) {
      if(seq(i).startsWith("B-") &&
        (i == seq.length - 1 || ! seq(i + 1).startsWith("I-")) &&
        words(i).length == 1) {
        seq(i) = RuleNER.OUTSIDE_LABEL
      }
      i += 1
    }

    //
    // Figure references, e.g., "Figure S2", should not be labeled
    //
    i = 1
    while(i < seq.length) {
      if(isFigRef(lemmas, i) && (seq(i).startsWith("I-") || seq(i).startsWith("B-"))) {
        val start = findEntityStart(i, seq)
        val end = findEntityEnd(i, seq)
        // println(s"FOUND fig ref from $start to $end")
        for(j <- start until end)
          seq(j) = RuleNER.OUTSIDE_LABEL
        i = end
      } else {
        i += 1
      }
    }

    //
    // XML tag leftovers should not be labeled
    //
    i = 0
    while(i < seq.length) {
      if ((seq(i).startsWith("B-") || seq(i).startsWith("I-")) &&
        words(i).startsWith("XREF_")) {
        seq(i) = RuleNER.OUTSIDE_LABEL
      }
      i += 1
    }

    //
    // Some single-token entities should not be labeled when in lower case, or upper initial
    //
    i = 0
    while(i < seq.length) {
      if (seq(i).startsWith("B-") &&
        (i == seq.length - 1 || !seq(i + 1).startsWith("I-")) &&
        (isLowerCase(words(i)) || isUpperInitial(words(i))) &&
        KBLoader.ENTITY_STOPLIST.contains(words(i).toLowerCase)) {
        seq(i) = RuleNER.OUTSIDE_LABEL
      }
      i += 1
    }

  }

  private def isLowerCase(s:String):Boolean = {
    for(i <- 0 until s.length) {
      val c = s.charAt(i)
      if(Character.isLetter(c) && ! Character.isLowerCase(c))
        return false
    }
    true
  }

  private def isUpperInitial(s:String):Boolean = {
    if(s.length < 1) return false
    if(Character.isLetter(s.charAt(0)) && ! Character.isUpperCase(s.charAt(0)))
      return false

    for(i <- 1 until s.length) {
      val c = s.charAt(i)
      if(Character.isLetter(c) && ! Character.isLowerCase(c))
        return false
    }
    true
  }

  private def isFigRef(lemmas:Array[String], offset:Int):Boolean = {
    assert(offset > 0)
    val m1 = POTENTIAL_FIGURE_TEXT.matcher(lemmas(offset - 1))
    val m2 = POTENTIAL_FIGURE_NUMBER.matcher(lemmas(offset))
    if(m1.matches() && m2.matches())
      return true
    false
  }

  private def isEntity(offset:MutableNumber[Int], seq:Array[String]):Boolean = {
    if(offset.value >= 0 && (seq(offset.value).startsWith("B-") || seq(offset.value).startsWith("I-"))) {
      offset.value = findEntityStart(offset.value, seq) - 1
      return true
    }
    false
  }

  private def findEntityStart(offset:Int, seq:Array[String]):Int = {
    var i = offset
    while(i > 0 && seq(i).startsWith("I-"))
      i -= 1
    i
  }

  private def findEntityEnd(offset:Int, seq:Array[String]):Int = {
    var i = offset
    if(seq(i).startsWith("B-"))
      i += 1
    while(i < seq.length && seq(i).startsWith("I-"))
      i += 1
    i
  }

  private def isCC(offset:MutableNumber[Int], tags:Array[String]):Boolean = {
    if(offset.value >= 0 && tags(offset.value) == "CC") {
      offset.value -= 1
      if(offset.value >= 0 && tags(offset.value) == ",")
        offset.value -= 1
      return true
    }
    false
  }
}

object HybridNER {
  private val CRF_MODEL_PATH = "org/clulab/processors/bionlp/ner/bioner.dat"

  private val POTENTIAL_FIGURE_NUMBER = Pattern.compile("[a-z]*\\d+", Pattern.CASE_INSENSITIVE)
  private val POTENTIAL_FIGURE_TEXT = Pattern.compile("(figure|figures|fig\\.?|figs\\.?)", Pattern.CASE_INSENSITIVE)
}
