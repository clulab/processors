package org.clulab.processors.bionlp.ner

import java.util

import org.clulab.processors.{Document, Sentence}
import HybridNER._
import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.pipeline.Annotation
import org.clulab.sequences.LexiconNER

import scala.collection.JavaConverters._

/**
  * Combines RuleNER and CRFNER into a, surprise surprise, hybrid NER
  * User: mihais
  * Date: 2/9/17
  * Last Modified: Update for Scala 2.12: java converters.
  */
class HybridNER(withCRFNER:Boolean, withRuleNER:Boolean) {
  lazy val bioNer = CRFNER.load(CRF_MODEL_PATH)
  lazy val ruleNer = KBLoader.loadAll(fromSerializedModel = false)

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
      val sas = annotation.get.get(classOf[SentencesAnnotation]).asScala
      var sentenceOffset = 0
      for (sa: CoreMap <- sas) {
        val ourSentence = doc.sentences(sentenceOffset) // our sentence
        val coreNLPSentence = sa.get(classOf[TokensAnnotation]).asScala // the CoreNLP sentence

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
          LexiconNER.mergeLabels(ourSentence.entities.get, bioNEs)
        } else {
          ourSentence.entities = Some(bioNEs)
        }

        if(verboseMerge)
          println(s"""ENTITIES AFTER MERGING: ${ourSentence.entities.get.mkString(" ")}""")

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
          bc2Labels(i) = LexiconNER.OUTSIDE_LABEL
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

  private def findEntityEnd(offset:Int, seq:Array[String]):Int = {
    var i = offset
    if(seq(i).startsWith("B-"))
      i += 1
    while(i < seq.length && seq(i).startsWith("I-"))
      i += 1
    i
  }
}

object HybridNER {
  private val CRF_MODEL_PATH = "org/clulab/processors/bionlp/ner/bioner.dat"
}
