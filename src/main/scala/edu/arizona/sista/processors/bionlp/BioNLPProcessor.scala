package edu.arizona.sista.processors.bionlp

import java.util
import java.util.Properties
import java.util.regex.Pattern

import edu.arizona.sista.processors.bionlp.ner.{BioNER, RuleNER}
import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.struct.MutableNumber
import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConversions._

import BioNLPProcessor._

/**
 * A processor for biomedical texts, based on CoreNLP, but with different tokenization and NER
 * User: mihais
 * Date: 10/27/14
 */
class BioNLPProcessor (internStrings:Boolean = true,
                       withCRFNER:Boolean = true,
                       withRuleNER:Boolean = true,
                       withDiscourse:Boolean = false,
                       maxSentenceLength:Int = 100,
                       removeFigTabReferences:Boolean = true)
  extends CoreNLPProcessor(internStrings, basicDependencies = false, withDiscourse, maxSentenceLength) {

  //lazy val banner = new BannerWrapper
  lazy val postProcessor = new BioNLPTokenizerPostProcessor
  lazy val bioNer = BioNER.load(CRF_MODEL_PATH)
  lazy val ruleNer = RuleNER.load(RULE_NER_KBS)

  override def mkTokenizerWithoutSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize")
    addBioTokenizerOptions(props)
    new StanfordCoreNLP(props)
  }

  override def mkTokenizerWithSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize, ssplit")
    addBioTokenizerOptions(props)
    new StanfordCoreNLP(props)
  }

  def addBioTokenizerOptions(props:Properties) {
    props.put("tokenize.options", "ptb3Escaping=false")
    props.put("tokenize.language", "English")
  }

  /**
   * Implements the bio-specific post-processing steps from McClosky et al. (2011)
   * @param originalTokens Input CoreNLP sentence
   * @return The modified tokens
   */
  override def postprocessTokens(originalTokens:Array[CoreLabel]) = postProcessor.process(originalTokens)

  /**
   * Removes Figure and Table references that appear within parentheses
   * @param origText The original input text
   * @return The preprocessed text
   */
  override def preprocessText(origText:String):String = {
    if (!removeFigTabReferences) return origText

    var noRefs = origText
    // the pattern with parens must run first!
    noRefs = removeFigTabRefs(BioNLPProcessor.FIGTAB_REFERENCE_WITH_PARENS, noRefs)
    noRefs = removeFigTabRefs(BioNLPProcessor.FIGTAB_REFERENCE, noRefs)
    noRefs
  }

  /**
   * Removes references to Tables and Figures
   * @param pattern Fig/Tab pattern
   * @param text The original text
   * @return The cleaned text
   */
  def removeFigTabRefs(pattern:Pattern, text:String):String = {
    val m = pattern.matcher(text)
    val b = new StringBuilder
    var previousEnd = 0
    while(m.find()) {
      b.append(text.substring(previousEnd, m.start()))
      // white out the reference, keeping the same number of characters
      for(i <- m.start() until m.end()) b.append(" ")
      previousEnd = m.end()
    }
    if(previousEnd < text.length)
      b.append(text.substring(previousEnd))
    b.toString()
  }

  override def resolveCoreference(doc:Document): Unit = {
    // TODO: add domain-specific coreference here!
    doc.coreferenceChains = None
  }

  /**
   * Improve POS tagging in the Bio domain
   * @param annotation The CoreNLP annotation
   */
  override def postprocessTags(annotation:Annotation) {
    val sas = annotation.get(classOf[SentencesAnnotation])

    sas.foreach{ sa =>
      val tas = sa.get(classOf[TokensAnnotation]).toList.toArray

      //
      // some of our would-be verbs are mistagged...
      //
      tas.foreach{ ta =>
        val text = ta.originalText().toLowerCase
        text match {
          case ubiq if ubiq.endsWith("ubiquitinates") => ta.setTag("VBZ")
          case ubiqNom if ubiqNom.endsWith("ubiquitinate") => ta.setTag("VB")
          case hydro if hydro.endsWith("hydrolyzes") => ta.setTag("VBZ")
          case aids if aids.toLowerCase == "aids" && aids != "AIDS" => ta.setTag("VBZ")
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
    }
  }

  override def recognizeNamedEntities(doc:Document) {
    val annotation = namedEntitySanityCheck(doc)
    if(annotation.isEmpty) return

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

        // the actual sequence classification
        val bioNEs = bioNer.classify(inputSent).toArray

        // store labels in the CoreNLP annotation for the sentence
        var labelOffset = 0
        for (token <- coreNLPSentence) {
          token.setNER(bioNEs(labelOffset))
          labelOffset += 1
        }

        // store labels in our sentence
        // the rule-based NER labels take priority!
        if(ourSentence.entities.isDefined) {
          RuleNER.mergeLabels(ourSentence.entities.get, bioNEs)
        } else {
          ourSentence.entities = Some(bioNEs)
        }

        // post-processing
        postProcessNamedEntities(ourSentence)

        // TODO: we should have s.norms as well...

        sentenceOffset += 1
      }
    }
  }

  /**
   * Fix common NER errors
   */
  def postProcessNamedEntities(sent:Sentence) {
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
        println(s"FOUND fig ref from $start to $end")
        for(j <- start until end)
          seq(j) = RuleNER.OUTSIDE_LABEL
        i = end
      } else {
        i += 1
      }
    }

  }

  def isFigRef(lemmas:Array[String], offset:Int):Boolean = {
    assert(offset > 0)
    val m1 = POTENTIAL_FIGURE_TEXT.matcher(lemmas(offset - 1))
    val m2 = POTENTIAL_FIGURE_NUMBER.matcher(lemmas(offset))
    if(m1.matches() && m2.matches())
      return true
    false
  }

  def isEntity(offset:MutableNumber[Int], seq:Array[String]):Boolean = {
    if(offset.value >= 0 && (seq(offset.value).startsWith("B-") || seq(offset.value).startsWith("I-"))) {
      offset.value = findEntityStart(offset.value, seq) - 1
      return true
    }
    false
  }

  def findEntityStart(offset:Int, seq:Array[String]):Int = {
    var i = offset
    while(i > 0 && seq(i).startsWith("I-"))
      i -= 1
    i
  }

  def findEntityEnd(offset:Int, seq:Array[String]):Int = {
    var i = offset
    if(seq(i).startsWith("B-"))
      i += 1
    while(i < seq.length && seq(i).startsWith("I-"))
      i += 1
    i
  }

  def isCC(offset:MutableNumber[Int], tags:Array[String]):Boolean = {
    if(offset.value >= 0 && tags(offset.value) == "CC") {
      offset.value -= 1
      if(offset.value >= 0 && tags(offset.value) == ",")
        offset.value -= 1
      return true
    }
    false
  }

  def mkSent(sentence:Sentence):util.List[CoreLabel] = {
    val output = new util.ArrayList[CoreLabel]()
    for(i <- 0 until sentence.size) {
      val l = new CoreLabel()
      l.setWord(sentence.words(i))
      l.setTag(sentence.tags.get(i))
      l.setLemma(sentence.lemmas.get(i))
      output.add(l)
    }
    output
  }
}

object BioNLPProcessor {
  val FIGTAB_REFERENCE_WITH_PARENS = Pattern.compile("\\((\\s*see)?(\\s*supplementary)?\\s*(figure|table|fig\\.|tab\\.)[^\\)]*\\)", Pattern.CASE_INSENSITIVE)
  val FIGTAB_REFERENCE = Pattern.compile("\\s*see(\\s*supplementary)?\\s*(figure|table|fig\\.|tab\\.)\\s*[0-9A-Za-z\\.]+", Pattern.CASE_INSENSITIVE)
  val POTENTIAL_FIGURE_NUMBER = Pattern.compile("[a-z]*\\d+", Pattern.CASE_INSENSITIVE)
  val POTENTIAL_FIGURE_TEXT = Pattern.compile("(figure|figures|fig\\.?|figs\\.?)", Pattern.CASE_INSENSITIVE)

  val CRF_MODEL_PATH = "edu/arizona/sista/processors/bionlp/ner/bioner.dat"

  val RULE_NER_KBS = List( // knowledge for the rule-based NER; order is important: it indicates priority!
    "edu/arizona/sista/processors/bionlp/ner/Gene_or_gene_product.tsv",
    "edu/arizona/sista/processors/bionlp/ner/Family.tsv",
    "edu/arizona/sista/processors/bionlp/ner/Site.tsv",
    "edu/arizona/sista/processors/bionlp/ner/Cellular_component.tsv",
    "edu/arizona/sista/processors/bionlp/ner/Simple_chemical.tsv"
  )

  val NORMALIZED_LABELS = Map[String, String]( // needed to convert  the CRF's labels (from BioCreative) to our labels
    "B-GENE" -> "B-Gene_or_gene_product",
    "I-GENE" -> "I-Gene_or_gene_product"
  )
}

